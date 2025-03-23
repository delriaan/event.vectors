# `inform()`: A function that calculates the Shannon self-information of the input
# vector in three steps:
# 1. The proportional representation of the frequency of each value in the vector is calculated
# 2. The Shannon self-information of the proportion is calculated
# 3. The geometric mean of the Shannon self-information is calculated
inform <- \(x, output = c("mean", "values")){
	as_geo_mean <- match.arg(output) == "mean"

	# Step 1:
	p <- as.vector(x) |> table() |> prop.table()
	# Step 2:
	info <- -log(p, base = 2)
	# Step 3:
	if (as_geo_mean){
		return(book.of.utilities::calc.geo_mean(as.vector(info)))
	} else {
		return(list(dy = as.integer(names(p)), p = p, info = info) |> lapply(as.vector))
	}
}

# `E_Idev()`
# - @param g Geometric PMF vector
# - @param idv Value vector
# `g x idv` below yields the expectation of `idv`
# The conditional `g == max(g)` is used because arguments `g` and `idv`
#		are vector inputs over the entire data set. While a maximal `g`
# 	exists, it is mapped to different values along `idv`
# The geometric mean (`calc.geo_mean()`) is chosen as `idv` is logarithmic.
E_Idev <- \(g, idv, obs_ctrl){
		res <- (g * idv)[g == max(g, na.rm = TRUE)] |>
			book.of.utilities::calc.geo_mean() |>
			magrittr::multiply_by(length(g) >= obs_ctrl$min_size)

			log(res + 1, base = 2)
	}

# Information encoder:
#' Encode Shannon Information
#'
#' \code{info_encoder} converts the \code{i} into distinct series based on monotonically occurring breaks.\cr
#' The cumulative proportionality within each series is converted into Shannon information as "bits".
#' @note \code{info_encoder()} requires the input to be the differences of an \emph{ordered} vector
#'
#' @param i (numeric) The input vector of differences
#' @param info.only (logical) \code{TRUE} returns the information column only
#' @param data.only (logical) \code{TRUE} returns columns \code{series} and \code{cyl} only, and \code{FALSE} returns everything
#' @return If \code{info.only} is \code{TRUE}, a numeric vector of Shannon information is returned, otherwise a \code{\link[data.table]{data.table}} with columns \code{series}, \code{cyl}, and \code{info} is returned.
#'
#' @family Signal Processor Functions
info_encoder <- function(i, info.only = FALSE, data.only = info.only){
	if (!is.atomic(i)){ i <- unlist(i, use.names = FALSE) }

	# Given a set of differenced values (over the differences of the
	# (possibly grouped) monotonically increasing input), a "signal break" is
	# assumed as a switch from positive to negative:
		is_signal_break <- c(0, diff(i)) < 0;

	# `signal_data`: The number of contiguous parts of the series before a break
		.out_names <- c("dy", "cyl", "series", "info")
		signal_data <- array(as.numeric(0), dim = c(length(i), length(.out_names)), dimnames = list(NULL, .out_names))
		dy <- as.numeric(i)
		cyl <- as.numeric(cumsum(is_signal_break) + 1)
		series <- info <- rep.int(0, length(dy))
		info <- rep.int(0, length(dy))

		cyl_idx <- unique(cyl) |> lapply(\(x){ which(cyl == x) })

		invisible(sapply(cyl_idx, \(i){
			series[i] <<- sum(dy[i])
			info[i] <<- inform(dy[i])
			}))

		if (info.only == -1){
			debug(inform)
			info.only == FALSE
		}

	# Determine the type of output:
	if (info.only){
			return(list(info = info))
		} else {
			if (data.only){
				data.table::data.table(cyl = cyl, series = series)
			} else {
				data.table::data.table(cyl = cyl, series = series, info = info)
			}
		}
}

# Geometric PMF:
#' Geometric Probability Mass Function
#'
#' @param x The cumulative sum of differences, representing the number of attempts before success.\cr These values imply contiguous parts of the series before a break.
#' @param p The cumulative proportion of the current group's difference-proportion mapped from \code{response.pmf}.
#'
#' @return \eqn{p(1 - p)^x}
#' @family Signal Processor Functions
geo_pmf <- function(x, p){

	assertive::assert_all_are_in_left_open_range(x = p, lower = 0, upper = 1, na_ignore = TRUE, severity = "stop");
	assertive::assert_all_are_greater_than_or_equal_to(x = x, y = 0, severity = "stop");
	x <- as.integer(x)

	# Return the probability mass function of the geometric distribution
	# Reference URL: https://en.wikipedia.org/wiki/Geometric_distribution
	p * (1 - p)^x;
}

#' Score Algorithm Output
#'
#' \code{score_algorithm_output} is a scoring function definition used in function \code{\link{exec_algorithm}}.
#'
#' @param algo_output (data frame) The output of \code{\link{exec_algorithm}}.
#' @param obs_ctrl (list) (See \code{\link{break_signal}})
#'
#' @return A \code{\link[data.table]{data.table}} with the following columns:\cr
#' \describe{
#' \item{k}{The vector of \eqn{k} values}
#' \item{d_Idev}{\eqn{\Delta{\text{Idev}}}}
#' \item{d2_Idev}{\eqn{\Delta^{2}{\text{Idev}}}}
#' \item{k_score}{The break score \eqn{\to} information deviation equation/model}
#' \item{k_sz}{The number of observations in each \eqn{k}}
#' \item{geo_pmf.max}{The maximal value of the geometric probability mass function returned by \code{geo_pmf}}
#' \item{Idev_wmean}{The weighted-mean of \eqn{\text{Idev}} using as weights the geometric PMF (\code{geo_pmf}) in order to find the optimal \eqn{k}}
#' \item{d_kscore, d_Idev_wmean}{For each \eqn{k}, \eqn{\Delta{k_\text{score}}} and \eqn{\Delta{\text{Idev}_\text{wmean}}}, respectively}
#' \item{tot_score}{The mutual relative proportionality of curvatures over \eqn{k} and \eqn{\text{Idev}_\text{wmean}}. The relative proportionality of \eqn{\Delta^{2}{\text{Idev}_\text{wmean}}} is subtracted from 1 to invert the proportionality.  This makes \code{tot_score} the product of the \emph{minimum} of one curvature and the \emph{maximum} of the othe (gradient descent is not used to determine the optimal \eqn{k} because \eqn{k} is a discrete value.)}
#' \item{best_k}{The value of \eqn{k} corresponding to the maximal value of \code{tot_score}}
#' \item{alt_k}{The next best \eqn{k}}
#' }
#'
#' @family Signal Processor Functions
score_algorithm_output <- function(algo_output, obs_ctrl){

	if (!data.table::is.data.table(algo_output)){
			X <- data.table::as.data.table(algo_output)
		} else {
			X <- data.table::copy(algo_output)
		}

	# :: Filter criteria ====
		row_filter <- X[
			, (k <= obs_ctrl$max_k) &
					(k > 0) &
					(d_Idev >= 0) & # Increasing or constant
					(d2_Idev <= 0) # Concave or local maximum
			]

	# :: Observations per `k` (i.e., conditioned) ====
		X <- X[
			(row_filter)
			, .(
				k_score = E_Idev(g = geo_pmf, idv = Idev, obs_ctrl)
				, k_sz = .N
				, geo_pmf.max = max(geo_pmf, na.rm = TRUE)
				, Idev_wmean = weighted.mean(Idev, geo_pmf, na.rm = TRUE)
				)
			, keyby = k
			][
			# Interim row filter and data subset preparation
			(k_score > 0), .SD[order(k, k_score)] |> unique()
			]

	# :: Differences (d'/dk) ====
		X[, `:=`(d_kscore = c(0, diff(k_score))/c(1, diff(k))
						, d_Idev_wmean = c(0, diff(Idev_wmean))/c(1, diff(k))
						)
			]
	# :: Difference of differences (d"/dk) ====
		X[, `:=`(d2_kscore = c(0, diff(d_kscore))
						, d2_Idev_wmean = c(0, diff(d_Idev_wmean))
						)
			]

	# :: Total Score ====
		X[, `:=`(tot_score =
						 	(1 - book.of.utilities::ratio(abs(d2_Idev_wmean), type = of.max)) *
						 	book.of.utilities::ratio(d2_kscore, type = of.max)
						 , best_k = -1
						 , alt_k = -1
						 )
			]

	# :: "Best" and alternate break values derivation ====
		X[, `:=`(
					best_k = k * (tot_score == max(tot_score, na.rm = TRUE))
					, alt_k = k * (tot_score >= quantile(tot_score, 0.9, names = FALSE)) *
							(tot_score != max(tot_score, na.rm = TRUE))
					)
		]
}

# Algorithm engine definition:
#' Execute Algorithm
#'
#' \code{exec_algorithm} executes the algorithm for a given data set and number of cross-assignment folds.
#' The following functions are called in the order listed below:\cr
#' \enumerate{
#' \item{\code{\link{info_encoder}}}
#' \item{\code{\link{geo_pmf}}}
#' \item{\code{\link{score_algorithm_output}}}
#' }
#'
#' @param Data The input data
#' @param obs_ctrl (See \code{\link{break_signal}})
#' @param ... Arguments for internal use
#' @param chatty (logical) Should the function be verbose?
#'
#' @return A \code{\link[data.table]{data.table}} object containing scoring information.
#' @family Signal Processor Functions
exec_algorithm <- function(Data, obs_ctrl, ..., chatty = FALSE){
		env <- rlang::caller_env();
		.outnames <- c("cyl", "series", "grp.info", "k", "geo_pmf", "Idev", "d_Idev", "d2_Idev")

		grp <- Data$grp
		grp_idx <- purrr::map(unique(grp) |> rlang::set_names(), \(x) which(grp == x))

		Data <- as.matrix(Data[, map(.SD[, !"grp"], as.numeric)]) %>%
			cbind(
				replicate(length(.outnames), rep.int(0, nrow(.))) |>
					magrittr::set_attr("dimnames", list(grp, .outnames))
				)

		.pbar <- if (chatty){
								list(format = "{cli::pb_spin} Augmenting with grouped info by cycle | Elapsed: {cli::pb_elapse} (Eta: {cli::pb_eta})")
							} else { NULL}

		purrr::walk(grp_idx, \(i){
				Data[i, c("cyl", "series", "grp.info")] <<- as.matrix(info_encoder(Data[i, "dy"], info.only = FALSE))
			}, .progress = .pbar)

		# :: Update Data by each element of 'grp_idx': ----
			purrr::walk(grp_idx, \(i){
				# Current subset of matrix 'Data':
				j <- Data[i,, drop = FALSE];

				# Cycle map:
					z <- j[ , "cyl"] %>% outer(unique(.) |> rlang::set_names(), `==`)

				# Within-group cumulative "attempts" subsequently passed to geometric PMF calculation
					j[, "k"] <- cumsum(j[, "dy"])

				# Within-group geometric PMF and break information deviation
					j[, "geo_pmf"] <- geo_pmf(x = j[, "series"], p = j[, "p"])
					j[, "Idev"] <- (j[, "info"] - j[, "grp.info"])^2

				# Within-group break-cycle information deviation change
					if (length(i) > 1){
						dk <- diff(j[, "k"])
						j[, "d_Idev"] <- c(0, diff(j[, "Idev"]) / dk)
						j[, "d2_Idev"] <- c(0, diff(j[, "d_Idev"]) / dk)
					}

				# Update `Data`
					Data[i, ] <<- j
				})

		# :: Return ----
		return(Data)
}

# The main algorithm function (exported):
#' Signal Processor
#'
#' \code{signal_processor} processes the "signal" of a "break_signal" object.
#'
#' @param object A "break_signal" \code{\link{S7}} object
#' @param ... Arguments for internal use
#'
#' @return The modified "break_signal" object with prescribed slots populated from the results of processing the "signal" (see \code{\link{break_signal}}).
#' @family Signal Processor Functions
#'
#' @export
signal_processor <- function(object, ...){
	S7::check_is_S7(object);
	trace <- list()

	# :: 0. User argument handling ----
		if (!all(c("min_size", "max_k") %in% names(object@obs_ctrl))){
			.nms <- c("min_size", "max_k")

			msg <- paste0(
				"`obs_ctrl` should be a list with named elements 'min_size' and 'max_k': missing "
				, paste(setdiff(.nms, names(object@obs_ctrl)) |> sprintf(fmt = "'%s'"), collapse = ", ")
				)

			stop(msg)
		}

		# `obs_ctrl`: Alias of object@obs_ctrl:
			makeActiveBinding("obs_ctrl", \(){
				object@obs_ctrl |>
					purrr::modify_at("min_size", \(x) magrittr::set_attr(x, "label", "Minimum grouped size to process")) |>
					purrr::modify_at("max_k", \(x) magrittr::set_attr(x, "label", "Maximum break allowed: \nrequires domain knowledge as this is an emperical-analytic task"))
				}, env= environment());

		# `y`: Alias of object@y:
			assertive::assert_any_are_true(
				class(object@y) %in% c("factor", "Date", "POSIXct", "POSIXlt", "character", "numeric", "integer")
				)
			makeActiveBinding("y", \() check_signal(object@y), env= environment());

			# `check_signal`: Ensures that the signal is numeric, either by position matching or type conversion
			check_signal <- \(s){
				switch(
					class(s)[1]
					, character = match(s, s)
					, factor = levels(s)[s] |> match(levels(s))
					, POSIXct = lubridate::seconds(s) |> as.numeric()
					, POSIXlt = lubridate::seconds(s) |> as.numeric()
					, as.numeric(s)
					)
			}

		# y_grp: Alias of object@grp:
			makeActiveBinding("y_grp", \() object@grp, env= environment());

	# :: 1. Grouped differentials of the observed measurements <grouped_response> ----
	  trace <- list()

	  # `grouped_response`
	    y_space <- unique(y_grp)
	    y_mtrx <- array(0, dim = c(length(y_grp), length(y_space)), dimnames = list(y_grp, y_space))

	    spsUtil::quiet(sapply(y_space, \(ys){
	        # browser()
	        i <- which(y_grp == ys)

	        if (rlang::is_empty(i)) return(NULL)

	        dy <- c(0, diff(y[i]))

	        y_mtrx[i, ys] <<- dy
	      }))

	  grouped_response <- (\(i){
	    data.table::data.table(grp = names(i), dy = i)
	    })(rowSums(y_mtrx))

	  # Checkpoint:
	  if (!any(grouped_response$dy > 0)){
	    stop("No breaks found in the observed measurements (all values are <= 0)");
	    } else {
	      grouped_response %<>% .[(dy > 0)]

	      # Save grouped response differentials to slot `object@k`
	      object@k <- grouped_response$dy;
	    }

	# :: 2. Global proportional response values and information encoding <response_pmf> ----
	  # `response_pmf` is the global proportional response values calculated as follows:
	  # 2a. Calculate the global proportional response values and self-information vector:
	    response_pmf <- data.table::as.data.table(inform(grouped_response$dy, output = "values"))
	    # Add to the trace objects:
	    trace$response_pmf <- response_pmf
	  # 2b. Merge the output of step 2 into prior object `grouped_response`
	    # Note that `:=` updates in place:
	    spsUtil::quiet(grouped_response[response_pmf, on = "dy", `:=`(p = p, info = info), by = .EACHI])
	  	trace$grouped_response <- grouped_response

	# :: 3. Derive resultant dataset ----
	  # `info_encoder()` is called by `exec_algorithm()`
	  .temp <- exec_algorithm(Data = data.table::copy(grouped_response), obs_ctrl = obs_ctrl)

	  .score <- score_algorithm_output(.temp, obs_ctrl)

	  viz <- plotly::plot_ly(
	    .score, x = ~k
	    , y = ~tot_score
	    , type = "scatter"
	    , mode = "markers"
	    , size = ~c(10, 15, 20)[1 + ifelse(k == best_k, 2, ifelse(k %in% alt_k, 1, 0))]
	    , color = ~c("K", "Alt. K", "Best K")[1 + ifelse(k == best_k, 2, ifelse(k %in% alt_k, 1, 0))]
	    , colors = c("#00009ACC", "#009A00FF", "#99999955")
	    , symbol = ~1 + ifelse(k == best_k, 2, ifelse(k %in% alt_k, 1, 0))
	    , symbols = c("square", "diamond", "circle")
	    , hoverinfo = "text"
	    , hovertext = ~sprintf(
	        "<b>Total Score (k = %s)</b>: %.2f<br><b>&#x2202;<sup>2</sup>k-score</b>: %.4f<br><b>&#x2202;<sup>2</sup>Wgt. Mean Idev</b>: %.2f"
	        , k, tot_score, d2_kscore, d2_Idev_wmean
	        )
	    ) |>
	    plotly::layout(
	      margin = list()
	      , xaxis = list(title = list(text = "k", font = list(family = "Georgia", color = I("#222222"))))
	      , yaxis = list(title = list(text = "Total Score", font = list(family = "Georgia", color = I("#222222"))))
	      )

	  # Leave everything else as-is:
	  if (!hasName(.score, "best_k") || rlang::is_empty(.score[(best_k > 0), unique(best_k)])){
	    message("Could not optimize `k`: returning the input as-is ...");
	    } else {
	      i <- .score[(k == best_k), which = TRUE]
	      object@data <- .score |> magrittr::set_attr("viz", viz)

	    # Assign additional values to `object`
	      object@obs_ctrl <- obs_ctrl;

	      object@best_k <- object@data[(best_k > 0), unique(best_k)]

	      object@alt_k <- object@data[, unique(alt_k[alt_k > 0 & alt_k != object@best_k])]

	      object@k_sz <- object@data$k_sz

	      object@score <- object@data$k_score
	    }

	  gc();

	# :: 4. Return `object` invisibly ----
	  (invisible(object))
}
