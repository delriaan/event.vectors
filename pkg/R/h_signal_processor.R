# Information encoder:
info_encoder <- function(i, info.only = FALSE, data.only = info.only){
	#' Encode Shannon Information
	#'
	#' \code{info_encoder} converts the \code{i} into distinct series based on monotonically occurring breaks.\cr
	#' The cumulative proportionality within each series is converted into Shannon information as "bits".
	#' @note \code{info_encoder()} requires the input to be the differences of an \emph{ordered} vector
	#'
	#' @param i (numeric) The input vector of differences
	#' @param info.only (logical) \code{TRUE} returns the information column only
	#' @param data.only (logical) \code{TRUE} returns columns \code{series} and \code{cyl} only, and \code{FALSE} returns everything
	#' @return If \code{info.only} is \code{TRUE}, a numeric vector of Shannon information is returned, otherwise a data frame with columns \code{series}, \code{cyl}, and \code{info} is returned.
	#' @family Signal Processor Functions


	if (!is.atomic(i)){
		i <- unlist(i, use.names = FALSE)
	}

	# Given a set of differenced values (over the differences of the
	# (possibly grouped) monotonically increasing input), a "signal break" is
	# assumed as a switch from positive to negative:
	is_signal_break <- c(0, diff(i)) < 0;

	# `signal_data`: The number of contiguous parts of the series before a break
	signal_data <- data.table::data.table(
									dy = i
									, cyl = book.of.utilities::count.cycles(is_signal_break, reset = FALSE)
									)[, series := sum(dy), by = cyl]

	# `inform()`: A function that calculates the Shannon self-information of the input
	# vector in three steps:
	# 1. The proportional representation of the frequency of each value in the vector is calculated
	# 2. The Shannon self-information of the proportion is calculated
	# 3. The geometric mean of the Shannon self-information is calculated
	inform <- \(x){
		# Step 1:
		res <- as.vector(x) |> 
			table() |> 
			book.of.utilities::ratio(sort.type = "num_label") |>
		# Step 2:
			log(base = 2) |>
			magrittr::multiply_by(-1) |>
			as.vector()
		# Step 3:
		book.of.utilities::calc.geo_mean(res)
	}

	if (info.only == -1){
		debug(inform)
		info.only == FALSE
	}

	# Determine the type of output:
	if (info.only){
		signal_data[, .(info = inform(dy)), by = cyl][, .(info)]
	} else {
		if (data.only){
			signal_data[, !c("dy")]
		} else {
			signal_data[, info := inform(dy), by = cyl][, !c("dy")]
		}
	}
}

# Geometric PMF:
geo_pmf <- function(x, p){
	#' Geometric Probability Mass Function
	#'
	#' @param x The cumulative sum of differences, representing the number of attempts before success.\cr These values imply contiguous parts of the series before a break.
	#' @param p The cumulative proportion of the current group's difference-proportion mapped from \code{response.pmf}.
	#'
	#' @return \eqn{p(1 - p)^x}
	#' @family Signal Processor Functions

	assertive::assert_all_are_in_left_open_range(x = p, lower = 0, upper = 1, na_ignore = TRUE, severity = "stop");
	assertive::assert_all_are_greater_than_or_equal_to(x = x, y = 0, severity = "stop");
	x <- as.integer(x)

	# Return the probability mass function of the geometric distribution
	# Reference URL: https://en.wikipedia.org/wiki/Geometric_distribution
	p * (1 - p)^x;
}

score_algorithm_output <- function(algo_output, obs_ctrl){
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
	algo_output[
		# Filter criteria ====
		(k <= obs_ctrl$max_k) &
			(k > 0) &
			(d_Idev >= 0) & # Increasing or constant
			(d2_Idev <= 0) # Concave or local maximum
		# Observations per `k` ====
		, .(
			k_score = (\(g, idv){
				# @param g Geometric PMF vector
				# @param idv Value vector

				# `g x idv` below yields the expectation of `idv`
				# The conditional `g == max(g)` is used because arguments `g` and `idv`
				#		are vector inputs over the entire data set.  While a maximal `g`
				# 	exists, it is mapped to different values along `idv`
				# The geometric mean (`calc.geo_mean()`) is chosen as `idv` is logarithmic.
				(g * idv)[g == max(g, na.rm = TRUE)] |>
					book.of.utilities::calc.geo_mean() |>
					magrittr::multiply_by(.N >= obs_ctrl$min_size) |>
					magrittr::add(1) |>
					log(base = 2)
			})(g = geo_pmf, idv = Idev)
			, k_sz = .N
			, geo_pmf.max = max(geo_pmf, na.rm = TRUE)
			, Idev_wmean = weighted.mean(Idev, geo_pmf, na.rm = TRUE)
			)
		, by = k
		][
		# Interim row filter and data subset preparation ====
		(k_score > 0), .SD[order(k, k_score)] |> unique()
		][
		# Slope (d'/dk) ====
		, `:=`(d_kscore = c(0, diff(k_score))/c(1, diff(k))
					, d_Idev_wmean = c(0, diff(Idev_wmean))/c(1, diff(k))
					)
		][
		# Curvature (d"/dk) ====
		, `:=`(d2_kscore = c(0, diff(d_kscore))
					, d2_Idev_wmean = c(0, diff(d_Idev_wmean))
					)
		][
		# Total Score ====
		, `:=`(tot_score =
					 	(1 - book.of.utilities::ratio(abs(d2_Idev_wmean), type = of.max)) *
					 	book.of.utilities::ratio(d2_kscore, type = of.max)
					 )
		][
		# "Best" and alternate break values derivation ====
		, `:=`(
				best_k = k[tot_score == max(tot_score, na.rm = TRUE)] |> unique()
				, alt_k = k * (book.of.utilities::ratio(tot_score, type = cumulative) >= 0.9) *
						(tot_score != max(tot_score, na.rm = TRUE))
				)
		];
}

# Algorithm engine definition:
exec_algorithm <- function(Data, obs_ctrl, cl_size = 1, ...){
	#' Execute Algorithm
	#'
	#' \code{exec_algorithm} executes the algorithm for a given data set and number of cross-assignment folds. The following functions are called in the order listed below:\cr
	#' \enumerate{
	#' \item{\code{\link{info_encoder}}}
	#' \item{\code{\link{geo_pmf}}}
	#' \item{\code{\link{score_algorithm_output}}}
	#' }
	#'
	#' @param Data The input data
	#' @param obs_ctrl (See \code{\link{break_signal}})
	#' @param cl_size (integer|1) The number of parallel processes to use
	#' @param ... Arguments for internal use
	#'
	#' @return A \code{\link[data.table]{data.table}} object containing scoring information.
	#' @family Signal Processor Functions

	env <- rlang::caller_env();
	.debug <- FALSE;
	if (hasName(list(...), ".debug")){ 
		.debug <- list(...)[[".debug"]] 
	}
	if (.debug) browser()

	# Generate holdout sets by group ----
	fold_map <- unique(Data$grp);

	if (length(fold_map) == 1){
		fold_map <- unique(Data$grp) |> rlang::set_names();
	} else {
		fold_map <- purrr::imap(fold_map, \(x, i) fold_map[-i]) |>
			rlang::set_names(unique(Data$grp));
	}

	# Augment the data:
	Data[
		# Encode information by group
		, `:=`(c("cyl", "series", "grp.info"), info_encoder(dy, info.only = FALSE))
		, by = grp
		];

	# Define the "engine":
	# `incl` refers to the cross-validation fold to "include"
	cv_fun <- \(incl){
			res <- Data[(grp %in% incl)]
			if (prod(dim(res)) == 0) return(NULL)
		
			tryCatch({
				res[
					, `:=`(
							# Within-group cumulative "attempts" subsequently passed to geometric PMF calculation ====
							k = cumsum(dy)
							# if any infinite values are found replace them with the maximum non-infinite value
							, grp.info = ifelse(is.infinite(grp.info), max(grp.info[!is.infinite(grp.info)]), grp.info)
							)
					, by = .(cyl, grp)
					][
					# Within-group geometric PMF and break information deviation
					, `:=`(
							geo_pmf = geo_pmf(x = k, p = p)
							, Idev = (info - grp.info)^2
							)
					, by = .(cyl, grp)
					][
					# Column selection ====
					, .(cyl, grp, series, k, info, geo_pmf, Idev)
					][
					# Within-group break-cycle information deviation slope ====
					, d_Idev := c(0, diff(Idev))
					, by = .(cyl, grp)
					][
					# Within-group break-cycle information deviation curvature ====
					, d2_Idev := c(0, diff(d_Idev))
					, by = .(cyl, grp)
					] |>
					score_algorithm_output(obs_ctrl = obs_ctrl)
				}, error = \(e){ x <- NULL; attr(x, "error", e); return(x) }
			)
	}

	# :: Parallelism topography (depends on user argument `cl_size`) ====
	# Determine the number of cluster nodes to use for parallel processing
	# based on the argument `cl_size`.
	if (cl_size <= 1L || length(fold_map) == 1L || !"parallelly" %in% rownames(installed.packages())){
		# No 'else' branch is needed when forcibly returning a value.
		return(f(fold_map) |> purrr::compact())
	}

	# Assign the cluster object to the calling environment to allow for manual termination in the event of an error:
	assign("cl", parallelly::makeClusterPSOCK(workers = cl_size, autoStop = TRUE), envir = env);

	parallel::clusterSetRNGStream(cl = env$cl, sample(-1E5:1E5, 1));

	# Export objects to cluster environment:
	parallel::clusterExport(
		cl = env$cl
		, varlist = c("obs_ctrl", "Data", "info_encoder", "geo_pmf", "score_algorithm_output")
		, envir = environment()
		);

	parallel::clusterEvalQ(cl = env$cl, expr = {
		library(magrittr);
		library(book.of.workflow);
		library(data.table);
		library(event.vectors)
	});

	cli::cli_alert_info("Created cluster `cl` in the calling environment")

	# Generate and score data by CV fold exclusion ----
	# cyl: The cycle identifier
	# grp: The group identifier
	res <- parallel::clusterApplyLB(x = fold_map, cl = env$cl, fun = cv_fun) |>
		purrr::compact() |>
		# Combine data and return
		data.table::rbindlist(idcol = "holdout_group");
	
	# Clean up parallelism objects:
	if ("cl" %in% ls(env)){
		parallel::stopCluster(env$cl)
		rm(cl, envir = env)
	}

	if (rlang::is_empty(res)){
		cli::cli_alert_danger("Error in crating output: returning `NULL`")
		return(NULL)
	} else {	
		return(res)
	}
}

# The main algorithm function (exported):
signal_processor <- function(object, cl_size = 1, ...){
	#' Signal Processor
	#'
	#' \code{signal_processor} processes the "signal" of a "break_signal" object.
	#'
	#' @param object A "break_signal" \code{\link{S7}} object
	#' @param cl_size (integer|1) The number of parallel workers to use during processing if greater than one.
	#' @param ... Arguments for internal use
	#'
	#' @return The modified "break_signal" object with prescribed slots populated from the results of processing the "signal" (see \code{\link{break_signal}}).
	#' @family Signal Processor Functions
	#'
	#' @export
	S7::check_is_S7(object);
	.debug <- FALSE;
	if (hasName(list(...), ".debug")){ 
		.debug <- list(...)[[".debug"]] 
	}
	if (.debug) browser()

	# :: User argument handling ====
	if (!all(c("min_size", "max_k") %in% names(object@obs_ctrl))){
		.nms <- c("min_size", "max_k");
		msg <- paste0(
						"`obs_ctrl` should be a list with named elements 'min_size' and 'max_k': missing "
						, paste(setdiff(.nms, names(object@obs_ctrl)) |> sprintf(fmt = "'%s'"), collapse = ", ")
						);
		
		stop(msg);
	}

	# obs_ctrl: Alias of object@obs_ctrl:
	makeActiveBinding("obs_ctrl", \(){
		object@obs_ctrl |>
			purrr::modify_at("min_size", \(x) magrittr::set_attr(x, "label", "Minimum grouped size to process")) |>
			purrr::modify_at("max_k", \(x) magrittr::set_attr(x, "label", "Maximum break allowed: \nrequires domain knowledge as this is an emperical-analytic task"))
		}, env= environment());

	# y: Alias of object@y:
	assertive::assert_any_are_true(
		class(object@y) %in% c("factor", "Date", "POSIXct", "POSIXlt", "character", "numeric", "integer")
		)
	
	makeActiveBinding("y", \() check_signal(object@y), env= environment());
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

	# :: Grouped differentials of the observed measurements ====
	grouped_response <- {
		split(y, f = sapply(y_grp, paste, collapse = "::")) |>
			lapply(\(i){
				# Used a 'length-1' check on argument `i` to prevent empty values from
				# returning when the split size is length-1.
				list(dy = if (rlang::has_length(i, 1)){ i } else { c(0, diff(as.numeric(i))) })
			}) |>
			(\(i) i[sapply(i, \(x) length(unlist(x)) >= obs_ctrl$min_size)])() |>
			data.table::rbindlist(idcol = "grp")
		}

	if (!any(grouped_response$dy > 0)){
		stop("No breaks found in the observed measurements (all values are <= 0)");
	} else{
		grouped_response %<>% dplyr::filter(dy > 0);

		# Save grouped response differentials to slot `object@k`
		object@k <- grouped_response$dy;
	}

	# :: Global proportional response values & information encoding ====
	# `response.pmf` is the global proportional response values calculated as follows:
	# if (.debug){ browser(); }

	# 1. Calculate the global proportional response values
	response.pmf <- table(grouped_response$dy) |>
		. => magrittr::divide_by(., sum(., na.rm = TRUE)) |>

	# 2. Calculate the information encoding of the global proportional response values
		(\(i){
			i[order(as.numeric(names(i)))] |>
					data.table::as.data.table() |>
					data.table::setnames(c("dy", "p")) |>
					purrr::modify_at("dy", as.numeric)
		})();

	# 3. Merge the output of step 2 into prior object `grouped_response`
	# Note that `:=` updates in place:
	spsUtil::quiet(grouped_response[
			response.pmf[, info := -log(p)]
			, on = "dy"
			, `:=`(p = p, info = info)
			, by = .EACHI
			]);

	# :: Derive resultant dataset ====
	if (.debug) browser();

	# Populate `.temp`:
	suppressWarnings(.temp <- exec_algorithm(
			Data = data.table::copy(grouped_response)
			, cl_size = cl_size
			, obs_ctrl = obs_ctrl
			, .debug = .debug
			));

	if (!hasName(.temp, "best_k") || rlang::is_empty(.temp[(best_k > 0), unique(best_k)])){
		message("Could not optimize `k`: returning the input as-is ...");
	} else {
		object@data <- .temp[(
				best_k == {
					table(best_k) |>
					sort(decreasing = TRUE) |>
					names() |>
					data.table::first() |>
					as.numeric() |>
					max(na.rm = TRUE)
				}
			)];

		# :: Assign additional values to `object`, clean up, and return the object invisibly ====
		object@obs_ctrl <- obs_ctrl;
		object@best_k <- object@data$best_k[1];
		object@alt_k <- .temp[!(best_k %in% object@best_k) & (k > 1), unique(best_k)];
		object@k_sz <- object@data$k_sz;
		object@score <- object@data$k_score;
	}

	gc();

	# Return `object` invisibly
	return(invisible(object));
}
