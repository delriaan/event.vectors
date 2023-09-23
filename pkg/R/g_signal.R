# S7 Properties
class_properties <- { list(
	y = S7::new_union(S7::class_numeric, S7::class_factor, S7::class_integer)
	, k = S7::new_property(class = S7::new_union(S7::class_numeric, S7::class_factor, S7::class_integer), default = 0)
	, grp = S7::class_any
	, obs_ctrl = S7::new_property(class = S7::class_list, default = list(min_size = 1L, max_k = 2L))
	, best_k = S7::new_property(class = S7::class_numeric, default = numeric())
	, alt_k = S7::new_property(class = S7::class_numeric, default = numeric())
	, k_sz = S7::new_property(class = S7::class_numeric, default = numeric())
	, score = S7::new_property(class = S7::class_numeric, default = numeric())
	, data = S7::new_property(class = S7::class_any, default = NULL)
	, plot = S7::new_property(class = S7::class_function, getter = function(self){
			plotly::plot_ly(
				data = self@data
				, split = ~k
				, x = ~(\(i){ i[i != 0] <- sign(i[i != 0]) * log10(abs(i[i != 0])); i })(d2_Idev_wmean)
				, y = ~d2_kscore
				, size = ~tot_score * (1 + (2 * (k %in% self@alt_k)) + (4 * (k == self@best_k)))
				, stroke = I("#000000")
				, hovertext = ~glue::glue("<b>k:</b> {k}{ifelse(k == self@best_k, '<sup> Best</sup>', ifelse(k %in% self@alt_k, '<sup> Alt</sup>', ''))}<br><b>Score:</b> {round(tot_score, 4)}")
				, type = "scatter"
				, mode = "markers"
				, width = 720
				, height = 600
				) |>
				plotly::config(mathjax = "cdn") |>
				plotly::layout(
					margin = list(t = -5, b = -5)
					, title = list(text = plotly::TeX("\\text{Break Score vs. Weighted-Mean Squared Information Deviation}_{\\text{ Size }\\to \\text{Total Score}}"))
					, xaxis = list(
						title = list(
							text = plotly::TeX("\\bar{I}^{''}_{\\text{log}_{10}}"))
						, showgrid = FALSE
						)
					, yaxis = list(
						title = list(
							text = plotly::TeX("{\\Omega^k}^{''}"))
						, showgrid = FALSE
						)
					, legend = list(title = list(text = plotly::TeX("\\enspace\\enspace{k}")))
					)
		})
	)}

#' Break Signal
#'
#' Functions \code{break_signal()} and \code{signal_processor()} are designed to select the best break of a “signal”, observed differences in the values of an ordered sequence. A “break” is simply a change in sign of the differences: the “best” break is the one that indicates the process that generates of observations has changed state.
#'
#' @slot y The un-transformed, monotonically ordered vector
#' @slot grp A vector containing group assignments along \code{y}
#' @slot obs_ctrl (list) Observation data control list having the following recognized elements:\cr
#' \itemize{
#' \item{\code{kmax}: The upper limit for breaks to allow under consideration. This should primarily be set to control the effecte of extreme values that appear frequently enough to be non-trivial but are due to confounding factors not related to the generative process under study.}
#' \item{\code{k_size_min}: The minimum observation size at each k to allow}
#' }
#'
#' @slot best_k,alt_k (numeric) Entropy-based optimized (and next best) break in the signal (passively set by \code{\link{signal_processor}})
#' @slot k_sz (numeric) The observation size at each \code{k} (passively set by \code{\link{signal_processor}})
#' @slot score (numeric[]) The scoring vector at each \code{k} (passively set by \code{\link{signal_processor}})
#' @slot data The generated data for which scores are generated (passively set by \code{\link{signal_processor}})
#' @slot plot A \code{\link[plotly]{plot_ly}} visualization after break optimization (passively set by \code{\link{signal_processor}})
#'
#' @export
break_signal <- S7::new_class(
	name = "break_signal"
	, package = "event.vectors"
	, properties = class_properties
	);

signal_processor <- function(object, ..., nfolds = 1, cl_size = 1, .debug = FALSE){
	#' Signal Processor
	#'
	#' @param object A "break_signal" \code{\link{S7}} object
	#' @param ... (Ignored)
	#' @param nfolds Number of folds to use during data generation and scoring
	#' @param cl Cluster object created from package \code{parallelly}
	#'
	#' @return The original "break_signal" object with prescribed slots populated from the results of processing the "signal" (see \code{\link{break_signal}})
	#'
	#' @export
	S7::check_is_S7(object);

	# :: User argument handling ====
	y <- object@y;
	y_grp <- object@grp;

	obs_ctrl <- object@obs_ctrl |>
		purrr::modify_at("min_size", \(x) magrittr::set_attr(x, "label", "Minimum grouped size to process")) |>
		purrr::modify_at("max_k", \(x) magrittr::set_attr(x, "label", "Maximum break allowed: \nrequires domain knowledge as this is an emperical-analytic task"))

	# :: Function definitions ====
	# Information encoder
	info_encoder <- function(i, info.only = FALSE, data.only = info.only){
		# Encode Shannon Information
		#
		# \code{info_encoder} converts the \code{i} into distinct series based on monotonically occurring breaks.\cr
		# The cumulative proportionality within each series is converted into Shannon information as "bits".
		# @note \code{info_encoder()} requires the input to be the differences of an \emph{ordered} vector
		#
		# @param i (numeric) The input vector of differences
		# @param info.only (logical) \code{TRUE} returns the information column only
		# @param data.only (logical) \code{TRUE} returns columns \code{series} and \code{cyl} only, and \code{FALSE} returns everything
		#

		is_signal_break <- c(0, diff(i)) < 0;

		signal_data <- data.table::data.table(
			dy = i
			, cyl = book.of.utilities::count.cycles(is_signal_break, reset = FALSE)
			)[, series := sum(dy), by = cyl]

		inform <- \(x, z){
			# Use each value of `x` (differences) to condition `z` (series) when `x` is
			# less than or equal to the current value `i`.  The result is converted
			# into an information bit.
			sapply(x, \(i){
				book.of.utilities::ratio(x <= i, type = "cumulative", d = 6) |>
					log(base = 2) |>
					magrittr::multiply_by(-1) |>
					sum(na.rm = TRUE)
			})
		}

		if (info.only){
			signal_data[, .(info = inform(dy, series)), by = cyl][, .(info)]
		} else {
			if (data.only){
				signal_data[, !c("dy")]
			} else {
				signal_data[, info := inform(dy, series), by = cyl][, !c("dy")]
			}
		}
	}

	# Geometric PMF
	geo_pmf <- function(x, p){
		# Geometric Probability Mass Function
		#
		# @param x The cumulative sum of differences, representing the number of attempts before success.\cr These values imply contiguous parts of the series before a break.
		# @param p The cumulative proportion of the current group's difference-proportion mapped from `response.pmf`.
		#
		# @return \code{p * (1 - p)^x}

		assertive::assert_all_are_in_left_open_range(x = p, lower = 0, upper = 1, na_ignore = TRUE, severity = "stop")
		assertive::assert_all_are_greater_than_or_equal_to(x = x, y = 0, severity = "stop")
		p * (1 - p)^x;
	}

	# Algorithm engine definition
	exec_algorithm <- function(Data, nfolds){
		# Execute Algorithm
		#
		# @param Data The input data
		# @param nfolds THe number of cross-assignment folds
		#
		# @return A \code{\link[data.table]{data.table}} object containing folded data generation and scoring

		assertive::assert_all_are_greater_than_or_equal_to(nfolds, 1, severity = "stop");

		if (nfolds == 1){ nfolds <- min(c(5, Data$grp |> data.table::uniqueN())) }
		fold_map <- data.table::data.table(seq_len(nfolds), Data$grp |> unique()) |> data.table::setnames(c("fold_id", "grp"));

		X <- Data[
			, `:=`(c("cyl", "series", "grp.info")
						 # Encode information ====
						 , info_encoder(dy, info.only = FALSE))
			, by = grp
			][
			# Assign CV folds gy group identifier ====
			fold_map, on = "grp", fold_id := fold_id, by = .EACHI
			];

		# Generate and score data by fold exclusion ====
		fold_map$fold_id |>
			unique() |>
			rlang::set_names() |>
			purrr::map(\(exclude_fold_id){
				X[(!fold_id %in% exclude_fold_id)][
					, `:=`(
						# Within-group cumulative "attempts" subsequently passed to geometric PMF calculation ====
						k = cumsum(dy)
						, grp.info = (\(arg){
							if (any(is.infinite(arg))){
								arg[is.infinite(arg)] <- max(arg[!is.infinite(arg)]);
							}
							arg;
						})(grp.info)
					)
					, by = .(cyl, grp)
				][
					# Within-group geometric PMF and break information deviation ====
					, `:=`(geo_pmf = geo_pmf(x = k, p = p)
								 , Idev = (info - grp.info)^2
					)
					, by = .(cyl, grp)
				][
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
					score_algorithm_output()
			}) |>
			# Combine data and return
			data.table::rbindlist(idcol = "exclude_fold_id");
	}

	# Scoring function definition
	score_algorithm_output <- function(algo_output){
		# Score Algorithm Output
		#
		# @param algo_output The algorithm output created from \code{exec_algorithm}
		#
		#
		algo_output[
		# Filter criteria ====
		(k <= obs_ctrl$max_k) &
			(k > 0) &
			(d_Idev >= 0) & # Increasing or constant
			(d2_Idev <= 0) # Concave or local maximum
		, .(
			# Break score (k_score): information deviation equation/model ====
			k_score = (\(g, EX){
				EX[g == max(g, na.rm = TRUE)] |>
					mean(na.rm = TRUE) |>
					magrittr::multiply_by(.N >= obs_ctrl$min_size) |>
					magrittr::add(1) |>
					log(base = 2)
			})(g = geo_pmf, EX = geo_pmf * Idev)
			# Observations per `k` ====
			, k_sz = .N
			, geo_pmf.max = max(geo_pmf, na.rm = TRUE)
			# PMF-weighted mean of break information deviation ====
			# This derives the expected value of squared information deviation (`Idev`)
			#   and is used to find the optimal `k`
			, Idev_wmean = weighted.mean(Idev, geo_pmf, na.rm = TRUE)
			)
		, by = k
		][
		# Interim row filter and data subset preparation ====
		(k_score > 0)
		, .SD[order(k, k_score)] |> unique()
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
		# Total Score ===
		# `tot_score` is the mutual relative proportionality of curvatures over `k` and `Idev_wmean`
		, `:=`(tot_score = (1 - book.of.utilities::ratio(abs(d2_Idev_wmean), type = "of.max", d = 6)) *
					 	book.of.utilities::ratio(d2_kscore, type = "of.max", d = 6))
		][
		, `:=`(
				# "Best" and alternate break values derivation ====
				best_k = k[tot_score == max(tot_score, na.rm = TRUE)] |> unique()
				, alt_k = k * (book.of.utilities::ratio(tot_score, type = "cumulative", d = 6) >= 0.9) *
					(tot_score != max(tot_score, na.rm = TRUE))
				)
		];
	}

	# :: Grouped differentials of the observed measurements ====
	grouped_response <- split(y, f = y_grp) |>
		lapply(\(i) list(dy = c(0, diff(as.numeric(i))))) %>%
		.[sapply(., \(x) length(unlist(x)) >= obs_ctrl$min_size)] |>
		data.table::rbindlist(idcol = "grp") |>
		dplyr::filter(dy > 0)

	object@k <- grouped_response$dy;

	# :: Proportional response values & information encoding ====
	response.pmf <- table(grouped_response$dy) %>% magrittr::divide_by(sum(., na.rm = TRUE));
	response.pmf <- response.pmf[as.numeric(names(response.pmf)) |> order()] |>
		data.table::as.data.table() |>
		data.table::setnames(c("dy", "p")) |>
		purrr::modify_at("dy", as.numeric);

	spsUtil::quiet(response.pmf[, info := -log(p)]);

	# Merge `response.pmf` into `grouped_response`
	spsUtil::quiet(grouped_response[response.pmf, on = "dy", `:=`(p = i.p, info = i.info), by = .EACHI]);

	# :: Parallelism topography (depends on user argument `cl_size`) ====
	.logi_vec <- (cl_size > 1L) & (data.table::uniqueN(grouped_response$g) > 1);

	if (.logi_vec){
		# Global assignment to allow for manual termination
		cl <- parallelly::makeClusterPSOCK(workers = cl_size, autoStop = TRUE);

		parallel::clusterSetRNGStream(cl = cl);

		parallel::clusterExport(
			cl = cl
			, varlist = c("info_encoder", "geo_pmf", "exec_algorithm", "score_algorithm_output", "nfolds", "response.pmf")
			, envir = environment()
			);

		spsUtil::quiet({
			parallel::clusterSplit(cl = cl, seq = unique(grouped_response$g)) |>
				purrr::walk(\(grps){
					rlang::expr(parallel::clusterEvalQ(cl = cl, expr = {
						library(magrittr);
						library(book.of.workflow);
						library(data.table);
						library(book.of.utilities);

						rm(list = c("X")[exists("X")]);

						grouped_response <- !!data.table::as.data.table(grouped_response[(grp %in% grps)]);
					})) |> eval();
				});
		});

		cat("[INFO] Created cluster `cl` in the calling environment\n");
	}

	# :: Derive resultant dataset ====
	if (.debug){ browser(); }

	.temp <- if (.logi_vec){
			spsUtil::quiet({
				parallel::clusterEvalQ(cl = cl, exec_algorithm(data.table::copy(grouped_response), nfolds = nfolds)) |>
					purrr::compact() |>
					data.table::rbindlist()
			})
		} else {
			exec_algorithm(data.table::copy(grouped_response), nfolds = nfolds);
		}

	# :: Greedy selection of `best_k` ====
	scored_result <- .temp[(best_k == max(na.rm = TRUE, table(best_k) |> sort(decreasing = TRUE) |> names() |> data.table::first()))];

	# :: Assign scored results to S7 object, clean up, and return the object invisibly ====
	object@obs_ctrl <- obs_ctrl;
	object@best_k <- scored_result[1, best_k];
	object@alt_k <- scored_result[(alt_k > 0), alt_k |> sort()];
	object@k_sz <- scored_result$k_sz;
	object@score <- scored_result$k_score;
	object@data <- scored_result;

	if (!rlang::is_empty(cl_size) & ("cl" %in% ls())){ parallel::stopCluster(cl); }
	return(invisible(object));
}
