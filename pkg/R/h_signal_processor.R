signal_processor <- function(object, nfolds = 1, cl_size = 1, ...){
	#' Signal Processor
	#'
	#' @param object A "break_signal" \code{\link{S7}} object
	#' @param nfolds Number of folds to use during data generation and scoring
	#' @param cl Cluster object created from package \code{parallelly}
	#' @param ... Arguments for internal use
	#'
	#' @return The modified "break_signal" object with prescribed slots populated from the results of processing the "signal" (see \code{\link{break_signal}})
	#'
	#' @export
	S7::check_is_S7(object);
	.debug <- FALSE;

	if (hasName(list(...), ".debug")){ .debug <- list(...)[[".debug"]] }

	# :: User argument handling ====
	# y: Alias of object@y
	# y_grp: Alias of object@grp
	y <- object@y;
	y_grp <- object@grp;

	if (!all(c("min_size", "max_k") %in% names(object@obs_ctrl))){
		.nms <- c("min_size", "max_k");
		msg <- paste0(
						"`obs_ctrl` should be a list with named elements 'min_size' and 'max_k': missing "
						, paste(setdiff(.nms, names(object@obs_ctrl)) |> sprintf(fmt = "'%s'"), collapse = ", ")
						);
		stop(msg);
	}

	obs_ctrl <- object@obs_ctrl |>
		purrr::modify_at("min_size", \(x) magrittr::set_attr(x, "label", "Minimum grouped size to process")) |>
		purrr::modify_at("max_k", \(x) magrittr::set_attr(x, "label", "Maximum break allowed: \nrequires domain knowledge as this is an emperical-analytic task"));

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

		# Given a set of differenced values (over the differences of the
		# (possibly grouped) monotonically increasing input), a "signal break" is
		# assumed as a switch from positive to negative
		is_signal_break <- c(0, diff(i)) < 0;

		# `signal_data`: The number of contiguous parts of the series before a break
		signal_data <- data.table::data.table(
										dy = i
										, cyl = book.of.utilities::count.cycles(is_signal_break, reset = FALSE)
										)[, series := sum(dy), by = cyl]

		# `inform()`: A function that calculates the Shannon self-information of the input
		# vector in three steps:
		# 1. The proportion of the frequency of each value in the vector is calculated
		# 2. The Shannon self-information of the proportion is calculated
		# 3. The geometric mean of the Shannon self-information is calculated
		inform <- \(x){
			table(x) |>
				as.vector() |>
				book.of.utilities::ratio(type = "of.sum", d = 6) |>
				log(2) |>
				magrittr::multiply_by(-1) %>%
				.[order(as.numeric(names(.)))] |>
				as.vector() |>
				book.of.utilities::calc.geo_mean()
		}

		# Determine the type of output
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

	# Geometric PMF
	geo_pmf <- function(x, p){
		# Geometric Probability Mass Function
		#
		# @param x The cumulative sum of differences, representing the number of attempts before success.\cr These values imply contiguous parts of the series before a break.
		# @param p The cumulative proportion of the current group's difference-proportion mapped from `response.pmf`.
		#
		# @return \code{p * (1 - p)^x}

		assertive::assert_all_are_in_left_open_range(x = p, lower = 0, upper = 1, na_ignore = TRUE, severity = "stop");
		assertive::assert_all_are_greater_than_or_equal_to(x = x, y = 0, severity = "stop");

		# Return the probability mass function of the geometric distribution
		# Reference URL: https://en.wikipedia.org/wiki/Geometric_distribution
		p * (1 - p)^x;
	}

	# Scoring function definition (used in function exec_algorithm() below)
	score_algorithm_output <- function(algo_output){
		algo_output[
			# Filter criteria ====
			(k <= obs_ctrl$max_k) &
				(k > 0) &
				(d_Idev >= 0) & # Increasing or constant
				(d2_Idev <= 0) # Concave or local maximum
			# Observations per `k` ====
			, .(
				# Break score (k_score): information deviation equation/model
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
				# PMF-weighted mean of break information deviation by each 'k'
				# 	Calculated the weighted mean of `Idev` (squared information deviation)
				# 	using as weights the geometric PMF (`geo_pmf`)
				# 	in order to find the optimal `k`
				, Idev_wmean = weighted.mean(Idev, geo_pmf, na.rm = TRUE)
				)
			, by = k
			][
			# Interim row filter and data subset preparation ====
			(k_score > 0), .SD[order(k, k_score)] |> unique()
			][
			# Slope (d'/dk) ====
			# 	For each `k`, the slope of the `k_score` and `Idev_wmean` is calculated
			, `:=`(d_kscore = c(0, diff(k_score))/c(1, diff(k))
						, d_Idev_wmean = c(0, diff(Idev_wmean))/c(1, diff(k))
						)
			][
			# Curvature (d"/dk) ====
			#		For each 'd_kscore' and 'd_Idev_wmean', the difference is calculated
			, `:=`(d2_kscore = c(0, diff(d_kscore))
						, d2_Idev_wmean = c(0, diff(d_Idev_wmean))
						)
			][
			# Total Score ====
			# 	`tot_score` is the mutual relative proportionality of curvatures over
			# 	`k` and `Idev_wmean`. The relative proportionality of d2_Idev_wmean
			# 	is subtracted from 1 to invert the proportionality.  This makes 'tot_score'
			#		the product of the minimum of one curvature and the maximum of the other.
			# 	[Note: gradient descent is not used to determine the optimal 'k' because
			#		'k' is a discrete value.]
			, `:=`(tot_score =
						 	(1 - book.of.utilities::ratio(abs(d2_Idev_wmean), type = "of.max", d = 6)) *
						 	book.of.utilities::ratio(d2_kscore, type = "of.max", d = 6)
						 )
			][
			# "Best" and alternate break values derivation ====
			#		best_k: The `k` value with the highest `tot_score`
			#		alt_k: The `k` value that is not the `best_k` having a
			# 		cumulative proportion of `tot_score` >= 0.9
			, `:=`(
					best_k = k[tot_score == max(tot_score, na.rm = TRUE)] |> unique()
					, alt_k = k * (book.of.utilities::ratio(tot_score, type = "cumulative", d = 6) >= 0.9) *
						(tot_score != max(tot_score, na.rm = TRUE))
					)
			];
	}

	# Algorithm engine definition
	exec_algorithm <- function(Data, nfolds){
		# Execute Algorithm
		#
		# @param Data The input data
		# @param nfolds The number of cross-assignment folds
		#
		# @return A \code{\link[data.table]{data.table}} object containing folded data generation and scoring

		assertive::assert_all_are_greater_than_or_equal_to(nfolds, 1, severity = "stop");

		if (nfolds == 1){ nfolds <- min(c(5, Data$grp |> data.table::uniqueN())) }

		# Generate CV folds by group ----
		fold_map <- data.table::data.table(seq_len(nfolds), Data$grp |> unique()) |>
									data.table::setnames(c("fold_id", "grp"));

		X <- Data[
				, `:=`(c("cyl", "series", "grp.info")
							 # Encode information by group
							 , info_encoder(dy, info.only = FALSE))
				, by = grp
				][
				# Assign CV folds by group identifier by joining `fold_map` on "grp"
				fold_map, on = "grp", fold_id := fold_id, by = .EACHI
				];

		# Generate and score data by CV fold exclusion ----
		# cyl: The cycle identifier
		# grp: The group identifier
		unique(fold_map$fold_id) |>
			rlang::set_names() |>
			purrr::map(\(exclude_fold_id){
				X[(!fold_id %in% exclude_fold_id)][
					, `:=`(
							# Within-group cumulative "attempts" subsequently passed to geometric PMF calculation ====
							k = cumsum(dy)
							, # if any infinite values are found replace them with the maximum non-infinite value
							grp.info = ifelse(is.infinite(grp.info), max(grp.info[!is.infinite(grp.info)]), grp.info)
						)
					, by = .(cyl, grp)
					][
					# Within-group geometric PMF and break information deviation
					, `:=`(geo_pmf = geo_pmf(x = k, p = p)
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
					score_algorithm_output()
			}) |>
			# Combine data and return
			data.table::rbindlist(idcol = "exclude_fold_id");
	}

	# :: Grouped differentials of the observed measurements ====
	grouped_response <- {
		split(y, f = y_grp) |>
			lapply(\(i){
				# Used a 'length-1' check on argument `i` to prevent empty values from
				# returning when the split size is length-1.
				list(dy = if (rlang::has_length(i, 1)){ i } else { c(0, diff(as.numeric(i))) })
			}) %>%
			.[sapply(., \(x) length(unlist(x)) >= obs_ctrl$min_size)] |>
			data.table::rbindlist(idcol = "grp") |>
			dplyr::filter(dy > 0)
		}

	# Save grouped response differentials to slot `object@k`
	object@k <- grouped_response$dy;

	# :: Global proportional response values & information encoding ====
		# `response.pmf` is the global proportional response values calculated as follows:
		# 1. Calculate the global proportional response values
		# 2. Calculate the information encoding of the global proportional response values
		# 3. Merge the output of step 2 into prior object `grouped_response`
	if (.debug){ browser(); }
	response.pmf <- table(grouped_response$dy) %>%
		magrittr::divide_by(sum(., na.rm = TRUE)) |>
		(\(i){
			i[order(as.numeric(names(i)))] |>
					data.table::as.data.table() |>
					data.table::setnames(c("dy", "p")) |>
					purrr::modify_at("dy", as.numeric)
		})();

	spsUtil::quiet(grouped_response[
			response.pmf[, info := -log(p)]
			, on = "dy"
			, `:=`(p = p, info = info)
			, by = .EACHI
			]);

	# :: Parallelism topography (depends on user argument `cl_size`) ====
		# Determine the number of cluster nodes to use for parallel processing
		# based on the argument `cl_size`
	.logi_vec <- (cl_size > 1L) & (data.table::uniqueN(grouped_response$g) > 1);

	if (.logi_vec){
		# Globally assign the cluster object to the environment to allow for
		# manual termination in the event of an error
		assign("cl", parallelly::makeClusterPSOCK(workers = cl_size, autoStop = TRUE), envir = environment());

		parallel::clusterSetRNGStream(cl = cl, sample(-1E5:1E5, 1));

		# Export objects to cluster environment
		parallel::clusterExport(
			cl = cl
			, varlist = c("info_encoder", "geo_pmf", "exec_algorithm", "score_algorithm_output", "nfolds", "response.pmf")
			, envir = environment()
			);

		# The following code block accomplishes the following:
		# 1. Split the grouped response data by group identifier to each cluster node
		# 2. Evaluate the `grouped_response` data on each cluster node by executing the `exec_algorithm` function
		# 3. Return the evaluated `grouped_response` data to the calling environment
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

	# Populate `.temp` depending on whether or not parallelism is enabled
	.temp <- if (.logi_vec){
			spsUtil::quiet({
				parallel::clusterEvalQ(cl = cl, exec_algorithm(data.table::copy(grouped_response), nfolds = nfolds)) |>
					purrr::compact() |>
					data.table::rbindlist()
			})
		} else {
			exec_algorithm(data.table::copy(grouped_response), nfolds = nfolds);
		}

	# Add column 'best_k' to `.temp` to store the globally optimal 'k' value
	# and save the object to slot `object@data`
	object@data <- .temp[(best_k == {
			table(best_k) |>
			sort(decreasing = TRUE) |>
			names() |>
			data.table::first() |>
			as.numeric() |>
			max(na.rm = TRUE)
		})];

	# :: Assign additional values to `object`, clean up, and return the object invisibly ====
	object@obs_ctrl <- obs_ctrl;
	object@best_k <- object@data$best_k[1];
	object@alt_k <- .temp[!(best_k %in% object@best_k) & (k > 1), unique(best_k)];
	object@k_sz <- object@data$k_sz;
	object@score <- object@data$k_score;

	# Clean up
	if (!rlang::is_empty(cl_size) & ("cl" %in% ls())){
		parallel::stopCluster(cl);
		rm(cl);
	}

	# Return `object` invisibly
	return(invisible(object));
}
