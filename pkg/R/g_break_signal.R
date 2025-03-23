# :: Class Properties
	class_properties <- { list(
		y = S7::new_union(
				S7::class_numeric
				, S7::class_integer
				, S7::class_Date
				, S7::class_POSIXct
				, S7::class_factor
				, S7::class_character
				, S7::class_expression
				)
		, k = S7::new_property(class = S7::new_union(S7::class_numeric, S7::class_factor, S7::class_integer), default = 0)
		, grp = S7::class_any
		, obs_ctrl = S7::new_property(class = S7::class_list, default = list(min_size = 1L, max_k = 2L))
		, best_k = S7::new_property(class = S7::class_numeric, default = numeric())
		, alt_k = S7::new_property(class = S7::class_numeric, default = numeric())
		, k_sz = S7::new_property(class = S7::class_numeric, default = numeric())
		, score = S7::new_property(class = S7::class_numeric, default = numeric())
		, data = S7::new_property(class = S7::class_any, default = NULL)
		, plot = S7::new_property(class = S7::class_function, getter = function(self){ self@data |> attr("viz") })
		)}

#' Break Signal
#'
#' Functions \code{break_signal()} and \code{signal_processor()} are designed to select the best break of a “signal”, observed differences in the values of an ordered sequence. A “break” is simply a change in sign of the differences: the “best” break is the one that indicates the process that generates of observations has changed state.
#'
#' @slot y The un-transformed, monotonically ordered vector.
#' If using dates, these values will be coerced into seconds. For other units of time, convert them into numeric values using code like the following: \code{y / as.numeric(lubridate::dhours(1))}
#' @slot k (used internally)
#' @slot grp A vector containing group assignments along \code{y}
#' @slot obs_ctrl (list) Observation data control list having the following recognized elements:\cr
#' \itemize{
#' \item{\code{max_k}: The upper limit for breaks to allow under consideration. This should primarily be set to control the effects of extreme values that appear frequently enough to be non-trivial but are due to confounding factors not related to the generative process under study.}
#' \item{\code{min_size}: The minimum observation size at each k to allow}
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
	)
