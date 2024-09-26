# S7 Properties
class_properties <- { list(
	y = S7::new_union(S7::class_numeric, S7::class_integer, S7::class_Date, S7::class_POSIXct, S7::class_factor, S7::class_character, S7::class_expression)
	, k = S7::new_property(class = S7::new_union(S7::class_numeric, S7::class_factor, S7::class_integer), default = 0)
	, grp = S7::class_any
	, obs_ctrl = S7::new_property(class = S7::class_list, default = list(min_size = 1L, max_k = 2L))
	, best_k = S7::new_property(class = S7::class_numeric, default = numeric())
	, alt_k = S7::new_property(class = S7::class_numeric, default = numeric())
	, k_sz = S7::new_property(class = S7::class_numeric, default = numeric())
	, score = S7::new_property(class = S7::class_numeric, default = numeric())
	, data = S7::new_property(class = S7::class_any, default = NULL)
	, plot = S7::new_property(class = S7::class_function, getter = function(self){
			suppressWarnings({
				plotly::plot_ly(
					data = self@data[(tot_score %in% sapply(k, \(this.k) max(tot_score[k == this.k])))]
					, split = ~k
					, x = ~(\(i){
							i[i != 0] <- sign(i[i != 0]) * log10(abs(i[i != 0]));
							i;
						})(d2_Idev_wmean) * d2_kscore
					, y = ~tot_score
					, size = ~15*(k == best_k) + 5*(k %in% alt_k) + 5
					, stroke = I("#000000")
					, hovertext = ~glue::glue("<b>k:</b> {k}{ifelse(k == self@best_k, '<sup> Best</sup>', ifelse(k %in% self@alt_k, '<sup> Alt</sup>', ''))}<br><b>Score:</b> {round(tot_score, 4)}")
					, type = "scatter"
					, mode = "markers"
					) |>
					plotly::config(mathjax = "cdn") |>
					plotly::layout(
						margin = list(t = -5, b = -5, r = -5)
						, title = list(text = plotly::TeX("\\text{Break Score vs. Weighted-Mean Squared Information Deviation}\\\\\\text{ Size }\\sim k \\text{(optimal, alternate, or other)}"))
						, xaxis = list(
								title = list(text = plotly::TeX("\\bar{I}^{''}_{\\text{log}_{10}}\\times{\\Omega^k}^{''}"))
								, showgrid = FALSE
								)
						, yaxis = list(
								title = list(text = plotly::TeX("\\text{Max Total Score @ k}"))
								, showgrid = FALSE
								)
						, legend = list(title = list(text = plotly::TeX("\\enspace\\enspace{k}")))
						)
			})
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
