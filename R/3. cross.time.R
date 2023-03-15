.onLoad <- function(libname, packagename){
	.evs_cache <<- cachem::cache_disk(dir = tempdir(), max_age = Inf, max_n = Inf, destroy_on_finalize = FALSE)
	}

#' Cross-Compare Temporal Boundaries
#'
#' @description
#' Given a four-element vector of start and end coordinates of two events, \code{cross.time()} compares the distances among the upper and lower boundaries of pairs of event vectors. This includes "like" boundary comparison (e.g., start #2 - start#1) and contrary boundary comparison (e.g. start #2 - end #1).
#'
#' \code{cross.time()} \code{\link[memoise]{memoise}}s arguments \code{s0}, \code{s1}, \code{e0}, and \code{e1} with a disk cache defined as \code{\link[cachem]{cache_disk}}\code{(dir = tempdir(), max_age = Inf, max_n = Inf, destroy_on_finalize = FALSE)}.  A future version will allow the cache to be customized in a post-hoc manner.
#'
#' @param s0 A numeric/date-coded vector containing the temporal lower boundary of the starting event duration
#' @param s1 A numeric/date-coded vector containing the temporal upper boundary of the starting event duration
#' @param e0 A numeric/date-coded vector containing the temporal lower boundary of the ending event duration
#' @param e1 A numeric/date-coded vector containing the temporal upper boundary of the ending event duration
#' @param control A length-2 sorted list with values indicating the range of allowable values for internal variable \code{beta} (the difference between ends of 'to' events and beginnings of 'from' events)
#' @param chatty (logical | \code{FALSE}) Verbosity flag
#' @param units Conversion temporal units compatible with \code{lubridate}
#' @param ... (Not used)
#'
#' @returns A \code{\link[data.table]{data.table}} object having the following fields:
#' \describe{
#' \item{\code{mGap: }}{Metric describing the difference between the following temporal boundaries: \code{TO.start}, \code{FROM.end}}
#' \item{\code{mSt: }}{Metric describing the difference between the following temporal boundaries: \code{TO.start}, \code{FROM.start}}
#' \item{\code{mEd: }}{Metrics describing the difference between the following temporal boundaries: \code{TO.end}, \code{FROM.end}}
#' \item{\code{from.len: }}{The duration of time of each "from" event: units are one the scale of the smallest increment of time represented (e.g., calendar dates in days will have lengths expressed in days)}
#' \item{\code{to.len: }}{The duration of time of each "to" event: units are one the scale of the smallest increment of time represented (e.g., calendar dates in days will have lengths expressed in days)}
#' \item{\code{epsilon: }}{A complex number (e.g., \code{1337 + 0.90210i}) describing the relational changes from one event to another with interpretation based on whether or not the real and imaginary parts are \code{> 0}, \code{< 0}, or \code{ == 0}:
#' 	\tabular{lll}{
#' 	  Re \tab Im \tab Desc \cr
#' 	  {> 0} \tab {0} \tab Disjoint \cr
#' 	  {0} \tab {> 0} \tab Concurrency \cr
#' 	  {> 0} \tab {> 0} \tab Full Concurrency \cr
#' 	  {0} \tab {0} \tab Continuity \cr
#' 	  { } \tab {< 1} \tab \code{to} event shorter than \code{from} event \cr
#' 	  { } \tab {= 1} \tab \code{to} event same length as \code{from} event \cr
#' 	  { } \tab {> 1} \tab \code{to} event longer than \code{from} event \cr
#'		}
#'	}
#' \item{\code{epsilon.desc: }}{A plain-language description of \code{epsilon}}
#' }
#'
#' @name cross.time
#' @export
cross.time <- function(s0, s1, e0, e1, control = list(-Inf, Inf), chatty = FALSE, units = "",  ...){
## Reference: https://www.r-bloggers.com/using-complex-numbers-in-r/
## Division by Pi/4 makes it easy to tell if one argument is larger than, smaller than, or the same magnitude as the other (same = Pi/4)
## All computations are in the direction of B.max to A.min when `events.ascending` is TRUE
	require(data.table);
	require(magrittr);

	.conversion <- if (units %ilike% "^(we|mo|da|ye|se|mi|na|ho|pi).+s$"){
										rlang::inject(`::`(lubridate, !!units)) } else { as.numeric }

	control <- if (any(is.infinite(unlist(control))) || any(is.na(unlist(control)))){ control <- list(-9000, 9000) }
	control <- control %>% { .[unlist(.) |> order()] |> .conversion() |> as.list() }

	# `descr_epsilon` creates the text descriptions of `epsilon` in the output
	descr_epsilon <- purrr::as_mapper(~{
			if (rlang::is_empty(.x)){
				"NA"
			} else if (!all(is.na(.x))){
				ifelse(
					is.na(.x)
					, "NA"
					, { c(`1` = "Disjoint", `10` = "Concurrency", `100` = "Full Concurrency", `1000` = "Continuity")[
							as.character({ cbind(
								((Re(.x) != 0) & (Im(.x) == 0))
								, ((Re(.x) == 0) & (Im(.x) != 0))
								, ((Re(.x) != 0) & (Im(.x) != 0)) | ((Re(.x) == 1) & (Im(.x) == 0))
								, ((Re(.x) == 0) & (Im(.x) == 0))
								) %*% (10^c(0:3))
							})]
						}
					)
			} else { "NA" }
		});

	out.names <- { c("mGap"
								 , "mSt", "mEd"
								 , "from.len", "to.len"
								 , "epsilon", "epsilon.desc"
								 , "from.coord", "to.coord"
								 , "from_timeframe", "to_timeframe"
								 )}

	epsilon_expr <- rlang::expr({
		# Do not algebraically reduce the following with respect to 'mGap': the sign is as important as the arguments
		.out = atan2(mEd, mSt) * atan2((mGap * beta), mGap)
		.tau = sign(to.len - from.len)

		# Scale back down to an angle: `sqrt()` needs to have a complex argument for handling negative arguments
		# The square-root of 'mGap'  differentiates offset events from cases where one event envelopes another
		.out = (sqrt(as.complex(.out)) + sqrt(as.complex(mGap)^.tau)) |>
						unlist() |>
						purrr::modify_if(~Re(.x) |> is.infinite(), ~as.complex(0))

		if (rlang::is_empty(.out)){ epsilon } else { .out }
	});

	XTIME <- { data.table::data.table(
							beta				= as.numeric(e1 - s0)
							, mGap			= as.numeric(s1 - e0)
							, mSt 			= as.numeric(s1 - s0)
							, mEd 			= as.numeric(e1 - e0)
							, from.len	= as.numeric(e0 - s0)
							, to.len		= as.numeric(e1 - s1)
							, epsilon					= complex()
							, epsilon.desc		= character()
							, from.coord			= purrr::map2_chr(as.character(s0), as.character(e0), paste, sep = ":")
							, to.coord  			= purrr::map2_chr(as.character(s1), as.character(e1), paste, sep = ":")
							, from_timeframe	= purrr::map2(s0, e0, lubridate::interval)
							, to_timeframe  	= purrr::map2(s1, e1, lubridate::interval)
							)[(beta <= control[[2]]) & (beta >= control[[1]])]
						}
	# rlang::eval_tidy(epsilon_expr, data = XTIME) |> print()

	# Handle the case when XTIME is empty due to filtering rows as a function of `beta` and `control:`
	if (!rlang::is_empty(XTIME)){
		XTIME[, epsilon := eval(epsilon_expr)
				][, epsilon.desc := descr_epsilon(epsilon)
				][, c(out.names), with = FALSE] |>
		purrr::modify_at(c("beta", "mGap", "mSt", "mEd", "from.len", "to.len"), .conversion)
	} else { XTIME }
}
