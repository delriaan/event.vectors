#' Cross-Compare Temporal Boundaries
#'
#' @description
#' Given a four-element vector of start and end coordinates of two events, \code{cross.time()} compares the distances among the upper and lower boundaries of pairs of event vectors. This includes "like" boundary comparison (e.g., start #2 - start#1) and contrary boundary comparison (e.g. start #2 - end #1).
#'
#' @param s0 A numeric/date-coded vector containing the temporal lower boundary of the starting event duration
#' @param s1 A numeric/date-coded vector containing the temporal upper boundary of the starting event duration
#' @param e0 A numeric/date-coded vector containing the temporal lower boundary of the ending event duration
#' @param e1 A numeric/date-coded vector containing the temporal upper boundary of the ending event duration
#' @param control A length-2 sorted list with values indicating the range of allowable values for internal variable \code{beta} (the difference between ends of 'to' events and beginnings of 'from' events)
#' @param chatty (logical | \code{FALSE}) Verbosity flag
#' @param unit One of the \code{lubridate} \code{d<duration>}() functions
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
cross.time <- function(s0, s1, e0, e1, control = list(-Inf, Inf), chatty = FALSE, unit = "",  ...){
## Reference: https://www.r-bloggers.com/using-complex-numbers-in-r/
## Division by Pi/4 makes it easy to tell if one argument is larger than, smaller than, or the same magnitude as the other (same = Pi/4)
## All computations are in the direction of B.max to A.min when `events.ascending` is TRUE
	.durations <- list(
		days = lubridate::ddays
		, hours = lubridate::dhours
		, microseconds = lubridate::dmicroseconds
		, milliseconds = lubridate::dmilliseconds
		, minutes = lubridate::dminutes
		, months = lubridate::dmonths
		, nanoseconds = lubridate::dnanoseconds
		, picoseconds = lubridate::dpicoseconds
		, seconds = lubridate::dseconds
		, weeks = lubridate::dweeks
		, years = lubridate::dyears
		);

	.unit_patterns <- grepl("^(we|mo|da|ye|se|mi|na|ho|pi)", unit, ignore.case = TRUE);
	.conversion <- if (any(.unit_patterns)){
										.durations[[which(.unit_patterns)]]
									} else { as.numeric }

	out.names <- { c("beta", "mGap"
									 , "mSt", "mEd"
									 , "from.len", "to.len"
									 , "from.coord", "to.coord"
									 , "from_timeframe", "to_timeframe"
									 )}

	beta			<- as.numeric(e1 - s0);

	control <- purrr::imap(control, \(x, y){
			ifelse(is.infinite(x)
						 , sign(x) * .conversion(10 * abs(beta))
						 , ifelse(rlang::is_empty(x)
						 				 , c(-1,1)[y] * .conversion(10 * abs(beta))
						 				 , x
						 				 )
						 )
		});

	x_filter  <- (.conversion(beta) <= control[[2]]) & (.conversion(beta) >= control[[1]]);
	if (rlang::is_empty(beta)){ return(NULL) }

	mGap			<- as.numeric(s1 - e0)
	mSt 			<- as.numeric(s1 - s0)
	mEd 			<- as.numeric(e1 - e0)
	from.len	<- as.numeric(e0 - s0)
	to.len		<- as.numeric(e1 - s1)
	from.coord			<- purrr::map2_chr(as.character(s0), as.character(e0), paste, sep = ":")
	to.coord  			<- purrr::map2_chr(as.character(s1), as.character(e1), paste, sep = ":")
	from_timeframe	<- purrr::map2(s0, e0, lubridate::interval)
	to_timeframe  	<- purrr::map2(s1, e1, lubridate::interval)
	epsilon <- {
		# Do not algebraically reduce the following with respect to 'mGap': the sign is as important as the arguments
		.out = atan2(mEd, mSt) * atan2((mGap * beta), mGap)
		.tau = sign(to.len - from.len)

		# Scale back down to an angle: `sqrt()` needs to have a complex argument for handling negative arguments
		# The square-root of 'mGap'  differentiates offset events from cases where one event envelopes another
		.out = (sqrt(as.complex(.out)) + sqrt(as.complex(mGap)^.tau)) |>
						unlist() |>
						purrr::modify_if(\(x) is.infinite(Re(x)), \(x) as.complex(0))

		if (rlang::is_empty(.out)){ complex() } else { .out }
	}
	# print(ls())
	epsilon.desc <- (\(i){
		.eval_epsilon <- \(x){
			ifelse(
				is.na(x) || !is.complex(x)
				, "NA"
				, rlang::set_names(
						c(((Re(x) != 0) & (Im(x) != 0)) | ((Re(x) == 1) & (Im(x) == 0))
							, (Re(x) == 0) & (Im(x) != 0)
							, (Re(x) == 0) & (Im(x) == 0)
							, (!Re(x) %in% c(0, 1)) & (Im(x) == 0)
							)
						, c("Full Concurrency", "Concurrency", "Continuity", "Disjoint")
						) %>% .[.] |> names()
					)
			}
		if (rlang::is_empty(i)){ NULL } else { sapply(i, .eval_epsilon) }
	})(epsilon);
	# print(ls())

	beta	<- .conversion(beta);
	mGap	<- .conversion(mGap);
	mSt 	<- .conversion(mSt);
	mEd 	<- .conversion(mEd);

	c(out.names, "epsilon", "epsilon.desc") |> mget() |> as.data.table()
}

# debug(cross.time)
# undebug(cross.time)