.onLoad <- function(libname, pkgname){
	evs_cache <<- cachem::cache_disk(dir = tempdir(), max_age = Inf, max_n = Inf, destroy_on_finalize = FALSE)
	.xtime = function(s0, s1, e0, e1, control, events.ascending = TRUE, chatty = FALSE, nosy = FALSE){
	#' Cross-Compare Temporal Boundaries
	#'
	#' Given a four-element vector of start and end coordinates of two events, \code{cross.time()} compares the distances among the upper and lower boundaries of pairs of event vectors. This includes "like" boundary comparison (e.g., start #2 - start#1) and contrary boundary comparison (e.g. start #2 - end #1).
	#'
	#' @param s0 A numeric/date-coded vector containing the temporal lower boundary of the starting event duration
	#' @param s1 A numeric/date-coded vector containing the temporal upper boundary of the starting event duration
	#' @param e0 A numeric/date-coded vector containing the temporal lower boundary of the ending event duration
	#' @param e1 A numeric/date-coded vector containing the temporal upper boundary of the ending event duration
	#' @param control A length-2 sorted list with values indicating the range of allowable values for '.beta' (the difference between ends of 'to' events and beginnings of 'from' events)
	#' @param events.ascending (logical | TRUE) \code{FALSE} assumes events provided are in descending order
	#' @param chatty (logical | \code{FALSE}) Use \code{chatty = TRUE} to see messages related to the execution.
	#' @param nosy (logical | FALSE) Used for debugging purposes
	#'
	#' @return A \code{\link[data.table]{data.table}} object (??event.vector.space)
	#'
	#' @export
		## Reference: https://www.r-bloggers.com/using-complex-numbers-in-r/
		## Division by Pi/4 makes it easy to tell if one argument is larger than, smaller than, or the same magnitude as the other (same = Pi/4)
		## All computations are in the direction of B.max to A.min when `events.ascending` is TRUE

		direction =  as.character(as.numeric(events.ascending));

		# `boundaries` ====
		boundaries = if (events.ascending) { cbind(s0, e0, s1, e1) } else { cbind(s1, e1, s0, e0) }

		# .beta: The maximum span between the extremes of temporal boundaries ====
		.beta	= boundaries[, 4] - boundaries[, 1];
		if (missing(control)){ control <- list(-Inf, Inf) }

		# Calculate ====
		output = {
			data.table::data.table(
				# The gap between the ordered events
				mGap = boundaries[, 3] - boundaries[, 2]
				# Difference of lower boundaries (start/lower-boundary)
				, mSt	= boundaries[, 3] - boundaries[, 1]
				# Difference of upper boundaries (end/upper-boundary)
				, mEd	= boundaries[, 4] - boundaries[, 2]
				# The duration of the first event
				, from.len	= boundaries[, 2] - boundaries[, 1]
				# The duration of the second event
				, to.len	= boundaries[, 4] - boundaries[, 3]
			)[
			# Relative change across boundary values
			, epsilon := {
					# Do not algebraically reduce the following with respect to '.beta': the sign is as important as the arguments
					.out = atan2(mEd, mSt) * atan2((mGap * .beta), mGap)

					# Scale back down to an angle: `sqrt()` needs to have a complex argument for handling negative arguments
					# The square-root of 'mGap'  differentiates offset events from cases where one event envelopes another
					.out = sqrt(as.complex(.out))/(0.25 * pi) + sqrt(as.complex(mGap))

					unlist(.out)
				}
			][
			, epsilon.desc := {
					c(`1` = "Disjoint", `10` = "Concurrency", `100` = "Full Concurrency", `1000` = "Continuity")[
					as.character({
						cbind(
							((Re(epsilon) != 0) & (Im(epsilon) == 0))
							, ((Re(epsilon) == 0) & (Im(epsilon) != 0))
							, ((Re(epsilon) != 0) & (Im(epsilon) != 0))
							, ((Re(epsilon) == 0) & (Im(epsilon) == 0))
							) %*% (10^c(0:3))
						})
					]
				}
			][
			# Filter out rows where '.beta' is not within the time control limits
			(.beta %between% control)
			];
		}

		if (is.null(output)) { message(sprintf("[%s] \t... ERROR: nothing to output", Sys.time())); }

		if (nosy & !exists("inspect", envir = globalenv())) { assign("inspect", list(), envir = globalenv()); }

		# Return ====
		output;
	}
	cross.time <<- memoise::memoise(f = .xtime, cache = evs_cache, omit_args = c("control", "events.ascending", "chatty", "nosy"))
}

.onUnload <- function(libpath){
	evs_cache$destroy()
	gc()
}
