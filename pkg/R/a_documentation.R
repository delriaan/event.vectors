#' Event Vectors
#'
#' @description
#' Package `event.vectors` provides a framework for temporal data processing with a focus on capturing the relationships among points and durations in time.  Some definitions should be kept in mind to understand the event vector framework:
#' - `Vector`: A vector numerically encodes magnitude and direction.
#' - `Event`: Theoretically, an instantaneous point in time associated with a change of state.  Practically, an event is temporal duration encoded as a pair of temporal points. This is the understanding used in `event.vectors`.
#'
#' The vector central to `event.vectors` is a complex vector that concisely describes various temporal relations between events.  The entire framework lends itself to temporal graph constructs in addition to tabular representations.  This allows for execution of queries and statistical analysis against the dataset.
#'
#' @name event.vectors-package
NULL

.onLoad <- function(...) {
	S7::methods_register()
}