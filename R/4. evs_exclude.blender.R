evs_exclude.blender <- function(x, y){
#' EVSpace Event-Pair Exclusions
#'
#' \code{evs_exclude.blender} is a wrapper for \code{\link[base]{expand.grid}} with some post-processing via \code{\link[data.table]{data.table}} and \code{\link[purrr]{map}}
#'
#' @param x See \code{\link[base]{expand.grid}}
#' @param x See \code{\link[base]{expand.grid}}
#'
#' @return A vector of pairs of event labels to be excluded in the call to \code{$exclude.mix()}
#'
#' @export

	expand.grid(x, y) |> apply(1, cbind) |> data.table::as.data.table() |> purrr::map(c) |> unname()
}
#
