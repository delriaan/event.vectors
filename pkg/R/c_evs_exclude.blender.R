#' Event-Pair Exclusions
#'
#' \code{evs_exclude.blender} is a wrapper for \code{\link[base]{expand.grid}} with some post-processing via \code{\link[purrr]{array_branch}} and \code{\link[purrr]{reduce}}
#'
#' @param x See \code{\link[base]{expand.grid}}
#' @param y See \code{\link[base]{expand.grid}}
#'
#' @return A vector of pairs of event labels to be excluded in the call to \code{$exclude.mix()}
#'
#' @export
evs_exclude.blender <- function(x, y){
	expand.grid(x, y, stringsAsFactors = FALSE) |>
		purrr::array_branch(margin = 1) |>
		purrr::reduce(rbind) |>
		unname()
}
#
