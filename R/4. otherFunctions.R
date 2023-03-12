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
evs_retrace <- function(self, ...){
#' Retrace Event Source Data
#'
#' \code{evs_retrace} creates vertex attribute \code{trace} and populates it with an expression that retrieves the source record when evaluated.
#'
#' @param self An R object of class "event.vector.space"
#' @param ... (\code{\link[rlang]{dots_list}}) Names of event graphs found in \code{self$evt_graphs}
# @param chatty Verbosity flag
#'
#' @return Because of the reference semantics of R6 classes, for each name given in \code{...}, graph updates are in place: \code{self} is returned invisibly.
#'
#' @export

	evt_gph = if (...length() == 0){
			names(self$evt_graphs) |> purrr::set_names()
		} else {
			purrr::modify_if(rlang::list2(...), is.numeric, ~names(self$evt_graphs[.x]), .else = ~as.character(.x)) |> purrr::set_names()
		}

	self$evt_graphs[names(evt_gph)] <- purrr::imap(evt_gph, ~{
		g = self$evt_graphs[[.y]];

		evs.cfg = self$config;

		evs.lkup = { self$space[(jk %in% igraph::E(g)$jk), .(jk, from.src, from.coord, to.src, to.coord)] %>%
				melt(measure.vars = list(src = c("from.src", "to.src"), coord = c("from.coord", "to.coord")), variable.name = "type", variable.factor = FALSE) |>
				unique() %>%
				.[, c(list(type = as.numeric(type))
							, paste(jk, src, coord, sep = ":") |>
								stringi::stri_split_fixed(":", simplify = TRUE) |>
								data.table::as.data.table() |>
								purrr::set_names(c("jk", "context", "seq_idx", "start_idx", "end_idx")) |>
								purrr::modify_at(c("jk", "seq_idx"), as.numeric)
							)
					]} %>% data.table::setorder(context, type, seq_idx)

		igraph::V(g)$title <- igraph::V(g)$name;
		igraph::V(g)$trace <- { igraph::V(g)$title |>
				purrr::map(~{
					vkey = stringi::stri_split_fixed(.x, ":", simplify = TRUE) |> as.list() |>
								purrr::set_names(c("context", "seq_idx")) |>
								purrr::modify_at("seq_idx", as.numeric)
					vlkup = evs.lkup[vkey, on = c("context", "seq_idx")][(type == min(type))]

					evs.cfg[(contexts %in% vlkup$context), {
						.map_fields = if (rlang::has_length(unlist(map.fields), 1)){
								stringi::stri_split_regex(unlist(map.fields), "[,|:]", simplify = TRUE) |> as.vector()
							} else { unlist(map.fields) }

						.map_fields %<>% purrr::set_names(c("who", "start", "end"))

						parse(text = sprintf(
							"%s[(%s == %s) & (%s == as.Date('%s')) & (%s == as.Date('%s'))]"
							, src.names
							, .map_fields["who"]  , vlkup$jk %>% as.numeric()
							, .map_fields["start"], vlkup$start_idx
							, .map_fields["end"]  , vlkup$end_idx
							))
					}];
				})
			}
		g
	})

	invisible(self)
}
#