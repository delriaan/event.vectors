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
melt_time <- function(x, start.names, end.names, ...){
#' Multiple Time Melter
#'
#' \code{melt_time} leverages data.table::melt to transform the input's temporal feature vector into a pair of features indicating temporal duration (i.e., start & stop).  It handles feature vectors of any length.  "Point-in-time" columns are duplicated before melting
#'
#' @param x (object) A data.frame, data.table, or coercible to data.table
#' @param start.names (string or vector) The names of the "start" columns: parsed if given as a delimited string of pattern "[,;|][ ]?"
#' @param end.names (same as \code{start.names})
#' @param ... (string list) Optional column names that are "point-in-time" columns.
#'
#' @return A "melted" \code{\link[data.table]{data.table}} with temporal feature vector containing \emph{start_date} and \emph{end_date}
#'
#' @export

	# %>%  Function to parse delimited names
	parse.delim = function(i) {
			if (grepl("[,;|][ ]?", i)) {
				stringi::stri_split_regex(i, "[,;|][ ]?", simplify = TRUE, omit_empty = TRUE) |> c()
			} else { i }
		}

	# :: Check for class "data.table"
	if (!data.table::is.data.table(x)) { data.table::as.data.table(x) }

	# ::  Check for additional column names that are understood to be "point-in-time" attributes: create duplicate columns if provided.
	if (!is.null(c(...))) { x[, c(paste("@", c(...), sep = "")) :=  purrr::pluck(.SD, c(...))] }

	start.names <- parse.delim(start.names) |> c(c(...));
	end.names <- parse.delim(end.names) |> c(names(x) |> purrr::keep(~.x %like% "@"));

	# :: Return the "melted" data.table
	melt(x, measure.vars = list(start.names, end.names), value.name	= c("start_date", "end_date"))[, variable := NULL][!(is.na(start_date))];
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