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

	expand.grid(x, y) %>% apply(1, cbind) %>% as.data.table %>% purrr::map(c) %>% unname()
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
			if (i %like% "[,;|][ ]?") {
			stringi::stri_split_regex(i, "[,;|][ ]?", simplify = T, omit_empty = T) %>% c
			} else { i }
		}

	# %>%  Check for class "data.table"
	if (!is.data.table(x)) { as.data.table(x) }

	# %>%  Check for additional column names that are understood to be "point-in-time" attributes: create duplicate columns if provided.
	if (!is.null(c(...))) {
		x[, c(paste("@", c(...), sep = "")) := .SD %>% pluck(c(...))]
		}

	start.names %<>% parse.delim %>% c(c(...));
	end.names %<>% parse.delim %>% c(names(x) %>% keep(~.x %like% "@"));

	# %>%  Return the "melted" data.table
	melt(x, measure.vars = list(start.names, end.names)
		, value.name	= c("start_date", "end_date")
		)[, variable := NULL][!(is.na(start_date))];
	}
#
make.evs_universe <- function(self, ..., time.control = list(-Inf, Inf), graph.control = NULL, furrr_opts = furrr::furrr_options(scheduling = Inf, seed = TRUE), omit.na = FALSE, chatty = FALSE){
#' Create the EVSpace Universe
#'
#' \code{make.evs_universe} supplies values to two class fields: \code{q_graph} and \code{space}, the latter being created from the former.
#'
#' @param evs An \code{EVSpace} object
#' @param ... (\code{\link[rlang]{dots_list}}) Logical expression that retain graph edges meeting the conditions
#' @param time.control A 2-element list containing the minimum and maximum values allowed for total temporal span between two events
#' @param graph.control An expression list containing \code{\link[igraph]{igraph-package}} calls to manipulate the internally-created graph in the order provided.  Use symbol \code{g} to generically denote the graph
#' @param furrr_opts \code{\link[furrr]{furrr_options}} defaulted as \code{scheduling = Inf} and \code{seed = TRUE}: internal globals are also set and will be appended to values provided here
#' @param omit.na (logical | \code{FALSE}) \code{TRUE} removes rows from class member \code{space} that have \code{NA} values
#' @param chatty (logical | \code{FALSE}) Verbosity flag
#'
#' @return Invisibly, the original object augmented with new member \code{space}
#'
#' @section Notes:
#' \itemize{
#' \item{Class member \code{$space} should have as many rows as the sum of all edge counts for graphs in \code{$q_graph}}
#' \item{The graphs in class member \code{$evt_graphs} are \code{\link[visNetwork]{visIgraph}}-ready}
#' \item{Parallelism is internally supported via package \code{furrr}: the user is responsible for setting the appropriate \code{\link[future]{plan}}}
#' }
#' @export

	force(self)
  edge.filter <- if (...length() > 0){
  	rlang::enquos(..., .named = FALSE, .ignore_empty = "all") |>
  		purrr::map_chr(~{ rlang::get_expr(.x) |> deparse() |> sprintf(fmt = "(%s)") }) |>
  		paste(collapse = " & ") |> str2lang()
  	} else { TRUE }

  # :: Create self$space from self$q_graph via calls to 'cross.time()'
  furrr_opts$globals <- furrr_opts$globals |> c(".evs_cache", "graph.control", ".vnames", ".graphs", "self", "cross.time") |> unique();

  .src_mix <- self$.__enclos_env__$private$q_table |> as.list() |> data.table::transpose() %>% purrr::set_names(purrr::map_chr(., paste, collapse = " -> "));

  # Retrieve the essential columns from sources and create a compact intermediate  data structure
	.tmp_space <- self$config$src.names |>
              purrr::map(~eval(str2lang(.x), envir = globalenv()) %>% .[, .(jk, start_idx, end_idx, src)]) |>
              data.table::rbindlist() %>%
              data.table::setkey(jk, start_idx);
	# Use optimization from 'data.table' to create `self$space`
	self$space <- { .tmp_space[
		  , c(purrr::map(.src_mix, ~{ .SD[(src %in% .x[1]), .(f_start_idx = start_idx, f_end_idx = end_idx, f_src = src)] %>% purrr::compact() }) |> data.table::rbindlist()
		      , purrr::map(.src_mix, ~{ .SD[(src %in% .x[2]), .(t_start_idx = start_idx, t_end_idx = end_idx, t_src = src)] %>% purrr::compact() }) |> data.table::rbindlist())
		  , by = jk
		  ][
		  , c(cross.time(
		  			s0 = f_start_idx
		  			, s1 = t_start_idx
		  			, e0 = f_end_idx
		  			, e1 = t_end_idx
		  			)
		     , list(from.coord			= purrr::map2_chr(f_start_idx, f_end_idx, paste, sep = ":")
					    , to.coord  			= purrr::map2_chr(t_start_idx, t_end_idx, paste, sep = ":")
					    , from_timeframe	= purrr::map2(f_start_idx, f_end_idx, lubridate::interval)
					    , to_timeframe  	= purrr::map2(t_start_idx, t_end_idx, lubridate::interval)
					    , from.src				= f_src
					    , to.src					= t_src
		    			)
		  	)
		  , by = .(jk, src.pair = sprintf("%s -> %s", f_src, t_src))
		  ][!is.na(epsilon) & eval(edge.filter)]
		}

  if (omit.na){
  	logi_vec = apply(X = self$space, MARGIN = 1, FUN = function(i){ !any(is.na(i)) })
  	self$space %<>% .[(logi_vec)]
  }

  # :: Create `self$evt_graphs` from `self$space`
  self$evt_graphs <- self$space |> split(by = "jk") |> purrr::map(~igraph::graph_from_data_frame(data.table::setcolorder(.x, c("from.src", "to.src"))));
  .graphs <- self$evt_graphs;

  # :: Finalize
	message(sprintf("[%s] ... finalizing", Sys.time()));
  self$evt_graphs <- furrr::future_map(self$evt_graphs, ~{
  	g = .x;
	  if (!rlang::is_empty(graph.control)){ for (i in graph.control){ eval(i) } }
  	g;
  }, .options = furrr_opts) |> purrr::compact();

	attr(self$space, "contexts")	<- self$config$contexts;
	message(sprintf("[%s] The vector space is ready for analysis", Sys.time()));

	invisible(self);
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
# @export

	evt_gph = if (...length() == 0){
		names(self$evt_graphs) |> purrr::set_names()
	} else {
		purrr::modify_if(rlang::list2(...), is.numeric, ~names(self$evt_graphs[.x]), .else = ~as.character(.x)) |> purrr::set_names()
	}

	self$evt_graphs[names(evt_gph)] <- purrr::imap(evt_gph, ~{
		g = self$evt_graphs[[.y]];

		evs.cfg = self$config;

		igraph::V(g)$name <- igraph::V(g)$title |> stringi::stri_extract_first_regex("[A-Z]+[:][0-9]+");
		igraph::V(g)$trace <- purrr::map(igraph::V(g)$title, ~{
				evs.lkup = stringi::stri_split_fixed(.x, ":", simplify = TRUE) |> as.list() |> purrr::set_names(c("jk", "start_idx", "end_idx", "context", "seq_idx"));

				self$config[(contexts == evs.lkup$context), {
					.map_fields = if (rlang::has_length(unlist(map.fields), 1)){
						stringi::stri_split_regex(unlist(map.fields), "[,|:]", simplify = TRUE) |> as.vector()
						} else { unlist(map.fields) }

					.map_fields %<>% purrr::set_names(c("who", "start", "end"))

					sprintf(
						"%s[(%s == %s) & (%s == as.Date('%s')) & (%s == as.Date('%s'))]"
						, src.names
						, .map_fields["who"]  , evs.lkup$jk %>% as.numeric()
						, .map_fields["start"], evs.lkup$start_idx
						, .map_fields["end"]  , evs.lkup$end_idx
						) |> str2lang();
				}];
			});
		g;
	})

	invisible(self)
}
#