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
make.evs_universe <- function(self, ..., time.control = list(-Inf, Inf), graph.control = NULL, omit.na = FALSE, chatty = FALSE){
#' Create the EVSpace Universe
#'
#' \code{make.evs_universe} supplies values to two class fields: \code{q_graph} and \code{space}, the latter being created from the former.
#'
#' @param evs An \code{EVSpace} object
#' @param ... (\code{\link[rlang]{dots_list}}) Logical expression that retain graph edges meeting the conditions
#' @param time.control A 2-element list containing the minimum and maximum values allowed for total temporal span between two events
#' @param graph.control An expression list containing \code{\link[igraph]{igraph-package}} calls to manipulate the internally-created graph in the order provided.  Use symbol \code{g} to generically denote the graph
#' @param omit.na (logical | \code{FALSE}) \code{TRUE} removes rows from class member \code{space} that have \code{NA} values
#' @param chatty (logical | \code{FALSE}) Verbosity flag
#'
#' @return Invisibly, the original object augmented with new member \code{space}
#'
#' @section Class Member \code{$space}:
#' \code{$space} should have as many rows as the sum of all edge counts for graphs in \code{$q_graph}
#'
#' @section Parallelism:
#' Parallelism over \code{$q_graph} is supported via package \href{https://rdocumentation.org/packages/furrr/versions/0.3.1}{furrr}: the user is responsible for setting a \code{\link[future]{plan}}.
#'
#' @export

	force(self)
  edge.filter <- if (...length() > 0){
  	rlang::enquos(..., .named = FALSE, .ignore_empty = "all") %>%
  		map_chr(~{ rlang::get_expr(.x) %>% deparse() %>% sprintf(fmt = "(%s)") }) %>%
  		paste(collapse = " & ") %>% str2lang()
  	} else { TRUE }

  # :: Create self$space from self$q_graph via calls to 'cross.time()'
  .furrr_opts <- furrr::furrr_options(
  		scheduling = Inf
  		, seed = TRUE
  		, packages = c("stringi", "igraph", "magrittr", "data.table")
  		, globals = c(".evs_cache", "graph.control", ".vnames", ".graphs", "self", "cross.time")
  		);

  self$space <- purrr::imap(names(self$q_graph), ~{
  # self$space <- furrr::future_imap(names(self$q_graph), ~{
  	# Capture the current value of 'jk'
  	jk = .x

  	# Capture the current graph
  	g = self$q_graph[[.x]];

  	# Traverse each row of the 2D array created via ends()
  	# Set event vector space metrics for each edge -> { cross.time() ~ from:to }
  	# These are the configuration edges

  	unravel_proto = function(z){
  		k = is.data.table(z);
  		while(!k){ z <- z[[1]]; k <- is.data.table(z); }
	  	return(copy(z))
  	}

  	unravel = memoise::memoise(unravel_proto);

		purrr::pmap(igraph::ends(graph = g, igraph::E(g)) %>% as.data.table(), ~{
			require
			fr_data = (unravel(igraph::V(g)[name == ..1]$data))[, .(jk, start_idx, end_idx, src)][, v_idx := .I] %>% setkey(jk)
			to_data = (unravel(igraph::V(g)[name == ..2]$data))[, .(jk, start_idx, end_idx, src)][, v_idx := .I] %>% setkey(jk)
			.tmp_data =  fr_data[to_data, allow.cartesian = TRUE] %>% setcolorder(purrr::keep(names(.), ~.x %ilike% "start_idx|end_idx"))

			.tmp_data[,{
				.out = cbind(
					from.coord = paste(jk, as.character(start_idx), as.character(end_idx), src, v_idx, sep = ":")
					, to.coord = paste(jk, as.character(i.start_idx), as.character(i.end_idx), i.src, i.v_idx, sep = ":")
					, src.pair = paste(src, i.src, sep = ":")
					, cross.time(
							s0 = start_idx - start_idx
							, s1 = i.start_idx - start_idx
							, e0 = end_idx - start_idx
							, e1 = i.end_idx - start_idx
							, control = time.control
							, chatty = chatty
							)
					, from_timeframe = purrr::map2(start_idx, end_idx, lubridate::interval)
					, to_timeframe   = purrr::map2(i.start_idx, i.end_idx, lubridate::interval)
					, jk = jk
					) %>% as.data.table()

				# NOTE: 'edge.filter' is evaluated here because after the mapping, 'purrr::compact()' will handle empty results
				# Also, filtering on 'self$space' is done because the actual event graphs are created from it.
				.out[!is.na(epsilon) & eval(edge.filter)]
			}]
		}) %>% purrr::compact() %>% purrr::reduce(rbind)
  }) %>%
  	purrr::compact() %>%
  	purrr::reduce(rbind)

  if (omit.na){ self$space %<>% na.omit() }

  # :: Create self$evt_graphs from self$space
  self$evt_graphs <- self$space %>% split(by = "jk") %>% purrr::map(igraph::graph_from_data_frame);

  .graphs = self$evt_graphs;
	.vnames = sprintf("(%s)[:][0-9]+?", paste(self$config$contexts, collapse = "|"));
	if (chatty){ print(.vnames) }

  # :: Finalize
	message(sprintf("[%s] ... finalizing", Sys.time()));
  self$evt_graphs <- furrr::future_map(seq_along(.graphs) %>% purrr::set_names(names(.graphs)), ~{
  	g = .graphs[[.x]];

  	igraph::V(g)$size 	<- tryCatch({
  		apply(
  			X = (stringi::stri_split_fixed(V(g)$name, ":", simplify = TRUE))[, c(2, 3)]
	  		, MARGIN = 1
  			, FUN = purrr::as_mapper(~as.Date(.x) %>% diff() %>% as.numeric() %>% sqrt() %>% as.integer())
  			)
  		}, error = function(e){ 10 })

  	igraph::V(g)$title	<- purrr::map(igraph::V(g)$name, stringi::stri_extract_last_regex, pattern = .vnames);
  	igraph::V(g)$key		<- igraph::V(g)$name;
  	igraph::V(g)$name		<- igraph::V(g)$name;

  	.vattrs <- igraph::V(g)$name %>%
  		stringi::stri_split_fixed(":", simplify = TRUE) %>%
  		purrr::array_branch(2) %>%
  		append(list(title = purrr::map_chr(igraph::V(g)$name, stringi::stri_extract_last_regex, pattern = .vnames))) %>%
  		purrr::set_names(c("jk", "start", "end", "source", "order", "title")) %>%
  		as.list();

	  if (!is.null(graph.control)){ for (i in graph.control){ eval(i) } }

  	igraph::vertex_attr(g) <- purrr::modify_at(.vattrs, c("start", "end"), as.Date) %>% purrr::modify_at("order", as.integer) %>% modify_at("source", as.factor)
  	g
  }, .options = .furrr_opts) %>% purrr::compact()

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
#' @param ... (\code{\link[rlang]{dots_list}})Names of event graphs found in \code{self$evt_graphs}: 'dynamic-dots' are supported via \code{\link[rlang]{list2}}
# @param chatty Verbosity flag
#'
#' @return Because of the reference semantics of R6 classes, for each name given in \code{...}, graph updates are in place: \code{self} is returned invisibly.
#'
#' @export

	evt_gph = if (...length() == 0){
		names(self$evt_graphs) %>% purrr::set_names()
	} else {
		purrr::modify_if(
			.x = rlang::list2(...)
			, .p = is.numeric
			, .f = ~names(self$evt_graphs[.x])
			, .else = ~as.character(.x)
			) %>% purrr::set_names()
	}

	self$evt_graphs[names(evt_gph)] <- purrr::imap(evt_gph, ~{
		g = self$evt_graphs[[.y]];

		evs.cfg = self$config;

		igraph::V(g)$name <- igraph::V(g)$title %>% stringi::stri_extract_first_regex("[A-Z]+[:][0-9]+");
		igraph::V(g)$trace <- purrr::map(igraph::V(g)$title, ~{
				evs.lkup = stringi::stri_split_fixed(.x, ":", simplify = TRUE) %>% as.list() %>%
									purrr::set_names(c("jk", "start_idx", "end_idx", "context", "seq_idx"));

				self$config[(contexts == evs.lkup$context), {
					.map_fields = if (rlang::has_length(unlist(map.fields), 1)){
						stringi::stri_split_regex(unlist(map.fields), "[,|:]", simplify = TRUE) %>% as.vector()
						} else { unlist(map.fields) }

					.map_fields %<>% purrr::set_names(c("who", "start", "end"))

					sprintf(
						"%s[(%s == %s) & (%s == as.Date('%s')) & (%s == as.Date('%s'))]"
						, src.names
						, .map_fields["who"]  , evs.lkup$jk %>% as.numeric()
						, .map_fields["start"], evs.lkup$start_idx
						, .map_fields["end"]  , evs.lkup$end_idx
						) %>% str2lang()
				}];
			});
		g;
	})

	invisible(self)
}
#