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

	force(self);

  edge.filter <- if (...length() > 0){
  	str2lang(rlang::enquos(..., .named = FALSE, .ignore_empty = "all") |>
  		purrr::map_chr(~{ rlang::get_expr(.x) |> deparse() |> sprintf(fmt = "(%s)") }) |>
  		paste(collapse = " & "))
  	} else { TRUE }

  # :: `make.event_key` is a function used to create sequential unique identifiers for events sources and time-markers
  make.event_key <- purrr::as_mapper(~{
			root = .x
			radix = .y
			index = rep.int(NA, length(root));

			# Populate 'index' based on unique values of 'root'
			purrr::walk(unique(root), ~{
				out.x = root[which(root %in% .x)]
				out.y = radix[which(root %in% .x)];
				index[which(root %in% .x)] <<- data.table::frank(out.y, ties.method = "dense")
			})
			index
		})

  # :: Create self$space from self$q_graph via calls to 'cross.time()'
  furrr_opts$globals <- furrr_opts$globals |> c("graph.control", "self", "cross.time", "time.control") |> unique();
  furrr_opts$packages <- furrr_opts$packages |> c("magrittr", "data.table") |> unique();

  .src_mix <- self$.__enclos_env__$private$q_table;

  # :: Retrieve the essential columns from sources and create a compact intermediate  data structure
	.tmp_space <- self$config$src.names |>
              purrr::map(~eval(str2lang(.x), envir = globalenv()) %>% .[, .(jk, start_idx, end_idx, src)]) |>
              data.table::rbindlist() |>
              data.table::setkey(jk, start_idx, end_idx) |>
  						data.table::setorder(jk, start_idx, end_idx) %>%
  						.[, f_src_exists := src %in% .src_mix[, from], by = jk] %>%
  						.[, t_src_exists := (src %in% .src_mix[, to]) & f_src_exists, by = jk] %>%
  						.[(f_src_exists & t_src_exists), !c("f_src_exists", "t_src_exists")] %>%
  						unique() |> as.list();

	# :: Use optimization from 'data.table' to create `self$space`
	self$space <- data.table::as.data.table(merge(
			.tmp_space[(.tmp_space$src %in% .src_mix$from)] |> purrr::compact()|> purrr::set_names(c("jk", "f_start_idx", "f_end_idx", "f_src"))
			, .tmp_space[(.tmp_space$src %in% .src_mix$to)] |> purrr::compact()|> purrr::set_names(c("jk", "t_start_idx", "t_end_idx", "t_src"))
			, by = "jk"
			))

  self$space <- data.table::rbindlist(
  		self$space |> split(by = c("jk")) |> furrr::future_map(~{ # Call `cross.time()`
		  	.x[, {
			  		out.names <- purrr::set_names(as.character(rlang::exprs(
								mGap, mSt, mEd, from.len, to.len, epsilon, epsilon.desc, from.coord, to.coord, from_timeframe, to_timeframe
							)));
		  			xtime = cross.time(s0 = f_start_idx, s1 = t_start_idx, e0 = f_end_idx, e1 = t_end_idx, control = time.control);
		  			if (nrow(xtime) == 0){ NULL } else { xtime[, c(out.names), with = FALSE] }
		  		}
		  		, by = .(jk, f_src, t_src)
		  		]
  			}) |> purrr::compact()
  		)[
		  # Enforce row filter rules before proceeding
		  !is.na(epsilon) & eval(edge.filter)
		  ][
		  # Impute sequencing on event sources: this has a direct impact when creating distinct vertex names during subsequent graph creation
		  , c("f_src", "t_src") := list(paste(f_src, make.event_key(f_src, from.coord), sep = ":")
		  															, paste(t_src, make.event_key(t_src, to.coord), sep = ":"))
		  , by  = .(jk)
		  ][, src.pair := sprintf("%s -> %s", f_src, t_src)] %>%
			data.table::setnames(c("f_src", "t_src"), c("from.src", "to.src"))

  attr(self$space, "contexts") <- self$config$contexts;

  # :: Create `self$evt_graphs` from `self$space`
	message(sprintf("[%s] ... creating event graphs", Sys.time()));
  self$evt_graphs <- self$space |> split(by = "jk") |>furrr::future_map(~{
			graph.control
			g = igraph::graph_from_data_frame(data.table::setcolorder(.x, c("from.src", "to.src")))
			if (!rlang::is_empty(graph.control)){ for (i in graph.control){ eval(i) }}
			g
		}, .options = furrr_opts)|> purrr::compact();

  # :: Return
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