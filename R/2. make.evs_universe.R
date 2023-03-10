make.evs_universe <- function(self, ..., time.control = list(-Inf, Inf), graph.control = NULL, units = "", furrr_opts = furrr::furrr_options(scheduling = Inf, seed = TRUE), graph.only = FALSE, chatty = FALSE){
#' Create the Event Vector Universe
#'
#' \code{make.evs_universe} supplies values to two class fields: \code{q_graph} and \code{space}, the latter being created from the former.
#'
#' @param evs An \code{EVSpace} object
#' @param ... (\code{\link[rlang]{dots_list}}) Logical expression that retain graph edges meeting the conditions
#' @param time.control A 2-element list containing the minimum and maximum values allowed for total temporal span between two events
#' @param graph.control An expression list containing \code{\link[igraph]{igraph-package}} calls to manipulate the internally-created graph in the order provided.  Use symbol \code{g} to generically denote the graph
#' @param units (See \code{\link{cross.time}})
#' @param furrr_opts \code{\link[furrr]{furrr_options}} defaulted as \code{scheduling = Inf} and \code{seed = TRUE}: internal globals are also set and will be appended to values provided here
#' @param graph.only (logical | \code{FALSE}) \code{TRUE} assumes class member \code{$space} exists (possibly after external modification) and recreates member \code{$evt_graphs}
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

  # :: `make.event_key` is a function used to create sequential unique identifiers for events sources and time-markers ----
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
		});

  # :: Create self$space from self$q_graph via calls to 'cross.time()' ----
  furrr_opts$globals <- furrr_opts$globals |> c("graph.control", "self", "cross.time", "time.control", "units") |> unique();
  furrr_opts$packages <- furrr_opts$packages |> c("magrittr", "data.table") |> unique();

  .src_mix <- self$.__enclos_env__$private$q_table;

  # :: Retrieve the essential columns from sources and create a compact intermediate data structure ----
	.tmp_space <- self$config$src.names |>
              purrr::map(~eval(str2lang(.x), envir = globalenv()) %>% .[, .(jk, start_idx, end_idx, src)]) |>
              data.table::rbindlist() |>
              data.table::setkey(jk, start_idx, end_idx) |>
  						data.table::setorder(jk, start_idx, end_idx) %>%
  						.[, f_src_exists := src %in% .src_mix[, from], by = jk] %>%
  						.[, t_src_exists := (src %in% .src_mix[, to]) & f_src_exists, by = jk] %>%
  						.[(f_src_exists & t_src_exists), !c("f_src_exists", "t_src_exists")] %>%
  						unique() |> as.list();

	# :: Use optimization from 'data.table' to create `self$space` ----
	if (!graph.only){
		# Step 1
		self$space <- data.table::as.data.table(merge(
			.tmp_space[(.tmp_space$src %in% .src_mix$from)] |> purrr::compact()|> purrr::set_names(c("jk", "f_start_idx", "f_end_idx", "f_src"))
			, .tmp_space[(.tmp_space$src %in% .src_mix$to)] |> purrr::compact()|> purrr::set_names(c("jk", "t_start_idx", "t_end_idx", "t_src"))
			, by = "jk")
			)[(f_start_idx <= t_start_idx)] |>
  	split(by = c("jk"));

		# Step 2
	  self$space <- data.table::rbindlist({
  		self$space |> furrr::future_map(~{
  		# Call `cross.time()`
		  	split(.x, by = c("jk", "f_src", "t_src")) |>
  				purrr::map(~{
  					JK <- .x$jk[1];

			  		out.names <- purrr::set_names(as.character(rlang::exprs(
								jk, f_src, t_src, mGap, mSt, mEd, from.len, to.len, epsilon, epsilon.desc, from.coord, to.coord, from_timeframe, to_timeframe
							)));

		  			xtime = .x %$% cross.time(s0 = f_start_idx, s1 = t_start_idx, e0 = f_end_idx, e1 = t_end_idx, control = time.control, units = units);
		  			if (nrow(xtime) == 0){ NULL } else { xtime[, c(.x, .SD)][, c(out.names), with = FALSE] }
		  		}) |>
  				purrr::compact() |>
  				data.table::rbindlist()
  			}) |> purrr::compact()
  		})[
		  # Enforce row filter rules before proceeding
		  !is.na(epsilon) & eval(edge.filter)
		  ][
		  # Impute sequencing on event sources: this has a direct impact when creating distinct vertex names during subsequent graph creation
		  , c("f_src", "t_src") := list(paste(f_src, make.event_key(f_src, from.coord), sep = ":")
		  															, paste(t_src, make.event_key(t_src, to.coord), sep = ":"))
		  , by = jk
		  ][, src.pair := sprintf("%s -> %s", f_src, t_src)
		  ][, data.table::setnames(.SD, c("f_src", "t_src"), c("from.src", "to.src"))
		  ][(from.src != to.src )] |> # Remove loops
	  	data.table::setattr("contexts", self$config$contexts);
	}

  # :: Create `self$evt_graphs` from `self$space` ----
	message(sprintf("[%s] ... creating event graphs", Sys.time()));
  self$evt_graphs <- self$space |> split(by = "jk") |> furrr::future_map(~{
			graph.control
			g = igraph::graph_from_data_frame(data.table::setcolorder(.x, c("from.src", "to.src")))

			if (!rlang::is_empty(graph.control)){ for (i in graph.control){ eval(i) }}
			g
		}, .options = furrr_opts) |> purrr::compact();

	# :: Return ----
	message(sprintf("[%s] The vector space is ready for analysis", Sys.time()));
	invisible(self);
}
#