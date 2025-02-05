#' @title
#' Event Vectors Class
#'
#' @description
#' \code{event.vectors} is an R6 class object that creates a temporally-compared space of "\emph{event vectors}", each comprised of a 'from' and 'to' temporal marker.
#'
#' The time between these events is the focus of derivation: properly, the event vector is a complex number that encodes the relationship between the boundaries of the events, thus allowing one to describe this relationship in a concise manner.
#'
#' @section Execution Workflow:
#' The initial execution order should look something like the following ...
#' \cr
#' \code{ event.vectors$new(...)$configure(...)$make.evs_universe(...) }.  The ability to execute the preceding workflow out of order exists, but it is best to adhere to the provided flow the first time around.
#' \cr
#' @section Class Member "Space":
#' \code{$space} is a \code{\link[data.table]{data.table}} that is populated upon execution of \code{$make.evs_universe()}:
#'    \describe{
#'      \item{\code{jk}}{Values of the "join key"}
#'      \item{"crossed" time output}{ See \code{\link{cross.time}()}}
#'      \item{\code{fr_*}, \code{to_*}}{The temporal ranges of each \emph{from}/\emph{to}event}
#'      \item{\code{src.pair}}{Values of the "from-to" pairings of events given by the contexts provided: FROM:TO}
#'      \item{\code{from.coord}}{String representation of temporal boundaries expressed as concatenated integers}
#'      \item{\code{to.coord}}{String representation of temporal boundaries expressed as concatenated integers}
#'    }
#' @import data.table
#' @import magrittr
#' @export
event.vectors <- { R6::R6Class(
	classname = "event.vectors"
	# _____ PUBLIC METHODS _____
	, public = { list(
		# _____ PUBLIC CLASS MEMBERS _____
		#' @field space The object containing the derived set of data (see 'Details')
		space	 = NULL
		#' @field q_graph A 'query graph' list containing the event interactions for each level of 'jk' (see 'Details')
		, q_graph	 = NULL
		#' @field evt_graphs Event graphs for each level of 'jk' (see 'Details')
		, evt_graphs = NULL
		# _____ PUBLIC CLASS METHODS _____
			# NEW() ====
		, #' @description
      #' Initialize the Event Vector Class
			#' @param ... (not used)
		initialize = function(...){
				# ::  CLASS MEMBERS INITIALIZATION
				message(sprintf("[%s] Initializing the event vector class", Sys.time()));
				invisible(self);
			}
		, # CONFIGURE() ====
			#' @description
			#' \code{$configure()} creates references to the source data to use.
			#'
      #' @param src.defs Strings or expressions that define the data to use (must return \code{\link{data.frame}}, \code{\link{data.table}}, or list)
      #' @param contexts An atomic vector of strings serving as labels for each data source
      #' @param map.fields A list of column name vectors for each definition in \code{src.defs} providing data for the \emph{join key} (\code{jk}), \emph{start} (\code{time_start_idx}), and \emph{end} (\code{time_end_idx}) references. Provide names for elements in each vector to specify which data source field is mapped to each name (\code{jk}, \code{time_start_idx}, and \code{time_end_idx}).\cr Regarding \emph{join key}: choose the columns having values that are shared across two or more event sources.
      #'
      #' @param src.mix \describe{
			#'	\item{"combination":}{ The default: generates unique pairs of sources}
			#'	\item{"reflexive":}{ Compares each source to itself}
			#'	\item{"all":}{ The union of the preceding options}
			#'	} \cr Partial matching is allowed, and generated combinations include the mirror (e.g., "A, B" will have a "B, A" combo pair generated).
			#'
      #' @param exclude.mix A list of vectors containing each source pair context to exclude (e.g. \code{list(c("A", "C"), c("u", "k"))}). \code{\link{evs_exclude.blender}} can be invoked to create this list more quickly.  Combinations are not automatically mirrored as is the case with \code{src.mix}.
      #'
      #' @param chatty (logical | \code{FALSE}) Verbosity flag
			configure =	function(src.defs, contexts, map.fields = NULL, src.mix = "comb", exclude.mix = NULL, chatty = FALSE){
					if (!rlang::is_empty(exclude.mix)){ message("Source-mix exlusions detected") }

					# private <- new.env()
					fld_nms <- c("jk", "time_start_idx", "time_end_idx")

					make_refs <- \(x) sapply(x, magrittr::freduce, list(eval, as.character)) |> rlang::parse_exprs()

					make_quos <- \(x, y){
						.this <- sapply(y, magrittr::freduce, list(eval, as.character))
						.that <- x
						.that <- rlang::set_names(.that, .this)
						rlang::as_quosures(.that, named = TRUE, env = rlang::caller_env(1))
					}

					set_fld_nms <- \(x){
							nms.x <- names(x)
							nm_pos <- which(nms.x %in% fld_nms)
							na_pos <- setdiff(seq_along(nms.x), nm_pos)
							if (rlang::is_empty(na_pos)){ na_pos <- seq_along(fld_nms) }
							nms.x[na_pos] <- setdiff(fld_nms, nms.x[nm_pos])
							nms.x
						}

					# Create the event data references
					event_refs <- make_refs(rlang::exprs(!!src.defs)) |> make_quos(rlang::enexprs(contexts))
					event_flds <- purrr::map(map.fields, \(x){
						rlang::parse_exprs(x) %>%
							rlang::set_names(set_fld_nms(.)) |>
							unlist(recursive = FALSE)
					})

					# Create a configuration quosure for each data source
					private$.params$config <- purrr::map2(event_refs, event_flds, \(x, y){
							.temp <- rlang::as_quosures(y, env = rlang::as_data_mask(rlang::eval_tidy(x)))[fld_nms];
							.temp$jk.vec <- .temp$jk |> rlang::eval_tidy() |> unique() |> sort();

							message(glue::glue("Validating source `{rlang::as_label(rlang::quo_get_expr(x))}`\n"));
							qa_check <- { c(
								glue::glue("{rlang::as_label(rlang::quo_get_expr(x))} exists: {assertthat::not_empty(rlang::eval_tidy(x))}")
								, glue::glue("- ${rlang::as_label(rlang::quo_get_expr(.temp$jk))} exists: {assertthat::not_empty(rlang::eval_tidy(.temp$jk))}" )
								, glue::glue("- ${rlang::as_label(rlang::quo_get_expr(.temp$time_start_idx))} exists: {assertthat::not_empty(rlang::eval_tidy(.temp$time_start_idx))}" )
								, glue::glue("- ${rlang::as_label(rlang::quo_get_expr(.temp$time_end_idx))} exists: {assertthat::not_empty(rlang::eval_tidy(.temp$time_end_idx))}" )
								, "====================================="
								)}
							if (chatty){ purrr::walk(qa_check, message)}

							data.table::setattr(.temp, "src.def", x)
						})

					# @def q_table sets the allowable comparisons before any calculations are done
					private$q_table <- {
						# .temp will be a matrix at this point
						.temp <- private$.params$config |> names() |> utils::combn(m = 2) %>% cbind(apply(., 2, rev)) |> t()

						# enforce 'src.mix'
						if (!grepl("reflex|all", src.mix, ignore.case = TRUE)){
							.temp <- .temp[.temp[, 1] != .temp[, 2], ]
						}

						# enforce 'exclude.mix' after converting .temp to a 'data.table' object
						.temp <- data.table::as.data.table(.temp) |> data.table::setnames(c("from", "to"));
						.temp[!purrr::pmap_lgl(.temp, \(...) list(c(...elt(1), ...elt(2))) %in% exclude.mix)] |> data.table::setkey(from, to)
					}

					private$.params$config <- {
						data.table::setattr(private$.params$config, "src.mix", as.character(rlang::enexpr(src.mix))) |>
						data.table::setattr("exclude.mix", sapply(exclude.mix, function(i){
							paste0(if (length(i) == 1){ c(i, i) } else if(length(i) > 2) { i[1:2] } else { i }, collapse = ", ")
						})) %>%
						data.table::setattr("jk", {
							purrr::map(., \(x) x$jk.vec |> rlang::eval_tidy()) |>
								magrittr::freduce(list(unlist, unique, sort, purrr::set_names))
							})
					}
					# invisible(private)
					invisible(self);
				}
		, # MAKE.EVS_UNIVERSE() ====
			#' @description
			#' \code{make.evs_universe} supplies values to two class fields: \code{q_graph} and \code{space}, the latter being created from the former.
			#'
			#' \emph{Additional Information}:
			#' \itemize{
			#' \item{Class member \code{$space} should have as many rows as the sum of all edge counts for graphs in \code{$q_graph}}
			#' \item{The graphs in class member \code{$evt_graphs} are \code{\link[visNetwork]{visIgraph}}-ready}
			#' \item{Parallelism is internally supported via package \code{furrr}: the user is responsible for setting the appropriate \code{\link[future]{plan}}}
			#' }
			#'
			#' @param ... (\code{\link[rlang]{dots_list}}) Logical expression that retain graph edges meeting the conditions
			#' @param time.control A 2-element list containing the minimum and maximum values allowed for total temporal span between two events
			#' @param graph.control An expression list containing \code{\link[igraph]{igraph-package}} calls to manipulate the internally-created graph in the order provided.  Use symbol \code{g} to generically denote the graph and \code{<<-} to update \code{g}.
			#' @param unit (See \code{\link{cross.time}})
			#' @param furrr_opts \code{\link[furrr]{furrr_options}} defaulted as \code{scheduling = Inf} and \code{seed = TRUE}: internal globals are also set and will be appended to values provided here
			#' @param graph.only (logical | \code{FALSE}) \code{TRUE} assumes class member \code{$space} exists (possibly after external modification) and recreates member \code{$evt_graphs}
			#' @param chatty (logical | \code{FALSE}) Verbosity flag
			#'
			#' @return Invisibly, the original object augmented with new member \code{space}
			make.evs_universe = function(..., time.control = list(-Inf, Inf), graph.control = NULL, unit = NULL
				, furrr_opts = furrr::furrr_options(scheduling = Inf, seed = TRUE)
				, graph.only = FALSE, chatty = FALSE){
			  # :: `make.event_key` is a function used to create sequential unique identifiers
				# 	for events sources and time-markers ----
			  make.event_key <-  function(...){
					from <- paste(...elt(1), ...elt(2), sep = "_")
					to <- paste(...elt(3), ...elt(4), sep = "_")

					from_to <- unique(sort(c(from, to))) |>
						(\(x) rlang::set_names(data.table::frank(x, ties.method="dense"), x))()

					event_index <- \(x){
							book.of.utilities::count.cycles(!duplicated(x), reset = FALSE) %>%
							stringi::stri_pad_left(width = as.integer(log10(max(., na.rm = TRUE))) + 1, pad = "0")
						}

					list(
						paste(...elt(1), from_to[from] |> event_index(), sep = ":")
						, paste(...elt(3), from_to[to] |> event_index(), sep = ":")
						)
				}

			  furrr_opts$globals <- furrr_opts$globals |>
			  	c("graph.control", "self", "cross.time", "time.control", "units") |>
			  	unique();

			  furrr_opts$packages <- furrr_opts$packages |>
			  	c("magrittr", "data.table") |>
			  	unique();

			  .src_mix <- self$.__enclos_env__$private$q_table;

			  # :: Retrieve the essential columns from sources and create a compact intermediate data structure ----
			  if (chatty){ message("Creating `.tmp_space` ...") }

			  .tmp_space <- purrr::imap(self$config, \(x, y){
						out <- magrittr::`%$%`(x, {
							data.table::data.table(
								jk = rlang::eval_tidy(jk)
								, time_start_idx = rlang::eval_tidy(time_start_idx)
								, time_end_idx = rlang::eval_tidy(time_end_idx)
								)
						})
						out[, src := y]
					}) |>
					data.table::rbindlist() |>
	        data.table::setkey(jk, time_start_idx, time_end_idx) |>
					data.table::setorder(jk, time_start_idx, time_end_idx) %>% {
						.[, f_src_exists := src %in% .src_mix[, from], by = jk][
							, t_src_exists := (src %in% .src_mix[, to]) & f_src_exists, by = jk][
							(f_src_exists & t_src_exists), !c("f_src_exists", "t_src_exists")
							]
					} |>
					unique() |>
					as.list();

				# :: Use optimization from 'data.table' to create `self$space` ----
				edge_filter <- rlang::enexprs(..., .named = FALSE, .ignore_empty = "all") |> c(TRUE);

				if (!graph.only){
				  if (chatty){ message("Creating `self$space`...") }

					self$space <- local({
						from_src <- .tmp_space[(.tmp_space$src %in% .src_mix$from)] |>
							purrr::compact() |>
							purrr::set_names(c("jk", "f_start_idx", "f_end_idx", "f_src")) |>
							data.table::as.data.table()

						to_src <- .tmp_space[(.tmp_space$src %in% .src_mix$to)] |>
							purrr::compact() |>
							purrr::set_names(c("jk", "t_start_idx", "t_end_idx", "t_src")) |>
							data.table::as.data.table()

						# browser()

						data.table::merge.data.table(x = from_src, y = to_src, by = "jk", allow.cartesian = TRUE)[
							(t_start_idx - f_start_idx) >= 0
							, cbind(.SD, cross.time(s0 = f_start_idx, s1 = t_start_idx
																			, e0 = f_end_idx, e1 = t_end_idx
																			, control = time.control, unit = unit))
							]
					})

				  if (chatty){ message("Finalizing `self$space`...") }

					edge_filter <- edge_filter |> as.character() |> paste(collapse = " & ") |>
						rlang::parse_expr() |> rlang::eval_tidy(data = self$space);

					self$space <- { self$space[
							# Enforce row filter rules before proceeding: note that this pre-derives a Boolean vector in order
							#		to make debugging easier and to ensure the integrity of the operation.
							(edge_filter)
							][
						  # Impute sequencing on event sources: this has a direct impact when creating distinct vertex names during subsequent graph creation
						  , c("f_src", "t_src") := make.event_key(f_src, from.coord, t_src, to.coord)
						  , by = jk
						  ][, src.pair := sprintf("%s -> %s", f_src, t_src)
						  ][, data.table::setnames(.SD, c("f_src", "t_src"), c("from.src", "to.src"))
						  ][, epsilon.desc := factor(epsilon.desc, levels = c("NA", "Full Concurrency", "Concurrency", "Continuity", "Disjoint"), ordered = TRUE)
						  ][
						  # Remove loops (may result in removing all records)
					  	(\(logi_vec){
								if (!any(logi_vec)){ 
									cli::cli_alert_warning("All relationships are loops: returning values as-is.")
									!logi_vec
								} else {
									if (chatty && (attr(self$config, "src.mix") == "comb")){
										cli::cli_alert_info("Removing loops")
									}
									logi_vec
								}
							})(from.src != to.src)
					  	, .(jk, from.src, to.src, beta, mGap, mSt, mEd, epsilon, epsilon.desc, from.len, to.len, from.coord, to.coord, src.pair, x_filter)
						  ]
					}
				}

				# :: Create `self$evt_graphs` from `self$space` ----
				message(sprintf("[%s] ... creating event graphs", Sys.time()));
				furrr_opts <- { furrr::furrr_options(
							scheduling	= Inf
							, seed			= TRUE
							, packages	= c("magrittr", "data.table", "rlang")
							, globals 	= c("time.control", "units", "cross.time", "edge_filter")
							)}

			  self$evt_graphs <- self$space[(x_filter)] |>
			  	split(by = "jk") |>
					furrr::future_map(\(x){
						graph.control;

						g <- igraph::graph_from_data_frame(setcolorder(x, c("from.src", "to.src")));

						if (!rlang::is_empty(graph.control)){
							purrr::walk(graph.control, \(i) eval(i, envir = environment()))
						}

						g
					}, .options = furrr_opts) |>
					purrr::compact();

			  # :: Return ----
				message(sprintf("[%s] The event vectors are ready for analysis", Sys.time()));
				invisible(self);
			}
		)}
	# _____ PUBLIC ACTIVE BINDINGS _____
	, active = { list(
		#' @field config This is an active binding that returns the configuration used to instantiate the class.
		config = function(...){ private$.params$config }
		)}
	# _____ PRIVATE _____
	, private = { list(
		# _____ PRIVATE CLASS MEMBERS _____
		.params = { list(config = NULL)}
		, q_table = NULL
		# _____ PRIVATE METHODS _____
		)}
	)}
