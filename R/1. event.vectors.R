#' Event Vectors
#'
#' @description
#' \code{event.vectors} is an R6 class object that creates a temporally-compared space of "\emph{event vectors}", each comprised of a 'from' and 'to' temporal marker.
#'
#' The time between these events is the focus of derivation: properly, the event vector is a complex number that encodes the relationship between the boundaries of the events, thus allowing one to describe this relationship in a concise manner.
#'
#' \cr
#' @section Execution Workflow:
#' The initial execution order should look something like the following ...
#' \cr
#' \code{ event.vectors$new(...)$configure(...)$set.data(...)$set.q_graphs(...) \%>\% make.evs_universe(...) }.  The ability to execute the preceding workflow out of order exists, but it is best to adhere to the provided flow the first time around.
#' \cr
#' \cr
#' @section Class Member "Space":
#' \code{$space} is a \code{\link[data.table]{data.table}} that is populated upon execution of \code{\link{make.evs_universe}()}:
#'    \describe{
#'      \item{\code{jk}}{Values of the "join key"}
#'      \item{"crossed" time output}{ See \code{\link{cross.time}()}}
#'      \item{\code{fr_*}, \code{to_*}}{The temporal ranges of each \emph{from}/\emph{to}event}
#'      \item{\code{src.pair}}{Values of the "from-to" pairings of events given by the contexts provided: FROM:TO}
#'      \item{\code{from.coord}}{String representation of temporal boundaries expressed as concatenated integers}
#'      \item{\code{to.coord}}{String representation of temporal boundaries expressed as concatenated integers}
#'    }
#' \cr
#'
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
      #' @param map.fields A list of column name vectors for each definition in \code{src.defs} providing data for the \emph{join key} (\code{jk}), \emph{start} (\code{time_start_idx}), and \emph{end} (\code{time_end_idx}) references. Provide names for elements in each vector to specify which data source field is mapped to each name (\code{jk}, \code{time_start_idx}, and \code{time_end_idx}).
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

					private <- new.env()
					fld_nms <- c("jk", "time_start_idx", "time_end_idx")

					make_refs <- purrr::as_mapper(~sapply(.x, magrittr::freduce, list(eval, as.character)) |> rlang::parse_exprs())

					make_quos <- purrr::as_mapper(~{
						.this <- sapply(.y, magrittr::freduce, list(eval, as.character))
						.that <- .x
						.that <- rlang::set_names(.that, .this)
						rlang::as_quosures(.that, named = TRUE, env = rlang::caller_env(1))
					})

					set_fld_nms <- purrr::as_mapper(~{
							nms.x <- names(.x)
							nm_pos <- which(nms.x %in% fld_nms)
							na_pos <- setdiff(seq_along(nms.x), nm_pos)
							if (rlang::is_empty(na_pos)){ na_pos <- seq_along(fld_nms) }
							nms.x[na_pos] <- setdiff(fld_nms, nms.x[nm_pos])
							nms.x
						})

					# Create the event data references
					event_refs <- make_refs(rlang::exprs(!!src.defs)) |>	make_quos(rlang::enexprs(contexts))
					event_flds <- purrr::map(map.fields, ~{ rlang::parse_exprs(.x) %>% rlang::set_names(set_fld_nms(.)) })

					# Create a configuration quosure for each data source
					private$.params$config <- list(event_refs, event_flds) |>
						purrr::pmap(~{
								.temp <- rlang::as_quosures(..2, env = rlang::as_data_mask(rlang::eval_tidy(..1)))[fld_nms];
								.temp$jk.vec <- .temp$jk |> rlang::eval_tidy() |> unique() |> sort();

								message(glue::glue("Validating source `{rlang::as_label(rlang::quo_get_expr(..1))}`\n"));
								qa_check <- { c(
									glue::glue("{rlang::as_label(rlang::quo_get_expr(..1))} exists: {assertthat::not_empty(rlang::eval_tidy(..1))}")
									, glue::glue("- ${rlang::as_label(rlang::quo_get_expr(.temp$jk))} exists: {assertthat::not_empty(rlang::eval_tidy(.temp$jk))}" )
									, glue::glue("- ${rlang::as_label(rlang::quo_get_expr(.temp$time_start_idx))} exists: {assertthat::not_empty(rlang::eval_tidy(.temp$time_start_idx))}" )
									, glue::glue("- ${rlang::as_label(rlang::quo_get_expr(.temp$time_end_idx))} exists: {assertthat::not_empty(rlang::eval_tidy(.temp$time_end_idx))}" )
									, "====================================="
									)}
								if (chatty){ purrr::walk(qa_check, message)}

								data.table::setattr(.temp, "src.def", ..1)
							})

					# @def q_table sets the allowable comparisons before any calculations are done
					private$q_table <- {
						# .temp will be a matrix at this point
						.temp = private$.params$config |> names() |> utils::combn(m = 2) %>% cbind(apply(., 2, rev)) |> t()

						# enforce 'src.mix'
						if (!grepl("reflex|all", src.mix, ignore.case = TRUE)){ .temp %<>% .[.[, 1] != .[, 2], ] }

						# enforce 'exclude.mix' after converting .temp to a 'data.table' object
						.temp %<>% data.table::as.data.table() |> data.table::setnames(c("from", "to"));
						.temp[!purrr::pmap_lgl(.temp, ~list(c(.x, .y)) %in% exclude.mix)] |> data.table::setkey(from, to)
					}

					private$.params$config %<>%
						data.table::setattr("src.mix", as.character(rlang::enexpr(src.mix))) |>
						data.table::setattr("exclude.mix", sapply(exclude.mix, function(i){
							paste0(if (length(i) == 1){ c(i, i) } else if(length(i) > 2) { i[1:2] } else { i }, collapse = ", ")
						})) %>%
						data.table::setattr("jk", {
							purrr::map(., ~.x$jk.vec |> rlang::eval_tidy()) |>
								magrittr::freduce(list(unlist, unique, sort, purrr::set_names))
							})

					# invisible(private)
					invisible(self);
				}
		, # SET.Q_GRAPHS() ====
			#' @description
			#' \code{set.q_graphs()} creates a list of 'query graphs' for each unique value of 'jk'.  The list is stored as class member \code{q_graph}
			#' @param chatty (logical | \code{FALSE}) Verbosity flag
			set.q_graphs = function(chatty = FALSE){
		 		if (is.null(private$.params$config)){
		 			stop("No class object configuration detected.  Provide a configuration set using `$configure()` involving at least two (2) temporal datasets.")}

		 		data.table::setattr(private$.params$config, "graphs_created", FALSE);

		 		events_by_jk <- purrr::imap(private$.params$config, ~{
						jk <- private$.params$config |> attr("jk");

						exists_in <- jk %in% rlang::eval_tidy(.x$jk);

						matrix(exists_in, ncol = 1, dimnames = list(jk, .y));
					}) |> magrittr::freduce(list(list, data.table::rbindlist));

				events_by_jk %<>% {
					.[
					, jk := private$.params$config |> attr("jk")
					][
					, purrr::pmap_dfr(.SD, function(...){
								jk_events <- ...names()[-1][c(...)[-1]];

								qt <- private$q_table[(from %in% jk_events), .SD[(to %in% jk_events), .(to)], by = from];

								if (nrow(qt) == 0){ list(from = NA, to = NA) } else { unique(qt) }
							})
					, by = jk
					][!is.na(from), .(from, to, jk)] |>
					split(by = "jk")
				}

		 		self$q_graph <- purrr::imap(events_by_jk, ~{
	 					# Graphs for each level of 'jk'
	 					g <- igraph::graph_from_data_frame(.x);

	 					this_jk <- rlang::parse_expr(.y);

	 					# Return the minimal information needed to reconstruct the source data
	 					# Note: the following allows for calling `igraph::V(g)$data$value()`
	 					igraph::V(g)$data <- purrr::imap(igraph::V(g), ~{
								event <- private$.params$config[[.y]];

								jk_val <- eval(as.call(list(rlang::parse_expr(paste0("as.", rlang::eval_tidy(event$jk)[1] |> typeof())), this_jk)));

								value <- function(time_start_idx = TRUE, time_end_idx = TRUE, trace = FALSE){
									# Argument 'trace' is used for retracing exact source row and uses the expressions for 'time_*_idx' for row-wise filtering

									vars <- event %$% mget(c("jk", "time_start_idx", "time_end_idx")) |> purrr::map(~rlang::quo_get_expr(.x))

									time_start_idx <- rlang::enexpr(time_start_idx);
									time_end_idx <- rlang::enexpr(time_end_idx);

									if (trace){
										if (length(time_start_idx) > 1){ time_start_idx[[2]] <- vars$time_start_idx }
										if (length(time_end_idx) > 1){ time_end_idx[[2]] <- vars$time_end_idx }
									}

									if (trace){ vars <- rlang::sym(".SD") } else { vars <- purrr::map_chr(unname(vars), rlang::as_label) }

									row_expr <- rlang::exprs(!!rlang::quo_get_expr(event$jk) == !!jk_val, !!time_start_idx, !!time_end_idx)[c(TRUE, trace, trace)] |>
										purrr::map_chr(rlang::as_label) |>
										sprintf(fmt = "(%s)") |>
										paste(collapse = " & ") |>
										rlang::parse_expr()

									rlang::expr(rlang::eval_tidy(attr(event, "src.def"))[(!!row_expr), !!vars]) |> print() |>eval()
								}

								mget(ls()) |> list2env(envir = new.env())
	 						});

	 					g
	 				});

		 		if (!rlang::is_empty(self$q_graph)){
			 		if (chatty){ message(sprintf("Created %s query graphs", self$q_graph |> length())) }

		 			data.table::setattr(private$.params$config, "graphs_created", TRUE);
		 		} else if (chatty){ message("No graphs greated") }

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
