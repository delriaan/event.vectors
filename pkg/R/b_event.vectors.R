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
#'      \item{"crossed" time output}{ See \code{\link{cross_time}()}}
#'      \item{\code{fr_\*}, \code{to_\*}}{The temporal ranges of each \emph{from}/\emph{to}event}
#'      \item{\code{src_pair}}{Values of the "from-to" pairings of events given by the contexts provided: FROM:TO}
#'      \item{\code{from_coord}}{String representation of temporal boundaries expressed as concatenated integers}
#'      \item{\code{to_coord}}{String representation of temporal boundaries expressed as concatenated integers}
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
      # @param src_defs Strings or expressions that define the data to use (must return \code{\link{data.frame}}, \code{\link{data.table}}, or list)
      # @param contexts An atomic vector of strings serving as labels for each data source
      # @param map_fields A list of column name vectors for each definition in \code{src_defs} providing data for the \emph{join key} (\code{jk}), \emph{start} (\code{time_start_idx}), and \emph{end} (\code{time_end_idx}) references. Provide names for elements in each vector to specify which data source field is mapped to each name (\code{jk}, \code{time_start_idx}, and \code{time_end_idx}).\cr Regarding \emph{join key}: choose the columns having values that are shared across two or more event sources.
      #' @param ... \code{\link[rlang]{dots_list}} Objects of class 'Event'
			#'
      #' @param src_mix \describe{
			#'	\item{"combination":}{ The default: generates unique pairs of sources}
			#'	\item{"refl0exive":}{ Compares each source to itself}
			#'	\item{"all":}{ The union of the preceding options}
			#'	} \cr Partial matching is allowed, and generated combinations include the mirror (e.g., "A, B" will have a "B, A" combo pair generated).
			#'
      #' @param exclude_mix A list of vectors containing each source pair context to exclude (e.g. \code{list(c("A", "C"), c("u", "k"))}). \code{\link{evs_exclude.blender}} can be invoked to create this list more quickly.  Combinations are not automatically mirrored as is the case with \code{src_mix}.
      #'
      #' @param chatty (logical | \code{FALSE}) Verbosity flag
			configure =	function(..., src_mix = "comb", exclude_mix = list(), chatty = FALSE){
					if (!rlang::is_empty(exclude_mix) & chatty){
						cli::cli_alert_info("Source-mix exlusions detected")
					}

					src_defs <- rlang::dots_list(..., .homonyms = "last", .check_assign = TRUE)
					assertive::assert_is_identical_to_true(!any(names(src_defs) == ""))
					assertive::assert_all_are_true(sapply(src_defs, \(x) x %isa% Event))
					contexts <- names(src_defs)
					private$.params$config <- src_defs

					# @def q_table sets the allowable comparisons before any calculations are done
					private$q_table <- local({
						# .temp will be a matrix after this operation
						.temp <- names(src_defs) |>
							utils::combn(m = 2) %>%
							cbind(apply(., 2, rev)) |>
							t()

						# enforce 'src_mix'
						if (!grepl("reflex|all", src_mix, ignore.case = TRUE)){
							.temp <- .temp[.temp[, 1] != .temp[, 2], ]
							}

						# enforce 'exclude_mix' after converting .temp to a 'data.table' object
						.temp <- data.table::as.data.table(.temp) |>
							data.table::setnames(c("from", "to"))

						.temp[!purrr::pmap_lgl(.temp, \(...) list(c(...elt(1), ...elt(2))) %in% exclude_mix)] |>
							data.table::setkey(from, to)
						})

					data.table::setattr(private$.params$config, "src_mix", as.character(rlang::enexpr(src_mix)))

					if (!rlang::is_empty(exclude_mix)){
						data.table::setattr(private$.params$config, "exclude_mix", sapply(exclude_mix, function(i){
								paste0(if (length(i) == 1){ c(i, i) } else if(length(i) > 2) { i[1:2] } else { i }, collapse = ", ")
							})) %>%
							data.table::setattr("jk", {
								purrr::map(., \(x) x$jk.vec |> rlang::eval_tidy()) |>
									magrittr::freduce(list(unlist, unique, sort, purrr::set_names))
								})
						} else {
							data.table::setattr(private$.params$config, "exclude_mix", NA)
						}

				# :: Return
				invisible(self)
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
			#' @param time_control A 2-element list containing the minimum and maximum values allowed for total temporal span between two events
			#' @param graph_control An expression list containing \code{\link[igraph]{igraph-package}} calls to manipulate the internally-created graph in the order provided.  Use symbol \code{g} to generically denote the graph and \code{<<-} to update \code{g}.
			#' @param unit (See \code{\link{cross_time}})
			#' @param in_parallel (logical | \code{FALSE}) Shoudl parallelism via \code{\link[furrr]{future_map}} be enabled?
			#' @param graph_only (logical | \code{FALSE}) \code{TRUE} assumes class member \code{$space} exists (possibly after external modification) and recreates member \code{$evt_graphs}
			#' @param chatty (logical | \code{FALSE}) Verbosity flag
      #' @param cache (See \code{\link{cross_time}})
			#'
			#' @return Invisibly, the original object augmented with new member \code{space}
			#'
			#' @note The user is responsible for setting the \code{\link[future]{plan}} when \code{in_parallel=TRUE}.
			#'
			make.evs_universe = function(..., time_control = list(-Inf, Inf), graph_control = NULL, unit = NULL, in_parallel = FALSE, graph_only = FALSE, chatty = FALSE, cache = NULL){
				if (!rlang::is_empty(cache)){
          if (any(grepl("cache", class(cache)))){ invisible(NULL) }
          if (is.logical(cache) && identical(TRUE, cache)){
              cache <- cachem::cache_disk(destroy_on_finalize = FALSE)
            }
        }
        # :: `make_event_key` is a function used to create sequential unique identifiers
				# 	for events sources and time-markers ----
					make_event_key <-  function(...){
								from <- paste(...elt(1), ...elt(2), sep = "_")
								to <- paste(...elt(3), ...elt(4), sep = "_")

								event_space <- unique(sort(c(from, to))) |>
										(\(x) rlang::set_names(data.table::frank(x, ties.method="dense"), x))()

								event_index <- \(x){
												book.of.utilities::count.cycles(!duplicated(x), reset = FALSE) %>%
												stringi::stri_pad_left(width = as.integer(log10(max(., na.rm = TRUE))) + 1, pad = "0")
										}

								list(
										paste(...elt(1), event_space[from] |> event_index(), sep = ":")
										, paste(...elt(3), event_space[to] |> event_index(), sep = ":")
										)
						}

				# :: Event combination exclusions:
					src_mix <- self$.__enclos_env__$private$q_table |> as.matrix()
					.src_space <- self$config |> names()

				exclude_mix <- attr(private$.params$config, "exclude_mix") |> na.omit()

				if (!rlang::is_empty(exclude_mix)){
						sapply(c(1,2), \(x){ which(src_mix[, x] %in% simplify2array(exclude_mix)[, x]) }) |>
						unlist() %>%
						.[duplicated(.)] |>
						(\(i){ invisible(src_mix <<- src_mix[-i, ]) })()
				}

				# :: Retrieve the essential columns from sources and create a compact intermediate data structure ----
					if (chatty){ message("Creating `.tmp_space` ...") }

					.tmp_space <- purrr::map(self$config, \(x){
								purrr::map(x[-1], rlang::eval_tidy) |>
									append(list(src = deparse(x$label))) |>
									data.table::as.data.table()
							}) |>
							data.table::rbindlist() |>
							data.table::setorder(jk, time_start_idx, time_end_idx) |>
							data.table::setkey(jk, time_start_idx, time_end_idx)

					.row_filter <- .tmp_space %$% cbind(
							f_src_exists = src %in% src_mix[, "from"]
							, t_src_exists = src %in% src_mix[, "to"]
							) %>%
							cbind(all_exists = .[,"f_src_exists"] & .[,"t_src_exists"])

					.tmp_space <- .tmp_space[(.row_filter[, "all_exists"])] |> unique()

				# :: Use optimization from 'data.table' to create `self$space` ----
					edge_filter <- rlang::enexprs(..., .named = FALSE, .ignore_empty = "all") |> c(TRUE);

					if (!graph_only){
						if (chatty){ message("Creating `self$space`...") }
						# browser()

						self$space <- local({
								from_src <- .tmp_space[which(.row_filter[, "f_src_exists"])] |>
									na.omit() |>
									data.table::setnames(c("jk", "f_start_idx", "f_end_idx", "f_src"))

								to_src <- .tmp_space[which(.row_filter[, "t_src_exists"])] |>
									na.omit() |>
									data.table::setnames(c("jk", "t_start_idx", "t_end_idx", "t_src"))

								data.table::merge.data.table(
									x = from_src
									, y = to_src
									, by = "jk"
									, allow.cartesian = TRUE
									)[
									# Monotonically-increasing time:
									(t_start_idx - f_start_idx) > 0
									][
									# Event combination exclusions:
									if (!rlang::is_empty(exclude_mix)){
										-which(paste(from_src$f_src, to_src$t_src, sep = ",") %in% sapply(exclude_mix, paste, collapse =  ","))
									} else { TRUE }
									# Cross Time:
									, cbind(
											.SD
											, cross_time(
													s0 = f_start_idx
													, s1 = t_start_idx
													, e0 = f_end_idx, e1 = t_end_idx
													, control = time_control
													, unit = unit
                          , cache = cache
													)
											)
									]
							})

            if (!"epsilon" %in% ls(self$space)){
              return(invisible(self))
            }

						if (chatty){ message("Finalizing `self$space`...") }

						edge_filter <- edge_filter |> as.character() |> paste(collapse = " & ") |>
								rlang::parse_expr() |> rlang::eval_tidy(data = self$space);

						self$space <- { self$space[
									# Enforce row filter rules before proceeding: note that this pre-derives a Boolean vector in order
									#		to make debugging easier and to ensure the integrity of the operation.
									(edge_filter)
									][
									# Impute sequencing on event sources: this has a direct impact when creating distinct vertex names during subsequent graph creation
									, c("f_src", "t_src") := make_event_key(f_src, from_coord, t_src, to_coord)
									, by = jk
									][, src_pair := sprintf("%s -> %s", f_src, t_src)
									][, data.table::setnames(.SD, c("f_src", "t_src"), c("from_src", "to_src"))
									][, epsilon_desc := factor(epsilon_desc, levels = c("NA", "Full Concurrency", "Concurrency", "Continuity", "Disjoint"), ordered = TRUE)
									][
									# Remove loops (may result in removing all records)
									(\(logi_vec){
											if (!any(logi_vec)){
													cli::cli_alert_warning("All relationships are loops: returning values as-is.")
													!logi_vec
											} else {
													if (chatty && (attr(self$config, "src_mix") == "comb")){
															cli::cli_alert_info("Removing loops")
													}
													logi_vec
											}
										})(from_src != to_src)
										# Column selection
									, .(jk, from_src, to_src, beta, mGap, mSt, mEd, epsilon, epsilon_desc, from_len, to_len, from_coord, to_coord, src_pair, x_filter)
									]
							}
							# self$space[, !"jk"] |> keep(\(x) !(is.character(x) | is.factor(x) | is.logical(x)))
							# self$space[, !"jk"] |> discard(\(x) !(is.character(x) | is.factor(x) | is.logical(x)))
					}

				# :: Create `self$evt_graphs` from `self$space` ----
					message(sprintf("[%s] ... creating event graphs", Sys.time()));
					# `furrr_opts` will be created whether or not the function is
					# invoked with `in_parallel = TRUE`
					furrr_opts <- { furrr::furrr_options(
											scheduling	= Inf
											, seed			= TRUE
											, packages	= c("magrittr", "data.table", "rlang", "cachem")
											, globals 	= c("graph_control", "time_control", "units", "cross_time", "edge_filter", "space")
											)}

					mapper <- if (in_parallel){ furrr::future_imap } else { purrr::imap }
					pbar <- if (in_parallel){ FALSE } else { list(format = "{cli::pb_spin} Creating event graphs | Elapsed: {cli::pb_elapsed} (ETA: {cli::pb_eta})") }
					space <- self$space

					spsUtil::quiet(self$evt_graphs <- self$space[(x_filter), unique(jk)] %>%
					 	rlang::set_names(
					 		sprintf("graph_jk_%s", stringi::stri_replace_all_fixed(make.names(.), ".", "_", vectorize_all = FALSE)) |>
	            	tolower()
					 		) |>
						mapper(\(x, nm, ..., cache_dir = NULL){
								gc()
                pid <- Sys.getpid()

								tryCatch({
									g <<- data.table::setcolorder(space, c("from_src", "to_src"))[(jk == x)] |>
										igraph::graph_from_data_frame()

									if (!rlang::is_empty(graph_control)){
                    purrr::walk(graph_control, \(i) eval(i, envir = environment()))
                  }

                  # Tag the output with the process ID (useful for distributed workers)
                  g <<- igraph::set_graph_attr(g, "pid", pid)

                  if (!rlang::is_empty(cache_dir)){
                      cache <- cachem::cache_disk(dir = cache_dir, destroy_on_finalize = FALSE)
                      cache$set(key = nm, value = g)

                      rlang::expr(readRDS(file = !!dir(cache$info()$dir, pattern = paste0(nm, "\\."), full.names = TRUE)))
                  } else { g }
								}, error = \(e){
                  cat(sprintf("[Error (%s | %s)", nm, pid), paste(deparse(e), collapse = "\n"), sep = "\n");
                  return(NULL)
                })
            }, .progress = pbar, .options = furrr_opts, cache_dir = (if (!rlang::is_empty(cache)) cache$info()$dir)) |>
						purrr::compact() |>
            magrittr::set_attr("cache", cache)
            )

				# :: Return ----
					message(sprintf("[%s] The event vectors are ready for analysis", as.character(Sys.time())))
					invisible(self)
			}
		)}
	# _____ PUBLIC ACTIVE BINDINGS _____
	, active = { list(
        #' @field config This is an active binding that returns the configuration used to instantiate the class.
        config = function(...){ private$.params$config },
        #' @field set_private Set or get private variables
        set_private = function(...) {
          dots <- rlang::dots_list(..., .homonyms = "last")
          if (...length() == 0) {
            invisible(self)
          } else {
            for (name in names(dots)) {
              private[[name]] <- dots[[name]]
            }
          }
        }
		  )}
	# _____ PRIVATE _____
	, private = { list(
		# _____ PRIVATE CLASS MEMBERS _____
		.params = { list(config = NULL)}
		, q_table = NULL
		# _____ PRIVATE METHODS _____
		)}
	)}
