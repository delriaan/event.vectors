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
event.vector.space <- { R6::R6Class(
	classname = "event.vector.space"
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
      #' Initialize a Vector Space
      #' @param config (see class method \code{$configure()})
			#' @param events.ascending When \code{TRUE}, the final output will only contain chronological events.
			#' @param ... (not used)
		initialize = function(config = NULL, ...){
				# ::  CLASS MEMBERS INITIALIZATION
				if (!is.null(config)) {
					.expr = rlang::expr(self$configure());

					.expr$src.names 	= config$src.names
					.expr$contexts		= config$contexts
					.expr$map.fields	= config$map.fields
					.expr$row.filters = config$row.filters

					eval(.expr)
				}

				message(sprintf("[%s] Initializing the event vector space", Sys.time()));
				invisible(self);
			}
		, # CONFIGURE() ====
			#' @description
			#' Configuration is achieved via the object passed to \code{config} used at initialization or this method:
			#' \itemize{
			#' \item If the former method is used, object must be a \code{\link{data.frame}}, \code{\link{data.table}}, \code{\link{list}}(-like) with the following columns: \code{src.names}, \code{contexts}, \code{map.fields}, and \code{row.filters}. It is important to set the configuration data.frame correctly by ensuring the elements of each row are related to the same dataset; otherwise, the wrong filter may attempted to be applied to a non-existent column.
			#' \item If using this method, the same fields are available as arguments with the additional arguments \code{src.mix} and \code{exclude.mix} that can be used to override the default behavior of how class method \code{$evs.universe()} cross-compares sources.
			#' }
      #' @param src.names An atomic vector of strings containing the R workspace names of the source datasets given in "ENVIRONMENT$object" or "object" format
      #' @param contexts An atomic vector of strings serving as labels for each data source
      #' @param map.fields One of two forms are supported:
      #' \itemize{
      #' \item An atomic vector of delimited strings, each string indicating the columns that serve as the join-key (\code{jk}), lower boundary (\code{time_start_idx}) and upper boundary (\code{time_end_idx}) in that order
      #' \item A list of length-three array of strings following the ordering rule stated above
      #' }
      #'
      #' @param row.filters One of two forms are supported:
      #' \itemize{
      #' \item An atomic vector of strings, each element being used to filter the corresponding dataset before processing.  Each string should contain a logical statement of predicates such as \code{\eqn{(a \le b) | ((i + 10) \ge k[1])}}
      #' \item An expression list of similar construction
      #' }
      #'
      #' @param src.mix \describe{
			#'	\item{"combination":}{ The default: generates unique pairs of sources}
			#'	\item{"reflexive":}{ Compares each source to itself}
			#'	\item{"all":}{ The union of the preceding options}
			#'	} \cr Partial matching is allowed, and generated combinations include the mirror (e.g., "A, B" will have a "B, A" combo pair generated).
			#'
      #' @param exclude.mix A list of vectors containing each source pair context to exclude (e.g. \code{list(c("A", "C"), c("u", "k"))}). \code{\link{evs_exclude.blender}} can be invoked to create this list more quickly.  Combinations are not automatically mirrored as is the case with 'src.mix'.
      #'
      #' @param update (logical) \code{TRUE} results in appending to the existing configuration
      #' @param chatty (logical | \code{FALSE}) Verbosity flag
			configure =	function(src.names, contexts, map.fields, row.filters, src.mix = "comb", exclude.mix = list(c("", "")), update = FALSE, chatty = FALSE){
				this.cfg <- data.table(src.names, contexts, map.fields, row.filters = purrr::map_chr(row.filters, ~ifelse(!is.character(.x), deparse(.x), .x)));

				private$.params$config <- if (!update | is.null(private$.params$config)){
						data.table::rbindlist(list(private$.params$config, this.cfg), use.names = TRUE)
					} else { this.cfg }

				private$.params$config[, jk.vec := purrr::map2(src.names, map.fields, ~{
							jk.col = if (length(.y) > 1){
								.y[1]
							} else {
								stringi::stri_split_regex(.y, "[,|: ]+", simplify = TRUE, omit_empty = TRUE) |> unlist() %>% .[1] }
								str2lang(sprintf("%s[, sort(unique(%s))]", .x, jk.col)) |> eval()
							})];

				setattr(private$.params$config, "src.mix", src.mix) |>
					setattr("exclude.mix", sapply(exclude.mix, function(i){
						paste0(if (length(i) == 1){ c(i, i) } else if(length(i) > 2) { i[1:2] } else { i }, collapse = ", ")
					})) |>
					setattr("jk", .$jk.vec |> unlist() |> unique() |> purrr::set_names()) |>
					setkey(contexts);

				# Sanity checks ...
				apply(X = this.cfg, MARGIN = 1, FUN = function(i){
						.cfg = as.list(i);
						.data = parse(text = .cfg$src.names) |> eval() |> eval(envir = globalenv());

						cat(sprintf("===== Validating '%s' =====\n", .cfg$src.names));

						if ((sum(dim(.data), na.rm = TRUE) > 0) & any(class(.data) %in% c("data.table", "data.frame", "tibble"))){
							message(sprintf("%s found ...", .cfg$src.names));
							.colnames = colnames(.data);

							.map.fields = if (length(unlist(.cfg$map.fields)) > 1){
									unlist(.cfg$map.fields)
								} else {
									.cfg$map.fields |> stringi::stri_split_regex("[, ]", simplify = TRUE, omit_empty = TRUE) |> unlist()
								}

							if (chatty){ print(list(map.fields = c(.map.fields), src.colnames = c(.colnames))) }

							if (all(.map.fields %in% .colnames)){
								message("All checks passed: continuing ..");
								} else {
									message(sprintf(
										"Error: %s missing but declared in mapping fields!"
											, setdiff(.map.fields, intersect(.map.fields, .colnames)) |> paste(collapse = ", ")
										));
									return();
								}
							} else {
								message(sprintf(
									"Error: %s not found or not of class {'data.table', 'data.frame', 'tibble'}"
									, .cfg$src.names
									));
								return();
							}
					});

				# @def q_table sets the allowable comparisons before any calculations are done
				private$q_table <- {
					# .temp will be a matrix at this point
					.temp = private$.params$config$contexts |> utils::combn(m = 2) %>% cbind(apply(., 2, rev)) |> t()

					# enforce 'src.mix'
					if (!src.mix %ilike% "reflex|all"){ .temp %<>% .[.[, 1] != .[, 2], ] }

					# enforce 'exclude.mix' after converting .temp to a 'data.table' object
					.temp %<>% as.data.table() |> setnames(c("from", "to"));
					.temp[!purrr::pmap_lgl(.temp, ~list(c(.x, .y)) %in% exclude.mix)]
		  	}

				invisible(self);
			}
		, # SET.DATA() ====
			#' @description
			#' \code{set.data()} adds column \code{src} to the objects referenced by the configuration argument.  It also converts temporal fields based on the value of parameter \code{units}
			#'
			#' @param chatty (logical | \code{FALSE}) Verbosity flag
		 set.data = function(chatty = FALSE){
				if (is.null(private$.params$config)){
					stop("No class object configuration detected. Provide a configuration set using `$configure()` involving at least two (2) temporal datasets."); 				}

				# `set.data()` adds columns used in the call to `cross.time()` to the objects referenced by `private$.params$config$src.names`
				setattr(private$.params$config, "data_is_set", FALSE);
				if (chatty) { message(sprintf("[%s] Augmenting source event data ...", Sys.time())) }

			  #  Adding columns `src` to the object referenced by the parsed value of .x
				purrr::pwalk(as.list(private$.params$config[, .(src.names,	contexts, map.fields, row.filters)]), ~{
					message(sprintf("Processing %s (%s)", ..1, ..2));

				  # Note that the following parses into a `data.table` syntax
					.mflds = stringi::stri_split_regex(..3, "[, ]", omit_empty = TRUE, simplify = TRUE) |> unlist() |> unclass();
					if (chatty){ print(.mflds) }

					rlang::expr({
						if (!is.data.table(!!str2lang(..1))){ !!str2lang(sprintf("%s <- as.data.table(%1$s)", ..1)) }

						(!!str2lang(..1))[
							, `:=`(
									src 				= !!..2
									, jk				= !!str2lang(.mflds[1])
									, start_idx = !!str2lang(.mflds[2])
									, end_idx 	= !!str2lang(.mflds[3])
									, row.filters = !!..4
									, rec_idx 	= sequence(nrow((!!str2lang(..1))))
									)
								] |>
							setkeyv(c('jk', 'start_idx', 'end_idx')) %>%
							setcolorder(c(key(.), 'row.filters'))
						}) %T>% { if (chatty) print(.) } |> eval()
				});

				setattr(private$.params$config, "data_is_set", TRUE);
				invisible(self);
			}
		, # SET.Q_GRAPHS() ====
			#' @description
			#' \code{set.q_graphs()} creates a list of 'query graphs' for each unique value of 'jk'.  The list is stored as class member \code{q_graph}
			#' @param chatty (logical | \code{FALSE}) Verbosity flag
			set.q_graphs = function(chatty = FALSE){
		 		if (is.null(private$.params$config)){
		 			stop("No class object configuration detected.  Provide a configuration set using `$configure()` involving at least two (2) temporal datasets.")}

		 		if (!attr(private$.params$config, "data_is_set")){
		 			message("Source event data not prepared.  Executing '$set.data()'");
		 			self$set.data(chatty = chatty);
		 		}

		 		setattr(private$.params$config, "graphs_created", FALSE);

		 		self$q_graph <- { purrr::map(attr(private$.params$config, "jk"), ~{
			 			# :: Select events for which the current value of jk has data
			 			this.jk = .x;

			 			.evts = private$.params$config[
			 				(purrr::map_lgl(jk.vec, ~this.jk %in% .x)), !"jk.vec"
							][
							, map.fields := if (is.list(map.fields)){
									purrr::map_chr(map.fields, paste, collapse = "|")
								} else map.fields
							] |> unique();

			 			# :: Create a configuration graph from the current value of jk and it's associated events
			 			g = cbind(private$q_table[c(and(from %in% .evts$contexts, to %in% .evts$contexts))], jk = this.jk);

			 			if (nrow(g) == 0){ NULL } else {
			 				g = igraph::graph_from_data_frame(g);

				 			# :: Retrieve the map fields, which include time, from the source events
				 			igraph::V(g)$data <- purrr::map(igraph::V(g), ~{
					 				v_name = stringi::stri_replace_first_regex(.x$name, "[:][0-9]+", "");
					 				.logi_vec = which(purrr::map_lgl(.evts$contexts, ~v_name %ilike% .x));

					 				if (identical(integer(), .logi_vec)){
					 					data.table(jk = this.jk, X = "no data")
					 				} else {
						 				.evts[(.logi_vec), sprintf("%s[(jk == %s) & (%s)]", src.names, this.jk, row.filters)] |>
					 						purrr::map(~str2lang(.x) |> eval(envir = globalenv()))
					 				}
						 		}) |> purrr::flatten();

				 			# :: Return the graph
				 			g
			 			}
			 		}) |> purrr::compact();
				}

		 		if (!rlang::is_empty(self$q_graph)){
			 		if (chatty){ message(sprintf("Created %s query graphs", self$q_graph |> length())) }
		 			setattr(private$.params$config, "graphs_created", TRUE);
		 		} else if (chatty){ message("No graphs greated") }

				invisible(self);
			}
		)}
	# _____ PUBLIC ACTIVE BINDINGS _____
	, active = { list(
		#' @field config This is an active binding that returns the configuration used to instantiate the class.
		config = function(...){
			copy(private$.params$config) |>
				setkey(contexts) %T>%
				setattr("events.ascending", private$.params$events.ascending)}
		)}
	# _____ PRIVATE _____
	, private = { list(
		# _____ PRIVATE CLASS MEMBERS _____
		.params = { list(config = NULL)}
		, q_table = NULL
		# _____ PRIVATE METHODS _____
		)}
	)}
