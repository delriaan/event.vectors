#' Event Vector Space
#'
#' A relational vector space of events
#'
#' @description
#' \code{event.vector.space} is an \code{\link{R6Class}} that creates a temporally-compared space of \emph{event vectors}, each comprised of a 'from' and 'to' date.  The time between these events is the focus of derivation: properly, the event vector is a complex number that encodes the relationship between the boundaries of the events, thus allowing one to describe this relationship in a concise manner.
#'
#' \cr
#' @section Execution Workflow:
#' The initial execution order should look something like the following ...
#' \cr
#' \code{ obj <- event.vector.space$new(...)$configure(...)$set.data(...)$graph.events(...)$time.controller() \%>\% make.evs_universe(...) }.  The ability to execute the preceding workflow out of order exists, but it is best to adhere to the provided flow the first time around.
#' \cr
#' \cr
#' @section Class Member "Space":
#' \code{$space} is a \code{\link{data.table}} that is populated upon execution of \code{\link{make.evs_universe}}:
#'    \describe{
#'      \item{\code{jk: }}{Values of the "join key"}
#'      \item{\code{mGap: }}{Metrics describing the difference between the following temporal boundaries: TO.start, FROM.end}
#'      \item{\code{mSt: }}{Metrics describing the difference between the following temporal boundaries: TO.start, FROM.start}
#'      \item{\code{mEd: }}{Metrics describing the difference between the following temporal boundaries: TO.end, FROM.end}
#'      \item{\code{from.len: }}{The duration of time of each "from" event: units are one the scale of the smallest increment of time represented (e.g., calendar dates in days will have lengths expressed in days)}
#'      \item{\code{to.len: }}{The duration of time of each "to" event: units are one the scale of the smallest increment of time represented (e.g., calendar dates in days will have lengths expressed in days)}
#'      \item{\code{epsilon: }}{A complex number (e.g., \code{1337 + 0.90210i}) describing the relational changes from one event to another with interpretation based on whether or not the real and imaginary parts are \code{> 0}, \code{< 0}, or \code{ == 0}:
#'      	\tabular{lll}{
#'      	  Re \tab Im \tab Desc \cr
#'      	  {> 0} \tab {== 0} \tab Disjoint \cr
#'      	  {== 0} \tab {> 0} \tab Concurrency \cr
#'      	  {> 0} \tab {> 0} \tab Full Concurrency \cr
#'      	  {== 0} \tab {== 0} \tab Continuity \cr
#'      	}}
#'      \item{\code{epsilon.desc: }}{A plain-language description of \code{epsilon}}
#'      \item{\code{fr_*: }}{The temporal ranges of each "from" event}
#'      \item{\code{to_*: }}{The temporal ranges of each "to" event}
#'      \item{\code{src.pair: }}{Values of the "from-to" pairings of events given by the contexts provided: FROM:TO}
#'      \item{\code{from.coord: }}{String representation of temporal boundaries expressed as concatenated integers}
#'      \item{\code{to.coord: }}{String representation of temporal boundaries expressed as concatenated integers}
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
		initialize = function(config = NULL, events.ascending = TRUE, ...){
				# ::  CLASS MEMBERS INITIALIZATION
				if (!is.null(config)) {
					.expr = substitute(self$configure());

					.expr$src.names 	= config$src.names
					.expr$contexts		= config$contexts
					.expr$map.fields	= config$map.fields
					.expr$row.filters = config$row.filters

					eval(.expr)
				}
				private$.params$events.ascending	= events.ascending;

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

				this.cfg = data.table(src.names, contexts, map.fields, row.filters = row.filters %>%
																map_chr(~ifelse(!is.character(.x), deparse(.x), .x)));

				private$.params$config <- if (!update | is.null(private$.params$config)){
					this.cfg } else{
						rbindlist(list(private$.params$config, this.cfg), use.names = TRUE) %>% unique()
					}

				private$.params$config[, jk.vec := map2(src.names, map.fields, ~{
						jk.col = if (length(.y) > 1){ .y[1] } else {
							stringi::stri_split_regex(.y, "[,|: ]+", simplify = TRUE, omit_empty = TRUE) %>% unlist() %>% .[1] }
							str2lang(sprintf("%s[, sort(unique(%s))]", .x, jk.col)) %>% eval()
						})];

				setattr(private$.params$config, "src.mix", src.mix) %>%
					setattr(
						"exclude.mix"
						, sapply(exclude.mix, function(i){ {
								if (length(i) == 1){ c(i, i) } else if(length(i) > 2) { i[1:2] } else { i }
								} %>% paste0(collapse = ", ")
							})
						) %>%
					setattr("jk", .$jk.vec %>% unlist() %>% unique() %>% set_names()) %>%
					setkey(contexts);

				# Sanity checks ...
				apply(X = this.cfg, MARGIN = 1, FUN = function(i){
						.cfg = as.list(i);
						.data = parse(text = .cfg$src.names) %>% eval %>% eval(envir = globalenv());

						cat(sprintf("===== Validating '%s' =====\n", .cfg$src.names));

						if ((sum(dim(.data), na.rm = TRUE) > 0) & any(class(.data) %in% c("data.table", "data.frame", "tibble"))){
							message(sprintf("%s found ...", .cfg$src.names));
							.colnames = colnames(.data);

							.map.fields = if (length(unlist(.cfg$map.fields)) > 1){
								unlist(.cfg$map.fields)
								} else {
									.cfg$map.fields %>% stringi::stri_split_regex("[, ]", simplify = TRUE, omit_empty = TRUE) %>% unlist()
								}

							if (chatty){ print(list(map.fields = c(.map.fields), src.colnames = c(.colnames))) }

							if (all(.map.fields %in% .colnames)){
								message("All checks passed: continuing ..");
								} else {
									message(sprintf(
										"Error: %s missing but declared in mapping fields!"
											, setdiff(.map.fields, intersect(.map.fields, .colnames)) %>% paste(collapse = ", ")
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

				# @def q_matrix sets the allowable comparisons before any calculations are done
				private$q_matrix <- {
						# .temp will be a matrix at this point
						.temp = private$.params$config$contexts %>% utils::combn(m = 2) %>% cbind(apply(., 2, rev)) %>% t()

						# enforce 'src.mix'
						if (!src.mix %ilike% "reflex|all"){ .temp %<>% .[.[, 1] != .[, 2], ] }

						# enforce 'exclude.mix' after converting .temp to a 'data.table' object
						.temp %<>% as.data.table() %>% setnames(c("from", "to"));
						.temp[!pmap_lgl(.temp, ~list(c(.x, .y)) %in% exclude.mix)]
			  	}

				invisible(self);
			}
		, # SET.DATA() ====
			#' @description
			#' \code{set.data()} adds column \code{src} to the objects referenced by the configuration argument
			#' @param chatty (logical | \code{FALSE}) Verbosity flag
		 set.data = function(chatty = FALSE){
				if (is.null(private$.params$config)){
					stop("No class object configuration detected. Provide a configuration set using `$configure()` involving at least two (2) temporal datasets."); }

				# `set.data()` adds columns used in the call to `cross.time()` to the objects referenced by `private$.params$config$src.names`
				setattr(private$.params$config, "data_is_set", FALSE);
				if (chatty) { message(sprintf("[%s] Augmenting source event data ...", Sys.time())); }

			  #  Adding columns `src` to the object referenced by the parsed value of .x
				pwalk(private$.params$config[, .(src.names,	contexts, map.fields, row.filters)] %>% as.list(), ~{
					message(sprintf("Processing %s (%s)", ..1, ..2));

				  # Note that the following parses into a `data.table` syntax
					.mflds = stringi::stri_split_regex(..3, "[, ]", omit_empty = TRUE, simplify = TRUE) %>% unlist() %>% unclass();
					if (chatty){ print(.mflds) }

					substitute({
						if (!is.data.table(i)){ i <- as.data.table(i) };

						i[, `:=`(src = j, jk = l, start_idx = m, end_idx = n, row.filters = k)] %>%
							setkeyv(c('jk', 'start_idx', 'end_idx')) %>%
							setcolorder(c(key(.), 'row.filters'))
						}
						, list(
								i = str2lang(..1)
								, j = ..2
								, k = ..4
								, l = str2lang(.mflds[1])
								, m = str2lang(.mflds[2])
								, n = str2lang(.mflds[3])
								)
						) %>% eval()
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

		 		self$q_graph <- { map(attr(private$.params$config, "jk"), ~{
			 			# :: Select events for which the current value of jk has data
			 			this.jk = .x;
			 			.evts = private$.params$config[(map_lgl(jk.vec, ~this.jk %in% .x)), !"jk.vec"
								 		][, map.fields := if (is.list(map.fields)){ map_chr(map.fields, paste, collapse = "|") } else map.fields] %>%
			 							unique();

			 			# :: Create a configuration graph from the current value of jk and it's associated events
			 			g = cbind(private$q_matrix[c(and(from %in% .evts$contexts, to %in% .evts$contexts))], jk = this.jk);

			 			if (nrow(g) == 0){ NULL } else {
			 				g = graph_from_data_frame(g);
			 				# V(g)$name <- V(g)$title %>% stri_extract_first_regex("[A-Z]+[:][0-9]+");

				 			# :: Retrieve the map fields, which include time, from the source events
				 			V(g)$data <- map(V(g), ~{
					 				v_name = stri_replace_first_regex(.x$name, "[:][0-9]+", "");
					 				.logi_vec = which(map_lgl(.evts$contexts, ~v_name %ilike% .x));

					 				if (identical(integer(), .logi_vec)){
					 					data.table(jk = this.jk, X = "no data")
					 				} else {
						 				.evts[(.logi_vec), sprintf("%s[(jk == %s) & (%s)]", src.names, this.jk, row.filters)] %>%
					 						map(~str2lang(.x) %>% eval(envir = globalenv()))
					 				}
						 		});

				 			# :: Return the graph
				 			g
			 			}
			 		}) %>% purrr::compact();
				}

		 		if (!rlang::is_empty(self$q_graph)){
			 		if (chatty){ message(sprintf("Created %s query graphs", self$q_graph %>% length())) }
		 			setattr(private$.params$config, "graphs_created", TRUE);
		 		} else if (chatty){ message("No graphs greated") }

				invisible(self);
			}
		)}
	# _____ PUBLIC ACTIVE BINDINGS _____
, active = { list(
		#' @field config This is an active binding that returns the configuration used to instantiate the class.
		config = function(...){ copy(private$.params$config) %>% setkey(contexts) %T>% setattr("events.ascending", private$.params$events.ascending)}
	)}
	# _____ PRIVATE _____
, private = { list(
		# _____ PRIVATE CLASS MEMBERS _____
		.params = { list(config = NULL, events.ascending	= NULL)}
		, q_matrix = NULL
		# _____ PRIVATE METHODS _____
		)}
)}
#
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

	expand.grid(x, y) %>% apply(1, cbind) %>% as.data.table %>% map(c) %>% unname()
}
#
melt_time <- function(x, start.names, end.names, ...){
#' Multiple Time Melter
#'
#' \code{melt_time} leverages data.table::melt to transform the input's temporal feature vector into a pair of features indicating temporal duration (i.e., start & stop).  It handles feature vectors of any length.  "Point-in-time" columns are duplicated before melting
#'
#' @param x (object) A data.frame, data.table, or coercible to data.table
#' @param start.names (string or vector) The names of the "start" columns: parsed if given as a delimited string of pattern "[,;|][ ]?"
#' @param end.names (same as `start.names`)
#' @param ... (string list) Optional column names that are "point-in-time" columns.
#'
#' @return A "melted" data.table with temporal feature vector V(start_date, end_date)
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
#' @param ... Logical expression that retain graph edges meeting the conditions
#' @param time.control A 2-element list containing the minimum and maximum values allowed for total temporal span between two events
#' @param graph.control An expression list containing \code{\link[igraph]{igraph-package}} calls to manipulate the internally-created graph in the order provided.  Use symbol \code{g} to generically denote the graph
#' @param omit.na (logical | FALSE) \code{TRUE} removes rows from class member \code{space} that have \code{NA} values
#' @param chatty (logical | FALSE) Verbosity flag
#'
#' @return Invisibly, the original object augmented with new member \code{space}
#'
#' @section Class Member \code{$space}:
#' \code{$space} should have as many rows as the sum of all edge counts for graphs in \code{$q_graph}
#'
#' @section Parallelism:
#' Parallelism over \code{$q_graph} is supported via \code{\link[furrr]{future_map}}.  The user is responsible for setting a \code{\link[future]{plan}}.
#'
#' @export

  edge.filter <- if (...length() > 0){
  	rlang::enquos(..., .named = FALSE, .ignore_empty = "all") %>%
  		map_chr(~{ rlang::get_expr(.x) %>% deparse() %>% sprintf(fmt = "(%s)") }) %>%
  		paste(collapse = " & ") %>% str2lang()
  	} else { TRUE }

  # :: Create self$space from self$q_graph via calls to 'cross.time()'
  .furrr_opts <- furrr_options(scheduling = Inf, packages = c("data.table", "purrr", "igraph", "stringi"));

  # self$space <- imap(self$q_graph, ~{
  self$space <- future_imap(self$q_graph, ~{
  	# Capture the current value of 'jk'
  	jk = as.integer(.y);

  	# Capture the current graph
  	g = .x;

  	# Traverse each row of the 2D array created via ends(); set event vector space metrics for each edge -> { cross.time() ~ from:to }
  	# These are the configuration edges
  	{ pmap(
	  		ends(graph = g, E(g)) %>% as.data.table()
	  		, ~{
		  			fr_data = as.data.table(V(g)[name == ..1]$data[[1]])[, unique(.SD[, .(jk, start_idx, end_idx, src)])][, v_idx := .I] %>% setkey(jk);
		  			to_data = as.data.table(V(g)[name == ..2]$data[[1]])[, unique(.SD[, .(jk, start_idx, end_idx, src)])][, v_idx := .I] %>% setkey(jk);

		  			fr_data[to_data, allow.cartesian = TRUE] %>%
	  				setcolorder(keep(names(.), ~.x %ilike% "start_idx|end_idx")) %$% {
	  					.out = cbind(
	  						from.coord = paste(jk, as.character(start_idx), as.character(end_idx), src, v_idx, sep = ":")
	  						, to.coord = paste(jk, as.character(i.start_idx), as.character(i.end_idx), i.src, i.v_idx, sep = ":")
	  						, src.pair = paste(src, i.src, sep = ":")
	  						, cross.time(s0 = start_idx, s1 = i.start_idx, e0 = end_idx, e1 = i.end_idx, control = time.control, chatty = chatty)
	  						, from_timeframe = map2(start_idx, end_idx, lubridate::interval)
	  						, to_timeframe   = map2(i.start_idx, i.end_idx, lubridate::interval)
	  						, jk = jk
	  						) %>% as.data.table()

	  					# NOTE: 'edge.filter' is evaluated here because after the mapping, 'purrr::compact()' will handle empty results
	  					# Also, filtering on 'self$space' is done because the actual event graphs are created from it.
	  					.out[!is.na(epsilon) & eval(edge.filter)]
	  				}
  		}) %>% purrr::compact() %>% reduce(rbind)
  	}
  # }) %>% purrr::compact() %>% reduce(rbind)
  }, .options = .furrr_opts) %>% purrr::compact() %>% reduce(rbind)

  if (omit.na){ self$space %<>% na.omit() }

  # :: Create self$evt_graphs from self$space
	if (!"package:igraph" %in% search()){ library("igraph") }

  self$evt_graphs <- self$space %>% split(by = "jk") %>% map(graph_from_data_frame);

  # :: Process arguments 'graph.control' and 'omit.na'
  self$evt_graphs <- future_map(self$evt_graphs, ~{
  	g = .x;

  	V(g)$size 	<- tryCatch({
  		apply(
  			X = (stri_split_fixed(V(g)$name, ":", simplify = TRUE))[, c(2, 3)]
	  		, MARGIN = 1
  			, FUN = as_mapper(~as.Date(.x) %>% diff() %>% as.numeric() %>% sqrt() %>% as.integer())
  			)
  		}, error = function(e){ 10 })
  	V(g)$title	<- V(g)$name;
  	V(g)$key		<- V(g)$name;
  	V(g)$name		<- map(V(g)$name, stri_extract_last_regex, pattern = "Data.+")

	  if (!is.null(graph.control)){ for (i in graph.control){ eval(i) }}
  	g
  }, .options = furrr_options(scheduling = .furrr_opts$scheduling, packages = .furrr_opts$packages, globals = "graph.control")) %>% purrr::compact()

	attr(self$space, "contexts")	<- self$config$contexts;
	message(sprintf("[%s] The vector space is ready for analysis", Sys.time()));

	invisible(self);
}
#
cross.time <- function(s0, s1, e0, e1, control, events.ascending = TRUE, chatty = FALSE, nosy = FALSE){
#' Cross-Compare Temporal Boundaries
#'
#' Given a four-element vector of start and end coordinates of two events, \code{cross.time()} compares the distances among the upper and lower boundaries of pairs of event vectors. This includes "like" boundary comparison (e.g., start #2 - start#1) and contrary boundary comparison (e.g. start #2 - end #1).
#'
#' @param s0 A numeric/date-coded vector containing the temporal lower boundary of the starting event duration
#' @param s1 A numeric/date-coded vector containing the temporal upper boundary of the starting event duration
#' @param e0 A numeric/date-coded vector containing the temporal lower boundary of the ending event duration
#' @param e1 A numeric/date-coded vector containing the temporal upper boundary of the ending event duration
#' @param control A length-2 sorted list with values indicating the range of allowable values for '.beta' (the difference between ends of 'to' events and beginnings of 'from' events)
#' @param events.ascending (logical | TRUE) \code{FALSE} assumes events provided are in descending order
#' @param chatty (logical | \code{FALSE}) Use \code{chatty = TRUE} to see messages related to the execution.
#' @param nosy (logical | FALSE) Used for debugging purposes
#'
#' @return A \code{\link[data.table]{data.table}} object (??event.vector.space)
#'
#' @export

	## Reference: https://www.r-bloggers.com/using-complex-numbers-in-r/
	## Division by Pi/4 makes it easy to tell if one argument is larger than, smaller than, or the same magnitude as the other (same = Pi/4)
	## All computations are in the direction of B.max to A.min when `events.ascending` is TRUE

	options("data.table.verbosse" = FALSE);
	direction =  as.character(as.numeric(events.ascending));

	# `boundaries` ====
	boundaries = if (events.ascending) { cbind(s0, e0, s1, e1) } else { cbind(s1, e1, s0, e0) }

	# .beta: The maximum span between the extremes of temporal boundaries ====
	.beta	= boundaries[, 4] - boundaries[, 1];
	if (missing(control)){ control <- list(-Inf, Inf) }

	# Calculate ====
	output = {
		data.table::data.table(
			# The gap between the ordered events
			mGap = boundaries[, 3] - boundaries[, 2]
			# Difference of lower boundaries (start/lower-boundary)
			, mSt	= boundaries[, 3] - boundaries[, 1]
			# Difference of upper boundaries (end/upper-boundary)
			, mEd	= boundaries[, 4] - boundaries[, 2]
			# The duration of the first event
			, from.len	= boundaries[, 2] - boundaries[, 1]
			# The duration of the second event
			, to.len	= boundaries[, 4] - boundaries[, 3]
		)[
		# Relative change across boundary values
		, epsilon := {
				# Do not algebraically reduce the following with respect to '.beta': the sign is as important as the arguments
				.out = atan2(mEd, mSt) * atan2((mGap * .beta), mGap)

				# Scale back down to an angle: `sqrt()` needs to have a complex argument for handling negative arguments
				# The square-root of 'mGap'  differentiates offset events from cases where one event envelopes another
				.out = sqrt(as.complex(.out))/(0.25 * pi) + sqrt(as.complex(mGap))

				unlist(.out)
			}
		][
		, epsilon.desc := {
				c(`1` = "Disjoint", `10` = "Concurrency", `100` = "Full Concurrency", `1000` = "Continuity")[
				as.character({
					cbind(
							((Re(epsilon) != 0) & (Im(epsilon) == 0))
						, ((Re(epsilon) == 0) & (Im(epsilon) != 0))
						, ((Re(epsilon) != 0) & (Im(epsilon) != 0))
						, ((Re(epsilon) == 0) & (Im(epsilon) == 0))
						) %*% (10^c(0:3))
					})
				]
			}
		][
		# Filter out rows where '.beta' is not within the time control limits
		(.beta %between% control)
		];
	}

	if (is.null(output)) { message(sprintf("[%s] \t... ERROR: nothing to output", Sys.time())); }

	if (nosy & !exists("inspect", envir = globalenv())) { assign("inspect", list(), envir = globalenv()); }

	# Return ====
	output;
}
#
evs_retrace <- function(self, ...){
#' Retrace Event Source Data
#'
#' \code{evs_retrace} creates vertex attribute \code{trace} and populates it with an expression that retrieves the source record when evaluated.
#'
#' @param self An R object of class "event.vector.space"
#' @param ... Names of event graphs found in \code{self$evt_graphs}: 'dynamic-dots' are supported via \code{\link[rlang]{list2}}
# @param chatty Verbosity flag
#'
#' @return Because of the reference semantics of R6 classes, for each name given in \code{...}, graph updates are in place: \code{self} is returned invisibly.
#'
#' @export

	evt_gph = if (...length() == 0){ names(self$evt_graphs) %>% set_names()
	} else { modify_if(.x = rlang::list2(...), .p = is.numeric, .f = ~names(self$evt_graphs[.x]), .else = ~as.character(.x)) %>% set_names() }

	self$evt_graphs[names(evt_gph)] <- imap(evt_gph, ~{
		g = self$evt_graphs[[.y]];
		evs.cfg = self$config;
		V(g)$name <- V(g)$title %>% stri_extract_first_regex("[A-Z]+[:][0-9]+");
		V(g)$trace <- map(V(g)$title, ~{
			evs.lkup = stri_split_fixed(.x, ":", simplify = TRUE) %>% as.list() %>% set_names(c("jk", "start_idx", "end_idx", "context", "seq_idx"));

			self$config[
				(contexts == evs.lkup$context)
				, {
					.map_fields = if (rlang::has_length(unlist(map.fields), 1)){
						stri_split_regex(unlist(map.fields), "[,|:]", simplify = TRUE) %>% as.vector()
					} else { unlist(map.fields) }

					.map_fields %<>% set_names(c("who", "start", "end"))

					sprintf(
						"%s[(%s == %s) & (%s == as.Date('%s')) & (%s == as.Date('%s'))]"
						, src.names
						, .map_fields["who"]  , evs.lkup$jk %>% as.numeric()
						, .map_fields["start"], evs.lkup$start_idx
						, .map_fields["end"]  , evs.lkup$end_idx
					) %>% str2lang()
				}
			];
		});
		g;
	})

	invisible(self)
}
#