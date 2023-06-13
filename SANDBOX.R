self <- test.evs
private <- test.evs$.__enclos_env__$private
chatty = TRUE
edge_filter = rlang::exprs(
	mSt <= quantile(mSt, 0.75)
	, abs(mGap) >= 5
	, abs(mGap) <= 120
	, .named = FALSE
	, .ignore_empty = "all"
	);
time.control = list(-Inf, Inf)
graph.control = {
		rlang::exprs(
			igraph::E(g)$title	<- igraph::ends(g, igraph::E(g)) %>% apply(1, paste, collapse = " -> ")
			, igraph::V(g)$color <- igraph::V(g)$name %>% stringi::stri_split_fixed(":", simplify = TRUE) %>% .[, 1L] %>% {
				x = .;
				y = purrr::set_names(unique(x), purrr::map_chr(unique(x), ~rgb(runif(1), runif(1), runif(1))))
				purrr::map_chr(x, ~names(y)[which(y == .x)])
			}
			, igraph::V(g)$src <- igraph::V(g)$name %>% stringi::stri_replace_first_regex("[:][0-9]+", "")
		)
	}
furrr_opts = furrr::furrr_options(scheduling = Inf, seed = TRUE)
#
# make.evs_universe.dev = function(..., time.control = list(-Inf, Inf), graph.control = NULL, unit = NULL, furrr_opts = furrr::furrr_options(scheduling = Inf, seed = TRUE), graph.only = FALSE, chatty = FALSE){
	# :: `make.event_key` is a function used to create sequential unique identifiers
	# 	for events sources and time-markers ----
	make.event_key <- \(x, y){
		index <- rep.int(NA, length(root));

		# Populate 'index' based on unique values of 'root'
		purrr::walk(unique(x), \(i){
			# out.x <- root[which(root %in% i)];
			.this_idx <- which(x %in% i);
			index[.this_idx] <<- data.table::frank(y[.this_idx], ties.method = "dense");
		});

		index
	};

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
		out <- x %$% {
			data.table(
				jk = rlang::eval_tidy(jk)
				, time_start_idx = rlang::eval_tidy(time_start_idx)
				, time_end_idx = rlang::eval_tidy(time_end_idx)
			)
		}
		out[, src := y]
	}) |>
		rbindlist() |>
		setkey(jk, time_start_idx, time_end_idx) |>
		setorder(jk, time_start_idx, time_end_idx) %>% {
			.[, f_src_exists := src %in% .src_mix[, from], by = jk][
				, t_src_exists := (src %in% .src_mix[, to]) & f_src_exists, by = jk][
					(f_src_exists & t_src_exists), !c("f_src_exists", "t_src_exists")
				]
		} |>
		unique() |>
		as.list();

	# :: Use optimization from 'data.table' to create `self$space` ----
	# edge_filter <- rlang::enexprs(..., .named = FALSE, .ignore_empty = "all");

	# if (!graph.only){
		if (chatty){ message("Creating `self$space` (part 1) ...") }

		# Step 1: Splitting by 'jk'
		self$space <- {
			as.data.table(merge(
				.tmp_space[(.tmp_space$src %in% .src_mix$from)] |>
					purrr::compact() |>
					purrr::set_names(c("jk", "f_start_idx", "f_end_idx", "f_src"))
				, .tmp_space[(.tmp_space$src %in% .src_mix$to)] |>
					purrr::compact() |>
					purrr::set_names(c("jk", "t_start_idx", "t_end_idx", "t_src"))
				, by = "jk"
			))[(t_start_idx - f_start_idx) >= 0]

			# 2023.06.12 Remove splitting by 'jk'
			#|> split(by = c("jk"));
		}

		# Step 2: Calling `cross.time()`
		if (chatty){ message("Creating `self$space` (part 2) ...") }

		furrr_opts <- { furrr::furrr_options(
			scheduling	= Inf
			, seed			= TRUE
			, packages	= c("magrittr", "data.table", "rlang")
			, globals 	= c("time.control", "units", "cross.time", "edge_filter")
		)}

		#' @note 2023.06.12 Pre-calculate cross.time() given the current state of `self$space`
		#'
		unit = NULL#"days"
		.unit_patterns <- grepl("^(we|mo|da|ye|se|mi|na|ho|pi)", unit, ignore.case = TRUE)
		unit_desc <- unit;
		unit <- if (any(.unit_patterns)){
			list(days = lubridate::ddays(1)
					 , hours = lubridate::dhours(1)
					 , microseconds = lubridate::dmicroseconds(1)
					 , milliseconds = lubridate::dmilliseconds(1)
					 , minutes = lubridate::dminutes(1)
					 , months = lubridate::dmonths(1)
					 , nanoseconds = lubridate::dnanoseconds(1)
					 , picoseconds = lubridate::dpicoseconds(1)
					 , seconds = lubridate::dseconds(1)
					 , weeks = lubridate::dweeks(1)
					 , years = lubridate::dyears(1)
			)[[which(.unit_patterns)]]
		} else { 1 }

		self$space[
			# 1:10000
			, cbind(
				.SD
				, cross.time(
						s0 = f_start_idx
						, s1 = t_start_idx
						, e0 = f_end_idx
						, e1 = t_end_idx
						, control = time.control
						, unit = "days"
						))
			# , by = jk
			]

		# self$space <- furrr::future_map(self$space, \(X){
		# 	# Call `cross.time()`
		# 	time.control; unit;
		#
		# 	xtime <- X[, cross.time(
		# 		s0 = f_start_idx, s1 = t_start_idx
		# 		, e0 = f_end_idx, e1 = t_end_idx
		# 		, control = time.control, unit = unit # <- Supplied by the function arguments
		# 	)
		# 	, by = .(jk, f_src, t_src) # <- Corresponds to the configured 'map.fields'
		# 	]
		#
		# 	# Check for instances where `xtime` is empty due to qualification on the output of `cross.time()`
		# 	if (rlang::is_empty(xtime) || rlang::is_empty(xtime[!is.na(epsilon)])){
		# 		NULL # <- NULL is filtered out in the list of results, so passing NULL is safest here
		# 	} else {
		# 		# Enforce row filter rules before proceeding: note that this pre-derives a Boolean vector in order
		# 		#		to make debugging easier and to ensure the integrity of the operation.
		# 		edge_filter <- edge_filter |>
		# 			as.character() |>
		# 			paste(collapse = " & ") |>
		# 			rlang::parse_expr() |>
		# 			rlang::eval_tidy(data = xtime)
		#
		# 		xtime[(edge_filter)][
		# 			# Impute sequencing on event sources: this has a direct impact when creating distinct vertex names during subsequent graph creation
		# 			, c("f_src", "t_src") := list(list(f_src, from.coord), list(t_src, to.coord)) |>
		# 				map(\(k) paste(k[[1]], make.event_key(k[[1]], k[[2]]), sep = ":"))
		# 		][, src.pair := sprintf("%s -> %s", f_src, t_src)
		# 		][, setnames(.SD, c("f_src", "t_src"), c("from.src", "to.src"))
		# 		][, epsilon.desc := factor(
		# 			epsilon.desc
		# 			, levels = c("NA", "Full Concurrency", "Concurrency", "Continuity", "Disjoint")
		# 			, ordered = TRUE
		# 		)
		# 		][(from.src != to.src)] # Remove loops (may result in removing all records)
		# 	}
		# }, .options = furrr_opts) |> purrr::compact()
	# }

	# :: Create `self$evt_graphs` from `self$space` ----
	message(sprintf("[%s] ... creating event graphs", Sys.time()));

	self$evt_graphs <- self$space %$% mget(ls()) |>
		furrr::future_map(\(x){
			graph.control;

			g <- igraph::graph_from_data_frame(setcolorder(x, c("from.src", "to.src")));

			if (!rlang::is_empty(graph.control)){
				purrr::walk(graph.control, \(i) eval(i, envir = environment()))
			}

			g
		}, .options = furrr_opts) |>
		purrr::compact();

	self$space <- self$space %$% mget(ls()) |> reduce(rbind);
	self$space[(x_filter), !"x_filter"]

	# :: Return ----
	message(sprintf("[%s] The event vectors are ready for analysis", Sys.time()));
	invisible(self);
# }