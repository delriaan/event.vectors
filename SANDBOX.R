# ::: Initialization ::: ----
# library(event.vectors);
library(purrr)
library(stringi)
library(tictoc);
library(future);
#
make.test_data <- function(j = 5, n = 5, m = 5, o = 1:10, dest = globalenv(), .debug = FALSE){
#' Make Test Data for Validation
#'
#' \code{make.test_data} creates several \code{\link[data.table]{data.table}} objects to be used to validate the package functionality
#' @param j (integer | 5L) The number of unique values for 'k' to generate
#' @param n (integer | 5L) The number of sources to create (maximum of 10)
#' @param m (integer | 5L) The maximum number of columns to generate for each created object (maximum of 15): each column represents an attribute not related to time
#' @param o (integer[] | 1:10) A vector of integers sampled to create end dates from a randomly-generated start date
#' @param dest (environment) The destination environment object
#' @param .debug (logical | \code{FALSE}) When \code{TRUE}, additional debugging items are printed
#'
#' @return One to \code{n} \code{\link[data.table]{data.table}} objects prefixed as 'test_data'.
#'
	j = max(c(3, abs(as.integer(j))));
	n = max(c(3, abs(as.integer(n)))); ifelse(n > 10, 10, n);
	m = max(c(3, abs(as.integer(m)))); ifelse(m > 15, 15, m);
	o = { ifelse(rlang::has_length(o, 1L)
						 , yes = 1:10
						 , no = ifelse(any(o <= 0)
						 							, yes = abs(o)
						 							, no = ifelse(rlang::has_length(o, 2L)
						 														, yes = `:`(o[1], o[2]) |> sort()
						 														, no =  o))
						 )}

	sequence(n) %>%
	purrr::set_names(paste0("test_data.", stringi::stri_pad_left(., width = 2, pad = "0")))|>
	purrr::map(~{
		set.seed(sample(.Random.seed, 1));
		.src = LETTERS[[.x]];

		.out = purrr::map_dfr(c(1:j), ~list(jk = rep.int(.x, sample(10:100, 1, TRUE)), src = .src)) %>% data.table::as.data.table();

		.init_date = c(as.Date(sprintf(
				"%s-%s-%s"
				, rep.int(data.table::year(Sys.Date()), nrow(.out))
				, sample(stringi::stri_pad_left(1:12, width = 2, pad = "0"), nrow(.out), TRUE)
				, sample(stringi::stri_pad_left(1:28, width = 2, pad = "0"), nrow(.out), TRUE)
				)));

		.out[
			, c("date.start", "date.end") := list(.init_date, .init_date + rpois(n = length(jk), lambda = sample(o, length(jk), TRUE)))
			][
			, paste0("X_", stringi::stri_pad_left(sample(30, m), width = 2, pad = "0")) := purrr::map(1:m, ~sample(runif(1E6), .N, TRUE))
			][
			runif(length(jk)) > 0.65
			] |>
			data.table::setkey(jk, src, date.start, date.end) |>
			data.table::setcolorder(c("jk", "date.start", "date.end", "src")) |>
			data.table::setnames("jk", "join_key")
		}) |>
	list2env(envir = dest);
}

BLAH <- new.env();
rm(list = ls(pattern = "^test_data"))
set.seed(sample(100000, 1));
make.test_data(j = 50, n = 5, m = 5, o = c(5, 20), dest = BLAH, .debug = !TRUE);
set.seed(sample(100000, 1));
make.test_data(j = 50, n = 3, m = 5, o = c(5, 20), dest = .GlobalEnv, .debug = !TRUE);

# ::: Replacement for $configure ::: ----
evs_configure =	function(src.defs, contexts, map.fields = NULL, src.mix = "comb", exclude.mix = NULL, chatty = FALSE){
					if (!rlang::is_empty(exclude.mix)){ message("Source-mix exlusions detected") }

					private <- new.env()
					fld_nms <- c("jk", "time_start_idx", "time_end_idx")

					make_refs <- purrr::as_mapper(~{
						sapply(.x, magrittr::freduce, list(eval, as.character)) |> rlang::parse_exprs()
						# if (!rlang::has_length(.this, 1)){ .this[-1] } else { .this}
					})
					make_quos <- purrr::as_mapper(~{
						.this <- sapply(.y, magrittr::freduce, list(eval, as.character))
						.that <- .x
						# if (!rlang::has_length(.this, 1)){ .this <- .this[-1] }

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
						data.table::setattr("jk", purrr::map(., ~.x$jk.vec |> rlang::eval_tidy()) |> unlist() |> unique() |> sort() |> purrr::set_names())

					invisible(private)
					# invisible(self);
				}

# debug(evs_configure)
private <- evs_configure(
	src.defs = c(ls(pattern = "^test") |>
							 	purrr::modify_at(3, ~paste0(.x, "[(join_key > 3)]")) |>
							 	purrr::modify_at(1, ~paste0(.x, "[lubridate::month(date.start)==1]")) |>
							 	rlang::parse_exprs()
							 , ("BLAH$" %s+% ls(BLAH, pattern = "^test")[1:3]) |>
							 	purrr::modify_at(3, ~paste0(.x, "[(join_key == 1)]")) |>
							 	purrr::modify_at(2, ~paste0(.x, "[lubridate::month(date.start)==8]")) |>
							 	rlang::parse_exprs()
							 )
	, contexts = rlang::parse_exprs("Event_" %s+% LETTERS[1:6])
	, map.fields = replicate(n = 6, c("join_key", "date.start", "date.end"), simplify = FALSE)
	, chatty = TRUE
	)

# Tests
{
private$.params$config$Event_F
private$.params$config$Event_A

# Indirectly retrieve a value
private$.params$config$Event_C$time_start_idx |> rlang::eval_tidy()
private$.params$config$Event_D$jk |> rlang::eval_tidy()
private$.params$config$Event_D$jk.vec
ENV$q$drat

private$.params$config |> purrr::map(~rlang::eval_tidy(.x$jk) |> unique() |> sort())

# Directly retrieve a value
private$.params$config$EVENT_A$time_start_idx |> rlang::quo_get_env() |> parent.env() %$% wt
}

# ::: Replacement for $set.q_graphs ::: ----
self <- new.env()
set.q_graphs = function(chatty = FALSE){
		 		if (is.null(private$.params$config)){
		 			stop("No class object configuration detected.  Provide a configuration set using `$configure()` involving at least two (2) temporal datasets.")}

		 		data.table::setattr(private$.params$config, "graphs_created", FALSE);
		 		events_by_jk <- data.table::rbindlist(list(purrr::imap(
		 					private$.params$config, ~{
								jk = private$.params$config |> attr("jk");
								exists_in = jk %in% rlang::eval_tidy(.x$jk)
								matrix(exists_in, ncol = 1, dimnames = list(jk, .y))
							})))[, jk := private$.params$config |> attr("jk")];

				events_by_jk <- {
					events_by_jk[
							, purrr::pmap_dfr(.SD, function(...){
									jk_events <- ...names()[-1][c(...)[-1]];
									qt <- private$q_table[list(jk_events, jk_events)];
									if (nrow(qt) == 0){ list(from = NA, to = NA) } else { unique(qt) }
								})
							, by = jk
							][!is.na(from), .(from, to, jk)]
					}

		 		self$q_graph <- { split(events_by_jk, by = "jk") |>
	 				purrr::imap(~{
	 					# Graphs for each level of 'jk'
	 					g <- igraph::graph_from_data_frame(.x);

	 					this_jk <- rlang::parse_expr(.y);

	 					# Return the minimal information needed to reconstruct the source data
	 					igraph::V(g)$data <- purrr::imap(igraph::V(g), ~{
	 						event <- private$.params$config[[.y]];

	 						list(event = event, jk_val = this_jk) |>
	 							purrr::modify_at("jk_val", ~as.call(list(rlang::parse_expr(paste0("as.", rlang::eval_tidy(event$jk)[1] |> typeof())), .x)) |> eval()) %>%
	 							append(list(action = rlang::expr(rlang::eval_tidy(attr(event, "src.def"))[(!!rlang::quo_get_expr(event$jk) == !!.$jk_val)])))
	 					});

	 					g
	 				})
				}

		 		if (!rlang::is_empty(self$q_graph)){
			 		if (chatty){ message(sprintf("Created %s query graphs", self$q_graph |> length())) }
		 			data.table::setattr(private$.params$config, "graphs_created", TRUE);
		 		} else if (chatty){ message("No graphs greated") }

				invisible(self);
}
set.q_graphs()
igraph::V(self[["q_graph"]][["1"]])$data
private$.params$config[["Event_A"]] %$% {
	# this.jk <- jk |> terms() |> attr("term.labels")
	print(rlang::quo_get_expr(jk) |> deparse() |> rlang::sym())
	rlang::expr(rlang::eval_tidy(attr(event, "src.def"))[(!!rlang::quo_get_expr(event$jk) == jk_val)])

}
