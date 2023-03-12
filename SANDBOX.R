# ::: Replacement for $configure :::

library(magrittr)
x <- mtcars[7:10]
y <- cbind(mtcars[, c(1, 6, 9)], W = rownames(mtcars))
ENV <- new.env()
ENV$y <- cbind(mtcars[, 1:4], A = rownames(mtcars))
ENV$q <- cbind(mtcars[, 5:6], B = rownames(mtcars))

evs_configure <- function(
		src.names
		, contexts
		, map.fields = NULL
		, src.mix = "comb"
		, exclude.mix = list(c())
		, chatty = FALSE
		){
	if (!rlang::is_empty(exclude.mix)){ message("Source-mix exlusions detected") }

	private <- new.env()
	fld_nms <- c("jk", "time_start_idx", "time_end_idx")
	make_refs <- purrr::as_mapper(~{
		.this <- sapply(.x, as.character) |> rlang::parse_exprs()
		if (!rlang::has_length(.this, 1)){ .this[-1] } else { .this}
	})
	make_quos <- purrr::as_mapper(~{
		.this <- sapply(.y, as.character)
		.that <- .x
		if (!rlang::has_length(.this, 1)){ .this <- .this[-1] }

		.that <- rlang::set_names(.that, .this)
		rlang::as_quosures(.that, named = TRUE, env = rlang::caller_env(1))
	})
	set_fld_nms <- purrr::as_mapper(~{
		nms.x <- names(.x)
		nm_pos <- which(nms.x %in% fld_nms)
		na_pos <- setdiff(seq_along(nms.x), nm_pos)
		nms.x[na_pos] <- setdiff(fld_nms, nms.x[nm_pos])
		nms.x
	})

	# Create the event data references
	event_refs <- make_refs(rlang::enexprs(src.names)) |>	make_quos(rlang::enexprs(contexts))

	# Create a configuration quosure for each data source
	private$.params$config <- list(event_refs
			, map.fields |> purrr::map(~rlang::parse_exprs(.x) |> rlang::set_names(set_fld_nms(.)))
			) |>
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

					.temp
			})

	# @def q_table sets the allowable comparisons before any calculations are done
	private$q_table <- {
		# .temp will be a matrix at this point
		.temp = private$.params$config |> names() |> utils::combn(m = 2) %>% cbind(apply(., 2, rev)) |> t()

		# enforce 'src.mix'
		if (!grepl("reflex|all", src.mix, ignore.case = TRUE)){ .temp %<>% .[.[, 1] != .[, 2], ] }

		# enforce 'exclude.mix' after converting .temp to a 'data.table' object
		.temp %<>% data.table::as.data.table() |> data.table::setnames(c("from", "to"));
		.temp[!purrr::pmap_lgl(.temp, ~list(c(.x, .y)) %in% exclude.mix)]
	}

	private$.params$config |>
		data.table::setattr("src.mix", as.character(rlang::enexpr(src.mix))) |>
		data.table::setattr("exclude.mix", sapply(exclude.mix, function(i){
			paste0(if (length(i) == 1){ c(i, i) } else if(length(i) > 2) { i[1:2] } else { i }, collapse = ", ")
		})) %>%
		data.table::setattr("jk", purrr::map(., ~.x$jk.vec |> rlang::eval_tidy()) |> unlist() |> unique() |> sort() |> purrr::set_names())

	invisible(private)
	# invisible(self);
}

inspect <- evs_configure(
	src.names = c(y, ENV$q[ENV$q$drat < 4, ])
	, contexts = c(EVENT_Z, EVENT_A)
	, map.fields = list(c("W", jk = "mpg", "wt"), c(time_end_idx = "B", "drat", time_start_idx = "wt"))
	, chatty = TRUE
	)

inspect$.params$config$EVENT_Z
inspect$.params$config$EVENT_A

# Indirectly retrieve a value
inspect$.params$config$EVENT_Z$time_start_idx |> rlang::eval_tidy()
inspect$.params$config$EVENT_A$jk |> rlang::eval_tidy()
ENV$q$drat

inspect$.params$config |> purrr::map(~rlang::eval_tidy(.x$jk) |> unique() |> sort())

# Directly retrieve a value
inspect$.params$config$EVENT_A$time_start_idx |> rlang::quo_get_env() |> parent.env() %$% wt


