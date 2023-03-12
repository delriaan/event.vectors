# ::: Replacement for $configure :::

library(magrittr)
x <- mtcars[7:10]
y <- cbind(mtcars[, c(1, 6, 9)], W = rownames(mtcars))
ENV <- new.env()
ENV$y <- cbind(mtcars[, 1:4], A = rownames(mtcars))
ENV$q <- cbind(mtcars[, 5:6], B = rownames(mtcars))

evs_configure <- function(src.names, contexts, map.fields = NULL, row.filters, src.mix = "comb", exclude.mix = list(c("", "")), update = FALSE, chatty = FALSE){
	private <- new.env()

	fld_nms <- c("jk", "start", "end")
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

	# Create the field references for each data reference
	this.cfg <- purrr::map2(
			event_refs
			, map.fields |> purrr::map(~rlang::parse_exprs(.x) |> rlang::set_names(set_fld_nms(.)))
			, ~{	rlang::as_quosures(.y, env = rlang::as_data_mask(rlang::eval_tidy(.x)))[fld_nms]
			})

	private$.params$config <- if (!update | is.null(private$.params$config)){	this.cfg } else { purrr::list_modify(private$.params$config, !!!this.cfg)}
}

inspect <- test_func(
	src.names = c(y, ENV$q)
	, contexts = c(EVENT_A, EVENT_B)
	, map.fields = list(c("W", jk = "mpg", "wt"), c(end = "B", "drat", start = "wt"))
	)

inspect$EVENT_A

# Indirectly retrieve a value
inspect$EVENT_A$start
inspect$EVENT_A$start |> rlang::eval_tidy()

# Directly retrieve a value
inspect$EVENT_A$start |> rlang::quo_get_env() |> parent.env() %$% W


setdiff(1:3, c(3,0,1))

