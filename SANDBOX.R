library(foreach)
library(doFuture)
library(igraph)
library(book.of.features)
library(matrixStats)
library(plotly)
registerDoFuture()
plan(sequential)
plan(tweak(multisession, workers = 5))
test.evs$evt_graphs$`1` |> vertex.attributes()

test.evs$evt_graphs %<>% furrr::future_map(~{
		g = .x
		igraph::V(g)$length <- purrr::map(igraph::V(g), ~{
			.x$trace |> eval(envir = globalenv()) %$% { end_idx - start_idx }
			})
		g
	}, .options = furrr::furrr_options(scheduling = Inf, seed = TRUE, globals = "BLAH"))

trans_matrix <- test.evs$evt_graphs$`1` |> igraph::get.adjacency(attr = "mGap", names = TRUE, sparse = FALSE)

# Transition Matrix
trans_matrix <- {
		trans_matrix |>
		rownames() |>
		purrr::map(~{
			c(from = list(stringi::stri_replace_first_regex(.x, "[:][0-9]+", ""))
				, colnames(trans_matrix)[trans_matrix[.x, ] != 0] %>%
						stringi::stri_replace_first_regex("[:][0-9]+", "") |>
							table()
				)
			}) |>
		rbindlist(use.names = TRUE, fill = TRUE) %>% {
			.[, purrr::map(.SD, sum, na.rm = TRUE), by = from
			][, .SD/sum(.SD), by = from] -> .tmp_obj

			.tmp_obj[, !"from"] %>%
				as.matrix() %>%
				setattr("dimnames", list(.tmp_obj$from, colnames(.)))
		}
}

markov_viz_data <- split(trans_matrix, f = rownames(trans_matrix)) |> imap(~{
		out = t(.x) |> as.matrix()

		purrr::walk(purrr::set_names(1:30), ~{
				out <<- rbind(out, (out[nrow(out), ] %*% trans_matrix))
			})

		out[, order(colnames(out))]
	}) |>
	purrr::imap(~data.table(from_src = .y, .x[c(TRUE, matrixStats::colDiffs(.x) |> apply(1, purrr::as_mapper(~all(round(.x, 3) != 0)))), ] %>% as.data.table())) |>
	rbindlist() |>
	melt(id.vars = "from_src", variable.name = "to_src", variable.factor = FALSE)

markov_viz_data[
	, map(.SD[, .(source = paste0("F_", from_src), target = paste0("T_", to_src))], ~{
			outer(.x, unique(sort(c(paste0("F_", from_src), paste0("T_", to_src)))), `==`) |> apply(1, which) - 1
		}) |> append(list(value = value))
	][order(source, value)] |>
modify_at(c("source", "target"), ~factor(.x, levels = sort(unique(.x)), ordered = TRUE)) |>
plotly::plot_ly(
	type = "sankey"
	, orientation = "h"
	, node = {
			.colors = replicate(markov_viz_data[, length(unique(sort(c(paste0("F_", from_src), paste0("T_", to_src)))))]
													, rlang::inject(rgb(!!!runif(3, .1, .8)))
													) |> sort()
			list(
	      label = markov_viz_data[, unique(sort(c(paste0("F_", from_src), paste0("T_", to_src))))]
	      , color = .colors
	      , pad = 15
	      , thickness = 20
	      , line = list(color = .colors, width = 0.5, stroke = I("#000000"))
	    	)}
	, link = { list(
      source = ~source
      , target = ~target
      , value =  ~value
    	)}
	) |>
	plotly::layout(
    xaxis = list(showgrid = FALSE, zeroline = FALSE)
    , yaxis = list(showgrid = FALSE, zeroline = FALSE)
    )

markov_viz_data[, c(.SD[, .(from_src)], book.of.features::xform.basis_vector(fvec = to_src, avec = value))][, map(.SD, book.of.utilities::calc.rms), by = from_src] %>%
plotly::plot_ly(
	x = ~names(.[, !"from_src"])
	, y = ~from_src
	, z = ~as.matrix(.[, !"from_src"])
	, type = "contour"
	# , mode = "marker"
	)
