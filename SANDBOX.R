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

test.evs$evt_graphs %<>% map(~{
	g = .x
	V(g)$length <- map(V(g), ~{
		.x$trace |>
			eval(envir = globalenv()) %$% { end_idx - start_idx }
		})
	V(g)$src <- V(g)$name %>% stringi::stri_replace_first_regex("[:][0-9]+", "")
	g
})

trans_matrix <- test.evs$evt_graphs$`1` %>% get.adjacency(attr = "mGap", names = TRUE, sparse = FALSE)

# Transition Matrix
trans_matrix <- {
		trans_matrix |>
		rownames() |>
		map(~{
			c(from = list(stringi::stri_replace_first_regex(.x, "[:][0-9]+", ""))
				, colnames(inspect)[trans_matrix[.x, ] != 0] %>%
						stringi::stri_replace_first_regex("[:][0-9]+", "") |>
							table()
				)
			}) |>
		rbindlist(use.names = TRUE, fill = TRUE) %>% {
		.[, map(.SD, sum, na.rm = TRUE), by = from
		][, .SD/sum(.SD), by = from] -> .tmp_obj

		.tmp_obj[, !"from"] %>%
			as.matrix() %>%
			setattr("dimnames", list(.tmp_obj$from, colnames(.)))
	}
	}

markov_viz <- split(trans_matrix, f = rownames(trans_matrix)) %>%
	imap(~{
		out = t(.x) %>% as.matrix()

		walk(purrr::set_names(1:30), ~{
				out <<- rbind(out, (out[nrow(out), ] %*% trans_matrix))
			})

		out %<>% .[, order(colnames(.))]

		plot_ly(
			x = sprintf("From %s: To %s", .y, colnames(out))
			, y = ~1:nrow(out)
			, z = out[-1, ] * (colDiffs(out) %>% round(4) != 0)
			, type = "contour"
			, name = .y
			)
	})

rlang::inject(subplot(!!!markov_viz, nrows = 3)) %>%
			plotly::layout(
				title = "Steady-State vs Initial Source"
				, margin = list(t = -.5)
				)


markov_viz
