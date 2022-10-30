inspect <-map(test.evs$q_graph[1:2], ~{ g = .x
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
					, cross.time(s0 = start_idx, s1 = i.start_idx, e0 = end_idx, e1 = i.end_idx)#, control = time.control, chatty = chatty)
					, from_timeframe = map2(start_idx, end_idx, lubridate::interval)
					, to_timeframe   = map2(i.start_idx, i.end_idx, lubridate::interval)
					, jk = jk
				) %>% as.data.table()

				# NOTE: 'edge.filter' is evaluated here because after the mapping, 'purrr::compact()' will handle empty results
				# Also, filtering on 'self$space' is done because the actual event graphs are created from it.
				# .out[!is.na(epsilon) & eval(edge.filter)]
			}
		}) %>% purrr::compact() %>% reduce(rbind)
	}
}) %>% purrr::compact() %>% reduce(rbind)

local({ x = V(g)[name == ..1]$data; k = is.data.table(x); while(!k){ x <- x[[1]]; k <- is.data.table(x) }; x})