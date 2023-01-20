
.src_mix <- ALYZ$grievance_evs$.__enclos_env__$private$q_table |> as.list() |> transpose() %>% set_names(map_chr(., paste, collapse = " -> "));

inspect <- ALYZ$grievance_evs$config$src.names |>
              map(~eval(str2lang(.x)) %>% .[, .(jk, start_idx, end_idx, rec_idx, src)]) |>
              rbindlist() %>%
              setkey(jk, start_idx);

inspect[
  , c(map(.src_mix, ~{ .SD[(src %in% .x$from), .(f_start_idx = start_idx, f_end_idx = end_idx, f_src = src)] %>% purrr::compact() }) %>% rbindlist()
      , map(.src_mix, ~{ .SD[(src %in% .x$to), .(t_start_idx = start_idx, t_end_idx = end_idx, t_src = src)] %>% purrr::compact() }) %>% rbindlist())
  , by = jk
  ][
  , c(cross.time(f_start_idx, t_start_idx, f_end_idx, t_end_idx)
     , list(from_timeframe = purrr::map2(f_start_idx, f_end_idx, lubridate::interval))
     , list(to_timeframe   = purrr::map2(t_start_idx, t_end_idx, lubridate::interval))
     )
  , by = .(jk, src.pair = sprintf("%s -> %s", f_src, t_src))
  ][
  !is.na(epsilon) & eval(edge.filter)
  ]

inspect <- { .tmp_space[
		  , c(purrr::map(.src_mix, ~{ .SD[(src %in% .x[1]), .(f_start_idx = start_idx, f_end_idx = end_idx, f_src = src)] %>% purrr::compact() }) |> data.table::rbindlist()
		      , purrr::map(.src_mix, ~{ .SD[(src %in% .x[2]), .(t_start_idx = start_idx, t_end_idx = end_idx, t_src = src)] %>% purrr::compact() }) |> data.table::rbindlist())
		  , by = jk
		  ][
		  , c(cross.time(f_start_idx, t_start_idx, f_end_idx, t_end_idx)
		     , list(from.coord = purrr::map2_chr(f_start_idx, f_end_idx, paste, sep = ":"))
		     , list(to.coord   = purrr::map2_chr(t_start_idx, t_end_idx, paste, sep = ":"))
		     , list(from_timeframe = purrr::map2(f_start_idx, f_end_idx, lubridate::interval))
		     , list(to_timeframe   = purrr::map2(t_start_idx, t_end_idx, lubridate::interval))
		     )
		  , by = .(jk, from.src = f_src, to.src = t_src, src.pair = sprintf("%s -> %s", f_src, t_src))
		  ]
}

.vnames <- sprintf("(%s).?[0-9]+?", paste(test.evs$config$contexts, collapse = "|"));

			g = test.evs$evt_graphs[[1]]
			igraph::V(g)$size <- tryCatch({
				apply(X = (stringi::stri_split_fixed(V(g)$name,
					":", simplify = TRUE))[, c(2, 3)], MARGIN = 1,
					FUN = purrr::as_mapper(~as.integer(sqrt(as.numeric(diff(as.Date(.x)))))))
			}, error = function(e) {
				10
			})
			igraph::V(g)$key <- igraph::V(g)$name
			igraph::V(g)$name <- igraph::V(g)$name %>% sapply()

			.vattrs <- append(
						purrr::array_branch(stringi::stri_split_fixed(igraph::V(g)$name,":", simplify = TRUE), 2)
					, list(title = purrr::map_chr(igraph::V(g)$name, stringi::stri_extract_last_regex, pattern = .vnames))
					)
				purrr::set_names(
					c("jk", "start", "end", "source", "order", "title")))

			igraph::vertex_attr(g) <- purrr::modify_at(purrr::modify_at(purrr::modify_at(.vattrs,
				c("start", "end"), as.Date), "order", as.integer),
				"source", as.factor)
