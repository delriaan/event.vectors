
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