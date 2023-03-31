retrace.evs <- function(event_graph, evs){
#' Retrace Events to Source
#'
#' \code{retrace.evs} uses the input graph and reference \code{event.vectors} object to return the source data rows corresponding to the event vertices.
#'
#' @param event_graph A graph contained within \code{$evt_graphs} (use the \code{`[[`} primitive if using a numeric index; otherwise, use \code{`$`})
#' @param evs An object of class \code{event.vectors}
#'
#' @return A list of distinct rows that map to the source event data.  Event graph vertex names can be used to subset the output.
#'
#' @export
  .haystack <- igraph::edge.attributes(event_graph) %$%
                mget(c("jk", "src.pair", "from.coord", "to.coord")) |>
                purrr::modify_at(c("from.coord", "to.coord"), stringi::stri_split_regex, "([:])|( -> )", simplify = TRUE) |>
                list2env(envir = new.env());

  # `.events` should be an array with dimensions N x 2
  .events <- .haystack$src.pair |> stringi::stri_split_fixed(" -> ", simplify = TRUE);

  .haystack$from.src <- .events[, 1];
  .haystack$to.src <- .events[, 2];

  .needle <- { rlang::exprs(
    jk = mget(ls(pattern = "jk$")) |> purrr::reduce(c)
    , src = mget(ls(pattern = "src$")) |> purrr::reduce(c)
    , time_start_idx = mget(ls(pattern = "^from.+coord")) |> purrr::reduce(c)
    , time_end_idx = mget(ls(pattern = "^to.+coord")) |> purrr::reduce(c)
    )}

  purrr::map(.needle, eval, envir = .haystack) |>
    data.table::as.data.table() |>
    purrr::pmap(~{
    lazy_refs <- evs$config[[stringi::stri_replace_first_regex(..2, "[:][0-9]+", "")]];
    src <- ..2;

    row_idx <- { lazy_refs %$%
      purrr::map2(
        .x = mget(c("jk", "time_start_idx", "time_end_idx"))
        , .y = list(..1, ..3, ..4)
        , ~which(rlang::eval_tidy(.x) == .y)
        ) |>
      unlist() |>
      # Frequency Table
      table() |>
      # Sort decreasing
      sort(decreasing = TRUE) |>
      # Index of largest value is the correct row of the filtered source data
      magrittr::extract(1) |>
      names() |>
      as.integer()
    };

    # Resolve the row data
    rlang::list2(!!src := rlang::eval_tidy(attr(lazy_refs, "src.def"))[row_idx])
  }) |>
  purrr::flatten() %>%
  .[!duplicated(names(.))]
}