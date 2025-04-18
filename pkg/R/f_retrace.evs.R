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
retrace.evs <- function(event_graph, evs){
  .haystack <- igraph::edge.attributes(event_graph) %$%
                mget(c("jk", "src_pair", "from_coord", "to_coord")) |>
                purrr::modify_at(c("from_coord", "to_coord"), stringi::stri_split_regex, "([:])|( -> )", simplify = TRUE) |>
                list2env(envir = new.env());

  # `.events` should be an array with dimensions N x 2
  .events <- .haystack$src_pair |> stringi::stri_split_fixed(" -> ", simplify = TRUE);
  .haystack$from_src <- .events[, 1];
  .haystack$to_src <- .events[, 2];

  .needle <- {
  	rlang::exprs(
	    jk = mget(ls(pattern = "jk$", sorted = FALSE)) |> purrr::reduce(c)
	    , src = mget(ls(pattern = "src$", sorted = FALSE)) |> purrr::reduce(c)
	    , time_start_idx = mget(ls(pattern = "^from.+coord", sorted = FALSE)) |> purrr::reduce(c)
	    , time_end_idx = mget(ls(pattern = "^to.+coord", sorted = FALSE)) |> purrr::reduce(c)
	    )}

  # Find each "needle" in the "haystack"
  purrr::map(.needle, eval, envir = .haystack) |>
    data.table::as.data.table() |>
    purrr::pmap(~{
	    lazy_refs <- evs$config[[stringi::stri_replace_first_regex(..2, "[:][0-9]+", "")]];
	    src <- ..2;

	    # Determine which row from the source data maps to the arguments
	    row_idx <- {
	    	purrr::map2(
					lazy_refs %$% mget(c("jk", "time_start_idx", "time_end_idx"))
	        , list(..1, ..3, ..4)
	        , \(x, y) which(rlang::eval_tidy(x) == y)
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
	    }

	    # Resolve the row data
	    rlang::list2(!!src := rlang::eval_tidy(attr(lazy_refs, "src_def"))[row_idx])
	  }) |>
	  purrr::flatten() %>%
	  .[!duplicated(names(.))]
}