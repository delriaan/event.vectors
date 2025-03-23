#' Continuity Creator
#'
#'  \code{continuity} is based on the \href{https://www.red-gate.com/simple-talk/sql/t-sql-programming/the-sql-of-gaps-and-islands-in-sequences/}{'islands & gaps'} concept.  
#' Use case for \code{continuity} is to roll up a sequence consisting of smaller duration into a larger epoch governed by some meaningful separation between \eqn{n_\text{lower}} and \eqn{{n - 1}_\text{upper}} segments.
#'
#' @param data (object): The source dataset, including all non-sessioning fields desired
#'
#' @param map_fields (string): A vector of strings or symbols indicating the field names that will partition `data`
#'
#' @param time_fields (string): A vector of strings or symbols indicating the field names to use as "start" and "stop" temporal indices.
#' If only one value is given, that value will be repeated as the "stop" index
#'
#' @param timeout The largest allowable 'gap' in a series of time values before a new 'island' begins: can be a quoted expression that conditionally determines the value. 
#' If using date or datetime values for \code{time_fields}, specify the timeout using an appropriate \code{lubridate} functions (e.g., \code{\link[lubridate]{days}}).
#'
#' @param boundary_name (string): The name root of the boundary column names (e.g., "episode" => "episode_start_idx", "episode_end_idx")
#'
#' @param archipelago (logical | TRUE): Should the output include the islands and gaps generated?
#'
#' @param show.all (logical | FALSE): Should the output include all of the columns of the output? \code{show.all} and \code{archipelago} are independent
#'
#' @importFrom book.of.utilities %tf%
#'
#' @return A \code{\link[data.table]{data.table}} with the following columns \code{<mapFields>, <X>, <Y>}:\cr
#' \describe{
#' \item{X (\code{archipelago = TRUE})}{\code{\{boundary_name\}_start_idx, \{boundary_name\}_end_idx, ISLAND, GAP}}
#' \item{Y (\code{show.all = TRUE})}{\code{island_idx, seq_idx, start_idx, stop_idx, rec_idx, map_partition, delta_start, delta_stop}}
#' }
#'
#' @family Data Generation
#'
#' @export
continuity <- function(data, map_fields, time_fields, timeout = 0, boundary_name = "window", archipelago = TRUE, show.all = FALSE){
	# :: Helper function to ensure length-2 ----
	check_len <- \(x) if (rlang::has_length(x, 1)){ c(x, x) } else { x }

  # Helper function to convert `map_fields` and `time_fields` into data symbols:
  check_fields <- \(expr){
    res <- rlang::data_syms(expr)

    if (is.call(expr)){ res <- res[-1] }

    nms <- res |> sapply(\(expr) as.list(expr) |> _[[3]] |> deparse())
    rlang::set_names(res, nms)
  }

	# :: Helper function to make various sequence partitions:
  make_partitions <- \(gap, timeout){
    seq_idx <- cumsum(gap > timeout)

    list(
      seq_idx = seq_idx
      , island_idx = cumsum(!duplicated(seq_idx))
      )
  }

  # :: Argument Handling: ----
    force(data);
    data <- data.table::as.data.table(data);
    timeout <- rlang::enexpr(timeout)
    boundary_name <- as.character(rlang::enexpr(boundary_name))

    optional_output <- {
      c(paste0(boundary_name, "_start_idx")
        , paste0(boundary_name, "_end_idx")
        , "ISLAND", "GAP"
        ) |>
        rlang::set_names() |>
        rlang::data_syms()
      }

    map_fields <- check_fields(rlang::enexpr(map_fields))
    time_fields <- check_fields(rlang::enexpr(time_fields)) |> 
      check_len() |> 
      rlang::set_names(c("start_idx", "stop_idx"))
  
    # Set the optional fields to be returned based on argument `archipelago`:
    output_fields <- if (archipelago){
        purrr::list_assign(map_fields, !!!optional_output)
      } else { map_fields }

	# :: Result data 1: ----
  res <- c(map_fields, time_fields) |>
    purrr::map(rlang::eval_tidy, data = data) |>
    data.table::as.data.table(names(c(map_fields, time_fields[1]))) |>
    # +{stop_idx, rec_idx, last_rec_idx} | Upper time index; record index; last record index flag
    _[, `:=`(
      stop_idx = {
        if (identical(
              rlang::eval_tidy(time_fields[[1]], data = data)
              , rlang::eval_tidy(time_fields[[2]], data = data)
              )){
          .logi_vec = diff(c(0, start_idx)) |> as.numeric() < eval(timeout);

          .choices = c(data.table::shift(
              start_idx
              , fill = data.table::last(start_idx) + eval(timeout)
              , type = "lead")
              ) %tf% c(start_idx + rlang::eval_tidy(timeout))

          # output value test
          ifelse(.logi_vec, .choices$true, .choices$false)
        } else { stop_idx }
      }
      # `rec_idx` and `last_rec_idx` are "within group" features:
      , rec_idx = seq_len(.N)
      , map_partition = .GRP
      )
  , by = c(names(map_fields))
  ];

	# :: Result data 2: ----
    outData <- res[
      # Gap precursor: column-wise sequential differences within start and stop indices using `diff()`
      , c("delta_start", "delta_stop") := purrr::map(
          list(start_idx, stop_idx)
          , \(x) diff(c(x[1], x))
          )
      , by = map_partition
      ][][
      # Gap: From one record to the next in a partitioned, ordered set: { stop[n] - stop[n-1] } - [stop - start]
      # Need a visual for this in help file
      , GAP := as.numeric(rec_idx > 1) * (delta_stop - (stop_idx - start_idx))
      , by = map_partition
      ][][
      # Correct GAPS with overlapping boundaries { start[n] < stop[n-1] }
      (GAP < 0)
      , `:=`(start_idx  = start_idx + GAP, stop_idx = stop_idx  + GAP)
      ][][is.na(GAP), GAP := 0
      ][][
      , c("seq_idx", "island_idx") := make_partitions(GAP, eval(timeout))
      , by = map_partition
      ][]
    
    # Derive values for `{boundary_name}_start_idx`, `{boundary_name}_end_idx`, `ISLAND`, and `GAP` for each 
    # group defined as follows: G ~ <map_fields> + seq_idx
      if (archipelago){
        outData[, c(names(optional_output)) := list(
            # `{boundary_name}_start_idx`
            min(start_idx, na.rm = TRUE)
            # `{boundary_name}_end_idx`
            , max(stop_idx, na.rm = TRUE)
            # `ISLAND`
            , as.numeric(max(stop_idx, na.rm = TRUE) - min(start_idx, na.rm = TRUE))
            # `GAP`
            , as.numeric(GAP)
            )
        , by = c("map_partition", "island_idx")
        ]
      }

	# :: Set column order:
    data.table::setcolorder(outData, names(output_fields))
    data.table::setcolorder(outData, c("island_idx", "seq_idx"), after = "GAP")
	# :: Return: ----
    if (show.all) { outData } else { outData[, mget(names(output_fields))] }
}
