continuity <- function(data, map_fields, time_fields, timeout = 0, boundary_name = "window", archipelago = TRUE, show.all = FALSE){
#' Continuity Creator
#'
#'  \code{continuity} is based on the \href{https://www.red-gate.com/simple-talk/sql/t-sql-programming/the-sql-of-gaps-and-islands-in-sequences/}{'islands & gaps'} concept.  Use case for \code{continuity} is to roll up a sequence consisting of smaller duration into a larger epoch governed by some meaningful separation between \eqn{n_\text{lower}} and \eqn{{n - 1}_\text{upper}} segments.
#'
#' @param data (object): The source dataset, including all non-sessioning fields desired
#'
#' @param map_fields (string): A vector of strings or symbols indicating the field names that will partition `data`
#'
#' @param time_fields (string): A vector of strings or symbols indicating the field names to use as "start" and "stop" temporal indices.  If only one value is given, that value will be repeated as the "stop" index
#'
#' @param timeout The largest allowable 'gap' in a series of time values before a new 'island' begins: can be a quoted expression that conditionally determines the value.
#'
#' @param boundary_name (string): The name root of the boundary column names (e.g., "episode" -> "episode_start_idx", "episode_end_idx")
#'
#' @param archipelago (logical | TRUE): Should the output include the islands and gaps generated?
#'
#' @param show.all (logical | FALSE): Should the output include all of the columns of the output?
#'
#' @importFrom book.of.utilities %tf%
#'
#' @return A data.table with columns <mapFields>, ..., timeOut, ISLAND, island_idx, where '...' is empty if `show.all` is FALSE
#'
#' @family Data Generation
#'
#' @export

	force(data);
	data <- data.table::as.data.table(data);

	# :: Helper function to ensure length-2 ----
	check_len <- purrr::as_mapper(~if (rlang::has_length(.x, 1)){ c(.x, .x) } else { .x });

	# :: Construction Objects ----
	map_fields <- { rlang::enexprs(map_fields, .named = TRUE) |>
		magrittr::extract2(1) |>
		as.list() |>
		magrittr::extract(-1) |>
		as.character() |>
		rlang::set_names() |>
		rlang::data_syms()
	}

	time_fields <- { rlang::enexprs(time_fields) |>
		magrittr::extract2(1) |>
		as.list() |>
		magrittr::extract(-1) |>
		as.character() |>
		check_len() |>
		magrittr::extract(c(1,2)) |>
		rlang::set_names(c("start_idx", "stop_idx")) |>
		rlang::data_syms()
	}

	boundary_name	<- as.character(substitute(boundary_name));

	optional_output <- {
		c("ISLAND"
			, paste0(boundary_name, "_start_idx")
			, paste0(boundary_name, "_end_idx")
			, "island_idx"
			) |>
			rlang::set_names() |>
			rlang::data_syms()
		}

	output_fields <- { list(map_fields, if (archipelago) { optional_output[-4] }) |>
										unique() |> purrr::reduce(append)}

	timeout <- rlang::enexpr(timeout)

	# :: Result data 1 ----
	outData <- c(map_fields, time_fields) |>
		purrr::map(rlang::eval_tidy, data = data) |>
		data.table::as.data.table(names(c(map_fields, time_fields[1])));

	outData[
	, # +{stop_idx, rec_idx, last_rec_idx} | Upper time index; record index; last record index flag
		`:=`(
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
			, rec_idx = seq_len(.N)
			, last_rec_idx = seq_len(.N) == .N
			)
	, by = names(map_fields)
	];

	#
	# :: Result data 2 ----
	map_fields <- names(map_fields);

	outData[
		, seg := 1:length(rec_idx), by = c(map_fields)
		][ # Gap precursor: column-wise sequential differences within start and stop indices using `diff()`
		, c("delta_start", "delta_stop") := purrr::map(list(start_idx, stop_idx), ~diff(c(data.table::first(.x), .x)))
		, by = c(map_fields)
		][
		# Gap: From one record to the next in a partitioned, ordered set: { stop[n] - stop[n-1] } - [stop - start]
		# Need a visual for this in help file
		, GAP := (1 * (seg > 1)) * (delta_stop - (stop_idx - start_idx))
		, by = rec_idx
		][
		# Correct GAPS with overlapping boundaries { start[n] < stop[n-1] }
		(GAP < 0)
		, `:=`(start_idx  = start_idx + GAP, stop_idx = stop_idx  + GAP)
		][(GAP < 0) | (is.na(GAP)), GAP := 0
		][
		, island_idx := book.of.utilities::count.cycles(GAP > eval(timeout), reset = FALSE)
		, by = c(map_fields)
		][
		# Set the optional fields to be returned based on the value for argument `archipelago` (default `TRUE`)
		# Derive values for  ISLAND, episode_start_idx, and episode_end_idx by grouping: G ~ map_fields + island_idx
		, c(names(optional_output[-4])) := list(
				# ISLAND
				max(stop_idx, na.rm = TRUE) - min(start_idx, na.rm = TRUE)
				# episode_start_idx
				, min(start_idx, na.rm = TRUE)
				# episode_end_idx
				, max(stop_idx, na.rm = TRUE)
				)
		, by = eval(c(map_fields, "island_idx"))
		][, partition := .GRP, by = c(map_fields)
		][(ISLAND == 0), ISLAND := 1
		][, .SD[, if (show.all) {c(1:(names(.SD) |> length())) } else { names(output_fields)  }, with = FALSE] |> unique()]
}
