#' Create an Event
#' 
#' This funciton leverages the `lambda.r` library to create \code{Event} types comprised of a list of quosures to make referencing data sources and elements easy with `rlang::eval_tidy()`.
#'
#' @param data (formula) A formula parsed as follows:\cr
#' \itemize{
#' \item {The LHS (if given) becomes the event label when argument \code{label} is not provided.}
#' \item {The RHS becomes a name or expression that returns the dataset when evaluated. If a pipe (\code{`|`}) is given after the data source name on the RHS, it is treated as an indication of an expression that will filter rows in the data when evaluated.}
#' }
#' @param jk,start,end(string) The name of the element in \code{data} denoting the \emph{'join-key'}, \emph{'start'}, and \emph{'end'} respectively.
#' @param label A unique label for the event source
#' @return A list comprised of the event label and three quosures (\code{jk}, \code{time_start_idx}, \code{time_end_idx})
#'
#' @examples
#' \dontrun{
#' library(event.vectors)
#' Event(
#'   data = Event_B ~ subset(evs_src_03, join_key != 2)
#'   , jk = "join_key"
#'   , start = "date.start"
#'   , end = "date.end"
#'   )
#' Event(
#'   "World Peace" ~ evs_src_01 | date.start >= "2026-02-15"
#'   , "join_key", "date.start", "date.end"
#'   )
#' }
#'
#' @export
#' @name Event
NULL

lambda.r::`%::%`(
  Event(data, jk, start, end, label = NULL)
  , formula : character : character : character : . : list
  )

lambda.r::`%as%`(
  Event(data, jk, start, end, label = NULL), {
    if (rlang::is_empty(label)){ label <- rlang::f_lhs(data) }
    if (!length(label)) stop("label must not be empty")

    data <- rlang::f_rhs(data)
    data_cond <- TRUE
    has_pipe <- any(grepl("[|]", rlang::expr_text(data)))

    if (has_pipe){
      cli::cli_alert_info("Conditional data source ({label}) indicated!")
      # DO NOT reverse the following order (`data_cond` then `data<-`):
      data_cond <- data[[3]]
      data <- data[[2]]
    }

    data_name <- as.character(data)
    is_expression <- length(data_name) > 1

    if (!is_expression){
      # Verify the existence of the object referenced by `data_name`:
      if (length(utils::find(data_name)) == 0) 
        stop(glue::glue("`{data_name}` not found"))
    }

    data <- eval(data) |> 
      data.table::as.data.table() |>
      _[eval(data_cond)] |> 
      as.environment()

    jk <- rlang::new_quosure(rlang::sym(jk), env = data)
    time_start_idx <- rlang::new_quosure(rlang::sym(start), env = data)
    time_end_idx <- rlang::new_quosure(rlang::sym(end), env = data)

    mget(c("label", "jk", "time_start_idx", "time_end_idx"))
  })