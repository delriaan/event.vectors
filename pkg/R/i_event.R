#' Create an Event
#' 
#' This funciton leverages the 'lambda.r' library to create \code{Event} types comprised of a list of quosures to make referencing data sources and elements easy with `rlang::eval_tidy()`.
#'
#' @param data (formula) A formula, where the LHS (if given) becomes the event label when argument \code{label} is not provided. The RHS becomes a reference to the source data by name. If a pipe (\code{`|`}) is given after the data source name on the RHS, it is treated as an indication of an expression that will filter rows in the data when evaluated.
#' 
#' @param jk,start,end(string) The name of the element in \code{data} denoting the \emph{'join-key'}, \emph{'start'}, and \emph{'end'} respectively.
#' 
#' @param label A unique label for the event source
#' 
#' @return a list comprised of the event label and three quosures (\code{jk}, \code{time_start_idx}, \code{time_end_idx})
#' 
#' @export
#' 
#' @name Event
NULL

lambda.r::`%::%`(Event(data, jk, start, end, label = NULL), formula : character : character : character : . : list)

lambda.r::`%as%`(
  Event(data, jk, start, end, label = NULL), {
  if (rlang::is_empty(label)){ 
    label <- rlang::f_lhs(data) 
  }

  assertive::assert_is_non_empty(label)

  data <- rlang::f_rhs(data)
  data_cond <- TRUE
  has_pipe <- any(grepl("[|]", rlang::expr_text(data)))

  if (has_pipe){
    cli::cli_alert_info("Conditional data source ({label}) indicated!")
    # DO NOT reverse the following order:
    data_cond <- data[[3]]
    data <- data[[2]]
  }

  assertive::assert_is_non_empty(find(as.character(data)))

  data <- eval(data) |> 
    data.table::as.data.table() |>
    _[eval(data_cond)] |> 
    as.environment()

  jk <- rlang::new_quosure(rlang::sym(jk), env = data)
  time_start_idx <- rlang::new_quosure(rlang::sym(start), env = data)
  time_end_idx <- rlang::new_quosure(rlang::sym(end), env = data)

  mget(c("label", "jk", "time_start_idx", "time_end_idx"))
})