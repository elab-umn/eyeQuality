#' Check that data timestamps are in chronological order
#'
#' @param data a dataframe
#' @param timestamps string indicating variable containing time course data to be checked
#'
#' @return logical indicating TRUE if timestamps are chronologically ordered, FALSE if disordered
#' @export
#'
checkTimestamps <- function(data, timestamps) {
  ordered_ts <- sort(data[[timestamps]])
  ordered <- identical(data[[timestamps]], ordered_ts)
  return(ordered)
}
