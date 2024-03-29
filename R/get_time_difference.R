#' get_time_difference - get current time
#'
#' @param start timestamp for start time
#' @param stop timestamp for stop time
#' @param units units difference should be returned, (secs, mins, hours, etc)
#' @return timestamp in seconds, from sys.time()
#' @export
#'

get_time_difference <- function(start, stop, units = "secs") {
  return(as.numeric(stop - start, units = units))
}
