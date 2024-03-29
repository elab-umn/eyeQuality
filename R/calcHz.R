#' calcHz
#' calculate eye tracker temporal resolution (Hz)
#'
#' @param data a dataframe, with recording timestamps IN MILLISECONDS
#' @param timepointIDs a range of timepoints by timepoint ID to include. Default includes all timepoints
#'
#' @return recHz
#' @export
#'
#' @examples
#' exampledata <- data.frame("recordingTimestamp_ms" = seq(1,1000,2))
#' calcHz(exampledata)
#' calcHz(exampledata, 2:nrow(exampledata)) #excludes the first timepoint
#'
calcHz <- function(data, timepointIDs = 1:nrow(data)) {
  recMs <- mean(diff(data$recordingTimestamp_ms[timepointIDs], lag = 1))
  recHz <- 1000 / round(recMs, 1)
  #TODO: check precision on rounding
  #TODO: set precision on recHz so it's consistent? 5 digits?
  return(recHz)
}
