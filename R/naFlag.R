#' naFlag
#' changes datapoints marked as -9999 in TobiiStudio to NA
#'
#' @param data a dataframe
#' @param software "TobiiPro" or "TobiiStudio
#'
#' @return data with recoded NA values
#' @export
#'
naFlag <- function(data, software) {
  if (str_detect(software, "TobiiStudio")) {
    data[data == -9999] <-
      NA #-9999 used to mark missing values, changed to NA
  }
  return(data)
}
