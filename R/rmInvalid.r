#' Remove invalid gazepoints
#'
#' @param data your eyetracking data (a dataframe)
#' @param software Eye tracking software source, "TobiiStudio" or "TobiiPro"
#' @param threshold a numeric (0, 1, 2, 3, or 4) - Only applicable if software = "TobiiStudio"
#' @param ... additional passed parameters from parent function
#'
#' @importFrom stringr str_detect
#' @importFrom dplyr mutate na_if
#' @return your processed data, with Invalid GP marked as NA
#'
#' @export
#'
#' @examples
#' mydataset <- tobii_studio_example_data
#' rmInvalidGP(mydataset, "TobiiStudio", threshold = 2)
#' mydataset2 <- tobii_pro_example_data
#' rmInvalidGP(mydataset2, "TobiiPro")
rmInvalidGP <- function(data, software, threshold = 2, ...) {
  #Replace invalid gazepoints with NA
  data <- ivFlag(data, eyeSelect = "left", software, threshold)
  data <- ivFlag(data, eyeSelect = "right", software, threshold)

  #Replace missing value codes with NA
  data <- naFlag(data, software)

  return(data)
}
