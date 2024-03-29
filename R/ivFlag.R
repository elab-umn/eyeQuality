#' ivFlag
#' function ran from rmInvalidGP which marks invalid points as NA
#'
#' @param data your dataset (processed by formatCols() function)
#' @param eyeSelect "left" or "right"
#' @param software either "TobiiStudio" or "TobiiPro"
#' @param threshold ONLY for Tobii Studio datasets. A numeric, indicating acceptable threshold for valid data.
#'
#' @return data with invalid points marked as NA in specified column
#' @export
#'
ivFlag <- function(data, eyeSelect, software, threshold = 2) {
  #extract relevant gaze point columns
  cols <-
    colnames(data)[grepl(eyeSelect, colnames(data), ignore.case = TRUE) &
                     !grepl("valid", colnames(data))]
  #define validity column
  validityCol <-
    colnames(data)[grepl(eyeSelect, colnames(data), ignore.case = TRUE) &
                     grepl("valid", colnames(data))]
  #replace gazepoint data with NA for invalid gazepoints
  if (str_detect(software, "TobiiPro")) {
    for (i in cols) {
      replaceRows <- data[, validityCol] == "Invalid"
      data[tidyr::replace_na(replaceRows, FALSE), i] <- NA
    }
  }
  else if (str_detect(software, "TobiiStudio")) {
    for (i in cols) {
      data[i][data[validityCol] > threshold] <- NA
    }
  }
  #FIXME: this function is overwriting the columns. we should create new columns?
  return(data)
}
