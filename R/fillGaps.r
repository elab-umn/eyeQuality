#' Gap fill-in (interpolation)
# Drawing from Eli's MATLAB code interpolateGazepointGaps to interpolate all x and y (left and right) gazepoint time-series within data (saving out as new columns) for gaps that don't exceed maxGap
#'
#' @param data dataframe
#' @param recHz recording Hz (calculated in calcHz)
#' @param intCols list of column names to be interpolated
#' @param max_gap_length integer for max ms of gaps to interpolate over. Default value 50ms (pg. 119 Liz's dissertation)
#' @param ... additional passed parameters from parent function
#'
#' @importFrom zoo na.approx
#'
#' @return data with 8 new columns of interpolated left & right X/Y gaze points, left & right pupils, left and right Z position
#' @export
#

fillGaps <-
  function(data, recHz, intCols, max_gap_length = 50, ...) {
    # gap duration / sampling rate = number of samples in gap to update
    sampling.dur <- 1000 / recHz
    max_gap_points <- floor(max_gap_length / sampling.dur)
    print(
      paste(
        "Filling in gaps of ",
        max_gap_length,
        "ms. For your sampling rate, this is equivalent to ",
        max_gap_points,
        " gaze points",
        sep = ""
      )
    )

    #Create new names for cols
    newCols <- paste0(intCols, ".int")

    #na.approx replaces NAs by interpolation
    for (int in 1:length(intCols)) {
      n <- newCols[int]
      i <- intCols[int]
      data[[n]] <-
        round(na.approx(data[[i]], na.rm = FALSE, maxgap = max_gap_points), 2)
    }


    return(data)

  }
