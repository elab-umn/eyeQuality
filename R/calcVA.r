# calculate visual angle
#' Title
#'
#' @param data dataframe
#' @param display.resx int display resolution for width in px
#' @param display.resy int display resolution for height in px
#' @param display.dimx int display width in mm
#' @param display.dimy int display height in mm
#' @param gazeX string column name of gazepoints on X axis
#' @param gazeY string column name of gazepoints on Y axis
#' @param distZ string column name of distance from screen for each timepoint
#' @param ... additional passed parameters from parent function
#'
#' @return data dataframe with calculated visual angles in columns appended .va
#' @export
#'
calcVA <-
  function(data,
           display.resx,
           display.resy,
           display.dimx,
           display.dimy,
           gazeX,
           gazeY,
           distZ,
           ...) {
    print("Calculating VA...")
    data1 <- data

    #calculate visual angle
    data1$gazeX_va <-
      round(visAngle(data[[gazeX]], data[[distZ]], display.resx, display.dimx), 2)
    data1$gazeY_va <-
      round(visAngle(data[[gazeY]], data[[distZ]], display.resy, display.dimy), 2)

    return(data1)
  }
