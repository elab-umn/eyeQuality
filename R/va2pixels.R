#' Calculate pixel position from visual angle
#'
#' @param va integer Visual angle to be converted to pixel space
#' @param distZ integer distance Z from the screen in mm
#' @param display.res display resolution in px
#' @param display.dim display dimension in mm
#'
#' @importFrom pracma deg2rad
#'
#' @return px the pixel value for the input VA
#' @export
#'
va2pixels <- function(va, distZ, display.res, display.dim) {
  px <-
    (tan(deg2rad(va)) * distZ * display.res / display.dim) + (display.res /
                                                                2)
  return(px)
}
