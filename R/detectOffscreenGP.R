#' Detect Offscreen Gazepoints
#'
#' @param data your dataframe
#' @param gazeX gaze point X coordinate
#' @param gazeY gaze point Y coordinate
#' @param display.resx Integer, Display resolution X in pixels
#' @param display.resy Integer Display resolution Y in pixels
#' @param display.resx.min Integer X minimum value within acceptable display resolution range Default 0 assumed for pixel space
#' @param display.resy.min Integer Y minimum value within acceptable display resolution range. Default 0 assumed for pixel space
#'
#' @return binary list to be saved to your dataframe as new column "offscreen.gp"
#' @export
#'
detectOffscreenGP <-
  function(data,
           gazeX,
           gazeY,
           display.resx,
           display.resy,
           display.resx.min = 0,
           display.resy.min = 0) {
    #flag any gazepoints that may be offscreen based on acceptable input range
    offscreen.gp <- rep(NA, length(data[[gazeX]]))
    offscreen.gp <-
      dplyr::if_else(
        !between(data[[gazeX]], display.resx.min, display.resx) |
          !between(data[[gazeY]], display.resy.min, display.resy),
        "offscreen","onscreen"
      )

    return(offscreen.gp)
  }
