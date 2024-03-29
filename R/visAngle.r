#' Calculate Visual Angle
#'
#' @param pospx Integer, gaze position as a pixel coordinate (X or Y)
#' @param posz  Integer, distance from the screen in mm
#' @param disp.res Integer, display resolution on the relevant dimension (X ir Y)
#' @param disp.dim Integer, display dimension on the relevant dimension (X or Y)
#'
#' @return returns Angle in degrees
#' @export
#'
visAngle <- function(pospx, posz, disp.res, disp.dim) {
  Rad = atan2(((pospx - ((
    disp.res + 1
  ) * .5)) * disp.dim), (posz * disp.res))
  Ang = Rad * (180 / pi)
  return(Ang)
}
