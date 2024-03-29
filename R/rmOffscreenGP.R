#' Remove offscreen gazepoints and related interpolations - rename to excludeOffscreenGazepoint?
#'
#' @param data your dataframe
#' @param gazeX string column containing name of variable with X gaze data
#' @param gazeY string column containing name of variable with Y gaze data
#' @param distZ string column containing name of variable with Z distance data in mm
#' @param overwrite optional list of column names in addition to gaze columns which should be replaced by NA when offscreen. Default = NA
#' @param display.resx X Display resolution in pixels
#' @param display.resy Y Display resolution in pixels
#' @param display.dimx X Display dimension in mm
#' @param display.dimy Y Display dimension in mm
#' @param valid_offscreen integer Degrees of Visual angle outside of screen which can be kept as valid. Default = 5 VA
#' @param ... additional arguments are of either the form value or tag = value. Component names are created based on the tag (if present) or the deparsed argument itself.
#'
#' @return data
#' @export
#'
rmOffscreenGP <-
  function(data,
           gazeX,
           gazeY,
           distZ,
           overwrite = NA,
           display.resx,
           display.resy,
           display.dimx,
           display.dimy,
           valid_offscreen = 5,
           ...) {
    #get participant's median distance from screen
    medZ = median(data[[distZ]], na.rm = TRUE)

    #get new acceptable boundaries
    display.vax <-
      round(visAngle(display.resx, medZ, display.resx, display.dimx), 2) + valid_offscreen
    display.vay <-
      round(visAngle(display.resy, medZ, display.resy, display.dimy), 2) + valid_offscreen

    #set acceptable boundaries in pixel space
    resx.max <-
      va2pixels(display.vax, medZ, display.resx, display.dimx)
    resx.min <-
      va2pixels(-display.vax, medZ, display.resx, display.dimx)
    resy.max <-
      va2pixels(display.vay, medZ, display.resy, display.dimy)
    resy.min <-
      va2pixels(-display.vay, medZ, display.resy, display.dimy)

    #get list to label datapoints as on or offscreen, based on acceptable boundaries
    invalid_offscreen <-
      detectOffscreenGP(
        data,
        gazeX,
        gazeY,
        display.resx = resx.max,
        display.resy = resy.max,
        display.resx.min = resx.min,
        display.resy.min = resy.min
      )

    #mark as NA
    #Get columns to be overwritten if flagged as offscreen
    replace_cols <- c(gazeX, gazeY, overwrite)
    replace_rows <- which(invalid_offscreen == "offscreen")

    #replace all offscreen columns with NA
    for (rc in replace_cols) {
      if (is.na(rc)){
        next()
      }
      data[replace_rows, rc] <- NA
    }

    #for these out-of-range gazepoints, update offscreen label to whether or not (1 / 0) gp is excluded as binary
    invalid_offscreen <-ifelse(invalid_offscreen == "offscreen", "offscreen.exclusionary", invalid_offscreen)

    #add new column for offscreen label
    if (grepl("Left", gazeX, ignore.case = TRUE)){
      data$gazeLeft.offscreen <- invalid_offscreen
    } else if (grepl("Right", gazeX, ignore.case = TRUE)){
      data$gazeRight.offscreen <- invalid_offscreen
    } else {
      data$gaze.offscreen <- invalid_offscreen
    }

    return(data)
  }
