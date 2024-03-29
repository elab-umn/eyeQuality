#' Classify Offscreen gaze points in preprocessing pipeline
#'
#' @param data dataframe
#' @param display.resx X display resolution in pixels
#' @param display.resy Y display resolution in pixels
#' @param eyeSelection "Maximize", "Strict", "Left", "Right"
#' @param ... additional arguments are of either the form value or tag = value. Component names are created based on the tag (if present) or the deparsed argument itself.
#'
#' @return data
#' @export
#'
classifyOffscreen <- function(data,
                              display.resx,
                              display.resy,
                              eyeSelection,
                              ...){

  ###Get offscreen information for left eye (after interpolation)
  gazeLeft.offscreenInRange <-detectOffscreenGP(data,
                      gazeX = "gazeLeftX.int",
                      gazeY = "gazeLeftY.int",
                      display.resx,
                      display.resy)

  #recode new column as offscreen exclusionary / within range or onscreen
  data$gazeLeft.offscreen <- ifelse(gazeLeft.offscreenInRange == "offscreen" & data$gazeLeft.offscreen != "offscreen.exclusionary",
                                      "offscreen.withinRange", data$gazeLeft.offscreen)
  data$gazeLeft.offscreen[gazeLeft.offscreenInRange == "onscreen"] <- "onscreen"

  ###Get offscreen information for right eye (after interpolation)
    gazeRight.offscreenInRange <-detectOffscreenGP(data,
                                                     gazeX = "gazeRightX.int",
                                                     gazeY = "gazeRightY.int",
                                                     display.resx,
                                                     display.resy)


    #recode new column as offscreen exclusionary / within range or onscreen
    data$gazeRight.offscreen <- ifelse(gazeRight.offscreenInRange == "offscreen" & data$gazeRight.offscreen != "offscreen.exclusionary",
                                      "offscreen.withinRange", data$gazeRight.offscreen)
    data$gazeRight.offscreen[gazeRight.offscreenInRange == "onscreen"] <- "onscreen"

  ###Get final classification of offscreen-ness based on EyeSelection option
  ##MAXIMIZE OR STRICT
  if (grepl("Maximize|Strict", eyeSelection)){
    data$offscreen.eyeSelect <-detectOffscreenGP(data,
                            gazeX = "gazeX.eyeSelect",
                            gazeY = "gazeY.eyeSelect",
                            display.resx,
                            display.resy)

    #recode new column as offscreen exclusionary / within range or onscreen
    data$offscreen.classification <- offscreenCategory(eyeSelection, data$gazeLeft.offscreen,
                                                     data$gazeRight.offscreen, data$offscreen.eyeSelect)

      data <- data %>%
        select(-offscreen.eyeSelect)
  }

    ##LEFT
    if (eyeSelection == "Left"){
      data$offscreen.classification <- data$gazeLeft.offscreen
    }
    ##RIGHT
    if (eyeSelection == "Right"){
      data$offscreen.classification <- data$gazeRight.offscreen
    }

  return(data)
}
