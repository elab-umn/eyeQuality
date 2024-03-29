#' generateEyeTrackingPlots.R
#'
#' @param data dataframe
#' @import ggplot2
#' @import ggpubr
#' @return plots
#' @export
#'
generateEyeTrackingPlots <- function(data) {

  #Plot raw data for each channel
  rawGaze_leftX <- data %>% plotGazeAndBlinks("gazeLeftX") + ggplot2::labs(y="Left eye x-position")
  rawGaze_leftY <- data %>% plotGazeAndBlinks("gazeLeftY") + ggplot2::labs(y="Left eye y-position")
  rawGaze_rightX <- data %>% plotGazeAndBlinks("gazeRightX") + ggplot2::labs(y="Right eye x-position")
  rawGaze_rightY <- data %>% plotGazeAndBlinks("gazeRightY") + ggplot2::labs(y="Right eye y-position")

  rawGaze_leftPupil <- data %>% plotGazeAndBlinks("pupilLeft") + ggplot2::labs(y="Left pupil measurement")
  rawGaze_rightPupil <- data %>% plotGazeAndBlinks("pupilRight") + ggplot2::labs(y="Right pupil measurement")

  rawGaze_leftZ <- data %>% plotGazeAndBlinks("distanceLeftZ") + ggplot2::labs(y="Left eye z-distance")
  rawGaze_rightZ <- data %>% plotGazeAndBlinks("distanceRightZ") + ggplot2::labs(y="Right eye z-distance")


  rawGazePlot <- ggpubr::ggarrange(
    rawGaze_leftX,
    rawGaze_rightX,
    rawGaze_leftY,
    rawGaze_rightY,
    rawGaze_leftZ,
    rawGaze_rightZ,
    rawGaze_leftPupil,
    rawGaze_rightPupil,
    labels = c("Left X", "Right X", "Left Y", "Right Y", "Left Z", "Right Z", "Left Pupil", "Right Pupil"),
    label.x = 0.7,
    label.y = 0.08,
    ncol = 2, nrow = 4,
    common.legend = TRUE, legend="bottom"
    )

  # Get gaze heatmap showing density of fixations on the screen
  gazeHeatmap <- plotGazeHeatmap(data)

  # Get final plot of smoothed gaze for X and Y positions
  gaze_X <- data %>% plotGazeAndBlinks("gazeX.preprocessed_px", show_fixations = TRUE) + ggplot2::labs(y="Gaze x-position")
  gaze_Y <- data %>% plotGazeAndBlinks("gazeY.preprocessed_px", show_fixations = TRUE) + ggplot2::labs(y="Gaze y-position")

  gazePlot <- ggpubr::ggarrange(
    gaze_X,
    gaze_Y,
    labels = c("Gaze X", "Gaze Y"),
    label.x = 0.7,
    label.y = 0.08,
    ncol = 2, nrow = 1,
    common.legend = TRUE, legend="bottom"
  )

  return(list(rawGazePlot, gazeHeatmap, gazePlot))

}



