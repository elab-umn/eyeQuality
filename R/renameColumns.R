#' rename column names to standard format from data input based on recording software
#'
#' To account for varying column names between data export types we rename columns to the following standard set:
#' (event, eventValue, recordingDuration_ms, resolutionHeight, resolutionWidth, eyeTrackerTimestamp,recordingTimestamp_ms, gazeLeftX, gazeLeftY, gazeRightX, gazeRightY, distanceLeftZ, distanceRightZ, pupilLeft, pupilRight, validityLeft, validityRight)
#'
#' @param data dataframe
#' @param software "TobiiStudio" or "TobiiPro"
#'
#' @importFrom stringr str_detect
#' @importFrom dplyr rename
#' @importFrom tidyr separate
#'
#' @return your reformatted data with standardized columns
renameColumns <- function(data, software) {
  if (str_detect(software, "TobiiStudio")) {
    data <- data %>%
      # dplyr::rename(
      #   event = .data$`StudioEvent`,
      #   eventValue = .data$`StudioEventData`,
      #   recordingDuration_ms = .data$`"RecordingDuration"`,
      #   # RecordingResolution = .data$`RecordingResolution`, #Default column name for TobiiStudio
      #   eyeTrackerTimestamp = .data$`"EyetrackerTimestamp"`,
      #   recordingTimestamp_ms = .data$`RecordingTimestamp`,
      #   gazeLeftX = .data$`"GazePointLeftX (ADCSpx)"`,
      #   gazeLeftY = .data$`"GazePointLeftY (ADCSpx)"`,
      #   gazeRightX = .data$`"GazePointRightX (ADCSpx)"`,
      #   gazeRightY = .data$`"GazePointRightY (ADCSpx)"`,
    #   distanceLeftZ = .data$`"EyePosLeftZ (ADCSmm)"`,
    #   distanceRightZ = .data$`"EyePosRightZ (ADCSmm)"`
    #   pupilLeft = `"PupilLeft"`,
    #   pupilRight = `"PupilRight"`,
    #   validityLeft = `"ValidityLeft"`,
    #   validityRight = `ValidityRight`
    # )
    # rename(
    #   event = `"StudioEvent"`,
    #   eventValue = `"StudioEventData"`,
    #   recordingDuration_ms = "RecordingDuration",
    #   # RecordingResolution = `RecordingResolution`, #Default column name for TobiiStudio
    #   eyeTrackerTimestamp = `"EyetrackerTimestamp"`,
    #   recordingTimestamp_ms = `"RecordingTimestamp"`,
    #   gazeLeftX = `"GazePointLeftX (ADCSpx)"`,
    #   gazeLeftY = `"GazePointLeftY (ADCSpx)"`,
    #   gazeRightX = `"GazePointRightX (ADCSpx)"`,
    #   gazeRightY = `"GazePointRightY (ADCSpx)"`,
    #   distanceLeftZ = `"EyePosLeftZ (ADCSmm)"`,
    #   distanceRightZ = `"EyePosRightZ (ADCSmm)"`
    #   pupilLeft = `"PupilLeft"`,
    #   pupilRight = `"PupilRight"`,
    #   validityLeft = `"ValidityLeft"`,
    #   validityRight = `"ValidityRight"`
    # )
    dplyr::rename(
      event = "StudioEvent",
      eventValue = "StudioEventData",
      recordingDuration_ms = "RecordingDuration",
      # RecordingResolution = `RecordingResolution`, #Default column name for TobiiStudio
      eyeTrackerTimestamp = "EyeTrackerTimestamp",
      recordingTimestamp_ms = "RecordingTimestamp",
      gazeLeftX = "GazePointLeftX (ADCSpx)",
      gazeLeftY = "GazePointLeftY (ADCSpx)",
      gazeRightX = "GazePointRightX (ADCSpx)",
      gazeRightY = "GazePointRightY (ADCSpx)",
      distanceLeftZ = "EyePosLeftZ (ADCSmm)",
      distanceRightZ = "EyePosRightZ (ADCSmm)",
      pupilLeft = "PupilLeft",
      pupilRight = "PupilRight",
      validityLeft = "ValidityLeft",
      validityRight = "ValidityRight"
    )

    # split RecordingResolution column into ResolutionWidth and ResolutionHeight
    data <-
      data %>% separate(
        .data$RecordingResolution,
        c("resolutionWidth", "resolutionHeight"),
        sep = " x ",
        convert = TRUE
      )
  }
  else if (str_detect(software, "TobiiPro")) {
    data <- data %>%
      # dplyr::rename(
      #   event = `"Event"`,
      #   eventValue = .data$`Event value`,
      #   recordingDuration_ms = .data$`Recording duration`,
      #   resolutionHeight = .data$`Recording resolution height`,
      #   resolutionWidth = .data$`Recording resolution width`,
      #   eyeTrackerTimestamp = .data$`Eyetracker timestamp`,
      #   recordingTimestamp_ms = .data$`Recording timestamp`,
      #   gazeLeftX = .data$`Gaze point left X`,
      #   gazeLeftY = .data$`Gaze point left Y`,
      #   gazeRightX = .data$`Gaze point right X`,
    #   gazeRightY = .data$`Gaze point right Y`,
    #   distanceLeftZ = .data$`Eye position left Z (DACSmm)`,
    #   distanceRightZ = .data$`Eye position right Z (DACSmm)`,
    #   pupilLeft = .data$`Pupil diameter left`,
    #   pupilRight = .data$`Pupil diameter right`,
    #   validityLeft = .data$`Validity left`,
    #   validityRight = .data$`Validity right`
    # )
    # rename(
    #   event = `"Event"`,
    #   eventValue = `"Event value"`,
    #   recordingDuration_ms = `"Recording duration"`,
    #   resolutionHeight = `"Recording resolution height"`,
    #   resolutionWidth = `"Recording resolution width"`,
    #   eyeTrackerTimestamp = `"Eyetracker timestamp"`,
    #   recordingTimestamp_ms = `"Recording timestamp"`,
    #   gazeLeftX = `"Gaze point left X"`,
    #   gazeLeftY = `"Gaze point left Y"`,
    #   gazeRightX = `"Gaze point right X"`,
    #   gazeRightY = `"Gaze point right Y"`,
    #   distanceLeftZ = `"Eye position left Z (DACSmm)"`,
    #   distanceRightZ = `"Eye position right Z (DACSmm)"`,
    #   pupilLeft = `"Pupil diameter left"`,
    #   pupilRight = `"Pupil diameter right"`,
    #   validityLeft = `"Validity left"`,
    #   validityRight = `"Validity right"`
    # )
    rename(
      `event` = "Event",
      `eventValue` = "Event value",
      `recordingDuration_ms` = "Recording duration",
      `resolutionHeight` = "Recording resolution height",
      `resolutionWidth` = "Recording resolution width",
      `eyeTrackerTimestamp` = "Eyetracker timestamp",
      `recordingTimestamp_ms` = "Recording timestamp",
      `gazeLeftX` = "Gaze point left X",
      `gazeLeftY` = "Gaze point left Y",
      `gazeRightX` = "Gaze point right X",
      `gazeRightY` = "Gaze point right Y",
      `distanceLeftZ` = "Eye position left Z (DACSmm)",
      `distanceRightZ` = "Eye position right Z (DACSmm)",
      `pupilLeft` = "Pupil diameter left",
      `pupilRight` = "Pupil diameter right",
      `validityLeft` = "Validity left",
      `validityRight` = "Validity right"
    )
  }
  return(data)
}
