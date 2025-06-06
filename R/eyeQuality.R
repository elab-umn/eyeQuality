#' eyeQuality wrapper - run all preprocessing functions
#'
#' @param filepath path for data file as .tsv
#' @param displayDimensionX_mm integer of display width in millimeters. For example our 1920x1080 screen has a width of 594 mm
#' @param displayDimensionY_mm integer of display height in millimeters. For example our 1920x1080 screen has a width of 344 mm
#' @param data instead of passing a filepath, you can pass a data frame with your loaded data. Default NULL
#' @param eyeSelection_method string with possible values "Maximize", "Strict", "Left", or "Right". Default "Maximize"
#' @param smoothGaze_boolean Boolean indicating if we should run smoothGaze script
#' @param maxValidityThreshold a numeric (0, 1, 2, 3, or 4) describing what validity rating is considered acceptable - Only applicable if software = "TobiiStudio"
#' @param saveData Boolean indicating if we should suppress output, and save data to files
#' @param includeIntermediates Boolean indicating if column outputs from all intermediate steps of the preprocessing should be included. Default = FALSE
#' @param studioEvents optional list of two values specifying the start and ending event labels for Tobii Studio files, c(startEventName, endEventName) . Default NULL
#' @param proEvents optional list of two values specifying the start and ending event labels for Tobii Pro files, c(startEventName, endEventName) . Default NULL
#' @param timeStart optional Recording Timestamp of the first data point to include. Default NULL
#' @param timeEnd optional Recording Timestamp of the last data point to include. Default NULL
#' @param batchName optional string to append output files with a specific batch run label. Default NULL
#' @param ... additional arguments are of either the form value or tag = value. Component names are created based on the tag (if present) or the deparsed argument itself.
#'
#' @importFrom readr read_delim
#' @importFrom stringr str_glue
#'
#' @return preprocessed data
#' @export

eyeQuality <- function(filepath,
                           displayDimensionX_mm,
                           displayDimensionY_mm,
                           data = NULL,
                           eyeSelection_method = "Maximize",
                           smoothGaze_boolean = TRUE,
                           maxValidityThreshold = 2,
                           saveData = FALSE,
                           includeIntermediates = FALSE,
                           studioEvents = NULL,
                           proEvents = NULL,
                           timeStart = NULL,
                           timeEnd = NULL,
                           batchName = NULL,
                           ...) {
  #Wrapper
  runtime_start <- getCurrentTime()

  namedargs <- list(
    filepath = filepath,
    displayDimensionX_mm = displayDimensionX_mm,
    displayDimensionY_mm = displayDimensionY_mm,
    data = data,
    eyeSelection_method = eyeSelection_method,
    smoothGaze_boolean = smoothGaze_boolean,
    saveData = saveData,
    studioEvents = studioEvents,
    proEvents = proEvents,
    timeStart = timeStart,
    timeEnd = timeEnd
  )
  args <- list(...)
  args <- append(args, namedargs)

  if (saveData) {
    #get file path for run log
    # runtime_Log <- create_new_filename(filepath, "_RUNLOG", ".txt")
    runtime_Log <-
      create_new_filename(filepath,
                          paste0(
                            "_desc-",
                            ifelse(is.null(batchName), NULL, paste0(batchName, "_")),
                            "preproc_runlog"
                          ),
                          ".txt")
    #if we are saving the output, sink all cmd messages to file
    sinkToOutputFile(runtime_Log)
    #save initial function call to run log.
    # inputFunctionCall <- match.call(expand.dots = TRUE)
    # lapply(args, write, runtime_Log, append = TRUE)
  } else
    (runtime_Log <- NULL)

  argList <-
    paste0("eyeQuality(",
           paste(stringr::str_glue("{names(args)} = {args}"), collapse = ", "),
           ")")
  print(paste0("command run: \n", argList))
  # print_or_save(argList, saveData, runtime_Log)

  #load data
  if (is.null(data)) {
    data <- importData(filepath)
  } else {
    # data <- data
  }

  #data <- sampleTobiiProTbl #for test data
  runtime_loadData <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 01. importData complete. (run duration: {getPipelineTiming(runtime_start, runtime_loadData)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 01. importData complete. (run duration: {getPipelineTiming(runtime_start, runtime_loadData)})"), saveData, runtime_Log)

  #detect software
  software <- detectImportSourceType(data)
  runtime_detectImportSourceType <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 02. detectImportSourceType returns software = {software}. (run duration: {getPipelineTiming(runtime_loadData, runtime_detectImportSourceType)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 02. detectImportSourceType returns software = {software}. (run duration: {getPipelineTiming(runtime_loadData, runtime_detectImportSourceType)})"), saveData, runtime_Log)

  #format data to standard columns across software
  data <- standardizeColumnNames(data, software)

  runtime_standardizeColumnNames <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 03. standardizeColumnNames complete. (run duration: {getPipelineTiming(runtime_detectImportSourceType, runtime_standardizeColumnNames)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 03. standardizeColumnNames complete. (run duration: {getPipelineTiming(runtime_detectImportSourceType, runtime_standardizeColumnNames)})"), saveData, runtime_Log)

  #Check that any valid gaze data exists. If not, abort.
  leftDataExists <- checkGazeDataExists(data, gazeColumn = "gazeLeftX", ...)
  rightDataExists <- checkGazeDataExists(data, gazeColumn = "gazeRightX", ...)
  if (!leftDataExists & !rightDataExists){
    print(
      paste0("No valid gaze data exists. Preprocessing for file",
                 filepath,
                 " has been aborted."
      )
    )
    stop()
  }

  runtime_checkGazeDataExists <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 04. checkGazeDataExists complete. (run duration: {getPipelineTiming(runtime_standardizeColumnNames, runtime_checkGazeDataExists)})"
    )
  )

  #save & remove event rows
  data_list <- extractEventRows(data, software)
  data <- data_list[[1]] #gazestream data
  eventData <- data_list[[2]] #event data

  runtime_extractEventRows <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 05. extractEventRows complete. (run duration: {getPipelineTiming(runtime_standardizeColumnNames, runtime_extractEventRows)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 04. extractEventRows complete. (run duration: {getPipelineTiming(runtime_standardizeColumnNames, runtime_extractEventRows)})"), saveData, runtime_Log)

  #check timestamp data is correctly ordered
  ordered <-
    checkOrderedTimestamps(data, timestamps = "recordingTimestamp_ms")
  if (!ordered) {
    print(
      paste0(
        "Data is not chronologically ordered based on timestamp. Pre-processing for file ",
        filepath,
        " has been aborted."
      )
    )
    # print_or_save(paste0("Data is not chronologically ordered based on timestamp. Pre-processing for file ", filepath, " has been aborted."), saveData, runtime_Log)
    stop()
  }

#get specific time range to process
  if (!isempty(studioEvents) && !isempty(proEvents)) {
  #set correct event identity based on software version
    if (software == "TobiiStudio"){
      firstEvent <- studioEvents[1]
      lastEvent <- studioEvents[2]
    } else if (software == "TobiiPro"){
      firstEvent <- proEvents[1]
      lastEvent <- proEvents[2]
    }
    #Specify start and end timestamp for task
      taskTimes <-
        getEventTimes(eventData,
                      firstEvent = firstEvent,
                      lastEvent = lastEvent,
                      ...)
      if (length(taskTimes) == 2 &&
          !anyNA(taskTimes) && taskTimes[[1]] < taskTimes[[2]]) {
        data <- setTimestamps(data, taskTimes[[1]], taskTimes[[2]], ...)
        runtime_setTimestamps <- getCurrentTime()
        print(
          stringr::str_glue(
            "--- 05a. setTimestamps complete based on event parameter inputs. (run duration: {getPipelineTiming(runtime_extractEventRows, runtime_setTimestamps)})"
          )
        )
        # print_or_save(stringr::str_glue("--- 04a. setTimestamps complete based on event parameter inputs. (run duration: {getPipelineTiming(runtime_extractEventRows, runtime_setTimestamps)})"), saveData, runtime_Log)
      } else {
        #if event cannot be identified
        runtime_setTimestamps <- getCurrentTime()
        print(
          paste0(
            "Start or end timestamps could not be identified. Pre-processing for file ",
            filepath,
            " aborted."
          )
        )
        # print_or_save(paste0("Start or end timestamps could not be identified. Pre-processing for file ", filepath, " aborted."), saveData, runtime_Log)
        stop()
      }
  } else if (!isempty(timeStart) & !isempty(timeEnd)) {
    #set based on input time stamps
    data <- setTimestamps(data, timeStart, timeEnd, ...)
    runtime_setTimestamps <- getCurrentTime()
    print(
      stringr::str_glue(
        "--- 05b. setTimestamps complete based on timestamp inputs. (run duration: {getPipelineTiming(runtime_extractEventRows, runtime_setTimestamps)})"
      )
    )
    # print_or_save(stringr::str_glue("--- 04b. setTimestamps complete based on timestamp inputs. (run duration: {getPipelineTiming(runtime_extractEventRows, runtime_setTimestamps)})"), saveData, runtime_Log)
  } else {
    #run whole file if no time range or events are specified
    runtime_setTimestamps <- getCurrentTime()
    print("No time range specified. Running pre-processing on full data file.")
    # print_or_save("No time range specified. Running pre-processing on full data file.", saveData, runtime_Log)
  }

  #Calculate recording Hz
  recordingFrequency_hz <- calculateFrequency_hz(data)
  runtime_calculateFrequency_hz <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 06. calculateFrequency_hz returns recordingFrequency_hz = {recordingFrequency_hz}. (run duration: {getPipelineTiming(runtime_setTimestamps, runtime_calculateFrequency_hz)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 05. calculateFrequecny_hz returns recordingFrequency_hz = {recordingFrequency_hz}. (run duration: {getPipelineTiming(runtime_setTimestamps, runtime_calculateFrequency_hz)})"), saveData, runtime_Log)

  #Mark invalid datapoints as NA
  ##check - create a function to mark event placeholders as NA?'
  data <- removeInvalidGaze(data, whichEye = "left", software, maxValidityThreshold)
  data <- removeInvalidGaze(data, whichEye = "right", software, maxValidityThreshold)
  runtime_removeInvalidGaze <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 07. removeInvalidGaze complete. (run duration: {getPipelineTiming(runtime_calculateFrequency_hz, runtime_removeInvalidGaze)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 06. removeInvalidGaze complete. (run duration: {getPipelineTiming(runtime_calculateFrequency_hz, runtime_removeInvalidGaze)})"), saveData, runtime_Log)

  #flag off-screen gazepoints (in pixel space)
  #get resolution
  displayResolutionX_px <-
    as.numeric(unique(data$resolutionWidth[!is.na(data$resolutionWidth)]))
  displayResolutionY_px <-
    as.numeric(unique(data$resolutionHeight[!is.na(data$resolutionHeight)]))
  #mark out-of-range offscreen gp for each eye
  data <-
    removeOffscreenGaze(
      data,
      gazeX = "gazeLeftX",
      gazeY = "gazeLeftY",
      distanceZ = "distanceLeftZ",
      overwrite = c("pupilLeft"),
      displayResolutionX_px,
      displayResolutionY_px,
      displayDimensionX_mm,
      displayDimensionY_mm,
      ...
    )
  data <-
    removeOffscreenGaze(
      data,
      gazeX = "gazeRightX",
      gazeY = "gazeRightY",
      distanceZ = "distanceRightZ",
      overwrite = c("pupilRight"),
      displayResolutionX_px,
      displayResolutionY_px,
      displayDimensionX_mm,
      displayDimensionY_mm,
      ...
    )

  runtime_removeOffscreenGaze <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 08. removeOffscreenGaze complete. (run duration: {getPipelineTiming(runtime_removeInvalidGaze, runtime_removeOffscreenGaze)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 07. removeOffscreenGaze complete. (run duration: {getPipelineTiming(runtime_removeInvalidGaze, runtime_removeOffscreenGaze)})"), saveData, runtime_Log)

  #interpolation
  columnsToInterpolate <-
    c(
      "gazeLeftX",
      "gazeLeftY",
      "gazeRightX",
      "gazeRightY",
      "distanceLeftZ",
      "distanceRightZ",
      "pupilLeft",
      "pupilRight"
    )
  data <- interpolateGaze(data, recordingFrequency_hz, columnsToInterpolate, ...)
  runtime_interpolateGaze <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 09. interpolateGaze complete. (run duration: {getPipelineTiming(runtime_removeOffscreenGaze, runtime_interpolateGaze)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 08. interpolateGaze complete. (run duration: {getPipelineTiming(runtime_removeOffscreenGaze, runtime_interpolateGaze)})"), saveData, runtime_Log)

  #blink detection
  data <-
    classifyBlinks(data,
                   pupilLeft = "pupilLeft.int",
                   pupilRight = "pupilRight.int",
                   recordingFrequency_hz,
                   ...)
  if (eyeSelection_method == "Maximize" || eyeSelection_method == "Strict") {
    data$blink.classification <- data$bothEyes.blink
  }
  else if (eyeSelection_method == "Left") {
    data$blink.classification <- data$pupilLeft.blink
  }
  else if (eyeSelection_method == "Right") {
    data$blink.classification <- data$pupilRight.blink
  }
  runtime_classifyBlinks <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 10. classifyBlinks complete. (run duration: {getPipelineTiming(runtime_interpolateGaze, runtime_classifyBlinks)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 09. classifyBlinks complete. (run duration: {getPipelineTiming(runtime_interpolateGaze, runtime_classifyBlinks)})"), saveData, runtime_Log)

  #eye selection
  data <- eyeSelection(data, eyeSelection_method = eyeSelection_method, ...)
  runtime_eyeSelection <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 11. eyeSelection complete. (run duration: {getPipelineTiming(runtime_classifyBlinks, runtime_eyeSelection)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 10. eyeSelection complete. (run duration: {getPipelineTiming(runtime_classifyBlinks, runtime_eyeSelection)})"), saveData, runtime_Log)

  #Check that valid gaze data exists after selection
  dataExists <- checkGazeDataExists(data, gazeColumn = "gazeX.eyeSelect", ...)
  if (!dataExists){
    print(
      paste0("No valid gaze data exists after eye selection. Preprocessing for file",
             filepath,
             " has been aborted."
      )
    )
    stop()
  }

  runtime_checkGazeDataExists2 <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 12. checkGazeDataExists complete. (run duration: {getPipelineTiming(runtime_eyeSelection, runtime_checkGazeDataExists2)})"
    )
  )

  #Classify all data as onscreen or offscreen (exclusionary / within range)
  data <- classifyOffscreenGaze(data, displayResolutionX_px, displayResolutionY_px, eyeSelection_method, ...)

  runtime_classifyOffscreenGaze <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 13. classifyOffscreenGaze complete. (run duration: {getPipelineTiming(runtime_checkGazeDataExists2, runtime_classifyOffscreenGaze)})"
    )
  )
  #smoothing - denoise function
  columnsToSmooth <-
    colnames(data)[grepl("\\.eyeSelect$", colnames(data), ignore.case = TRUE)]   #get interpolated column names
  # noise_reduction_check <- ifelse(("smoothGaze_boolean" %in% names(args)) & smoothGaze_boolean == FALSE, FALSE, TRUE) #by default we denoise
  data <-
    smoothGaze(data, recordingFrequency_hz, columnsToSmooth, smoothGaze_boolean = smoothGaze_boolean, ...)
  runtime_smoothGaze <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 13. smoothGaze complete. (run duration: {getPipelineTiming(runtime_classifyOffscreenGaze, runtime_smoothGaze)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 11. smoothGaze complete. (run duration: {getPipelineTiming(runtime_eyeSelection, runtime_smoothGaze)})"), saveData, runtime_Log)

  #calculate visual angle
  if (smoothGaze_boolean) {
    data <-
      calculateGaze_va(
        data,
        displayResolutionX_px,
        displayResolutionY_px,
        displayDimensionX_mm,
        displayDimensionY_mm,
        gazeX = "gazeX.smooth",
        gazeY = "gazeY.smooth",
        distanceZ = "distanceZ.smooth",
        ...
      )
  }
  else if (!smoothGaze_boolean) {
    data <-
      calculateGaze_va(
        data,
        displayResolutionX_px,
        displayResolutionY_px,
        displayDimensionX_mm,
        displayDimensionY_mm,
        gazeX = "gazeX.eyeSelect",
        gazeY = "gazeY.eyeSelect",
        distanceZ = "distanceZ.eyeSelect",
        ...
      )
  }
  runtime_calculateGaze_va <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 14. calculateGaze_va complete. (run duration: {getPipelineTiming(runtime_smoothGaze, runtime_calculateGaze_va)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 12. calculateGaze_va complete. (run duration: {getPipelineTiming(runtime_smoothGaze, runtime_calculateGaze_va)})"), saveData, runtime_Log)

  #calculate velocity (using VA calculated gaze points)
  data <-
    calculateVelocity_va_ms(data,
            gazeX_va = "gazeX_va",
            gazeY_va = "gazeY_va",
            timestamp = "recordingTimestamp_ms",
            ...)
  runtime_calculateVelocity_va_ms <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 15. calculateVelocity_va_ms complete. (run duration: {getPipelineTiming(runtime_calculateGaze_va, runtime_calculateVelocity_va_ms)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 13. calculateVelocity_va_ms complete. (run duration: {getPipelineTiming(runtime_calculateGaze_va, runtime_calculateVelocity_va_ms})"), saveData, runtime_Log)

  ##smooth velocity
  velCols <-
    colnames(data)[grepl("_va_ms", colnames(data), ignore.case = TRUE)]  #get velocity columns
  data <- smoothVelocity(data, recordingFrequency_hz, velocity = velCols, ...)
  runtime_smoothVelocity <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 16. smoothVelocity complete. (run duration: {getPipelineTiming(runtime_calculateVelocity_va_ms, runtime_smoothVelocity)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 14. smoothVelocity complete. (run duration: {getPipelineTiming(runtime_calculateVelocity_va_ms, runtime_smoothVelocity)})"), saveData, runtime_Log)

  #filter -  classifyGazeIVT from Liz, includes mergeAdj, removeShortfix
  IVT_list <-
    classifyGazeIVT(
      data,
      velocity = "velocityEuclidean.smooth_va_ms",
      gazeX_va = "gazeX_va",
      gazeY_va = "gazeY_va",
      recordingFrequency_hz = recordingFrequency_hz,
      ...
    )
  #Summarized IVT file
  data <- IVT_list[[1]]
  data$IVT.classification[which(data$IVT.classification == "missing" &
                                  data$blink.classification == 1)] <- "blink"
  #All metrics IVT file
  data_IVT_all <- IVT_list[[2]]

  runtime_classifyGazeIVT <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 17. classifyGazeIVT complete. (run duration: {getPipelineTiming(runtime_smoothVelocity, runtime_classifyGazeIVT)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 15. classifyGazeIVT complete. (run duration: {getPipelineTiming(runtime_smoothVelocity, runtime_classifyGazeIVT)})"), saveData, runtime_Log)

  #Assign final column labels
  data <- assignFinalColumnNames(data, smoothGaze_boolean, ...)

  runtime_assignFinalColumnNames <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 18. assignFinalColumnNames complete. (run duration: {getPipelineTiming(runtime_classifyGazeIVT, runtime_assignFinalColumnNames)})"
    )
  )

  #get final runtime
  runtime_final <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- PREPROCESSING COMPLETE (preprocessing run duration: {getPipelineTiming(runtime_start, runtime_final)})"
    )
  )
  # print_or_save(stringr::str_glue("--- PREPROCESSING COMPLETE (preprocessing run duration: {getPipelineTiming(runtime_start, runtime_final)})"), saveData, runtime_Log)


  #data quality - robustness, contiguity, precision, %missingness, etc...
  summaryMetrics <- calculateOutputMetrics(data)
  runtime_calcOutputMetrics <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- OUTPUT METRICS CALCULATED (total run duration: {getPipelineTiming(runtime_start, runtime_calcOutputMetrics)})"
    )
  )
  # print_or_save(stringr::str_glue("--- OUTPUT METRICS CALCULATED (total run duration: {getPipelineTiming(runtime_start, runtime_calcOutputMetrics)})"), saveData, runtime_Log)

  times <- list(
    start = runtime_start,
    loadData = runtime_loadData,
    detectImportSourceType = runtime_detectImportSourceType,
    standardizeColumnNames = runtime_standardizeColumnNames,
    extractEventRows = runtime_extractEventRows,
    setTimestamps = runtime_setTimestamps,
    calculateFrequency_hz = runtime_calculateFrequency_hz,
    removeInvalidGaze = runtime_removeInvalidGaze,
    removeOffscreenGaze = runtime_removeOffscreenGaze,
    interpolateGaze = runtime_interpolateGaze,
    classifyBlinks = runtime_classifyBlinks,
    eyeSelection = runtime_eyeSelection,
    smoothGaze = runtime_smoothGaze,
    calculateGaze_va = runtime_calculateGaze_va,
    calculateVelocity_va_ms = runtime_calculateVelocity_va_ms,
    smoothVelocity = runtime_smoothVelocity,
    classifyGazeIVT = runtime_classifyGazeIVT,
    final = runtime_final,
    calcOutputMetrics = runtime_calcOutputMetrics,
    total = getCurrentTime()
  )

  durations <- list(
    loadData_duration = calculateTimeDifference(runtime_start, runtime_loadData, units =
                                              "secs"),
    detectImportSourceType_duration = calculateTimeDifference(runtime_loadData, runtime_detectImportSourceType, units =
                                                      "secs"),
    standardizeColumnNames_duration = calculateTimeDifference(runtime_detectImportSourceType, runtime_standardizeColumnNames, units =
                                                "secs"),
    extractEventRows_duration = calculateTimeDifference(runtime_standardizeColumnNames, runtime_extractEventRows, units =
                                                     "secs"),
    setTimestamps_duration = calculateTimeDifference(runtime_extractEventRows, runtime_setTimestamps, units =
                                                  "secs"),
    calculateFrequency_hz_duration = calculateTimeDifference(runtime_setTimestamps, runtime_calculateFrequency_hz, units =
                                            "secs"),
    removeInvalidGaze_duration = calculateTimeDifference(runtime_calculateFrequency_hz, runtime_removeInvalidGaze, units =
                                                 "secs"),
    removeOffscreenGaze_duration = calculateTimeDifference(runtime_removeInvalidGaze, runtime_removeOffscreenGaze, units =
                                                       "secs"),
    interpolateGaze_duration = calculateTimeDifference(runtime_removeOffscreenGaze, runtime_interpolateGaze, units =
                                              "secs"),
    classifyBlinks_duration = calculateTimeDifference(runtime_interpolateGaze, runtime_classifyBlinks, units =
                                                  "secs"),
    eyeSelection_duration = calculateTimeDifference(runtime_classifyBlinks, runtime_eyeSelection, units =
                                               "secs"),
    smoothGaze_duration = calculateTimeDifference(runtime_eyeSelection, runtime_smoothGaze, units =
                                             "secs"),
    calculateGaze_va_duration = calculateTimeDifference(runtime_smoothGaze, runtime_calculateGaze_va, units =
                                            "secs"),
    calculateVelocity_va_ms_duration = calculateTimeDifference(runtime_calculateGaze_va, runtime_calculateVelocity_va_ms, units =
                                             "secs"),
    smoothVelocity_duration = calculateTimeDifference(runtime_calculateVelocity_va_ms, runtime_smoothVelocity, units =
                                               "secs"),
    classifyGazeIVT_duration = calculateTimeDifference(runtime_smoothVelocity, runtime_classifyGazeIVT, units =
                                                   "secs"),
    final_duration = calculateTimeDifference(runtime_classifyGazeIVT, runtime_final, units =
                                           "secs"),
    calcOutputMetrics_duration = calculateTimeDifference(runtime_final, runtime_calcOutputMetrics, units =
                                                       "secs"),
    total_duration = calculateTimeDifference(runtime_start, runtime_calcOutputMetrics, units =
                                           "secs")
  )

  timingData = do.call(c, list(times, durations))

#Clean up final dataset output

  #remove columns created in preprocessing pipeline that are not the final outputs / categories
  if (!includeIntermediates){
    #remove temporary eye selection calculation columns
    tempCols <- c(colnames(data)[grepl("\\.temp$", colnames(data), ignore.case = TRUE)],
                  colnames(data)[grepl("\\.es.selection$", colnames(data), ignore.case = TRUE)])
    for (t in tempCols) {
      data[[t]] <- NULL
    }
    
    data <- removeIntermediateCols(data, ...)
  }

  if (saveData) {
    #save event data
    # saveFiles(filepath, args, data, eventData, timingData, summaryMetrics, batchName)
    saveFiles(filepath,
              data,
              eventData,
              timingData,
              summaryMetrics,
              batchName)
    sinkReset()
  } else {
    return(data)
  }
}
