#' Wrapper - run all preprocessing functions
#'
#' @param filepath path for data file as .tsv
#' @param display.dimx integer of display width in millimeters. For example our 1920x1080 screen has a width of 594 mm
#' @param display.dimy integer of display height in millimeters. For example our 1920x1080 screen has a width of 344 mm
#' @param data instead of passing a filepath, you can pass a data frame with your loaded data. Default NULL
#' @param eyeSelection string with possible values "Maximize", "Strict", "Left", or "Right". Default "Maximize"
#' @param noise_reduction Boolean indicating if we should run denoise script
#' @param savedata Boolean indicating if we should suppress output, and save data to files
#' @param includeIntermediates Boolean indicating if column outputs from all intermediate steps of the preprocessing should be included. Default = FALSE
#' @param firstEvent optional String of the first event marker to include. Default NULL
#' @param lastEvent optional String of the last event marker to include. Default NULL
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

run_preprocess <- function(filepath,
                           display.dimx,
                           display.dimy,
                           data = NULL,
                           eyeSelection = "Maximize",
                           noise_reduction = TRUE,
                           savedata = FALSE,
                           includeIntermediates = FALSE,
                           firstEvent = NULL,
                           lastEvent = NULL,
                           timeStart = NULL,
                           timeEnd = NULL,
                           batchName = NULL,
                           ...) {
  #Wrapper
  runtime_start <- get_time()

  namedargs <- list(
    filepath = filepath,
    display.dimx = display.dimx,
    display.dimy = display.dimy,
    data = data,
    eyeSelection = eyeSelection,
    noise_reduction = noise_reduction,
    savedata = savedata,
    firstEvent = firstEvent,
    lastEvent = lastEvent,
    timeStart = timeStart,
    timeEnd = timeEnd
  )
  args <- list(...)
  args <- append(args, namedargs)

  if (savedata) {
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
    paste0("run_preprocess(",
           paste(stringr::str_glue("{names(args)} = {args}"), collapse = ", "),
           ")")
  print(paste0("command run: \n", argList))
  # print_or_save(argList, savedata, runtime_Log)

  #load data
  if (is.null(data)) {
    data <- importData(filepath)
  } else {
    # data <- data
  }

  #data <- sampleTobiiProTbl #for test data
  runtime_loadData <- get_time()
  print(
    stringr::str_glue(
      "--- 01. importData complete. (run duration: {pipeline_timing(runtime_start, runtime_loadData)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 01. importData complete. (run duration: {pipeline_timing(runtime_start, runtime_loadData)})"), savedata, runtime_Log)

  #detect software
  software <- detectImportSourceType(data)
  runtime_detectImportSourceType <- get_time()
  print(
    stringr::str_glue(
      "--- 02. detectImportSourceType returns software = {software}. (run duration: {pipeline_timing(runtime_loadData, runtime_detectImportSourceType)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 02. detectImportSourceType returns software = {software}. (run duration: {pipeline_timing(runtime_loadData, runtime_detectImportSourceType)})"), savedata, runtime_Log)

  #format data to standard columns across software
  data <- formatCols(data, software)

  runtime_formatCols <- get_time()
  print(
    stringr::str_glue(
      "--- 03. formatCols complete. (run duration: {pipeline_timing(runtime_detectImportSourceType, runtime_formatCols)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 03. formatCols complete. (run duration: {pipeline_timing(runtime_detectImportSourceType, runtime_formatCols)})"), savedata, runtime_Log)

  #Check that any valid gaze data exists. If not, abort.
  leftDataExists <- checkGazeData(data, "gazeLeftX", ...)
  rightDataExists <- checkGazeData(data, "gazeRightX", ...)
  if (!leftDataExists & !rightDataExists){
    print(
      paste0("No valid gaze data exists. Preprocessing for file",
                 filepath,
                 " has been aborted."
      )
    )
    stop()
  }

  runtime_checkGazeData <- get_time()
  print(
    stringr::str_glue(
      "--- 04. checkGazeData complete. (run duration: {pipeline_timing(runtime_formatCols, runtime_checkGazeData)})"
    )
  )

  #save & remove event rows
  data_list <- removeEventRows(data, software)
  data <- data_list[[1]] #gazestream data
  eventData <- data_list[[2]] #event data

  runtime_removeEventRows <- get_time()
  print(
    stringr::str_glue(
      "--- 05. removeEventRows complete. (run duration: {pipeline_timing(runtime_formatCols, runtime_removeEventRows)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 04. removeEventRows complete. (run duration: {pipeline_timing(runtime_formatCols, runtime_removeEventRows)})"), savedata, runtime_Log)

  #check timestamp data is correctly ordered
  ordered <-
    checkTimestamps(data, timestamps = "recordingTimestamp_ms")
  if (!ordered) {
    print(
      paste0(
        "Data is not chronologically ordered based on timestamp. Pre-processing for file ",
        filepath,
        " has been aborted."
      )
    )
    # print_or_save(paste0("Data is not chronologically ordered based on timestamp. Pre-processing for file ", filepath, " has been aborted."), savedata, runtime_Log)
    stop()
  }

  ####Temporary code to set event labels for E-Lab tasks
  if (software == "TobiiPro") {
    firstEvent <- "VideoStimulusStart"
    lastEvent <- "VideoStimulusEnd"
  } else if (software == "TobiiStudio") {
    firstEvent <- "MovieStart"
    lastEvent <- "MovieEnd"
  }
  ####Temporary code end

  #Specify start and end timestamp for task
  if (!isempty(firstEvent) & !isempty(lastEvent)) {
    taskTimes <-
      getEventTimes(eventData,
                    firstEvent = firstEvent,
                    lastEvent = lastEvent,
                    ...)
    if (length(taskTimes) == 2 &&
        !anyNA(taskTimes) && taskTimes[[1]] < taskTimes[[2]]) {
      data <- setTimeRange(data, taskTimes[[1]], taskTimes[[2]], ...)
      runtime_setTimeRange <- get_time()
      print(
        stringr::str_glue(
          "--- 05a. setTimeRange complete based on event parameter inputs. (run duration: {pipeline_timing(runtime_removeEventRows, runtime_setTimeRange)})"
        )
      )
      # print_or_save(stringr::str_glue("--- 04a. setTimeRange complete based on event parameter inputs. (run duration: {pipeline_timing(runtime_removeEventRows, runtime_setTimeRange)})"), savedata, runtime_Log)
    } else {
      runtime_setTimeRange <- get_time()
      print(
        paste0(
          "Start or end timestamps could not be identified. Pre-processing for file ",
          filepath,
          " aborted."
        )
      )
      # print_or_save(paste0("Start or end timestamps could not be identified. Pre-processing for file ", filepath, " aborted."), savedata, runtime_Log)
      stop()
    }
  } else if (!isempty(timeStart) & !isempty(timeEnd)) {
    data <- setTimeRange(data, timeStart, timeEnd, ...)
    print(
      stringr::str_glue(
        "--- 05b. setTimeRange complete based on timestamp inputs. (run duration: {pipeline_timing(runtime_removeEventRows, runtime_setTimeRange)})"
      )
    )
    # print_or_save(stringr::str_glue("--- 04b. setTimeRange complete based on timestamp inputs. (run duration: {pipeline_timing(runtime_removeEventRows, runtime_setTimeRange)})"), savedata, runtime_Log)
  } else {
    runtime_setTimeRange <- get_time()
    print("No time range specified. Running pre-processing on full data file.")
    # print_or_save("No time range specified. Running pre-processing on full data file.", savedata, runtime_Log)
  }

  #Calculate recording Hz
  recHz <- calcHz(data)
  runtime_calcHz <- get_time()
  print(
    stringr::str_glue(
      "--- 06. calcHZ returns recHz = {recHz}. (run duration: {pipeline_timing(runtime_setTimeRange, runtime_calcHz)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 05. calcHZ returns recHz = {recHz}. (run duration: {pipeline_timing(runtime_setTimeRange, runtime_calcHz)})"), savedata, runtime_Log)

  #Mark invalid datapoints as NA
  ##check - create a function to mark event placeholders as NA?'
  data <- rmInvalidGP(data, software, ...)
  runtime_rmInvalidGP <- get_time()
  print(
    stringr::str_glue(
      "--- 07. rmInvalidGP complete. (run duration: {pipeline_timing(runtime_calcHz, runtime_rmInvalidGP)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 06. rmInvalidGP complete. (run duration: {pipeline_timing(runtime_calcHz, runtime_rmInvalidGP)})"), savedata, runtime_Log)

  #flag off-screen gazepoints (in pixel space)
  #get resolution
  display.resx <-
    as.numeric(unique(data$resolutionWidth[!is.na(data$resolutionWidth)]))
  display.resy <-
    as.numeric(unique(data$resolutionHeight[!is.na(data$resolutionHeight)]))
  #mark out-of-range offscreen gp for each eye
  data <-
    rmOffscreenGP(
      data,
      gazeX = "gazeLeftX",
      gazeY = "gazeLeftY",
      distZ = "distanceLeftZ",
      overwrite = c("pupilLeft"),
      display.resx,
      display.resy,
      display.dimx,
      display.dimy,
      ...
    )
  data <-
    rmOffscreenGP(
      data,
      gazeX = "gazeRightX",
      gazeY = "gazeRightY",
      distZ = "distanceRightZ",
      overwrite = c("pupilRight"),
      display.resx,
      display.resy,
      display.dimx,
      display.dimy,
      ...
    )

  runtime_rmOffscreenGP <- get_time()
  print(
    stringr::str_glue(
      "--- 08. rmOffscreenGP complete. (run duration: {pipeline_timing(runtime_rmInvalidGP, runtime_rmOffscreenGP)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 07. detectOffscreenGP complete. (run duration: {pipeline_timing(runtime_rmInvalidGP, runtime_detectOffscreenGP)})"), savedata, runtime_Log)

  #interpolation
  intCols <-
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
  data <- fillGaps(data, recHz, intCols, ...)
  runtime_fillGaps <- get_time()
  print(
    stringr::str_glue(
      "--- 09. fillGaps complete. (run duration: {pipeline_timing(runtime_rmOffscreenGP, runtime_fillGaps)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 08. fillGaps complete. (run duration: {pipeline_timing(runtime_detectOffscreenGP, runtime_fillGaps)})"), savedata, runtime_Log)

  #blink detection
  if (eyeSelection == "Maximize" || eyeSelection == "Strict") {
    data <-
      detectBlinks(data,
                   pupilLeft = "pupilLeft.int",
                   pupilRight = "pupilRight.int",
                   recHz,
                   ...)
    data$blink.classification <- data$bothEyes.blink
  }
  else if (eyeSelection == "Left") {
    data <- detectBlinks(data, pupil_cols = "pupilLeft.int", recHz, ...)
    data$blink.classification <- data$pupilLeft.blink
  }
  else if (eyeSelection == "Right") {
    data <-
      detectBlinks(data, pupil_cols = "pupilRight.int", recHz, ...)
    data$blink.classification <- data$pupilRight.blink
  }
  runtime_detectBlinks <- get_time()
  print(
    stringr::str_glue(
      "--- 10. detectBlinks complete. (run duration: {pipeline_timing(runtime_fillGaps, runtime_detectBlinks)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 09. detectBlinks complete. (run duration: {pipeline_timing(runtime_fillGaps, runtime_detectBlinks)})"), savedata, runtime_Log)

  #eye selection
  data <- eyeSelect(data, eyeSelection = eyeSelection, ...)
  runtime_eyeSelect <- get_time()
  print(
    stringr::str_glue(
      "--- 11. eyeSelect complete. (run duration: {pipeline_timing(runtime_detectBlinks, runtime_eyeSelect)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 10. eyeSelect complete. (run duration: {pipeline_timing(runtime_detectBlinks, runtime_eyeSelect)})"), savedata, runtime_Log)

  #Check that valid gaze data exists after selection
  dataExists <- checkGazeData(data, "gazeX.eyeSelect", ...)
  if (!dataExists){
    print(
      paste0("No valid gaze data exists after eye selection. Preprocessing for file",
             filepath,
             " has been aborted."
      )
    )
    stop()
  }

  runtime_checkGazeData2 <- get_time()
  print(
    stringr::str_glue(
      "--- 12. checkGazeData complete. (run duration: {pipeline_timing(runtime_eyeSelect, runtime_checkGazeData2)})"
    )
  )

  #Classify all data as onscreen or offscreen (exclusionary / within range)
  data <- classifyOffscreen(data, display.resx, display.resy, eyeSelection, ...)

  runtime_classifyOffscreen <- get_time()
  print(
    stringr::str_glue(
      "--- 13. classifyOffscreen complete. (run duration: {pipeline_timing(runtime_checkGazeData2, runtime_classifyOffscreen)})"
    )
  )
  #smoothing - denoise function
  input_cols <-
    colnames(data)[grepl("\\.eyeSelect$", colnames(data), ignore.case = TRUE)]   #get interpolated column names
  # noise_reduction_check <- ifelse(("noise_reduction" %in% names(args)) & noise_reduction == FALSE, FALSE, TRUE) #by default we denoise
  data <-
    denoise(data, recHz, input_cols, noise_reduction = noise_reduction, ...)
  runtime_denoise <- get_time()
  print(
    stringr::str_glue(
      "--- 13. denoise complete. (run duration: {pipeline_timing(runtime_classifyOffscreen, runtime_denoise)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 11. denoise complete. (run duration: {pipeline_timing(runtime_eyeSelect, runtime_denoise)})"), savedata, runtime_Log)

  #calculate visual angle
  if (noise_reduction) {
    data <-
      calcVA(
        data,
        display.resx,
        display.resy,
        display.dimx,
        display.dimy,
        gazeX = "gazeX.smooth",
        gazeY = "gazeY.smooth",
        distZ = "distanceZ.smooth",
        ...
      )
  }
  else if (!noise_reduction) {
    data <-
      calcVA(
        data,
        display.resx,
        display.resy,
        display.dimx,
        display.dimy,
        gazeX = "gazeX.eyeSelect",
        gazeY = "gazeY.eyeSelect",
        distZ = "distanceZ.eyeSelect",
        ...
      )
  }
  runtime_calcVA <- get_time()
  print(
    stringr::str_glue(
      "--- 14. calcVA complete. (run duration: {pipeline_timing(runtime_denoise, runtime_calcVA)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 12. calcVA complete. (run duration: {pipeline_timing(runtime_denoise, runtime_calcVA)})"), savedata, runtime_Log)

  #calculate velocity (using VA calculated gaze points)
  data <-
    calcVel(data,
            xcoord = "gazeX_va",
            ycoord = "gazeY_va",
            timestamp = "recordingTimestamp_ms",
            ...)
  runtime_calcVel <- get_time()
  print(
    stringr::str_glue(
      "--- 15. calcVel complete. (run duration: {pipeline_timing(runtime_calcVA, runtime_calcVel)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 13. calcVel complete. (run duration: {pipeline_timing(runtime_calcVA, runtime_calcVel)})"), savedata, runtime_Log)

  ##smooth velocity
  velCols <-
    colnames(data)[grepl("_va_ms", colnames(data), ignore.case = TRUE)]  #get velocity columns
  data <- smoothVel(data, recHz, velCols, ...)
  runtime_smoothVel <- get_time()
  print(
    stringr::str_glue(
      "--- 16. smoothVel complete. (run duration: {pipeline_timing(runtime_calcVel, runtime_smoothVel)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 14. smoothVel complete. (run duration: {pipeline_timing(runtime_calcVel, runtime_smoothVel)})"), savedata, runtime_Log)

  #filter -  classifierIVT from Liz, includes mergeAdj, removeShortfix
  IVT_list <-
    classifierIVT(
      data,
      input_velocity = "velocityEuclidean.smooth_va_ms",
      gazeX_va = "gazeX_va",
      gazeY_va = "gazeY_va",
      recHz = recHz,
      ...
    )
  #Summarized IVT file
  data <- IVT_list[[1]]
  data$IVT.classification[which(data$IVT.classification == "missing" &
                                  data$blink.classification == 1)] <- "blink"
  #All metrics IVT file
  data_IVT_all <- IVT_list[[2]]

  runtime_classifierIVT <- get_time()
  print(
    stringr::str_glue(
      "--- 17. classifierIVT complete. (run duration: {pipeline_timing(runtime_smoothVel, runtime_classifierIVT)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 15. classifierIVT complete. (run duration: {pipeline_timing(runtime_smoothVel, runtime_classifierIVT)})"), savedata, runtime_Log)

  #Assign final column labels
  data <- assignFinalCols(data, noise_reduction, ...)

  runtime_assignFinalCols <- get_time()
  print(
    stringr::str_glue(
      "--- 18. assignFinalCols complete. (run duration: {pipeline_timing(runtime_classifierIVT, runtime_assignFinalCols)})"
    )
  )

  #get final runtime
  runtime_final <- get_time()
  print(
    stringr::str_glue(
      "--- PREPROCESSING COMPLETE (preprocessing run duration: {pipeline_timing(runtime_start, runtime_final)})"
    )
  )
  # print_or_save(stringr::str_glue("--- PREPROCESSING COMPLETE (preprocessing run duration: {pipeline_timing(runtime_start, runtime_final)})"), savedata, runtime_Log)


  #data quality - robustness, contiguity, precision, %missingness, etc...
  summaryMetrics <- calculateOutputMetrics(data)
  runtime_calcOutputMetrics <- get_time()
  print(
    stringr::str_glue(
      "--- OUTPUT METRICS CALCULATED (total run duration: {pipeline_timing(runtime_start, runtime_calcOutputMetrics)})"
    )
  )
  # print_or_save(stringr::str_glue("--- OUTPUT METRICS CALCULATED (total run duration: {pipeline_timing(runtime_start, runtime_calcOutputMetrics)})"), savedata, runtime_Log)

  times <- list(
    start = runtime_start,
    loadData = runtime_loadData,
    detectSourceType = runtime_detectImportSourceType,
    formatCols = runtime_formatCols,
    removeEventRows = runtime_removeEventRows,
    setTimeRange = runtime_setTimeRange,
    calcHz = runtime_calcHz,
    rmInvalidGP = runtime_rmInvalidGP,
    # detectOffscreenGP = runtime_detectOffscreenGP,
    detectOffscreenGP = runtime_rmOffscreenGP,
    fillGaps = runtime_fillGaps,
    detectBlinks = runtime_detectBlinks,
    eyeSelect = runtime_eyeSelect,
    denoise = runtime_denoise,
    calcVA = runtime_calcVA,
    calcVel = runtime_calcVel,
    smoothVel = runtime_smoothVel,
    classifierIVT = runtime_classifierIVT,
    final = runtime_final,
    calcOutputMetrics = runtime_calcOutputMetrics,
    total = get_time()
  )

  durations <- list(
    loadData_duration = get_time_difference(runtime_start, runtime_loadData, units =
                                              "secs"),
    detectSourceType_duration = get_time_difference(runtime_loadData, runtime_detectImportSourceType, units =
                                                      "secs"),
    formatCols_duration = get_time_difference(runtime_detectImportSourceType, runtime_formatCols, units =
                                                "secs"),
    removeEventRows_duration = get_time_difference(runtime_formatCols, runtime_removeEventRows, units =
                                                     "secs"),
    setTimeRange_duration = get_time_difference(runtime_removeEventRows, runtime_setTimeRange, units =
                                                  "secs"),
    calcHz_duration = get_time_difference(runtime_setTimeRange, runtime_calcHz, units =
                                            "secs"),
    rmInvalidGP_duration = get_time_difference(runtime_calcHz, runtime_rmInvalidGP, units =
                                                 "secs"),
    # detectOffscreenGP_duration = get_time_difference(runtime_rmInvalidGP, runtime_detectOffscreenGP, units =
    #                                                    "secs"),
    detectOffscreenGP_duration = get_time_difference(runtime_rmInvalidGP, runtime_rmOffscreenGP, units =
                                                       "secs"),
    # fillGaps_duration = get_time_difference(runtime_detectOffscreenGP, runtime_fillGaps, units =
    #                                           "secs"),
    fillGaps_duration = get_time_difference(runtime_rmOffscreenGP, runtime_fillGaps, units =
                                              "secs"),
    detectBlinks_duration = get_time_difference(runtime_fillGaps, runtime_detectBlinks, units =
                                                  "secs"),
    eyeSelect_duration = get_time_difference(runtime_detectBlinks, runtime_eyeSelect, units =
                                               "secs"),
    denoise_duration = get_time_difference(runtime_eyeSelect, runtime_denoise, units =
                                             "secs"),
    calcVA_duration = get_time_difference(runtime_denoise, runtime_calcVA, units =
                                            "secs"),
    calcVel_duration = get_time_difference(runtime_calcVA, runtime_calcVel, units =
                                             "secs"),
    smoothVel_duration = get_time_difference(runtime_calcVel, runtime_smoothVel, units =
                                               "secs"),
    classifierIVT_duration = get_time_difference(runtime_smoothVel, runtime_classifierIVT, units =
                                                   "secs"),
    final_duration = get_time_difference(runtime_classifierIVT, runtime_final, units =
                                           "secs"),
    calcOutputMetrics_duration = get_time_difference(runtime_final, runtime_calcOutputMetrics, units =
                                                       "secs"),
    total_duration = get_time_difference(runtime_start, runtime_calcOutputMetrics, units =
                                           "secs")
  )

  timingData = do.call(c, list(times, durations))

#Clean up final dataset output

  #remove temporary eye selection calculation columns
  tempCols <- c(colnames(data)[grepl("\\.temp$", colnames(data), ignore.case = TRUE)],
                colnames(data)[grepl("\\.es.selection$", colnames(data), ignore.case = TRUE)])
  for (t in tempCols) {
    data[[t]] <- NULL
  }

  #remove columns created in preprocessing pipeline that are not the final outputs / categories
  if (!includeIntermediates){
    data <- removeIntermediateCols(data, ...)
  }

  if (savedata) {
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
