#' batch_preprocess is used to run the preprocessing function for a set of files in a BIDS like directory.
#' This will process files in parallel, and compute total runtime to process.
#'
#' @param directoryBIDS filepath to the BIDS directory of ET data to process
#' @param batch_name string label for what to call the batch. This will be added to the output files
#' @param num_cores optional parameter to specify number of cores to use. If not specified, function will use 80\\% of available cores
#' @param ... additional parameters
#'
#' @importFrom readr read_delim
#' @importFrom stringr str_glue
#' @import parallel
#'
#' @return data
#' @export
#'
batch_preprocess <-
  function(directoryBIDS,
           batch_name,
           num_cores = NULL,
           ...) {
    # options(error=traceback)

    batch_run_summary <-
      paste0(directoryBIDS,
             paste0("/preprocessing_batch_summary_desc-", batch_name, ".txt"))
    # batch_run_out <- paste0(directoryBIDS, paste0("/preprocessing_batch_output_desc-", batch_name, ".txt"))


    #save outputs to file at root of directoryBIDS
    # sinkToOutputFile(batch_run_summary)

    starttime <- get_time()

    tsv_files_to_batch_process <- list_bids_files(directoryBIDS)

    print_or_save(
      stringr::str_glue(
        "-----------------"
      ),
      TRUE,
      batch_run_summary
    )

    # print(paste0("starting batch run: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    print_or_save(paste0("starting batch run: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
                  TRUE,
                  batch_run_summary)

    # print(tsv_files_to_batch_process)
    print_or_save(paste(tsv_files_to_batch_process, collapse = "\n"),
                  TRUE,
                  batch_run_summary)

    # Create a cluster with multiple cores for parallel processing
    if (is.null(num_cores)) {
      # if not specified use 80% of available cores
      numcores <-
        ifelse(floor(detectCores() * 0.8) == 0, 1, floor(detectCores() * 0.8))
    } else {
      # otherwise use specified number of cores
      numcores <- ifelse(num_cores <= 0, 1, num_cores)
    }
    # print(stringr::str_glue("number of cores = {numcores}"))
    print_or_save(stringr::str_glue("number of cores = {numcores}"),
                  TRUE,
                  batch_run_summary)
    cl <- parallel::makeCluster(numcores, outfile = "")

    # Parallelize the processing of TSV files
    parallel::clusterExport(cl, "run_preprocess")  # Export the run_preprocess function to the cluster
    parallel::parLapply(
      cl,
      tsv_files_to_batch_process,
      fun = function(x) {
        # sink(file = get_file_run_log_name(x, batch_name), append = FALSE)
        tryCatch(
          run_preprocess(
            x,
            display.dimx = 594,
            display.dimy = 344,
            savedata = TRUE,
            batchName = batch_name,
            ...
          ),
          error = function(e)
            print(e)
          # error = function(e) {print(e), print(traceback())}
        )
        # sink()
      }
      # file = paste0(x, "/derivatives/eyeQuality-v1/", runtime_Log)
    )

    # Stop the cluster
    parallel::stopCluster(cl)

    endtime <- get_time()


    # print(stringr::str_glue("--- BATCH PREPROCESSING COMPLETE! (run duration: {pipeline_timing(starttime, endtime)})"))
    print_or_save(
      stringr::str_glue(
        "--- BATCH PREPROCESSING COMPLETE! (run duration: {pipeline_timing(starttime, endtime)})"
      ),
      TRUE,
      batch_run_summary
    )

    qcsummaryPattern <-
      paste0("_desc-",
             ifelse(is.null(batch_name), NULL, paste0(batch_name, "_")),
             "preproc_qcsummary\\.tsv$")

    completedfiles <-
      list.files(
        directoryBIDS,
        pattern = qcsummaryPattern,
        recursive = TRUE,
        full.names = TRUE
      )
    # completedfiles <- list_bids_derivative_files(directoryBIDS, modality_pattern = qcsummaryPattern)
    # print(completedfiles)
    # print_or_save(completedfiles, TRUE, batch_run_summary)
    print_or_save(
      stringr::str_glue(
        "------ Successfully processed files (n = {length(completedfiles)}):  "
      ),
      TRUE,
      batch_run_summary
    )
    print_or_save(paste(completedfiles, collapse = "\n"),
                  TRUE,
                  batch_run_summary)

    #compare list to batch process with the completed file list.
    failedfiles <- tsv_files_to_batch_process[!grepl(gsub("\\|$", "", paste0(
      fs::path_file(
        gsub(qcsummaryPattern, "", completedfiles)
      ), sep = "|", collapse = ""
    )),
    tsv_files_to_batch_process)]


    print_or_save(
      stringr::str_glue(
        "------ Files that failed processing (n = {length(failedfiles)}):  "
      ),
      TRUE,
      batch_run_summary
    )
    print_or_save(paste(failedfiles, collapse = "\n"),
                  TRUE,
                  batch_run_summary)

    print_or_save(
      stringr::str_glue(
        "--- BATCH PROCESSING SUMMARY:  "
      ),
      TRUE,
      batch_run_summary
    )
    # print(stringr::str_glue('"directory": "{directoryBIDS}", "data size (MB)": "{get_filesizes(tsv_files_to_batch_process)}", "n (ET Files)": "{length(tsv_files_to_batch_process)}", "n (preprocessed)": "{length(completedfiles)}", "n (failed preprocessing)": "{length(tsv_files_to_batch_process)-length(completedfiles)}", "run duration": "{pipeline_timing(starttime, endtime)}",  "runtime (s)": "{get_time_difference(starttime, endtime)}"'))
    print_or_save(
      stringr::str_glue(
        '"directory": "{directoryBIDS}", "data size (MB)": "{get_filesizes(tsv_files_to_batch_process)}", "n (ET Files)": "{length(tsv_files_to_batch_process)}", "n (preprocessed)": "{length(completedfiles)}", "n (failed preprocessing)": "{length(tsv_files_to_batch_process)-length(completedfiles)}", "run duration": "{pipeline_timing(starttime, endtime)}",  "runtime (s)": "{get_time_difference(starttime, endtime)}"'
      ),
      TRUE,
      batch_run_summary
    )



    # sinkReset()
    # sink()

  }
