#' parsePreprocessingBatchSummary
#'
#' parse preprocessing_batch_summary files
#'
#' @param batch_summary_file filepath to preprocessing_batch_summary.tsv file
#' @param info_to_extract string with value of "summary" | "failedfiles" | "successfulfiles"
#'
#' @return data dataframe exported data from preprocessing_batch_summary
#' @export
#'
#' @importFrom readr read_tsv
#' @importFrom stringr str_glue
#' @importFrom stringr str_extract
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
#'
parsePreprocessingBatchSummary <-
  function(batch_summary_file, info_to_extract = "summary") {
    if (info_to_extract == "summary") {
      read_tsv(
        str_glue(batch_summary_file),
        col_names = FALSE,
        show_col_types = FALSE
      ) %>%
        tail(n = 1) %>%
        mutate(
          directory = str_extract(.data[[1]], "directory: ([\\S]+),", group = 1),
          datasize = as.numeric(
            str_extract(.data[[1]], "data size \\(MB\\): ([\\S]+),", group = 1)
          ),
          nfiles = as.numeric(str_extract(
            .data[[1]], "n \\(ET Files\\): ([\\S]+),", group = 1
          )),
          nPreprocessed = as.numeric(
            str_extract(.data[[1]], "n \\(preprocessed\\): ([\\S]+),", group = 1)
          ),
          nFailed = as.numeric(
            str_extract(.data[[1]], "n \\(failed preprocessing\\): ([\\S]+),", group = 1)
          ),
          runDuration = str_extract(.data[[1]], "run duration: ([\\S\\s]+),", group = 1),
          runTime = as.numeric(str_extract(
            .data[[1]], "runtime \\(s\\): ([\\S]+)$", group = 1
          )),
        ) %>%
        select(directory,
               datasize,
               nfiles,
               nPreprocessed,
               nFailed,
               runDuration,
               runTime) %>%
        return()
    } else if (info_to_extract == "failedfiles") {

    }
  }
