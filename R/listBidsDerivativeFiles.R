#' listBidsDerivativeFiles
#'
#' @description
#' `listBidsDerivativeFiles()` gets list of all ET like files in  derivatives.
#'
#' @param directory path for data file as .tsv
#' @param subjectPattern_regex regex match pattern for subjects
#' @param sessionPattern_regex regex match pattern for sessions
#' @param derivativesPattern_regex regex match pattern for derivatives
#' @param pipeline_pattern regex match pattern for pipeline
#' @param modalityPattern_regex regex match pattern for specific modality
#'
#' @return list of ET derivatives data files to in BIDS-like directory
#' @export
#'
listBidsDerivativeFiles <- function(directory,
                                       subjectPattern_regex = "sub-[A-Z0-9]+",
                                       session_pattern = "ses-[0-9]+",
                                       derivativesPattern_regex = "derivatives",
                                       pipeline_pattern = "eyeQuality-v1",
                                       modalityPattern_regex = NULL) {
  subject_dirs <-
    list.dirs(directory, full.names = TRUE, recursive = FALSE)

  files <- list()

  for (subject_dir in subject_dirs) {
    if (grepl(subjectPattern_regex, subject_dir)) {
      session_dirs <-
        list.dirs(subject_dir,
                  full.names = TRUE,
                  recursive = FALSE)
      for (session_dir in session_dirs) {
        if (!is.null(sessionPattern_regex) &&
            !grepl(sessionPattern_regex, session_dir)) {
          next
        }
        derivs_dir <-
          paste0(session_dir,
                 "/",
                 derivativesPattern_regex,
                 "/",
                 pipeline_pattern,
                 "/")
        if (is.null(modalityPattern_regex)) {
          tsv_files <-
            list.files(derivs_dir,
                       pattern = "\\.tsv$",
                       full.names = TRUE)
          files <- c(files, tsv_files)
        } else {
          modality_files <-
            list.files(derivs_dir,
                       pattern = modalityPattern_regex,
                       full.names = TRUE)
          files <- c(files, modality_files)
        }

      }
    }
  }

  return(unlist(files))
}
