#' list_bids_derivative_files
#'
#' @description
#' `list_bids_derivative_files()` gets list of all ET like files in  derivatives.
#'
#' @param directory path for data file as .tsv
#' @param subject_pattern regex match pattern for subjects
#' @param session_pattern regex match pattern for sessions
#' @param derivatives_pattern regex match pattern for derivatives
#' @param pipeline_pattern regex match pattern for pipeline
#' @param modality_pattern regex match pattern for specific modality
#'
#' @return list of ET derivatives data files to in BIDS-like directory
#' @export
#'
list_bids_derivative_files <- function(directory,
                                       subject_pattern = "sub-[A-Z0-9]+",
                                       session_pattern = "ses-[0-9]+",
                                       derivatives_pattern = "derivatives",
                                       pipeline_pattern = "eyeQuality-v1",
                                       modality_pattern = NULL) {
  subject_dirs <-
    list.dirs(directory, full.names = TRUE, recursive = FALSE)

  files <- list()

  for (subject_dir in subject_dirs) {
    if (grepl(subject_pattern, subject_dir)) {
      session_dirs <-
        list.dirs(subject_dir,
                  full.names = TRUE,
                  recursive = FALSE)
      for (session_dir in session_dirs) {
        if (!is.null(session_pattern) &&
            !grepl(session_pattern, session_dir)) {
          next
        }
        derivs_dir <-
          paste0(session_dir,
                 "/",
                 derivatives_pattern,
                 "/",
                 pipeline_pattern,
                 "/")
        if (is.null(modality_pattern)) {
          tsv_files <-
            list.files(derivs_dir,
                       pattern = "\\.tsv$",
                       full.names = TRUE)
          files <- c(files, tsv_files)
        } else {
          modality_files <-
            list.files(derivs_dir,
                       pattern = modality_pattern,
                       full.names = TRUE)
          files <- c(files, modality_files)
        }

      }
    }
  }

  return(unlist(files))
}
