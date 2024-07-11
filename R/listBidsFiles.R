#' listBidsFiles - get list of all ET like files.
#'
#' @param directory path for data file as .tsv
#' @param subjectPattern_regex regex match pattern for subjects
#' @param sessionPattern_regex regex match pattern for sessions
#' @param modalityPattern_regex regex match pattern for specific modality
#' @param ... additional parameters that may get passed from wrapper functions
#'
#' @return list of raw ET data files to in BIDS-like directory
#' @export
#'
listBidsFiles <-
  function(directory,
           subjectPattern_regex = "sub-[A-Z0-9]+",
           sessionPattern_regex = "ses-[0-9]+",
           modalityPattern_regex = NULL,
           ...) {
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
          if (is.null(modalityPattern_regex)) {
            tsv_files <-
              list.files(session_dir,
                         pattern = "\\.tsv$",
                         full.names = TRUE)
            files <- c(files, tsv_files)
          } else {
            modality_files <-
              list.files(session_dir,
                         pattern = modalityPattern_regex,
                         full.names = TRUE)
            files <- c(files, modality_files)
          }
        }
      }
    }

    return(unlist(files))
  }
