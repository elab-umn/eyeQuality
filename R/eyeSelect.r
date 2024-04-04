#' Eye selection
#'
#' @param data dataframe
#' @param eyeSelection "Maximize", "Strict", "Left", or "Right"
#' @param gpLeft.X string column name for data containing left eye X-axis gazepoints
#' @param gpLeft.Y string column name for data containing left eye Y-axis gazepoints
#' @param gpRight.X string column name for data containing right eye X-axis gazepoints
#' @param gpRight.Y string column name for data containing right eye Y-axis gazepoints
#' @param pupilLeft string column name for data containing left eye pupil data
#' @param pupilRight string column name for data containing right eye pupil data
#' @param distLeft.Z string column name for data containing left eye distance from screen in mm (Z distance)
#' @param distRight.Z string column name for data containing right eye distance from screen in mm  (Z distance)
#' @param ... additional passed parameters from parent function
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr rowwise
#' @importFrom rlang sym
#'
#' @return data
#' @export
#'
eyeSelect <-
  function(data,
           eyeSelection = "Maximize",
           gpLeft.X = "gazeLeftX.int",
           gpLeft.Y = "gazeLeftY.int",
           gpRight.X = "gazeRightX.int",
           gpRight.Y = "gazeRightY.int",
           pupilLeft = "pupilLeft.int",
           pupilRight = "pupilRight.int",
           distLeft.Z = "distanceLeftZ.int",
           distRight.Z = "distanceRightZ.int",
           ...) {

    # hacky way to get rid of "no visible binding for global variable" note
    distanceZ.eyeSelect <- gazeX.eyeSelect <- gazeY.eyeSelect <- pupil.eyeSelect <- NULL

    #Maximize option - replace NAs if data exists for one eye and not the other
    if (eyeSelection == "Maximize") {
      print("selecting eye based on maximized approach")
      #When one eye/pupil is == NA replace with the other eye/pupil
      #Gaze
      data[["gpLeft.X.temp"]] <-
        ifelse(is.na(data[[gpLeft.X]]), data[[gpRight.X]], data[[gpLeft.X]])
      data[["gpLeft.Y.temp"]] <-
        ifelse(is.na(data[[gpLeft.Y]]), data[[gpRight.Y]], data[[gpLeft.Y]])
      data[["gpRight.X.temp"]] <-
        ifelse(is.na(data[[gpRight.X]]), data[[gpLeft.X]], data[[gpRight.X]])
      data[["gpRight.Y.temp"]] <-
        ifelse(is.na(data[[gpRight.Y]]), data[[gpLeft.Y]], data[[gpRight.Y]])
      #Pupils
      data[["pupilLeft.temp"]] <-
        ifelse(is.na(data[[pupilLeft]]), data[[pupilRight]], data[[pupilLeft]])
      data[["pupilRight.temp"]] <-
        ifelse(is.na(data[[pupilRight]]), data[[pupilLeft]], data[[pupilRight]])
      #Dist
      data[["distLeft.Z.temp"]] <-
        ifelse(is.na(data[[distLeft.Z]]), data[[distRight.Z]], data[[distLeft.Z]])
      data[["distRight.Z.temp"]] <-
        ifelse(is.na(data[[distRight.Z]]), data[[distLeft.Z]], data[[distRight.Z]])

      #Set eye selection parameter
      #Gaze
      data[["gazeX.es.selection"]] <- ifelse(
        !is.na(data[[gpLeft.X]]) &
          !is.na(data[[gpRight.X]]),
        "mean_maximized",
        ifelse(
          !is.na(data[[gpLeft.X]]) & is.na(data[[gpRight.X]]),
          "left_only",
          ifelse(
            is.na(data[[gpLeft.X]]) & !is.na(data[[gpRight.X]]),
            "right_only",
            ifelse(is.na(data[[gpLeft.X]]) &
                     is.na(data[[gpRight.X]]), "both_na",
                   "other_maximized")
          )
        )
      )
      data[["gazeY.es.selection"]] <- ifelse(
        !is.na(data[[gpLeft.Y]]) &
          !is.na(data[[gpRight.Y]]),
        "mean_maximized",
        ifelse(
          !is.na(data[[gpLeft.Y]]) & is.na(data[[gpRight.Y]]),
          "left_only",
          ifelse(
            is.na(data[[gpLeft.Y]]) & !is.na(data[[gpRight.Y]]),
            "right_only",
            ifelse(is.na(data[[gpLeft.Y]]) &
                     is.na(data[[gpRight.Y]]), "both_na",
                   "other_maximized")
          )
        )
      )
      #Pupils
      data[["pupil.es.selection"]] <- ifelse(
        !is.na(data[[pupilLeft]]) &
          !is.na(data[[pupilRight]]),
        "mean_maximized",
        ifelse(
          !is.na(data[[pupilLeft]]) & is.na(data[[pupilRight]]),
          "left_only",
          ifelse(
            is.na(data[[pupilLeft]]) & !is.na(data[[pupilRight]]),
            "right_only",
            ifelse(is.na(data[[pupilLeft]]) &
                     is.na(data[[pupilRight]]), "both_na",
                   "other_maximized")
          )
        )
      )
      #Dist
      data[["distZ.es.selection"]] <- ifelse(
        !is.na(data[[distLeft.Z]]) &
          !is.na(data[[distRight.Z]]),
        "mean_maximized",
        ifelse(
          !is.na(data[[distLeft.Z]]) & is.na(data[[distRight.Z]]),
          "left_only",
          ifelse(
            is.na(data[[distLeft.Z]]) & !is.na(data[[distRight.Z]]),
            "right_only",
            ifelse(is.na(data[[distLeft.Z]]) &
                     is.na(data[[distRight.Z]]), "both_na",
                   "other_maximized")
          )
        )
      )

      # print(names(data))

      #Calculate maximized averages
      print(
        "calculating maximized averages for gazeX.eyeSelect, gazeY.eyeSelect, pupil.eyeSelect, and distanceZ.eyeSelect"
      )
      data <- data %>%
        dplyr::rowwise() %>%
        mutate(
          gazeX.eyeSelect := mean(c(
            .data$gpLeft.X.temp, .data$gpRight.X.temp
          ), na.rm = TRUE),
          gazeY.eyeSelect := mean(c(
            .data$gpLeft.Y.temp, .data$gpRight.Y.temp
          ), na.rm = TRUE),
          pupil.eyeSelect := mean(c(
            .data$pupilLeft.temp, .data$pupilRight.temp
          ), na.rm = TRUE),
          distanceZ.eyeSelect := mean(c(
            .data$distLeft.Z.temp, .data$distRight.Z.temp
          ), na.rm = TRUE)
        )
    }

    # Both Strict Option
    else if (eyeSelection == "Strict") {
      #Set eye selection parameter
      #Gaze
      data[["gazeX.es.selection"]] <- ifelse(
        !is.na(data[[gpLeft.X]]) & !is.na(data[[gpRight.X]]),
        "mean_strict",
        ifelse(
          !is.na(data[[gpLeft.X]]) & is.na(data[[gpRight.X]]),
          "right_na",
          ifelse(
            is.na(data[[gpLeft.X]]) & !is.na(data[[gpRight.X]]),
            "left_na",
            ifelse(is.na(data[[gpLeft.X]]) &
                     is.na(data[[gpRight.X]]), "both_na",
                   "other_strict")
          )
        )
      )
      data[["gazeY.es.selection"]] <- ifelse(
        !is.na(data[[gpLeft.Y]]) & !is.na(data[[gpRight.Y]]),
        "mean_strict",
        ifelse(
          !is.na(data[[gpLeft.Y]]) & is.na(data[[gpRight.Y]]),
          "right_na",
          ifelse(
            is.na(data[[gpLeft.Y]]) & !is.na(data[[gpRight.Y]]),
            "left_na",
            ifelse(is.na(data[[gpLeft.Y]]) &
                     is.na(data[[gpRight.Y]]), "both_na",
                   "other_strict")
          )
        )
      )
      #Pupils
      data[["pupil.es.selection"]] <- ifelse(
        !is.na(data[[pupilLeft]]) &
          !is.na(data[[pupilRight]]),
        "mean_strict",
        ifelse(
          !is.na(data[[pupilLeft]]) & is.na(data[[pupilRight]]),
          "right_na",
          ifelse(
            is.na(data[[pupilLeft]]) & !is.na(data[[pupilRight]]),
            "left_na",
            ifelse(is.na(data[[pupilLeft]]) &
                     is.na(data[[pupilRight]]), "both_na",
                   "other_strict")
          )
        )
      )
      #Dist
      data[["distZ.es.selection"]] <- ifelse(
        !is.na(data[[distLeft.Z]]) & !is.na(data[[distRight.Z]]),
        "mean_strict",
        ifelse(
          !is.na(data[[distLeft.Z]]) & is.na(data[[distRight.Z]]),
          "right_na",
          ifelse(
            is.na(data[[distLeft.Z]]) & !is.na(data[[distRight.Z]]),
            "left_na",
            ifelse(is.na(data[[distLeft.Z]]) &
                     is.na(data[[distRight.Z]]), "both_na",
                   "other_strict")
          )
        )
      )


      data <- data %>%
        dplyr::rowwise() %>%
        mutate(
          gazeX.eyeSelect := mean(c(
            !!rlang::sym(gpLeft.X),!!rlang::sym(gpRight.X)
          ), na.rm = FALSE),
          gazeY.eyeSelect := mean(c(
            !!rlang::sym(gpLeft.Y),!!rlang::sym(gpRight.Y)
          ), na.rm = FALSE),
          # gazeX.eyeSelect := ifelse(!! rlang::sym(gpLeft.X) == !! rlang::sym(p))
          pupil.eyeSelect := mean(c(
            !!rlang::sym(pupilLeft),!!rlang::sym(pupilRight)
          ), na.rm = FALSE),
          distanceZ.eyeSelect := mean(c(
            !!rlang::sym(distLeft.Z),!!rlang::sym(distRight.Z)
          ), na.rm = FALSE)
        )
    }

    # Single Eye Select Options
    else if (eyeSelection == "Left") {
      data[["gazeX.es.selection"]] <-
        case_when(!is.na(data[[gpLeft.X]]) ~ "left_only",
                  is.na(data[[gpLeft.X]]) ~ "left_na",
                  TRUE ~ "other")
      data[["gazeY.es.selection"]] <-
        case_when(!is.na(data[[gpLeft.Y]]) ~ "left_only",
                  # is.na(data[[gpLeft.Y]]) ~ "left_na",
                  TRUE ~ "other")
      data[["pupil.es.selection"]] <-
        case_when(!is.na(data[[pupilLeft]]) ~ "left_only",
                  is.na(data[[pupilLeft]]) ~ "left_na",
                  TRUE ~ "other")
      data[["distZ.es.selection"]] <-
        case_when(!is.na(data[[distLeft.Z]]) ~ "left_only",
                  is.na(data[[distLeft.Z]]) ~ "left_na",
                  TRUE ~ "other")

      data <- data %>%
        dplyr::rowwise() %>%
        mutate(
          .data$gazeX.eyeSelect := (!!rlang::sym(gpLeft.X)),
          .data$gazeY.eyeSelect := (!!rlang::sym(gpLeft.Y)),
          .data$pupil.eyeSelect := (!!rlang::sym(pupilLeft)),
          .data$distanceZ.eyeSelect := (!!rlang::sym(distLeft.Z))
        )
    }
    else if (eyeSelection == "Right") {
      data[["gazeX.es.selection"]] <-
        case_when(!is.na(data[[gpRight.X]]) ~ "right_only",
                  is.na(data[[gpRight.X]]) ~ "right_na",
                  TRUE ~ "other")
      data[["gazeY.es.selection"]] <-
        case_when(!is.na(data[[gpRight.Y]]) ~ "right_only",
                  is.na(data[[gpRight.Y]]) ~ "right_na",
                  TRUE ~ "other")
      data[["pupil.es.selection"]] <-
        case_when(!is.na(data[[pupilRight]]) ~ "right_only",
                  is.na(data[[pupilRight]]) ~ "right_na",
                  TRUE ~ "other")
      data[["distZ.es.selection"]] <-
        case_when(!is.na(data[[distRight.Z]]) ~ "right_only",
                  is.na(data[[distRight.Z]]) ~ "right_na",
                  TRUE ~ "other")

      data <- data %>%
        rowwise() %>%
        mutate(
          .data$gazeX.eyeSelect := (!!rlang::sym(gpRight.X)),
          .data$gazeY.eyeSelect := (!!rlang::sym(gpRight.Y)),
          .data$pupil.eyeSelect := (!!rlang::sym(pupilRight)),
          .data$distanceZ.eyeSelect := (!!rlang::sym(distRight.Z))
        )
    }

    return(data)
  }
