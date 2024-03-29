#' Merge Adjacent Fixations
#'
#' @param data dataframe
#' @param entry.class list of IVT-assigned class assignments per gazepoint
#' @param rle_fix_index list of integers, generated from output of function findFixationIndices
#' @param xcoords list of x gaze coordinates as visual angles
#' @param ycoords list of y gaze coordinates as visual angles
#' @param end.fix list of fixation endpoints as indexes, generated from output of function findFixationIndices
#' @param start.fix list of fixation start points as indexes, generated from output of function findFixationIndices
#' @param lengths.fix list of fixation durations, generated from output of function findFixationIndices
#' @param merge.dist maximum distance (in VA) between two fixations to be merged. Default: 0/5 VA
#' @param merge.time maximum time lapsed (in ms) between two fixations to be merged. Default: 75 ms
#'
#' @return return list of updated fixation metrics, including classification (class.adj), duration, and distance
#' @export
#'
# This function iterates through each proposed fixation in a time intensive manner (see Tobii white paper for merge fixation logic)
mergeAdj <-
  function(data,
           entry.class,
           rle_fix_index,
           xcoords,
           ycoords,
           end.fix,
           start.fix,
           lengths.fix,
           merge.dist = 0.5,
           merge.time = 75) {
    #list2env(findFixationIndices(data.raw.df$class), envir = globalenv())

    fix_x <-
      fix_y <-
      fix_euc_adj <- gap_dur_adj <- rep(NA, length(rle_fix_index))

    data$class.adj <- entry.class
    data$class.adj.euc <-
      data$class.adj.gap.dur <-
      data$class.adj.xva <-
      data$class.adj.yva <-
      data$class.adj.num <- data$class.adj.dur <- NA

    ifix <- 1
    fixation_set_complete <- FALSE
    while (fixation_set_complete == FALSE) {
      fixation_complete <- FALSE
      fixation_b_complete <- FALSE

      # primary forward fixation merge loop

      while (fixation_complete == FALSE) {
        if (ifix == 1 | ifix %% 100 == 0) {
          print(paste(
            "Fixation: ",
            ifix,
            "; Indices: ",
            start.fix[rle_fix_index][ifix],
            " to ",
            end.fix[rle_fix_index][ifix]
          ))
        }

        # starting point exists
        #if(length(grep("TRUE",!is.na(xcoords[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]])))>0){

        # initial proposed fixation identify median x,y coordinate
        fix_x[ifix] <-
          data$class.adj.xva[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
          median(xcoords[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]], na.rm =
                   TRUE)
        fix_y[ifix] <-
          data$class.adj.yva[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
          median(ycoords[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]], na.rm =
                   TRUE)

        # assess entire length of proposed new fixation
        dur <-
          length(start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix])

        # assess temporal distance between this fixation and prior fixation
        start.change.index <- end.fix[rle_fix_index][ifix] + 1
        end.change.index <- start.fix[rle_fix_index][ifix + 1] - 1

        if (!is.na(fix_x[ifix]) & !is.na(fix_y[ifix])) {
          # assuming there is an index for a later point and
          if (!is.na(end.change.index + 1)) {
            # next fixations identified
            fix_x[ifix + 1] <-
              median(xcoords[start.fix[rle_fix_index][ifix + 1]:end.fix[rle_fix_index][ifix + 1]], na.rm =
                       TRUE)
            fix_y[ifix + 1] <-
              median(ycoords[start.fix[rle_fix_index][ifix + 1]:end.fix[rle_fix_index][ifix + 1]], na.rm =
                       TRUE)

            # assess euclidean distance between this fixation and prior fixation
            fix_euc <-
              sqrt((fix_x[ifix] - fix_x[ifix + 1]) ^ 2 + (fix_y[ifix] - fix_y[ifix +
                                                                                1]) ^ 2)
            fix_euc_adj[ifix] <- fix_euc

            gap_dur <- length(end.change.index:start.change.index)
            gap_dur_adj[ifix] <- gap_dur

            if ((is.na(fix_euc))) {
              print("NA FOUND IN SECOND FIXATION")
            }

            # if two consecutive fixations are fairly close to eachother...
            if (!is.na(fix_euc) & fix_euc < merge.dist) {
              # ... and if not too much time has transpired, then merge
              if (gap_dur * 3.3 < merge.time) {
                data$class.adj[start.change.index:end.change.index] <- "fixation"
              }
              if (gap_dur * 3.3 >= merge.time) {
                data$class.adj.xva[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
                  fix_x[ifix]
                data$class.adj.yva[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
                  fix_y[ifix]
                data$class.adj.euc[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
                  fix_euc
                data$class.adj.gap.dur[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
                  gap_dur
                data$class.adj.dur[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
                  dur
                data$class.adj.num[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
                  ifix
                #print(data[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix + 1],c("class","class.adj","class.adj.euc","class.adj.gap.dur","class.adj.xva","class.adj.yva","class.adj.num")])
                #print(paste("Too Long: ",gap_dur*3.3, " ms duration ,", fix_euc, "va distance between fixations"))
                fixation_complete <- TRUE
              }

            }
            if (!is.na(fix_euc) &
                fix_euc >= merge.dist | (is.na(fix_euc))) {
              data$class.adj.xva[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
                fix_x[ifix]
              data$class.adj.yva[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
                fix_y[ifix]
              data$class.adj.euc[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
                fix_euc
              data$class.adj.gap.dur[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
                gap_dur
              data$class.adj.dur[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
                dur
              data$class.adj.num[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
                ifix
              fixation_complete <- TRUE
            }
          }
          if (is.na(end.change.index + 1)) {
            data$class.adj.xva[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
              fix_x[ifix]
            data$class.adj.yva[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
              fix_y[ifix]
            data$class.adj.dur[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
              dur
            data$class.adj.num[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
              ifix
            fixation_complete <- TRUE
          }

          # setup for another pass through the while loop or exit if done
          if (fixation_complete == FALSE) {
            rle_fix_index <- which(rle(data$class.adj)$values == "fixation")
            rle_sac_index <-
              which(rle(data$class.adj)$values == "saccade")

            end.fix = cumsum(rle(data$class.adj)$lengths)
            start.fix = c(1, lag(end.fix)[-1] + 1)
            lengths.fix <- end.fix - start.fix + 1

            # check if ran out of potential fixations to merge, if not back to top of while loop
            if (ifix >= length(rle_fix_index)) {
              data$class.adj.xva[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
                fix_x[ifix]
              data$class.adj.yva[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
                fix_y[ifix]
              data$class.adj.euc[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
                fix_euc
              data$class.adj.gap.dur[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
                gap_dur
              data$class.adj.dur[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
                dur
              data$class.adj.num[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
                ifix

              fix_x[ifix] <-
                data$class.adj.xva[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
                median(xcoords[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]], na.rm =
                         TRUE)
              fix_y[ifix] <-
                data$class.adj.yva[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
                median(ycoords[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]], na.rm =
                         TRUE)

              fixation_set_complete <- TRUE
              print("Done")
              break
            }
          }
        }
        if (is.na(fix_x[ifix]) | is.na(fix_y[ifix])) {
          print("NA FOUND IN FIRST FIXATION")
          data$class.adj[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
            "unclassified"

          rle_fix_index <-
            which(rle(data$class.adj)$values == "fixation")
          rle_sac_index <-
            which(rle(data$class.adj)$values == "saccade")

          end.fix = cumsum(rle(data$class.adj)$lengths)
          start.fix = c(1, lag(end.fix)[-1] + 1)
          lengths.fix <- end.fix - start.fix + 1

          fixation_complete <- TRUE
          fixation_b_complete <- TRUE

          ifix <- ifix - 1

        }

      }

      # primary backward fixation merge loop

      if (ifix >= 2) {
        while (fixation_b_complete == FALSE) {
          fix_xb <-
            median(xcoords[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]], na.rm =
                     TRUE)
          fix_yb <-
            median(ycoords[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]], na.rm =
                     TRUE)

          # prev fixations identified
          fix_xa <-
            median(xcoords[start.fix[rle_fix_index][ifix - 1]:end.fix[rle_fix_index][ifix - 1]], na.rm =
                     TRUE)
          fix_ya <-
            median(ycoords[start.fix[rle_fix_index][ifix - 1]:end.fix[rle_fix_index][ifix - 1]], na.rm =
                     TRUE)

          # assess length of proposed fixation
          dur <-
            length(start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix])

          # assess euclidean distance between this fixation and prior fixation
          fix_euc <- sqrt((fix_xb - fix_xa) ^ 2 + (fix_yb - fix_ya) ^ 2)

          # assess temporal distance between this fixation and prior fixation
          start.change.index <- end.fix[rle_fix_index][ifix - 1] + 1
          end.change.index <- start.fix[rle_fix_index][ifix] - 1

          gap_dur <- length(end.change.index:start.change.index)

          # if two consecutive fixations are fairly close to eachother...
          if (!is.na(fix_euc) &
              !is.na(merge.dist) & fix_euc < merge.dist) {
            # ... and if not too much time has transpired, then merge
            if (gap_dur * 3.3 < merge.time) {
              data$class.adj[start.change.index:end.change.index] <- "fixation"
              ifix <- ifix - 1
            }
            if (gap_dur * 3.3 >= merge.time) {
              fixation_b_complete <- TRUE
            }
          }
          if ((!is.na(fix_euc) &
               !is.na(merge.dist)) &
              fix_euc >= merge.dist | (is.na(fix_euc) | is.na(merge.dist))) {
            fixation_b_complete <- TRUE
          }

          # setup for another pass through the while loop or exit if done
          if (fixation_b_complete == FALSE) {
            rle_fix_index <- which(rle(data$class.adj)$values == "fixation")
            rle_sac_index <-
              which(rle(data$class.adj)$values == "saccade")

            end.fix = cumsum(rle(data$class.adj)$lengths)
            start.fix = c(1, lag(end.fix)[-1] + 1)
            lengths.fix <- end.fix - start.fix + 1

            # check if ran out of potential fixations to merge, if not back to top of while loop
            if (ifix == 1) {
              ifix <- ifix - 1
              fixation_b_complete <- TRUE

            }

          }

          if (ifix > 1) {
            data$class.adj.xva[start.fix[rle_fix_index][ifix - 1]:end.fix[rle_fix_index][ifix -
                                                                                           1]] <- fix_xa
            data$class.adj.yva[start.fix[rle_fix_index][ifix - 1]:end.fix[rle_fix_index][ifix -
                                                                                           1]] <- fix_ya
            data$class.adj.xva[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
              fix_xb
            data$class.adj.yva[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
              fix_yb
            data$class.adj.euc[start.fix[rle_fix_index][ifix - 1]:end.fix[rle_fix_index][ifix -
                                                                                           1]] <- fix_euc
            data$class.adj.gap.dur[start.fix[rle_fix_index][ifix - 1]:end.fix[rle_fix_index][ifix -
                                                                                               1]] <- gap_dur
            data$class.adj.dur[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
              dur
            data$class.adj.num[start.fix[rle_fix_index][ifix - 1]:end.fix[rle_fix_index][ifix -
                                                                                           1]] <- ifix - 1
            data$class.adj.num[start.fix[rle_fix_index][ifix]:end.fix[rle_fix_index][ifix]] <-
              ifix
          }
        }
      }

      ifix <- ifix + 1

      # check if done with fixations
      if (ifix > length(rle_fix_index)) {
        fixation_set_complete <- TRUE
        print("Done merging fixations")
      }

    }

    return(data[, c(
      "class.adj",
      "class.adj.euc",
      "class.adj.gap.dur",
      "class.adj.xva",
      "class.adj.yva",
      "class.adj.num",
      "class.adj.dur"
    )])
    #return(data$class.adj)
  }
