#' Removes proposed fixations that are too short
#'
#' @param data dataframe
#' @param entry.class label for entry classification
#' @param rle_fix_index list of integers, generated from output of function findFixationIndices
#' @param end.fix list of fixation endpoints as indexes, generated from output of function findFixationIndices
#' @param start.fix list of fixation start points as indexes, generated from output of function findFixationIndices
#' @param lengths.fix integer list of fixation durations, generated from output of function findFixationIndices
#' @param short.time threshold for minimum duration of a given fixation to be considered acceptable. Recommended default: 60 ms
#'
#' @return return list of updated fixation metrics, including classification (class.adj), duration, and distance
#' @export
#'
removeShortfix <-
  function(data,
           entry.class,
           rle_fix_index,
           end.fix,
           start.fix,
           lengths.fix,
           short.time) {
    print("Removing short fixations")
    fix_x <-
      fix_y <-
      fix_euc_adj <- gap_dur_adj <- rep(NA, length(rle_fix_index))
    data$class.adj.shortfix <- entry.class

    for (ifix in 1:length(rle_fix_index)) {
      if (lengths.fix[rle_fix_index][ifix] * 3.3 < short.time) {
        end.change.index <- end.fix[rle_fix_index][ifix]
        start.change.index <- start.fix[rle_fix_index][ifix]
        data$class.adj.shortfix[start.change.index:end.change.index] <-
          "unclassified"
        data$class.adj.euc[start.change.index:end.change.index] <- NA
        data$class.adj.gap.dur[start.change.index:end.change.index] <-
          NA
        data$class.adj.xva[start.change.index:end.change.index] <- NA
        data$class.adj.yva[start.change.index:end.change.index] <- NA
        data$class.adj.num[start.change.index:end.change.index] <- NA
        data$class.adj.dur[start.change.index:end.change.index] <- NA
      }
    }

    return(data[, c(
      "class.adj.shortfix",
      "class.adj.euc",
      "class.adj.gap.dur",
      "class.adj.xva",
      "class.adj.yva",
      "class.adj.num",
      "class.adj.dur"
    )])

  }
