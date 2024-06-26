% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mergeAdj.R
\name{mergeAdj}
\alias{mergeAdj}
\title{Merge Adjacent Fixations}
\usage{
mergeAdj(
  data,
  entry.class,
  rle_fix_index,
  xcoords,
  ycoords,
  end.fix,
  start.fix,
  lengths.fix,
  merge.dist = 0.5,
  merge.time = 75
)
}
\arguments{
\item{data}{dataframe}

\item{entry.class}{list of IVT-assigned class assignments per gazepoint}

\item{rle_fix_index}{list of integers, generated from output of function findFixationIndices}

\item{xcoords}{list of x gaze coordinates as visual angles}

\item{ycoords}{list of y gaze coordinates as visual angles}

\item{end.fix}{list of fixation endpoints as indexes, generated from output of function findFixationIndices}

\item{start.fix}{list of fixation start points as indexes, generated from output of function findFixationIndices}

\item{lengths.fix}{list of fixation durations, generated from output of function findFixationIndices}

\item{merge.dist}{maximum distance (in VA) between two fixations to be merged. Default: 0/5 VA}

\item{merge.time}{maximum time lapsed (in ms) between two fixations to be merged. Default: 75 ms}
}
\value{
return list of updated fixation metrics, including classification (class.adj), duration, and distance
}
\description{
Merge Adjacent Fixations
}
