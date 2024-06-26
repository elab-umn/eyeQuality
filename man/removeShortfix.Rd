% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/removeShortfix.R
\name{removeShortfix}
\alias{removeShortfix}
\title{Removes proposed fixations that are too short}
\usage{
removeShortfix(
  data,
  entry.class,
  rle_fix_index,
  end.fix,
  start.fix,
  lengths.fix,
  short.time
)
}
\arguments{
\item{data}{dataframe}

\item{entry.class}{label for entry classification}

\item{rle_fix_index}{list of integers, generated from output of function findFixationIndices}

\item{end.fix}{list of fixation endpoints as indexes, generated from output of function findFixationIndices}

\item{start.fix}{list of fixation start points as indexes, generated from output of function findFixationIndices}

\item{lengths.fix}{integer list of fixation durations, generated from output of function findFixationIndices}

\item{short.time}{threshold for minimum duration of a given fixation to be considered acceptable. Recommended default: 60 ms}
}
\value{
return list of updated fixation metrics, including classification (class.adj), duration, and distance
}
\description{
Removes proposed fixations that are too short
}
