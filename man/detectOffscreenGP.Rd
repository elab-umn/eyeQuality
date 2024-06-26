% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detectOffscreenGP.R
\name{detectOffscreenGP}
\alias{detectOffscreenGP}
\title{Detect Offscreen Gazepoints}
\usage{
detectOffscreenGP(
  data,
  gazeX,
  gazeY,
  display.resx,
  display.resy,
  display.resx.min = 0,
  display.resy.min = 0
)
}
\arguments{
\item{data}{your dataframe}

\item{gazeX}{gaze point X coordinate}

\item{gazeY}{gaze point Y coordinate}

\item{display.resx}{Integer, Display resolution X in pixels}

\item{display.resy}{Integer Display resolution Y in pixels}

\item{display.resx.min}{Integer X minimum value within acceptable display resolution range Default 0 assumed for pixel space}

\item{display.resy.min}{Integer Y minimum value within acceptable display resolution range. Default 0 assumed for pixel space}
}
\value{
binary list to be saved to your dataframe as new column "offscreen.gp"
}
\description{
Detect Offscreen Gazepoints
}
