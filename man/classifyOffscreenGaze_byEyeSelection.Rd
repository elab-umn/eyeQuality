% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classifyOffscreenGaze_byEyeSelection.R
\name{classifyOffscreenGaze_byEyeSelection}
\alias{classifyOffscreenGaze_byEyeSelection}
\title{Categorize offScreen gazepoints based on eye selection method}
\usage{
classifyOffscreenGaze_byEyeSelection(
  eyeSelection_method,
  gazeLeft.offscreen,
  gazeRight.offscreen,
  offscreen.eyeSelect
)
}
\arguments{
\item{eyeSelection_method}{"Maximize", "Strict", "Left", "Right"}

\item{gazeLeft.offscreen}{list of ordered gaze point offscreen assignments for left eye}

\item{gazeRight.offscreen}{list of ordered gaze point offscreen assignments for right eye}

\item{offscreen.eyeSelect}{list of ordered gaze point offscreen assignments for eyeSelect column}
}
\value{
categoryList
}
\description{
Categorize offScreen gazepoints based on eye selection method
}
