% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assignFinalColumnNames.R
\name{assignFinalColumnNames}
\alias{assignFinalColumnNames}
\title{Assign values to final preprocessed column names}
\usage{
assignFinalColumnNames(data, smoothGaze_boolean, ...)
}
\arguments{
\item{data}{dataframe}

\item{...}{additional arguments are of either the form value or tag = value. Component names are created based on the tag (if present) or the deparsed argument itself.}

\item{noise_reduction}{Boolean indicating if noise reduction should be implemented on gaze data}
}
\value{
data
}
\description{
Assign values to final preprocessed column names
}
