% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractEventRows.R
\name{extractEventRows}
\alias{extractEventRows}
\title{Run extractEventRows, after you have ran rmInvalidGP function to standardize data values.}
\usage{
extractEventRows(data, software, ...)
}
\arguments{
\item{data}{dataframe}

\item{software}{Tobii Software version}

\item{...}{additional arguments are of either the form value or tag = value. Component names are created based on the tag (if present) or the deparsed argument itself.}
}
\value{
a list: 1. dataframe with only gazestream data extracted, 2. dataframe with only event data extracted
}
\description{
Run extractEventRows, after you have ran rmInvalidGP function to standardize data values.
}
