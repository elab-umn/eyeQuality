% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getEventTimes.R
\name{getEventTimes}
\alias{getEventTimes}
\title{Get timestamps for beginning and end of events}
\usage{
getEventTimes(
  eventData,
  firstEvent,
  firstInd = 1,
  lastEvent,
  lastInd = NULL,
  ...
)
}
\arguments{
\item{eventData}{a dataframe generated by removeEventRows containing all event data}

\item{firstEvent}{a string defining the first event of interest in the recording}

\item{firstInd}{an integer marking which occurrence of the event type should be set as the first. Default 1 sets the first occurance.}

\item{lastEvent}{a string defining the last event of interest in the recording}

\item{lastInd}{an integer marking which occurrence of the event type should be set as the first. Default 1 sets the first occurance.}

\item{...}{additional arguments are of either the form value or tag = value. Component names are created based on the tag (if present) or the deparsed argument itself.}
}
\value{
list containing start and end times
}
\description{
Get timestamps for beginning and end of events
}
