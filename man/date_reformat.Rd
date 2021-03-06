% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{date_reformat}
\alias{date_reformat}
\title{Reformat date into format = 'yyyy-mm-dd HH:MM:SS'}
\usage{
date_reformat(data, startdate, endate, date_format, splits, event)
}
\arguments{
\item{data}{A data frame.}

\item{startdate}{Start date variable name.}

\item{endate}{End date variable name.}

\item{date_format}{date format, options = \code{c("yyyy-mm-dd", "dd-mm-yyyy", "mm-dd-yyyy")}.}

\item{splits}{list of data variables to return along with startdate and endate.}

\item{event}{binary variable indicating if an individual did not survive (was not censured)}
}
\value{
a dataframe containing \code{startdate, endate, event, tenure, splits}.
\code{tenure} is the difference in time between startdate and endate.
}
\description{
Reformat date into format = 'yyyy-mm-dd HH:MM:SS'
}
