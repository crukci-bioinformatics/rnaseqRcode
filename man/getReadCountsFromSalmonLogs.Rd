% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getReadCountsFromSalmonLogs.R
\name{getReadCountsFromSalmonLogs}
\alias{getReadCountsFromSalmonLogs}
\title{gets reads pre sample table from salmon log files}
\usage{
getReadCountsFromSalmonLogs(s_sheet, quantOut)
}
\arguments{
\item{s_sheet}{a data frame; metadata sheet}

\item{quantOut}{a vector of length one; salmon output folder}
}
\value{
a data frame; sample and number of fragments table.
}
\description{
gets reads pre sample table from salmon log files
}
\examples{

}
