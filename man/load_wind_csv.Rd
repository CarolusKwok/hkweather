% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_wind_csv.R
\name{load_wind_csv}
\alias{load_wind_csv}
\title{Download Wind and Gust data from HKO in CSV format}
\usage{
load_wind_csv(
  ETime = Sys.time(),
  DDays = 7,
  STime = NA,
  lan = "en",
  listfail = F
)
}
\arguments{
\item{ETime}{The newest csv to be downloaded, starting from the latest time. Only accepts POSIXct (tip: create POSIXct via ISOdatetime).}

\item{DDays}{The duration of images to be downloaded in days. Only accepts numerical values.}

\item{STime}{The oldest csv to be downloaded, ending at the earliest time. Only accepts POSIXct (tip: create POSIXct via ISOdatetime)}

\item{lan}{Language of the descriptive text}

\item{listfail}{List the date/time of failed download attempts. Only accepts T/F/1/0.}
}
\value{

}
\description{
Downloads and stores them in the working directory orderly according to date.
}
\examples{
load_wind_csv()
}
