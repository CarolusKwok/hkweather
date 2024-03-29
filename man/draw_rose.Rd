% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_rose.R
\name{draw_rose}
\alias{draw_rose}
\title{Draw rose plot}
\usage{
draw_rose(drct, wind, unit = "kmh", time_unit = 0.5, scale = "beaufort")
}
\arguments{
\item{drct}{Direction of the wind}

\item{wind}{Wind speed or any other type of data.}

\item{unit}{Wind speed unit, provided in enq_wind. Default as "kmh"}

\item{time_unit}{Time per observation, in hours. Default as 0.5.}

\item{scale}{Scale of the wind speed for classification. Accepts "beaufort" or "hko". Default as "beaufort".}
}
\value{
a list including ggplot and data
}
\description{
Draw rose plot
}
\examples{
draw_rose(100, seq(0, 220, 5))
}
