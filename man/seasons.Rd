% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{seasons}
\alias{seasons}
\title{Seasons}
\format{
This data frame contains the following columns:
\describe{
  \item{\code{version}}{Country code for the version of the show}
  \item{\code{season}}{The season number}
  \item{\code{subtitle}}{Season subtitle}
  \item{\code{location}}{Location}
  \item{\code{country}}{Country}
  \item{\code{region}}{Region}
  \item{\code{n_survivors}}{Number of survivors. Season 4 there were 7 teams of 2.}
  \item{\code{lat}}{Latitude}
  \item{\code{lon}}{Longitude}
  \item{\code{date_drop_off}}{Date the survivors where dropped off}
}
}
\source{
\url{https://en.wikipedia.org/wiki/Alone_(TV_series)}
}
\usage{
seasons
}
\description{
Season summary includes location and other season level information
}
\examples{
library(dplyr)

seasons |>
count(country)
}
\keyword{datasets}
