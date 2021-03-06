% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/waterleveldownload.R
\name{waterleveldownload}
\alias{waterleveldownload}
\title{Download Water Level Data}
\usage{
waterleveldownload(lakeid, metric = TRUE)
}
\arguments{
\item{lakeid}{Minnesota lake identifier (DOWLKNUM) for the lake of interest.}

\item{metric}{Logical indicating if the elevations should be in meters. If TRUE, the data will be in meters, if FALSE, the data will be in feet. The default is TRUE.}
}
\value{
a data.frame with water level data
}
\description{
This function downloads water level data from the MNDNR Lakefinder website and converts the elevation to meters. The data are already formatted into the
Sentinel Lakes format.
}
\examples{
x <- waterleveldownload("11041300")

}
\keyword{Minnesota}
\keyword{data}
\keyword{levels}
\keyword{water}
