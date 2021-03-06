% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fishstockdownload.R
\name{fishstockdownload}
\alias{fishstockdownload}
\title{Download Fish Stocking Data}
\usage{
fishstockdownload(lakeid)
}
\arguments{
\item{lakeid}{Minnesota lake identifier (DOWLKNUM) for the lake of interest.}
}
\value{
a data.frame with water level data
examples
x <- fishstockdownload("11041300")
}
\description{
This function downloads fish stocking data from the MNDNR Lakefinder website.
}
\keyword{Minnesota}
\keyword{data}
\keyword{fish}
\keyword{stocking}
