% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gpx2shp.R
\name{gpx2shp}
\alias{gpx2shp}
\title{Convert Garmin GPX files to ESRI shapefiles}
\usage{
gpx2shp(folder, saveto = NULL)
}
\arguments{
\item{folder}{file folder containing the .gpx files.}

\item{saveto}{file folder to save the shapefiles. If left blank, the files will be saved into a "SHP" file located in the same folder as the GPX files.
Default is NULL}
}
\value{
an ESRI shapefile
}
\description{
This function converts gpx files exported from a Garmin GPS to shapefiles
}
\examples{
\dontrun{
gpx2shp("C:/Data/GPS/GPX")
}

}
\seealso{
Other Sentinel Lakes Tools: \code{\link{addunderscore}},
  \code{\link{fixlakeid}}, \code{\link{lakeid2name}},
  \code{\link{lakename2id}}
}
\concept{Sentinel Lakes Tools}
