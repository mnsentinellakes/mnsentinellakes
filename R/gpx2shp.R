#' Convert Garmin GPX files to ESRI shapefiles
#'
#' This function converts gpx files exported from a Garmin GPS to shapefiles
#' @param folder file folder containing the .gpx files.
#' @param saveto file folder to save the shapefiles. If left blank, the files will be saved into a "SHP" file located in the same folder as the GPX files.
#' Default is NULL
#' @return an ESRI shapefile
#' @family Sentinel Lakes Tools
#' @examples
#' \dontrun{
#' gpx2shp("C:/Data/GPS/GPX")
#' }
#' @export

gpx2shp=function(folder,saveto=NULL){

  folder="D:/Datasets/GPS/Montana_2/GPX"

  gpxfiles=list.files(folder,pattern = ".gpx")

  for (i in gpxfiles){

    x=plotKML::readGPX(
      paste0(folder,"/",i),
      metadata = FALSE,
      bounds = FALSE,
      waypoints = TRUE,
      tracks = FALSE,
      routes = FALSE)$waypoints

    if (!is.null(x)){
      y=sp::SpatialPointsDataFrame(
        coords = as.matrix(data.frame(x$lon,x$lat)),
        proj4string = sp::CRS("+init=epsg:4326"),
        data = x
      )

      if (is.null(saveto)){
        dir.create(paste0(saveto,"/SHP"))

        rgdal::writeOGR(
          obj = y,
          dsn = paste0(folder,"/SHP"),
          layer = gsub(".gpx","",i),
          driver = "ESRI Shapefile"
        )
      }else{
        rgdal::writeOGR(
          obj = y,
          dsn = saveto,
          layer = gsub(".gpx","",i),
          driver = "ESRI Shapefile"
        )
      }
    }
  }
}
