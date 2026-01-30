#' Download Water Level Data
#'
#' This function downloads water level data from the MNDNR Lakefinder website and converts the elevation to meters. The data are already formatted into the
#' Sentinel Lakes format.
#' @param basinid Minnesota lake identifier (DOWLKNUM) for the lake of interest.
#' @param metric Logical indicating if the elevations should be in meters. If TRUE, the data will be in meters, if FALSE, the data will be in feet. The default is TRUE.
#' @keywords water levels Minnesota data
#' @return a data.frame with water level data
#' @examples
#' x <- waterleveldownload("11041300")
#'
#' @export



waterleveldownload = function(basinid,metric = TRUE){
  #Quick fix for Belle BasinId
  if(basinid == "47004901"){
    wtrlvldata = as.data.frame(
      data.table::fread(
        paste0("https://maps.dnr.state.mn.us/cgi-bin/lakefinder/water_levels_export.cgi?format=csv&id=47004900")
      )
    )
  }else{

    wtrlvldata = as.data.frame(
      data.table::fread(
        paste0("https://maps.dnr.state.mn.us/cgi-bin/lakefinder/water_levels_export.cgi?format=csv&id=",basinid)
      )
    )
  }

  if (nrow(wtrlvldata) > 0){
    wtrlvldata$ELEVATION = as.numeric(wtrlvldata$ELEVATION)
    wtrlvldata$READ_DATE = as.Date(as.character(wtrlvldata$READ_DATE))
    if (metric == TRUE){
      wtrlvldata["elev_m"] = round(as.numeric(wtrlvldata$ELEVATION * 0.3048),digits = 2)
      wtrlvldf = data.frame(
        "basin_name" = mnsentinellakes::basinid2name(basinid),
        "basin_id" = basinid,
        "date" = wtrlvldata$READ_DATE,
        "elevation_m" = wtrlvldata$elev_m,
        "datum_adj" = wtrlvldata$DATUM_ADJ
      )
    }else{
      wtrlvldf = data.frame(
        "basin_name" = mnsentinellakes::basinid2name(basinid),
        "basin_id" = basinid,
        "date" = wtrlvldata$READ_DATE,
        "elevation_ft" = wtrlvldata$ELEVATION,
        "datum_Adj"=wtrlvldata$DATUM_ADJ
      )
    }
  }else{
    wtrlvldf = NULL
    warning("No water level data available.")
  }
  return(wtrlvldf)
}
