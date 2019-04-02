#' Download Water Level Data
#'
#' This function downloads water level data from the MNDNR Lakefinder website and converts the elevation to meters. The data are already formatted into the
#' Sentinel Lakes format.
#' @param lakeid Minnesota lake identifier (DOWLKNUM) for the lake of interest.
#' @keywords water levels Minnesota data
#' @return a data.frame with water level data
#' examples
#' x <- waterleveldownload("11041300")
#'
#' @export

waterleveldownload = function(lakeid){
  wtrlvldata=as.data.frame(data.table::fread(paste0("http://webapps5.dnr.state.mn.us/cgi-bin/lk_levels_dump.pl?format=csv&id=",lakeid)))

  if (nrow(wtrlvldata)>0){
  wtrlvldata$ELEVATION=as.numeric(wtrlvldata$ELEVATION)
  wtrlvldata["elev_m"]=as.numeric(wtrlvldata$ELEVATION*0.3048)
  wtrlvldf=data.frame("Lake"=mnsentinellakes::mnlakesmetadata$Lake[mnsentinellakes::fixlakeid(mnsentinellakes::mnlakesmetadata$LakeId)==lakeid],
                      "LakeId"=lakeid,"Date"=wtrlvldata$READ_DATE,"Elevation_m"=wtrlvldata$elev_m,"Datum_Adj"=wtrlvldata$DATUM_ADJ)
  }else{
    wtrlvldf=print("No water level data available")
  }
  return(wtrlvldf)
}
