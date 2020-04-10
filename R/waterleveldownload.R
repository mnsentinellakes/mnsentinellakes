#' Download Water Level Data
#'
#' This function downloads water level data from the MNDNR Lakefinder website and converts the elevation to meters. The data are already formatted into the
#' Sentinel Lakes format.
#' @param lakeid Minnesota lake identifier (DOWLKNUM) for the lake of interest.
#' @param metric Logical indicating if the elevations should be in meters. If TRUE, the data will be in meters, if FALSE, the data will be in feet. The default is TRUE.
#' @keywords water levels Minnesota data
#' @return a data.frame with water level data
#' @examples
#' x <- waterleveldownload("11041300")
#'
#' @export



waterleveldownload = function(lakeid,metric=TRUE){
  #Quick fix for Belle LakeId
  if(lakeid=="47004901"){
    wtrlvldata=as.data.frame(data.table::fread(paste0("http://files.dnr.state.mn.us/cgi-bin/lk_levels_dump.cgi?format=csv&id=47004900")))
  }else{

    wtrlvldata=as.data.frame(data.table::fread(paste0("http://files.dnr.state.mn.us/cgi-bin/lk_levels_dump.cgi?format=csv&id=",lakeid)))
  }

  if (nrow(wtrlvldata)>0){
    wtrlvldata$ELEVATION=as.numeric(wtrlvldata$ELEVATION)
    wtrlvldata$READ_DATE=as.Date(as.character(wtrlvldata$READ_DATE))
    if (metric==TRUE){
      wtrlvldata["elev_m"]=round(as.numeric(wtrlvldata$ELEVATION*0.3048),digits = 2)
      wtrlvldf=data.frame("Lake"=mnsentinellakes::lakeid2name(lakeid),"LakeId"=lakeid,"Date"=wtrlvldata$READ_DATE,"Elevation_m"=wtrlvldata$elev_m,
                          "Datum_Adj"=wtrlvldata$DATUM_ADJ)
    }else{
      wtrlvldf=data.frame("Lake"=mnsentinellakes::lakeid2name(lakeid),"LakeId"=lakeid,"Date"=wtrlvldata$READ_DATE,"Elevation_ft"=wtrlvldata$ELEVATION,
                          "Datum_Adj"=wtrlvldata$DATUM_ADJ)
    }
  }else{
    wtrlvldf=NULL
    warning("No water level data available.")
  }
  return(wtrlvldf)
}
