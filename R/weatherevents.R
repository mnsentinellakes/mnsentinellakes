#' Weather Events
#'
#' Determines which days experienced wind (>5 knots), precipitation, or clouds for more than half the day.
#'
#' @param lakeid Minnesota lake identifier (DOWLKNUM) for the lake of interest.
#' @param startdate a date indicating the beginning of the date range to be downloaded. Format: "yyyy-mm-dd".
#' @param enddate a date indicating the end of the date range to be downloaded. Format: "yyyy-mm-dd".
#' @keywords weather
#' @examples
#' x = weatherevents(
#'  lakeid = "16007700",
#'  startdate = "2019-04-01",
#'  enddate = "2019-04-30")
#'
#' @export
#'
weatherevents = function(lakeid,startdate,enddate){

  weatherdata=mnsentinellakes::weatherdownload(
    lakeid = lakeid,
    startdate = startdate,
    enddate = enddate,
    parameters = c("Wind Speed","Precipitation","Clouds")
  )

  weatherdata$valid=as.POSIXct(weatherdata$valid,format = "%Y-%m-%d %H:%M")
  weatherdata$sknt = suppressWarnings(as.numeric(weatherdata$sknt))
  weatherdata$p01m = suppressWarnings(as.numeric(weatherdata$p01m))

  weatherdates=unique(as.Date(weatherdata$valid))

  weatherocc=NULL
  for (i in weatherdates){

    weatherday=weatherdata[which(as.Date(weatherdata$valid)==i),]

    #wind
    if(nrow(weatherday[which(weatherday$sknt>5),])/nrow(weatherday)>.5){
      windy=TRUE
    }else{
      windy=FALSE
    }

    #precip
    if(nrow(weatherday[which(weatherday$p01m>0),])/nrow(weatherday)>.5){
      precip=TRUE
    }else{
      precip=FALSE
    }

    #Clouds
    if(nrow(weatherday[which(weatherday$skyc1=="OVC"|weatherday$skyc2=="OVC"|weatherday$skyc3=="OVC"),])/nrow(weatherday)>.5){
      cloudy = TRUE
    }else{
      cloudy = FALSE
    }
    weatheroccrow=data.frame("LakeId"=lakeid,'Date'=as.Date(i,origin = "1970-01-01"),"Wind"=windy,"Precip"=precip,"Clouds"=cloudy)
    weatherocc=rbind(weatherocc,weatheroccrow)
  }

  return(weatherocc)
}
