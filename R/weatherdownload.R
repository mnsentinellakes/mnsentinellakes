#' Downloads Airport Weather Data
#'
#' This function downloads airport weather station data from the Iowa State Mesonet from the station nearest to chosen lake and converts it into a
#' standardized Sentinel Lakes data format.
#' @param lakeid Minnesota lake identifier (DOWLKNUM) for the lake of interest.
#' @param startdate a date indicating the beginning of the date range to be downloaded. Format: "yyyy-mm-dd".
#' @param enddate a date indicating the end of the date range to be downloaded. Format: "yyyy-mm-dd".
#' @param parameters a list of parameters to download. Valid parameters include "Air Temperature", "Dew Point", "Relative Humidity", "Wind Direction",
#' "Wind Speed", "Altimeter", "Precipitation", and "Gust Speed". Default includes all parameters.
#' @keywords weather mesonet iowa state air temperature dew point relative humidity wind direction speed altimeter precipitation gust
#' @family Weather
#' @examples
#' x <- weatherdownload(
#'        lakeid = 21005700,
#'        startdate = "2019-03-20",
#'        enddate = "2019-04-01")
#' @export

weatherdownload = function(lakeid,startdate,enddate,parameters=c("Air Temperature","Dew Point","Relative Humidity","Wind Direction","Wind Speed",
                                                                                 "Altimeter","Precipitation","Gust Speed")){

  #Standardize lake name for lookup

  if (lakeid %in% mnsentinellakes::sentinellakesmetadata$LakeId){
    IATA=mnsentinellakes::sentinellakesmetadata$IATA[mnsentinellakes::sentinellakesmetadata$LakeId==lakeid]
  }else{
    IATA=mnsentinellakes::mnlakesmetadata$IATA[mnsentinellakes::mnlakesmetadata$LakeId==lakeid]
  }


  #Setup first portion of URL
  wthrdata=paste0("https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?station=",IATA)

  #Parameters
  #Air temperature
  if("Air Temperature" %in% parameters){
    atp="&data=tmpc"
  }else{
    atp=""
  }

  #Dew Point
  if("Dew Point" %in% parameters){
    dwp="&data=dwpc"
  }else{
    dwp=""
  }

  #Relative Humidity
  if("Relative Humidity" %in% parameters){
    rlh="&data=relh"
  }else{
    rlh=""
  }

  #Wind Direction
  if("Wind Direction" %in% parameters){
    wdr="&data=drct"
  }else{
    wdr=""
  }

  #Wind Speed
  if("Wind Speed" %in% parameters){
    wsd="&data=sknt"
  }else{
    wsd=""
  }

  #Altimeter
  if("Altimeter" %in% parameters){
    alt="&data=alti"
  }else{
    alt=""
  }

  #Sea level pressure
  slp=""
  # if(input$slp==TRUE){
  #   slp="&data=mslp"
  # }else{
  #   slp=""
  # }

  #Precipitation
  if("Precipitation" %in% parameters){
    pcp="&data=p01m"
  }else{
    pcp=""
  }

  #Visibility
  vsb=""
  # if(input$vis==TRUE){
  #   vsb="&data=vsby"
  # }else{
  #   vsb=""
  # }

  #Gust
  if("Gust Speed" %in% parameters){
    gsd="&data=gust"
  }else{
    gsd=""
  }

  #Cloud Coverage
  sky=""
  # if(input$skyl==TRUE){
  #   sky="&data=skyc1&data=skyc2&data=skyc3&data=skyl1&data=skyl2&data=skyl3"
  # }else{
  #   sky=""
  # }

  #Compile URL based upon the selected parameters
  wthrdata=paste0(wthrdata,atp,dwp,rlh,wdr,wsd,alt,slp,pcp,vsb,gsd,sky,
                  "&year1=",lubridate::year(startdate),
                  "&month1=",lubridate::month(startdate),
                  "&day1=",lubridate::day(startdate),
                  "&year2=",lubridate::year(enddate),
                  "&month2=",lubridate::month(enddate),
                  "&day2=",lubridate::day(enddate),
                  "&tz=Etc%2FUTC&format=onlycomma&latlon=no&direct=no&report_type=1&report_type=2")

  #Download data
  wthrdld=data.table::fread(wthrdata)
  wthrdld=as.data.frame(wthrdld)

  wthrdld["LakeId"]=lakeid

  return(wthrdld)
}
