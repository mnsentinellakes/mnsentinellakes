#' Weather Data Sentinel Formatting
#'
#' Reorganizes weather data downloaded from Iowa State's Mesonet website into a common Sentinel Lakes format, making it easier to integrate with other Sentinel
#' Lakes formatted datasets. This will also convert all units to metric. All numbers rounded to 2 decimal places.
#' @param weatherdata a weather data.frame downloaded using the weatherdownload() function.
#' @keywords weather
#' @family Formatting
#' @examples
#' #Download the data
#' x <- weatherdownload(
#'        lakeid = 21005700,
#'        startdate = "2019-03-20",
#'        enddate = "2019-04-01")
#' #Convert to Sentinel Lakes formatting
#' y <- weather2sentinel(x)
#' @export

weather2sentinel=function(weatherdata){

  lakename=mnsentinellakes::lakeid2name(weatherdata$LakeId)

  weather=data.frame("Lake"=lakename,"LakeId"=weatherdata$LakeId,"Station"=weatherdata$station,"Date_Time"=weatherdata$valid)

  if("tmpc" %in% colnames(weatherdata)){
    weather["Temperature_C"]=round(as.numeric(weatherdata$tmpc),digits=2)
  }
  if("dwpc" %in% colnames(weatherdata)){
    weather["Dew_Point_C"]=round(as.numeric(weatherdata$dwpc),digits = 2)
  }
  if("relh" %in% colnames(weatherdata)){
    weather["Relative_Humidity"]=round(as.numeric(weatherdata$relh),digits = 2)
  }
  if("drct" %in% colnames(weatherdata)){
    weather["Wind_Direction"]=round(as.numeric(weatherdata$drct),digits = 2)
  }
  if("sknt" %in% colnames(weatherdata)){
    weather["Wind_Speed_kph"]=round(as.numeric(weatherdata$sknt)*1.852,digits = 2)
  }
  if("gust" %in% colnames(weatherdata)){
    weather["Wind_Gust_kph"]=round(as.numeric(weatherdata$gust)*1.852,digits = 2)
  }
  if("alti" %in% colnames(weatherdata)){
    weather["Altimeter_Pa"]=round(as.numeric(weatherdata$alti)*3376.85,digits = 2)
  }
  if("p01m" %in% colnames(weatherdata)){
    weather["Precipitation_mm"]=round(as.numeric(weatherdata$p01m),digits = 2)
  }

  return(weather)
}
