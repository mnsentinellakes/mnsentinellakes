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
#'        basinid = 21005700,
#'        startdate = "2019-03-20",
#'        enddate = "2019-04-01")
#' #Convert to Sentinel Lakes formatting
#' y <- weather2sentinel(x)
#' @export

weather2sentinel=function(weatherdata){

  basinname = mnsentinellakes::basinid2name(weatherdata$basin_id)

  weather=data.frame(
    "basin_name" = basinname,
    "basin_id" = weatherdata$basin_id,
    "station" = weatherdata$station,
    "date_time"=weatherdata$valid)

  if("tmpc" %in% colnames(weatherdata)){
    weather["temperature_c"] = round(as.numeric(weatherdata$tmpc),digits = 2)
  }
  if("dwpc" %in% colnames(weatherdata)){
    weather["dew_point_c"] = round(as.numeric(weatherdata$dwpc),digits = 2)
  }
  if("relh" %in% colnames(weatherdata)){
    weather["relative_humidity"] = round(as.numeric(weatherdata$relh),digits = 2)
  }
  if("drct" %in% colnames(weatherdata)){
    weather["wind_direction"] = round(as.numeric(weatherdata$drct),digits = 2)
  }
  if("sknt" %in% colnames(weatherdata)){
    weather["wind_speed_kph"] = round(as.numeric(weatherdata$sknt) * 1.852,digits = 2)
  }
  if("gust" %in% colnames(weatherdata)){
    weather["wind_gust_kph"]=round(as.numeric(weatherdata$gust) * 1.852,digits = 2)
  }
  if("alti" %in% colnames(weatherdata)){
    weather["altimeter_pa"]=round(as.numeric(weatherdata$alti) * 3376.85,digits = 2)
  }
  if("p01m" %in% colnames(weatherdata)){
    weather["precipitation_mm"] = round(as.numeric(weatherdata$p01m),digits = 2)
  }

  return(weather)
}
