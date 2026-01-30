#' Weather Events
#'
#' Determines which days experienced wind (>5 knots), precipitation, or clouds for more than half the day.
#'
#' @param basinid Minnesota lake identifier (DOWLKNUM) for the lake of interest.
#' @param startdate a date indicating the beginning of the date range to be downloaded. Format: "yyyy-mm-dd".
#' @param enddate a date indicating the end of the date range to be downloaded. Format: "yyyy-mm-dd".
#' @importFrom dplyr %>%
#' @keywords weather
#' @examples
#' x = weatherevents(
#'  basinid = "16007700",
#'  startdate = "2019-04-01",
#'  enddate = "2019-04-30")
#'
#' @export
#'
weatherevents = function(basinid,startdate,enddate){

  weatherdata = mnsentinellakes::weatherdownload(
    basinid = basinid,
    startdate = startdate,
    enddate = enddate,
    parameters = c("Air Temperature","Wind Speed","Precipitation","Clouds")
  )

  weatherocc = weatherdata %>%
    dplyr::mutate(
      valid = as.POSIXct(.data$valid,format = "%Y-%m-%d %H:%M",tz = "UTC"),
      tmpc = suppressWarnings(as.numeric(.data$tmpc)),
      sknt = suppressWarnings(as.numeric(.data$sknt)),
      p01m = suppressWarnings(as.numeric(.data$p01m))
    ) %>%
    dplyr::mutate(date = as.Date(.data$valid)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(
      basin_id = dplyr::first(.data$basin_id),
      mean_temp = round(mean(.data$tmpc, na.rm = TRUE),2),
      max_temp = round(max(.data$tmpc, na.rm = TRUE),2),
      min_temp = round(min(.data$tmpc, na.rm = TRUE),2),
      wind = mean(.data$sknt > 5, na.rm = TRUE) > 0.5,
      precip = round(sum(.data$p01m,na.rm = TRUE),2),
      clouds = mean(.data$skyc1 == "OVC" | .data$skyc2 == "OVC" | .data$skyc3 == "OVC",na.rm = TRUE) > 0.5
    )

  return(weatherocc)
}
