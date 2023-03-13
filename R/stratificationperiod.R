#' Lake Stratification Periods
#'
#' This function calculates the start and end of lake thermal stratification using daily lake temperature profile data (This will only work on data collected at or summarized to a daily scale). In this calculation, a lake is considered stratified when there is a difference of one degree C between one meter depth intervals anywhere within the water column.
#' @param tempdata a vector of temperature values
#' @param datedata a vector of dates associated with the temperature values
#' @param depthdata a vector of depths associated with the temperature values
#' @param tablestructure determines if the output table should be in a "tall" or "wide" format. If "tall" the table will include "year", "date", "event", and "cons_days" (consecutive days) fields. If "wide" the table will include "year", "start_date", "end_date", and "cons_days" fields.
#' @param consecutivedays the number of consecutive days of uninterrupted stratification required to consider the lake stratified for the season. A value of 0 will give all days the lake is stratified. Default is 10 days.
#' @keywords lake stratification temperature thermal profile
#' @return a data.frame indicating the start and end dates of thermal lake stratification. If table structure is tall, stratification "events" include "S" - start, "E" - end, and "SE" - stratification started and ended on the same day.
#' @examples
#' \dontrun{
#' x <-stratificationperiod(
#'       tempdata = temperaturedata$mean_temp,
#'       datedata = temperaturedata$date,
#'       depthdata = temperaturedata$depth,
#'       tablestructure = "wide",
#'       consecutivedays = 15
#'       )
#' }
#' @export

stratificationperiod = function(tempdata,datedata,depthdata,tablestructure = "tall",consecutivedays = 10){

  #Format Data as needed
  if (any(is.na(tempdata))){
    tempdata = tempdata[which(!is.na(tempdata))]
  }

  if (!lubridate::is.Date(datedata)){
    datedata = as.Date(datedata)
  }

  if (!is.numeric(depthdata)){
    depthdata = as.numeric(depthdata)
  }

  #Combine input data into a data frame
  combidata = data.frame("date" = datedata,"depth" = depthdata,"temp" = tempdata)

  #Remove unneeded inputs
  rm(tempdata,datedata,depthdata)

  #Calculate changes in temp over changes in depths
  stratcheck = combidata |>
    dplyr::arrange(.data$date,.data$depth) |>
    dplyr::group_by(.data$date) |> dplyr::reframe(date = .data$date,depth = .data$depth,strat = diff(c(NA,.data$temp))/diff(c(NA,.data$depth)))

  #Calculate if which dates have a change of more than 1 degree over 1 depth
  stratdates = unique(stratcheck$date[which((stratcheck$strat * -1) >= 1)])

  #Function for selecting the start and end dates of stratification based upon the required number of consecutive days
  conscheck = function(dates,days){
    #Build a data frame with year, date and difference in successive dates in data frame
    dates = data.frame("year" = lubridate::year(dates),"date" = dates,"diff" = c(0,diff(dates)))

    #Calculation
    consdays = dates |>
      #Group data into time periods based upon whether dates are consecutive or not
      dplyr::mutate(year = .data$year,diff = .data$diff,periodID = 1 + cumsum(.data$diff > 1)) |>
      dplyr::group_by(.data$periodID) |>
      #Filter to include only dates that are at the beginning or end of a time period or are isolated
      dplyr::filter(
        (date == min(.data$date) & date != max(.data$date)) |
          (date == max(.data$date) & date != min(.data$date)) |
          (date == min(.data$date) & date == max(.data$date))
        ) |>
      dplyr::group_by(.data$periodID) |>
      #Reframe data, listing the start and end dates of each time period and the number of consecutive days
      dplyr::reframe(
        year = .data$year,
        periodID = .data$periodID,
        start_date = min(.data$date),
        end_date = max(.data$date),
        cons_days = as.numeric(max(.data$date) - min(.data$date))
        ) |>
      dplyr::distinct() |>
      #Filter data to only include periods with consecutive days more than or equal to the amount entered by the user
      dplyr::filter(.data$cons_days >= days)
      #Reframe to include only needed columns

      if (tablestructure == "tall"){
        consdays = consdays |>
          dplyr::group_by(.data$periodID) |>
          dplyr::reframe(
            year = .data$year,
            date = c(.data$start_date,.data$end_date),
            event = dplyr::case_when(
              start_date == .data$end_date ~ "SE",
              date == .data$start_date ~"S",
              date == .data$end_date ~"E"
              ),
            cons_days = .data$cons_days
          ) |>
          dplyr::distinct() |>
          dplyr::select(-.data$periodID)
      }else if (tablestructure == "wide"){
        consdays = consdays |>
          dplyr::select(-c(.data$periodID))
      }
    return(consdays)
  }

  stratevents = conscheck(stratdates,consecutivedays)

  return(stratevents)
}
