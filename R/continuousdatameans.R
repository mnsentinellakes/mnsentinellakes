#' Continous Lake Data Summary
#'
#' This function calculates the mean value of a continuous data set for a specified time period.
#' @param contdata vector with continuous numeric data. Must have the same number of values as the timedata and stratifieddata parameters.
#' @param timedata vector with date and/or time data in POSIXcT format. Must have the same number of values as the contdata and stratifieddata values.
#' @param stratifieddata vector with additional data that indicates how the contdata should be stratified. For example, depth data for water temperature. Must have the same number of values as the contdata and timedata values.
#' @param period the time period to calculate the mean. Choices include daily, weekly, monthly, or yearly.
#' @keywords lake temperature thermal profile
#' @return a data.frame with averaged continuous data.
#' @examples
#' \dontrun{
#'
#' x <-continuousdatasummary(
#'      contdata = temperaturedata,
#'      timedata = date_time,
#'      stratifieddata = logger_depths,
#'      period = "daily")
#'
#' }
#' @export

continuousdatameans = function(contdata, timedata, stratifieddata, period = c("daily" | "weekly" | "monthly" | "yearly")){

    datacombine = data.frame("Contdata" = contdata,"Time" = timedata,"Stratdata" = as.character(stratifieddata),stringsAsFactors = FALSE)


    if(any(is.na(datacombine$Time))){
      datacombine = datacombine[!is.na(datacombine$Time),]
      warning("Removed NAs from timedata")
    }

    if(any(is.na(datacombine$Stratdata))){
      datacombine$Stratdata[is.na(datacombine$Stratdata)] = "No Data"
    }

    analysisresults = NULL
    for (i in unique(datacombine$Stratdata)) {
      analysisdata = datacombine[which(datacombine$Stratdata == i),]
      analysisdata.xts = xts::xts(analysisdata$Contdata, order.by = analysisdata$Time)
      if(period == "daily"){
        analysismean.xts = xts::apply.daily(analysisdata.xts,"mean")
        analysiscount.xts = xts::apply.daily(analysisdata.xts,"length")
      }else if(period == "weekly"){
        analysismean.xts = xts::apply.weekly(analysisdata.xts,"mean")
        analysiscount.xts = xts::apply.weekly(analysisdata.xts,"length")
      }else if(period == "monthly"){
        analysismean.xts = xts::apply.monthly(analysisdata.xts,"mean")
        analysiscount.xts = xts::apply.monthly(analysisdata.xts,"length")
      }else if(period == "yearly"){
        analysismean.xts = xts::apply.yearly(analysisdata.xts,"mean")
        analysiscount.xts = xts::apply.yearly(analysisdata.xts,"length")
      }

      analysisresultsrow=data.frame("Timespan" = period,"Time" = zoo::index(analysismean.xts),"Mean" = zoo::coredata(analysismean.xts),"Stratification_Level" = i,
                                    "Count" = zoo::coredata(analysiscount.xts))
      analysisresults = rbind(analysisresults,analysisresultsrow)
    }

    analysisresults$Stratification_Level[analysisresults$Stratification_Level == "No Data"] = NA
    analysisresults = analysisresults[order(analysisresults$Time),]
  return(analysisresults)
}
