#' Process Downloaded Water Quality Data
#'
#' This function extracts selected parameters for selected months from the MNPCA water quality data. If multiple months are selected, the function will average each parameter across the selected months.
#' @param wqdata a water quality data.frame downloaded using the wqdatadownload() function.
#' @param parameters a character vector of the parameters to be extracted from the data.
#' @param months a numeric vector of the months to be extracted from the data.
#' @param startyear a numeric of the earliest year under consideration in the YYYY format. Default is NULL.
#' @param endyear a numeric of the latest year under consideration in the YYYY format. Default is NULL.
#' @keywords mnpca minnesota pollution water quality processing
#' @return A data.frame
#' @family Water Quality
#' @examples
#' #Download the data
#' x <- wqdatadownload(c("15-0010-00-100","15-0010-00-101","15-0010-00-102"))
#'
#' #Process a single parameter for a single month
#' y <- wqmonthtable(
#'        wqdata = x,
#'        parameters = "Depth, Secchi disk depth",
#'        months = 6
#'      )
#'
#' #Process multiple parameters across multiple months within a given year range
#' y <- wqmonthtable(
#'        wqdata = x,
#'        parameters = c("Depth, Secchi disk depth","Temperature, water","pH"),
#'        months = c(7,8,9),
#'        startyear = 2008,
#'        endyear = 2018
#'      )
#'
#'  #Process all parameters above a specific sample size across a specified time period
#'  y <- wqmonthtable(
#'         wqdata = x,
#'         parameters = wqparameters(x,minsample = 5),
#'         months = c(6,7,8,9),
#'         startyear = 2008
#'      )
#' @export

wqmonthtable=function(wqdata,parameters,months,startyear=NULL,endyear=NULL){
  # wqdata=x
  # parameters=wqparameters(x)[12]
  # startyear=2008
  # months=c(6,7,8)

  wqdata$result=suppressWarnings(as.numeric(wqdata$result))

  #Pull lake name from EDA data
  lakename=unique(tools::toTitleCase(tolower(wqdata$stationName)))

  #Set year limits
  if (!is.null(startyear)){
    wqdata=wqdata[lubridate::year(wqdata$sampleDate)>=startyear,]
  }
  if (!is.null(endyear)){
    wqdata=wqdata[lubridate::year(wqdata$sampleDate)<=endyear,]
  }

  #Begin formatting the data
  alldata=NULL
  for (i in parameters){

    #Select data according to parameter and month inputs
    wqdataparameter=wqdata[wqdata$parameter==i & lubridate::month(wqdata$sampleDate) %in% months,]
    if (all(!is.na(wqdataparameter$result))){

    #If there are multiple depths, select the row with the shallowest for each day
    wqdataselect=NULL
    for (k in unique(wqdataparameter$sampleDate)){
      wqdataselectrow=wqdataparameter[wqdataparameter$sampleDate==k,]
      if (nrow(wqdataselectrow)>1){
        wqdataselectrow=wqdataselectrow[wqdataselectrow$sampleUpperDepth==min(wqdataselectrow$sampleUpperDepth),]
      }
      wqdataselect=rbind(wqdataselect,wqdataselectrow)
    }

    #If there are multiple samples in a month or if multiple months have been selected, take the mean of those months
    if(!is.null(wqdataselect)){
    monthsdata=NULL
    for (j in unique(lubridate::year(wqdataselect$sampleDate))){
      monthsdataselect=wqdataselect[lubridate::year(wqdataselect$sampleDate)==j,]
      if (nrow(monthsdataselect)>1){
        resultreturn=mean(monthsdataselect$result,na.rm=TRUE)
      }else{
        resultreturn=monthsdataselect$result
      }
      monthsdatarow=data.frame("Lake"=as.character(lakename),"Months"=toString(months),"Year"=j,"Sample_Size"=nrow(monthsdataselect),
                               "Parameter"=as.character(unique(monthsdataselect$parameter)),"Value"=resultreturn,
                               "units"=as.character(unique(monthsdataselect$resultUnit)),stringsAsFactors = FALSE)
      monthsdata=rbind(monthsdata,monthsdatarow)
    }
    monthsdata=monthsdata[!is.na(monthsdata$Value),]
    monthsdata=monthsdata[order(monthsdata$Year),]
    }else{
      monthsdata=NULL
    }
    alldata=rbind(alldata,monthsdata)
    }
  }
  return(alldata)
}
