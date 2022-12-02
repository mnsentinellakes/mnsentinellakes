#' Water Quality Data Sentinel Formatting
#'
#' Reorganizes water quality data downloaded from the MNPCA's EDA website into a common Sentinel Lakes format, making it easier to integrate with other Sentinel
#' Lakes formatted datasets.
#' @param wqdata a water quality data.frame downloaded using the wqdatadownload() function.
#' @keywords water quality mnpca sentinel format
#' @family Formatting
#' @examples
#' #Download the data
#' x <- wqdatadownload(c("15-0010-00-100","15-0010-00-101","15-0010-00-102"))
#'
#' #Convert to Sentinel Lakes formatting
#' y <- wq2sentinel(x)
#' @export

wq2sentinel = function(wqdata){

  wqoutput = NULL
  for (i in unique(wqdata$stationName)){

    wqdataselect = wqdata[wqdata$stationName == i,]
    lakeid = mnsentinellakes::fixlakeid(unique(substr(wqdataselect$stationId,1,10)))
    lakename = mnsentinellakes::lakeid2name(lakeid)
    #Build Data Frame
    wqoutputrow = data.frame("Lake" = lakename,"LakeId"=lakeid,"StationId" = wqdataselect$stationId,"Date" = as.Date(wqdataselect$sampleDate),
                           "Time" = wqdataselect$sampleTime,"Parameter" = as.character(wqdataselect$parameter),"GTLT" = wqdataselect$gtlt,
                           "Value" = wqdataselect$result,"Unit" = as.character(wqdataselect$resultUnit),
                           "Fraction_Type" = as.character(wqdataselect$sampleFractionType),"Type" = as.character(wqdataselect$sampleType),
                           "Method" = as.character(wqdataselect$testMethodName),
                           "Upper_Depth" = suppressWarnings(as.numeric(as.character(wqdataselect$sampleUpperDepth))),
                           "Lower_Depth" = suppressWarnings(as.numeric(as.character(wqdataselect$sampleLowerDepth))),
                           "Collecting_Organization" = wqdataselect$collectingOrg,
                           "Comments" = as.character(wqdataselect$comments),stringsAsFactors = FALSE)
    wqoutput = rbind(wqoutput,wqoutputrow)
  }
    wqoutput = wqoutput[order(wqoutput$Lake,wqoutput$Date),]
    row.names(wqoutput) = NULL
    wqoutput$GTLT[wqoutput$GTLT == "(null)"] = NA
    wqoutput$Value[wqoutput$Value == "(null)"] = NA
    wqoutput$Comments[wqoutput$Comments == "(null)"] = NA

  return(wqoutput)
}
