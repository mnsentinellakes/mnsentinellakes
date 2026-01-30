#' Lakefinder Fish Survey Data
#'
#' A function to extract fish survey data from the data downloaded using the readlakefinder() function.
#' @param lakefinderdata results from the readlakefinder() function.
#' @keywords lakefinder fish data
#' @return a data.frame of fish survey data
#' @family Fish
#' @examples
#' #Retrieve the lakefinder data
#' x <- lakefinderdownload("11041300")
#' #Extract the fish survey data
#' y <- fishsurveydata(x)
#'
#' @export

fishsurveydata = function(lakefinderdata){

  if (!is.null(lakefinderdata)){
    LOISurveyData = NULL
    for (j in 1:as.numeric(length(lakefinderdata$result$surveys$surveyDate))){

      LOISurveyDatarow = as.data.frame(lakefinderdata$result$surveys$fishCatchSummaries[j])
      if (nrow(LOISurveyDatarow) > 0){
        LOISurveyDatarow["date"] = as.Date(lakefinderdata$result$surveys$surveyDate[j])
        LOISurveyDatarow["basin_id"] = as.character(lakefinderdata$basin_id)
        LOISurveyDatarow["ecoregion"] = as.character(mnsentinellakes::mnlakesmetadata$Ecoregion[
          mnsentinellakes::mnlakesmetadata$LakeId == mnsentinellakes::fixbasinid(lakefinderdata$basin_id)])

        # mnsentinellakes::mnlakesmetadata[
        #   mnsentinellakes::mnlakesmetadata$LakeId == fixbasinid(lakefinderdata$basin_id),]

        names(LOISurveyDatarow) = tolower(names(LOISurveyDatarow))

        LOISurveyData = rbind(LOISurveyData,LOISurveyDatarow)
      }
    }
    # if(is.null(LOISurveyData)){
    #   print("No fisheries survey data available")
    # }
  }else{
    LOISurveyData = NULL
    warning("No Lake Finder data available")
  }
  return(LOISurveyData)
}
