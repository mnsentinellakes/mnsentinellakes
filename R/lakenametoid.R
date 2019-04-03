#' Lake Name to LakeIds
#'
#' This function creates a data frame with the LakeIds for all lakes with the input name, as well as the County they are located in. If multiple lakes have the
#' same name, this will return a list of LakeIds.
#' @param lakename name of the lake.
#' @param county county where the lake is located. Default is NULL.
#' @keywords lakeid dowlknum
#' @return a data.frame
#' @family Sentinel Lakes Tools
#' @examples
#' lakenametoid("Echo")
#'
#' @export

lakenametoid=function(lakename,county=NULL){

  lakedata=mnsentinellakes::mnlakesmetadata

  if (!is.null(county)){
    lakedata=lakedata[lakedata$County==county,]
  }
  lakeoutput=lakedata$LakeId[lakedata$Lake==lakename]

  if (length(lakeoutput)>1)(
    warning("There are multiple lakes with this name.")
  )else if (length(lakeoutput)==0){
    warning("There are no lakes with this name.")
  }
  return(lakeoutput)
}
