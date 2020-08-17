#' Download MNPCA Water Quality Data
#'
#' This function allows you to download data directly from the Minnesota Pollution Control Agency's EDA website.
#' @param stationids a character vector of station ids in the format "##-####-##-###".
#' @keywords mnpca minnesota pollution water quality download.
#' @return data.frame with the downloaded data.
#' @family Water Quality
#' @examples
#' wqdatadownload("15-0010-00-100")
#' wqdatadownload(c("15-0010-00-100","15-0010-00-101","15-0010-00-102"))
#'
#' @export

wqdatadownload = function(stationids){
  wqdata=NULL
  for (i in stationids) {
    wqdatadown = as.data.frame(
      data.table::fread(
        paste0("https://services.pca.state.mn.us/api/v1/surfacewater/monitoring-stations/results?stationId=",i,"&format=csv"),
        stringsAsFactors = FALSE
      )
    )



    if (nrow(wqdatadown)>0){
      wqdata=rbind(wqdata,wqdatadown)
    }
  }

  if(is.null(wqdata)){
    warning("There are no data for these stationids")
  }

  return(wqdata)
}
