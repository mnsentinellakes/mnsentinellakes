#' Download Lakefinder Data
#'
#' This function downloads the MNDNR Lakefinder website json tables for the selected lake.
#' @param basinid a character indicating the Basin Id (DOWLKNUM) for the lake to be downloaded.
#' @keywords lakefinder survey data natural resources minnesota
#' @return a list of data
#' @family Lake
#' @family Fish
#' @examples
#' x <- lakefinderdownload("11041300")
#'
#' @export

lakefinderdownload = function(basinid) {
  basinid = mnsentinellakes::fixbasinid(basinid)

  res = httr::GET(
    url = "https://maps.dnr.state.mn.us/cgi-bin/lakefinder/detail.cgi",
    query = list(
      type = "lake_survey",
      callback = "",
      id = basinid,
      `_` = as.numeric(Sys.time())
    )
  )
  httr::stop_for_status(res)
  out = httr::content(res, as = "text", encoding = "UTF-8")
  out = jsonlite::fromJSON(out, flatten = TRUE)
  if (!is.null(out$result)){
    out[["basin_id"]] = basinid
  }else{
    warning(out$message)
    out = NULL
  }
  return(out)
}
