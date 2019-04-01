#' Download Lakefinder Data
#'
#' This function downloads the MNDNR Lakefinder website json tables for the selected lake. It was developed by user hrbrmstr at https://stackoverflow.com/questions/46517463/scraping-html-data-table-using-rvest.
#' @param lakeid a character indicating the LakeId (DOWLKNUM) for the lake to be downloaded.
#' @keywords lakefinder survey data natural resources minnesota
#' @return a list of data
#' @family Lake
#' @family Fish
#' @examples
#' x <- lakefinderdownload("11041300")
#'
#' @export

lakefinderdownload = function(lakeid) {
  lakeid = lakeid[1]
  if (grepl("^htt", lakeid)) {
    tmp = httr::parse_url(lakeid)
    if (!is.null(tmp$query$downum)) {
      lakeid <- tmp$query$downum
    } else {
      stop("Invalid URL specified", call.=FALSE)
    }
  }
  httr::GET(
    url = "http://maps2.dnr.state.mn.us/cgi-bin/lakefinder/detail.cgi",
    query = list(
      type = "lake_survey",
      callback = "",
      id = lakeid,
      `_` = as.numeric(Sys.time())
    )
  ) -> res
  httr::stop_for_status(res)
  out = httr::content(res, as="text", encoding="UTF-8")
  out = jsonlite::fromJSON(out, flatten=TRUE)
  out[["LakeId"]]=lakeid
  out
}
