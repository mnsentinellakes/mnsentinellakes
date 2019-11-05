#' Fix Minnesota Lake IDs
#'
#' This function ensures Minnesota Lake IDs (also known as DOWLKNUMs) are characters with "0" at the front and do not have any dashes.
#' @param lakeid lakeid number
#' @keywords lakeid dowlknum
#' @return a character
#' @family Sentinel Lakes Tools
#' @examples
#' fixlakeid(6000200)
#' fixlakeid("06-0002-00")
#'
#' @export

fixlakeid=function(lakeid){
  lakeid=gsub("-","",as.character(lakeid))
  lakeid[nchar(lakeid)==7]=paste0("0",lakeid[nchar(lakeid)<8])
  return(lakeid)
}
