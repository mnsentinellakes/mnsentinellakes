#' Fix Minnesota Basin IDs
#'
#' This function ensures Minnesota Basin Ids (also known as DOWLKNUMs) are characters with "0" at the front and do not have any dashes.
#' @param basinids Basin Id numbers
#' @keywords basinids dowlknum
#' @return a character
#' @family Sentinel Lakes Tools
#' @examples
#' fixbasinid(6000200)
#' fixbasinid("06-0002-00")
#'
#' @export

fixbasinid = function(basinids){
  basinids = gsub("-", "", as.character(basinids))
  basinids[nchar(basinids) == 7] = paste0("0",basinids[nchar(basinids) < 8])
  return(basinids)
}
