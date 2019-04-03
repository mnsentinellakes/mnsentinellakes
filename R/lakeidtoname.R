#' LakeId to Lake Name
#'
#' This function returns the name of the lake for the given LakeId.
#' @param lakeid LakeId number.
#' @keywords lakeid lakename dowlknum
#' @return a character
#' @family Sentinel Lakes Tools
#' @examples
#' lakeidtoname("02000400")
#'
#' @export

lakeidtoname=function(lakeid){

  lakename=mnsentinellakes::mnlakesmetadata$Lake[mnsentinellakes::mnlakesmetadata$LakeId==mnsentinellakes::fixlakeid(lakeid)]

  if (length(lakename)==0){
    lakename=NULL
    warning("No lakes with that LakeId.")
  }
  return(lakename)
}
