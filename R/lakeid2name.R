#' LakeId to Lake Name
#'
#' This function returns the name of the lake for the given LakeId.
#' @param lakeid LakeId number.
#' @keywords lakeid lakename dowlknum
#' @return a character
#' @family Sentinel Lakes Tools
#' @examples
#' lakeid2name("02000400")
#'
#' @export

lakeid2name=function(lakeid){

  #Retrieve all lake names
  getnames=mnsentinellakes::mnlakesmetadata$Lake

  #Name all lake names using the LakeId
  names(getnames)=mnsentinellakes::mnlakesmetadata$LakeId

  #Select lake names based on the input lakeid
  lakename=unname(getnames[fixlakeid(lakeid)])

  if (length(lakename)==0){
    lakename=NULL
    warning("No lakes with that LakeId.")
  }
  return(lakename)
}


