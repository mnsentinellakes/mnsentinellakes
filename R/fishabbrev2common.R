#' Fish Abbreviations to Common Names
#'
#' A function that returns the common name of a fish based upon the fish abbreviation provided.
#' @param fishabbreviation a three character string
#' @return a character
#' @family Fish
#' @examples
#' fishabbrev2common("BLG")
#'
#' @export

fishabbrev2common=function(fishabbreviation){
  if(fishabbreviation %in% mnsentinellakes::fishabbreviations$Code){

    fishnameout=mnsentinellakes::fishabbreviations$Name[mnsentinellakes::fishabbreviations$Code==fishabbreviation & mnsentinellakes::fishabbreviations$Prime==TRUE]
  }else{
    warning("No name associated with this abbreviation")
    fishnameout=NULL
  }
  return(fishnameout)
}
