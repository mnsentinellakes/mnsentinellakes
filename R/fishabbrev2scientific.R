#' Fish Abbreviations to Scientific Names
#'
#' A function that returns the scientific name of a fish based upon the fish abbreviation provided.
#' @param fishabbreviation a three character string
#' @return a character
#' @family Fish
#' @examples
#' fishabbrev2scientific("BLG")
#'
#' @export

fishabbrev2scientific=function(fishabbreviation){
  if(fishabbreviation %in% mnsentinellakes::fishabbreviations$Code){
    fishnameout=mnsentinellakes::fishabbreviations$Scientific_Name[mnsentinellakes::fishabbreviations$Code==fishabbreviation]
  }else{
    warning("No name associated with this abbreviation")
    fishnameout=NULL
  }
  return(fishnameout)
}
