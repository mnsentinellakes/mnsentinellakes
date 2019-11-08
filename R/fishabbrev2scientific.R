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
  getscientific=mnsentinellakes::fishabbreviations$Scientific_Name[which(mnsentinellakes::fishabbreviations$Prime==TRUE)]
  names(getscientific)=mnsentinellakes::fishabbreviations$Code[which(mnsentinellakes::fishabbreviations$Prime==TRUE)]
  fishscientific=unname(getscientific[fishabbreviation])

  return(fishscientific)
}
