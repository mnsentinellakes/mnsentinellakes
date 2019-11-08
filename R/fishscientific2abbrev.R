#' Fish Scientific Names to Abbreviations
#'
#' A function that returns the abbreviation of a fish based upon the scientific name provided.
#' @param scientificname a character string
#' @return a character
#' @family Fish
#' @examples
#' fishscientific2abbrev("Lepomis macrochirus")
#'
#' @export

fishscientific2abbrev=function(scientificname){
  getabbrev=mnsentinellakes::fishabbreviations$Code[which(mnsentinellakes::fishabbreviations$Prime==TRUE)]
  names(getabbrev)=mnsentinellakes::fishabbreviations$Scientific_Name[which(mnsentinellakes::fishabbreviations$Prime==TRUE)]
  fishabbrev=unname(getabbrev[scientificname])

  return(fishabbrev)
}
