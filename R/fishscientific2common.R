#' Fish Scientific Names to Common Names
#'
#' A function that returns the common name(s) of a fish based upon the scientific name provided.
#' @param scientificname a character string
#' @return a character
#' @family Fish
#' @examples
#' fishscientific2common("Lepomis macrochirus")
#'
#' @export

fishscientific2common=function(scientificname){
  getcommon=mnsentinellakes::fishabbreviations$Name[which(mnsentinellakes::fishabbreviations$Prime==TRUE)]
  names(getcommon)=mnsentinellakes::fishabbreviations$Scientific_Name[which(mnsentinellakes::fishabbreviations$Prime==TRUE)]
  fishcommon=unname(getcommon[scientificname])
  return(fishcommon)
}
