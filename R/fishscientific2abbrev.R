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
  if(scientificname %in% mnsentinellakes::fishabbreviations$Scientific_Name){
    fishabbrevout=mnsentinellakes::fishabbreviations$Code[mnsentinellakes::fishabbreviations$Scientific_Name==scientificname]
  }else{
    warning("No abbreviation associated with this name")
    fishabbrevout=NULL
  }
  return(fishabbrevout)
}
