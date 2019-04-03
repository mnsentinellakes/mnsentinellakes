#' Fish Common Names to Abbreviations
#'
#' A function that returns the abbreviation of a fish based upon the common name provided.
#' @param commonname a character string
#' @return a character
#' @family Fish
#' @examples
#' fishcommon2abbrev("Bluegill")
#'
#' @export

fishcommon2abbrev=function(commonname){
  if(commonname %in% mnsentinellakes::fishabbreviations$Name){
    fishabbrevout=mnsentinellakes::fishabbreviations$Code[mnsentinellakes::fishabbreviations$Name==commonname]
  }else{
    warning("No abbreviation associated with this name")
    fishabbrevout=NULL
  }
  return(fishabbrevout)
}
