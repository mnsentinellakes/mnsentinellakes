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
  getabbrev=mnsentinellakes::fishabbreviations$Code
  names(getabbrev)=mnsentinellakes::fishabbreviations$Name
  fishabbrev=unname(getabbrev[tools::toTitleCase(commonname)])
  return(fishabbrev)
}
