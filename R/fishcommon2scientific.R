#' Fish Common Names to Scientific Names
#'
#' A function that returns the scientific name of a fish based upon the common name provided.
#' @param commonname a character string
#' @return a character
#' @family Fish
#' @examples
#' fishcommon2scientific("Bluegill")
#'
#' @export

fishcommon2scientific=function(commonname){
  getscientific=mnsentinellakes::fishabbreviations$Scientific_Name
  names(getscientific)=mnsentinellakes::fishabbreviations$Name
  fishscientific=unname(getscientific[tools::toTitleCase(commonname)])
  return(fishscientific)
}
