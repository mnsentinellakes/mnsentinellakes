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
  if(commonname %in% mnsentinellakes::fishabbreviations$Name){
    fishnameout=mnsentinellakes::fishabbreviations$Scientific_Name[mnsentinellakes::fishabbreviations$Name==commonname]
  }else{
    warning("No scientific name associated with this common name")
    fishnameout=NULL
  }
  return(fishnameout)
}
