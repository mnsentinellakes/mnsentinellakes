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
  if(scientificname %in% mnsentinellakes::fishabbreviations$Scientific_Name){
    fishnameout=mnsentinellakes::fishabbreviations$Name[mnsentinellakes::fishabbreviations$Scientific_Name==scientificname &
                                                          mnsentinellakes::fishabbreviations$Prime==TRUE]
    fishnameout=fishnameout[!is.na(fishnameout)]
  }else{
    warning("No common name associated with this scientific name")
    fishnameout=NULL
  }
  return(fishnameout)
}
