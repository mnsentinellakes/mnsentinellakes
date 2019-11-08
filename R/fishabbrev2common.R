#' Fish Abbreviations to Common Names
#'
#' A function that returns the common name of a fish based upon the fish abbreviation provided.
#' @param fishabbreviation a three character string
#' @return a character
#' @family Fish
#' @examples
#' fishabbrev2common("BLG")
#'
#' @export

fishabbrev2common=function(fishabbreviation){
    getcommon=mnsentinellakes::fishabbreviations$Name[which(mnsentinellakes::fishabbreviations$Prime==TRUE)]
    names(getcommon)=mnsentinellakes::fishabbreviations$Code[which(mnsentinellakes::fishabbreviations$Prime==TRUE)]
    fishcommon=unname(getcommon[fishabbreviation])

  return(fishcommon)
}
