#' Add Underscore
#'
#' A function that removes punctuation and spaces from a character string
#' @param term a character string with punctuation and spaces.
#' @return a character
#' @family Sentinel Lakes Tools
#' @examples
#' addunderscore("St. James")
#'
#' @export

addunderscore=function(term){
  output=gsub("\\s+","_",gsub("[[:punct:]]","",term))
  return(output)
}
