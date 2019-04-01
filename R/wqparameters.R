#' Water Quality Parameters
#'
#' This function returns a vector of the water quality parameters present in data downloaded from the Minnesota Pollution Control Agency's EDA website.
#' @param wqdata a water quality data.frame downloaded using the wqdatadownload() function.
#' @param minsample the minimum number of parameter samples required to be included in the list. Defaults to 0.
#' @keywords mnpca minnesota pollution water quality parameters
#' @return A vector of water quality parameters.
#' @family Water Quality
#' @examples
#' x <- wqdatadownload("15-0010-00-100")
#' wqparameters(x)
#'
#' @export

wqparameters=function(wqdata,minsample=0){
  counttable=dplyr::count(wqdata,parameter)

  output=counttable$parameter[counttable$n>=minsample]
  return(output)
}
