#' Basin Ids to Basin Names
#'
#' This function returns the name of the Basin for the given Basin Id.
#' @param basinids Basin Id number
#' @param mainbasin Should the results return the name of the main basin or sub-basin (if applicable)
#' @keywords basinid basinname dowlknum
#' @return a character
#' @family Sentinel Lakes Tools
#' @examples
#' basinid2name("02000400")
#'
#' @export

basinid2name = function(basinids,mainbasin = TRUE){

  #Retrieve all lake names
  if(mainbasin == TRUE){
    getnames = mnsentinellakes::basin_ids_names$parent_basin_name
  }else{
    getnames = mnsentinellakes::basin_ids_names$basin_name
  }

  #Name all lake names using the LakeId
  names(getnames) = mnsentinellakes::basin_ids_names$basin_id

  #Select lake names based on the input lakeid
  basinname = unname(getnames[mnsentinellakes::fixbasinid(basinids)])

  if (length(basinname) == 0){
    basinname = NULL
    warning("No basin names with one or more of the input basinids.")
  }
  return(basinname)
}
