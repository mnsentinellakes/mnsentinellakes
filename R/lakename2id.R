#' Lake Name to LakeIds
#'
#' This function creates a data frame with the LakeIds for all lakes with the input name, as well as the County they are located in. If multiple lakes have the
#' same name, this will return a list of LakeIds.
#' @param lakename name of the lake.
#' @param county county where the lake is located. Default is NULL.
#' @keywords lakeid dowlknum
#' @return a data.frame
#' @family Sentinel Lakes Tools
#' @examples
#' lakename2id("Echo")
#'
#' @export

lakename2id=function(lakename,county=NULL){

  getids=lapply(
    lakename, function(x){
      ids=mnsentinellakes::mnlakesmetadata$LakeId
      names(ids)=mnsentinellakes::mnlakesmetadata$Lake
      ids=unname(ids[names(ids) == x])
    }
  )

  if(!is.null(county)){

    getids=lapply(1:length(getids), function(x){
      countyselect=county[x]
      idselect=unlist(getids[x])

      getcounties=mnsentinellakes::mnlakesmetadata$LakeId
      names(getcounties)=mnsentinellakes::mnlakesmetadata$County
      countyids=as.vector(unname(getcounties[names(getcounties) %in% countyselect]))
      unlist(idselect[idselect %in% countyids])
    }
    )
  }
  getids=unlist(getids)

  if (length(getids)>length(lakename)){
    if (length(lakename)==1){
      warning("There are multiple lakes with this name")
    }else if (length(lakename)>1){
      warning("There are multiple lakes with one or both of these names")
    }
  }
  return(getids)
}
