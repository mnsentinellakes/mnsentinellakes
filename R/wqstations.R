#' Water Quality Stations
#'
#' This function returns a list of station ids located on the lake with the supplied LakeId.
#' @param lakeid a character indicating the LakeId (DOWLKNUM) for the lake to be downloaded.
#' @keywords mnpca water quality minnesota pollution control stations
#' @family Water Quality
#' @return a list of water quality station ids
#' @export

wqstations = function(lakeid){

  if (lakeid %in% unique(mnsentinellakes::sentinelmnpcastations$LakeId)){
    stations=mnsentinellakes::sentinelmnpcastations$ID_CODE[mnsentinellakes::sentinelmnpcastations$LakeId==lakeid]
  }else if (lakeid %in% unique(mnsentinellakes::mnpcastations$LakeId)){
    stations=mnsentinellakes::mnpcastations$ID_CODE[mnsentinellakes::mnpcastations$LakeId==lakeid & mnsentinellakes::mnpcastations$Status=="Active"]
  }else{
    stations=NULL

    warning("There are no stations on this lake.")
  }
  return(stations)
}
