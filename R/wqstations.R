#' Water Quality Stations
#'
#' This function returns a list of station ids located on the lake with the supplied LakeId.
#' @param lakeid a character indicating the LakeId (DOWLKNUM) for the lake to be downloaded.
#' @keywords mnpca water quality minnesota pollution control stations
#' @family Water Quality
#' @return a list of water quality station ids
#' @export

wqstations = function(lakeid){

  if (lakeid %in% unique(mnsentinellakes::mnpcastations$LakeId) | lakeid %in% unique(mnsentinellakes::mnpcastations$BasinId)){
    stations = mnsentinellakes::mnpcastations$StationId[which(mnsentinellakes::mnpcastations$LakeId == lakeid)]
    if (is.null(stations)){
      stations = mnsentinellakes::mnpcastations$StationId[which(mnsentinellakes::mnpcastations$BasinId == lakeid)]
    }

  }else{
    stations = NULL
    warning("There are no stations on this lake.")
  }
  return(stations)
}
