#' Download Minnesota Climatology Lake Ice Data
#'
#' This function downloads lake ice in and out dates from the Minnesota Climatology website: https://www.dnr.state.mn.us/ice_out/index.html and
#' https://www.dnr.state.mn.us/ice_in/index.html. The data are already formatted into the Sentinel Lakes format.
#' @param lakeid a character indicating the LakeId (DOWLKNUM) for the lake to be downloaded.
#' @keywords ice climate data download
#' @return a data.frame with lake ice data.
#' @family Ice
#' @examples
#' \dontrun{
#' x <- icedownload("21005700")
#' }
#' @export


icedownload = function(lakeid){

  #Get lake name
  lakename = mnsentinellakes::mnlakesmetadata$Lake[mnsentinellakes::mnlakesmetadata$LakeId==mnsentinellakes::fixlakeid(lakeid)]
  #Ensure lakeid is legit
  if (length(lakename) > 0){
    iceoutput = NULL
    #Loop through in and out
    for (i in c("in","out")){
      if (i == "in"){
        icestatus = "In"
      }else if (i == "out"){
        icestatus = "Out"
      }

      #Build URL
      inputurl = paste0("https://maps.dnr.state.mn.us/cgi-bin/climatology/ice_",i,"_by_lake_as_csv.cgi?id=",lakeid)
      #Download data
      icedata = readr::read_csv(inputurl,skip = 2,show_col_types = FALSE)

      #Build data frame
      if(nrow(icedata) > 0){
        colnames(icedata)[1] = "Date"

        icedata = data.frame("Lake" = lakename,"LakeId" = lakeid,"Date" = icedata$Date,"Ice_Status" = icestatus,
                             "Source" = icedata$Source,"Comments" = NA)

        iceoutput = rbind(iceoutput,icedata)
      }
    }

    #Arrange data
    if (!is.null(iceoutput)){
      iceoutput = dplyr::arrange(iceoutput,Date)
    }else{
      warning("No ice data for this lake.")
    }
  }else{
    iceoutput = print("No ice data for this lake")
  }

  return(iceoutput)
}
