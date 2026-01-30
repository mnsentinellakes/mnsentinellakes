#' Download Minnesota Climatology Lake Ice Data
#'
#' This function downloads lake ice in and out dates from the Minnesota Climatology website: https://www.dnr.state.mn.us/ice_out/index.html and
#' https://www.dnr.state.mn.us/ice_in/index.html. The data are already formatted into the Sentinel Lakes format.
#' @param basinid a character indicating the Basin Id (DOWLKNUM) for the lake to be downloaded.
#' @keywords ice climate data download
#' @return a data.frame with lake ice data.
#' @family Ice
#' @examples
#' \dontrun{
#' x <- icedownload("21005700")
#' }
#' @export


icedownload = function(basinid){

  #Get lake name
  basinname = basinid2name(fixbasinid(basinid))
  #Ensure lakeid is legit
  if (length(basinname) > 0){
    iceoutput = NULL
    #Loop through in and out
    for (i in c("in","out")){
      if (i == "in"){
        icestatus = "In"
      }else if (i == "out"){
        icestatus = "Out"
      }

      #Build URL
      inputurl = paste0("https://maps.dnr.state.mn.us/cgi-bin/climatology/ice_",i,"_by_lake_as_csv.cgi?id=",basinid)
      #Download data
      icedata = readr::read_csv(inputurl,skip = 2,show_col_types = FALSE)

      #Build data frame
      if(nrow(icedata) > 0){
        colnames(icedata)[1] = "Date"

        icedata = data.frame("basin_name" = basinname,"basin_id" = basinid,"date" = icedata$Date,"ice_status" = icestatus,
                             "source" = icedata$Source,"comments" = NA)

        iceoutput = rbind(iceoutput,icedata)
      }
    }

    #Arrange data
    if (!is.null(iceoutput)){
      iceoutput = dplyr::arrange(iceoutput,.data$date)
    }else{
      warning("No ice data for this lake.")
    }
  }else{
    iceoutput = print("No ice data for this lake")
  }

  return(iceoutput)
}
