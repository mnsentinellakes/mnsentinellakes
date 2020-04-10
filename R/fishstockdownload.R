#' Download Fish Stocking Data
#'
#' This function downloads fish stocking data from the MNDNR Lakefinder website.
#' @param lakeid Minnesota lake identifier (DOWLKNUM) for the lake of interest.
#' @keywords fish stocking Minnesota data
#' @return a data.frame with water level data
#' examples
#' x <- fishstockdownload("11041300")
#'
#' @export

fishstockdownload=function(lakeid){

  #Build the URL where the table is located
  buildurl=paste0("https://www.dnr.state.mn.us/lakefind/showstocking.html?downum=",lakeid,"&context=desktop")

  #Download the data from lakefinder

  downloaddata=rvest::html_table(rvest::html_nodes(xml2::read_html(buildurl),'table'))


  #Format the data
  if (length(downloaddata)>0){
    downloaddata=downloaddata[[1]]

    #Fill in blank years using the year of the row above
    downloaddata=tidyr::fill(downloaddata,downloaddata$Year)

    downloaddata$Number=as.numeric(gsub(",","",downloaddata$Number))
    downloaddata=data.frame("Lake"=mnsentinellakes::lakeid2name(lakeid),"LakeId"=lakeid,"Year"=downloaddata$Year,"Species"=downloaddata$Species,"Size"=downloaddata$Size,
                            "Number"=downloaddata$Number,"Pounds"=downloaddata$Pounds)
  }else{
    downloaddata=NULL
    warning("No stocking data available")
  }
  return(downloaddata)

}
