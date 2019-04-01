#' Download Minnesota Climatology Lake Ice Data
#'
#' This function downloads lake ice in and out dates from the Minnesota Climatology website: https://www.dnr.state.mn.us/ice_out/index.html and
#' https://www.dnr.state.mn.us/ice_in/index.html. The data are already formatted into the Sentinel Lakes format.
#' @param lakeid a character indicating the LakeId (DOWLKNUM) for the lake to be downloaded.
#' @keywords ice climate data download
#' @return a data.frame with lake ice data.
#' @family Ice
#' @examples
#' x <- icedownload("21005700)
#' @export

icedownload=function(lakeid){

  lakename=mnsentinellakes::mnlakesmetadata$Lake[mnsentinellakes::mnlakesmetadata$LakeId==lakeid]
  if (length(lakename)>0){
  iceoutput=NULL
  for (i in c("in","out")){

    if (i=="in"){
      icestatus="In"
    }else if (i=="out"){
      icestatus="Out"
    }
    inputurl=paste0("https://maps1.dnr.state.mn.us/cgi-bin/climatology/ice_",i,"_by_lake.cgi?id=",lakeid)


    x=as.character(xml2::read_html(inputurl))
    x=as.character(regmatches(x,gregexpr("\\[.+\\]",x)))
    if(x=="character(0)"){
      reshapedf=NULL
    }else{
      x=gsub(c("\\["),"",x)
      x=gsub(c("\\]"),"",x)
      x=data.frame(strsplit(x,"[},{]"))
      colnames(x)="Row"
      x=suppressWarnings(tidyr::separate(
        x,
        "Row",
        into = c("Header","Value"),
        sep = "[:]"))
      x=x[!is.na(x$Value),]
      x$Header=gsub("\"","",x$Header)
      x$Value=gsub("\"","",x$Value)
      reshapedf=NULL

      for (k in 0:((nrow(x)/3)-1)){
        selection=(k*3)+c(1,2,3)
        rows=x[selection,]

        #Date
        daterow=rows$Value[which(rows$Header=="date")]
        #Source
        sourcerow=rows$Value[which(rows$Header=="source")]
        #Comment
        commentrow=rows$Value[which(rows$Header=="comments")]
        reshaperow=data.frame("Date"=daterow,"Source"=sourcerow,"Comment"=commentrow)
        reshapedf=rbind(reshapedf,reshaperow)
      }
      reshapedf["Ice_Status"]=icestatus
    }

    iceoutput=rbind(iceoutput,reshapedf)
  }
  iceoutput$Date=as.Date(iceoutput$Date)
  iceoutput=iceoutput[order(iceoutput$Date),]

  iceoutput=data.frame("Lake"=lakename,"LakeId"=lakeid,"Date"=iceoutput$Date,"Ice_Status"=iceoutput$Ice_Status,
                       "Source"=iceoutput$Source,"Comments"=iceoutput$Comment)

  }else{
    iceoutput=print("No ice data for this lake")
  }
  return(iceoutput)

}
