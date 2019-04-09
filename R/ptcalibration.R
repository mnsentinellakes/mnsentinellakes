#' Pressure Transducer Calibration
#'
#' This function calibrates pressure transducer data to known staff gauge or other lake level measurements.
#' @param ptdata a data frame of pressure transducer data with "Date", "Time", and "Value" fields.
#' @param lakeid the lakeid for the lake where the pressure transducer is located.
#' @param staffgauge a data frame staff gauge or other lake level measurement data that the pressure transducer data are calibrated to. Includes "Lake",
#' "Gauge_Reading", "Date", "Time", and "Notes" fields.
#' @param notes any additional notes for this transducer and time period. If NA, the function will look for notes in the staffgauge data.
#' @return a list of two data frames, one with the calibrated data and the other with metadata.
#' @family Water Levels
#' @examples
#' \dontrun{
#' x <- ptcalibration(
#'        ptdata = data,
#'        lakeid = "15001000",
#'        staffgauge = staffgauge
#'      )
#' }
#'
#' @export

ptcalibration=function(ptdata,lakeid,staffgauge,notes=NA){

  ptdata=as.data.frame(ptdata)

  ptdata[,3]=as.numeric(ptdata[,3])
  ptdata["date_time"]=as.POSIXct(paste(as.character(ptdata[,1]),as.character(ptdata[,2],format="%H:%M")), format="%Y-%m-%d %H:%M")

  staffgauge=staffgauge[staffgauge$Lake==mnsentinellakes::lakeid2name(lakeid),]
  staffgauge["date_time"]=as.POSIXct(paste(as.character(staffgauge$Date),as.character(staffgauge$Time,format="%H:%M")), format="%Y-%m-%d %H:%M")
  if(is.na(staffgauge$Time)){
    if(staffgauge$Date %in% ptdata[,1]){
      print("Method 1")
      ptdataday=ptdata[as.Date(ptdata[,4])==as.Date(staffgauge$date_time),]
      ptdatadaymean=mean(ptdataday[,3])
      offset=ptdatadaymean-as.numeric(as.character(staffgauge$Gauge_Reading))
      offsetdatetime=staffgauge$date_time
      offsetmethod="Specific time not recorded for staff gauge reading, calculated the offset using the difference between the mean daily pressure transducer value and the staff gauge reading for the indicated date."
    }else{
      stop("The date of the staff gauge reading is not in the pressure transducer data.")
    }
  }else{
    if(staffgauge$Date %in% ptdata[,1] & staffgauge$Time %in% ptdata[,2]){
      print("Method 2")
      ptdatatime=ptdata[ptdata[,4]==staffgauge$date_time,]
      offset=ptdatatime[,3]-staffgauge$Gauge_Reading
      ptdatadaymean=NA
      offsetdatetime=staffgauge$date_time
      offsetmethod="Calculated the offset using the difference between the pressure transducer value and the staff gauge reading for the indicated date and time."
    }else{
      stop("The date and time of the staff gauge reading is not in the pressure transducer data.")
    }
  }

  if(is.na(notes) & !is.na(staffgauge$Notes)){
    notes=staffgauge$Notes
  }

    dataoutput=data.frame("Lake"=mnsentinellakes::lakeid2name(lakeid),"LakeId"=lakeid,"Date_Time"=ptdata[,4],
                          "Original_Vaue"=ptdata[,3],"Adjusted_Value"=round(ptdata[,3]-offset,digits = 2))
    datametadata=data.frame("Lake"=mnsentinellakes::lakeid2name(lakeid),"LakeId"=lakeid,
                            "Offset_Date_Time"=offsetdatetime,"Offset_value"=offset,
                            "Offset_Method"=offsetmethod,"Notes"=notes)

    outputdata=list("Data"=dataoutput,"Metadata"=datametadata)

  return(outputdata)
}
