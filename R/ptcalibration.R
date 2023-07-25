#' Pressure Transducer Calibration
#'
#' This function calibrates pressure transducer data to known staff gauge or other lake level measurements.
#' @param ptdata a data frame of pressure transducer data with "Date", "Time", and "Value" fields.
#' @param lakeid the lakeid for the lake where the pressure transducer is located.
#' @param staffgaugereadings a data frame staff gauge or other lake level measurement data that the pressure transducer data are calibrated to. Includes "Lake",
#' "Gauge_Reading", "Date", "Time", and "Notes" fields.
#' @return a list of two data frames, one with the calibrated data and the other with metadata.
#' @family Water Levels
#' @examples
#' \dontrun{
#' x <- ptcalibration(
#'        ptdata = data,
#'        lakeid = "15001000",
#'        staffgaugereadings = staffgauge
#'      )
#' }
#'
#' @export

ptcalibration = function(ptdata,lakeid,staffgaugereadings){

  #Convert inputs to data frames
  ptdata = as.data.frame(ptdata)
  staffgaugereadings = as.data.frame(staffgaugereadings)

  #Update pt data column names
  colnames(ptdata) = c("date_time","value")

  #get lake name from lakeid
  lakename = mnsentinellakes::lakeid2name(lakeid)

  #Select Staff Gauge Readings for Lake
  staffgaugereadings = staffgaugereadings[which(staffgaugereadings$Lake == lakename),]

  #Remove NA Values in the pt data
  ptdata = ptdata[which(!is.na(ptdata$value)),]

  #Check if there are any staffgaugereadings for the identified lake
  if (nrow(staffgaugereadings) > 0){
    #Get the min and max dates in the pt data
    mindate = as.Date(min(ptdata$date_time))
    maxdate = as.Date(max(ptdata$date_time))

    #Determine if there are any non-NA times in the staff gauge readings and remove NAs
    if (any(!is.na(staffgaugereadings$Time))){
      staffgaugereadings = staffgaugereadings[which(!is.na(staffgaugereadings$Time)),]
    }

    #Select staff gauge readings that only fall withing the min and max dates of the data. If there are still multiple rows, select the first.
    staffgaugereadings = staffgaugereadings[which(staffgaugereadings$Date >= mindate & staffgaugereadings$Date <= maxdate),]
    staffgaugereadings = staffgaugereadings[1,]
    print(staffgaugereadings$Time)

    if (!is.na(staffgaugereadings$Time)){
      message("Using Provided Time to Calibrate Data")

      #Get time zone from the pt data
      tzid = lubridate::tz(ptdata$date_time)

      #Get staffgauge date and time
      staffgaugedate = staffgaugereadings$Date
      staffgaugetime = as.character(format(staffgaugereadings$Time,format = "%H:%M:%S"))

      #Get the date and time of the staff gauge reading
      staffgaugedatetime = as.POSIXct(paste(staffgaugedate,staffgaugetime),
                                      format = "%Y-%m-%d %H:%M",tz = tzid)

      #Calculate the time differences between each pt data point and the staffgaugedatetime
      ptdata$diff_time = abs(ptdata$date_time - staffgaugedatetime)

      #Get calibration value (pt - sg)
      ptvalue = ptdata$value[which(ptdata$diff_time == min(ptdata$diff_time))][1]
      sgvalue = staffgaugereadings$Gauge_Reading

      calvalue = round(ptvalue - sgvalue,2)
      offsetmethod = "Calculated the offset using the difference between the pressure transducer value and the staff gauge reading for the indicated date and time."
    }else{
      message("Using Daily Mean to Calibrate Data")

      staffgaugedate = staffgaugereadings$Date
      staffgaugetime = staffgaugereadings$Time

      #Calculate the doy of of the staff gauge reading
      sfdoy = lubridate::yday(staffgaugereadings$Date)

      #Calculate doy of the pt data
      ptdata$doy = lubridate::yday(ptdata$date_time)

      #Get mean pt value for sf doy
      ptvalue = mean(ptdata[which(ptdata$doy == sfdoy),2])
      sgvalue = staffgaugereadings$Gauge_Reading

      calvalue = round(ptvalue - sgvalue)
      offsetmethod = "Specific time not recorded for staff gauge reading, calculated the offset using the difference between the mean daily pressure transducer value
      and the staff gauge reading for the indicated date."
    }

    dataoutput = data.frame("Lake" = lakename,"LakeId" = lakeid,"Date_Time" = ptdata$date_time,
                            "Transducer_Value" = ptdata$value,"Adjusted_Value" = round(ptdata$value - calvalue,digits = 2))

    datametadata = data.frame("Lake" = lakename,"LakeId" = lakeid,"Offset_Date" = staffgaugedate,"Offset_Time" = staffgaugetime,"Transducer" = ptvalue,
                              "Staff_Gauge" = sgvalue,"Offset_value" = calvalue,"Offset_Method" = offsetmethod,"Notes" = staffgaugereadings$Notes)


  }else{
    message(paste0("No staff gauge readings for ",lakename))
  }

  output = list(dataoutput,datametadata)

  return(output)
}
