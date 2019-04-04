#' Lake Stratification Periods
#'
#' This function calculates the start and end of lake thermal stratification using daily lake temperature profile data. In this calculation, a lake is considered stratified when there is a difference of one degree C between one meter depth intervals anywhere within the water column.
#' @param tempdata a table with "Date","Depth", and "Temperature" fields.
#' @param consecutivedays the number of consecutive days of uninterrupted stratification required to consider the lake stratified for the season. Default is 10 days.
#' @keywords lake stratification temperature thermal profile
#' @return a data.frame indicating the start (S) and end (E) stratification dates.
#' @examples
#' \dontrun{
#' x <-stratificationperiod(
#'       tempdata = temperaturedata,
#'       consecutivedays = 15
#'       )
#' }
#' @export

stratificationperiod=function(tempdata,consecutivedays=10){
  #Format Data
  tempdata$Date=as.Date(tempdata$Date)
  tempdata$Depth=as.numeric(tempdata$Depth)
  tempdata=tempdata[!is.na(tempdata$Temperature),]


  #List of unique depths
  stratificationdates=unique(tempdata$Date)
  stratification=NULL

  #Loop through each date and determine if it is stratified
  for (i in stratificationdates){
    stratdate=tempdata[tempdata$Date==i,]
    #
    strat=diff(stratdate$Temperature)/diff(stratdate$Depth)
    if (any((strat*-1)>=1)){
      stratificationrow=data.frame("Date"=as.Date(i,origin="1970-01-01"),"Stratified"=T)
    }else{
      stratificationrow=data.frame("Date"=as.Date(i,origin="1970-01-01"),"Stratified"=F)
    }
    stratification=rbind(stratification,stratificationrow)
  }

  #Determine stratification start and end dates with a buffer of required consecutively stratified days
  if (consecutivedays==0){
    consecutivedays=1
  }
  stratevents=NULL
  for (i in c(consecutivedays+1:(nrow(stratification)-consecutivedays))){
    starttest=NULL
    endtest=NULL
    for (j in 1:consecutivedays){
      #Test each day within the buffer zone follows these rules
      startdaytest=as.logical(stratification[i,2]==TRUE & stratification[(i-1),2]==FALSE & stratification[(i+j),2]==TRUE)
      starttest=rbind(starttest,startdaytest)
      enddaytest=as.logical(stratification[i,2]==TRUE & stratification[(i+1),2]==FALSE & stratification[(i-j),2]==TRUE)
      endtest=rbind(endtest,enddaytest)
    }
    #If all days are TRUE, the day is considered to be the start or end of stratification
    if (all(starttest==TRUE)){
      startday=data.frame("Date"=stratification[i,1],"Event"="S")
      stratevents=rbind(stratevents,startday)
    }
    if (all(endtest==TRUE) & all(!is.na(endtest))){
      endday=data.frame("Date"=stratification[i,1],"Event"="E")
      stratevents=rbind(stratevents,endday)
    }
  }
  return(stratevents)
}
