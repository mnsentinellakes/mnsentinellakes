#' Fish Data table
#'
#' This function selects fish catch data for the appropriate gear for the list of fish species in the fishmetadata table.
#' @param fishsurvey results from the fishsurveydata() function.
#' @param fishspecies a list of fish species names present in the fishspeciesmetadata table. Valid species include: "White Sucker", "Black Crappie", "Bluegill",
#' "Largemouth Bass", "Rock Bass", "Smallmouth Bass", "Muskellunge", "Northern Pike", "Black Bullhead", "Brown Bullhead", "Channel Catfish", "Yellow Bullhead",
#' "White Bass", "Walleye", "Yellow Perch", and "Lake Trout".
#' @param startyear a numeric of the earliest year under consideration in the YYYY format. Default is NULL.
#' @param endyear a numeric of the latest year under consideration in the YYYY format. Default is NULL.
#' @keywords fish lakefinder survey gear
#' @return a data.frame of fish catch data
#' @family Fish
#' @examples
#' #Retrieve the lakefinder data
#' x <- lakefinderdownload("11041300")
#' #Extract the fish survey data
#' y <- fishsurveydata(x)
#' #Select the fish catch data
#' z <-fishtable(
#'       fishsurvey = y,
#'       fishspecies = c("Bluegill","Largemouth Bass"))
#'
#' @export

fishtable=function(fishsurvey,fishspecies,startyear=NULL,endyear=NULL){
  if(!is.null(fishsurvey)){
    if (!is.null(startyear)){
      fishsurvey=fishsurvey[lubridate::year(fishsurvey$Date)>=startyear,]
    }

    if (!is.null(endyear)){
      fishsurvey=fishsurvey[lubridate::year(fishsurvey$Date<=endyear),]
    }

    LOIData=NULL
    for (i in 1:length(fishspecies)){

      speciescode=mnsentinellakes::fishspeciesmetadata$Code[mnsentinellakes::fishspeciesmetadata$Name==fishspecies[i]]
      speciesgear=mnsentinellakes::fishspeciesmetadata$Gear[mnsentinellakes::fishspeciesmetadata$Name==fishspecies[i]]
      if(!is.null(fishspecies[i])){
        if (!is.na(fishspecies[i])){
          LOIDatarow=fishsurvey[fishsurvey$species==speciescode & fishsurvey$gear==speciesgear,]
          if (nrow(LOIDatarow)>0){
            LOIDatarow=LOIDatarow
          }else{
            LOIDatarow=LOIDatarow[nrow(LOIDatarow)+1,]
            LOIDatarow[1,1:11]=NA
            LOIDatarow$species=fishspecies[i]
          }
          LOIData=rbind(LOIData,LOIDatarow)
        }
      }
    }
    LOIData["Year"]=lubridate::year(LOIData$Date)
    LOIData=LOIData[!is.na(LOIData$CPUE),]
    LOIData=LOIData[order(LOIData$Date),]
  }else{
    LOIData=NULL
    warning("No Fish Survey Data Available")
  }
  return(LOIData)
}
