#' Fish Survey Data Sentinel Formatting
#' 
#' Reorganizes fish survey data downloaded from lakefinder into a common Sentinel Lakes format, making it easier to integrate with other Sentinel Lakes 
#' formatted datasets.
#' @param fishsurvey results from the fishsurveydata() function.
#' @keywords fish sentinel format
#' @family Formatting
#' @examples 
#' #' #Retrieve the lakefinder data
#' x <- lakefinderdownload("11041300")
#' 
#' #Extract the fish survey data
#' y <- fishsurveydata(x)
#' 
#' #Convert to Sentinel Lakes formatting
#' z <- fish2sentinel(y)
#' 
#' @export

fish2sentinel = function(fishsurvey){
  
  lakename=mnsentinellakes::lakeidtoname(unique(fishsurvey$LakeId))
  
  fishsurvey$quartileCount[fishsurvey$quartileCount=="N/A"]=NA
  fishsurvey$quartileWeight[fishsurvey$quartileWeight=="N/A"]=NA
  
  fishoutput=data.frame("Lake"=lakename,"LakeId"=unique(fishsurvey$LakeId),"Date"=fishsurvey$Date,"Species"=fishsurvey$species,
                        "Quartile_Count"=fishsurvey$quartileCount,"CPUE"=fishsurvey$CPUE,"Total_Catch"=fishsurvey$totalCatch,"Total_Weight"=fishsurvey$totalWeight,
                        "Quartile_Weight"=fishsurvey$quartileWeight,"Average_Weight"=fishsurvey$averageWeight,"Gear"=fishsurvey$gear,
                        "Gear_Count"=fishsurvey$gearCount)
  fishoutput=suppressWarnings(dplyr::left_join(fishoutput,mnsentinellakes::fishabbreviations,by=c("Species"="Code")))
  colnames(fishoutput)[names(fishoutput)=="Name"]="Species_Name"
  fishoutput=fishoutput[,c(1,2,3,4,13,5,6,7,8,9,10,11,12)]
  
  fishoutput=fishoutput[order(fishoutput$Date),]
  row.names(fishoutput)=NULL
  return(fishoutput)
}