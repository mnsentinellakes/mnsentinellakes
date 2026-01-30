#' Fish Data table
#'
#' This function selects fish catch data for the appropriate gear for the list of fish species in the fishmetadata table.
#'
#' Some surveys use Shallow and Deep gillnets as opposed to Standard gillnets. When this occurs, the total catch between those two gear types will be added and divided by the
#' gear count.
#'
#' Some data have different units for total weights and average weights. In some cases both measurements are in pounds, but in other cases total weights are in grams and
#' average weights are in pounds. Currently this function maintains these differences.
#'
#'
#' @param fishsurvey results from the fishsurveydata() function.
#' @param fishspecies a list of fish species names present in the fishspeciesmetadata table. Valid species include: "White Sucker", "Black Crappie", "Bluegill",
#' "Largemouth Bass", "Rock Bass", "Smallmouth Bass", "Muskellunge", "Northern Pike", "Black Bullhead", "Brown Bullhead", "Channel Catfish", "Yellow Bullhead",
#' "White Bass", "Walleye", "Yellow Perch", "Cisco", and "Lake Trout".
#' @param startyear a numeric of the earliest year under consideration in the YYYY format. Default is NULL.
#' @param endyear a numeric of the latest year under consideration in the YYYY format. Default is NULL.
#' @keywords fish lakefinder survey gear
#' @return a data.frame of fish catch data
#' @family Fish
#' @examples
#' \dontrun{
#' #Retrieve the lakefinder data
#' x <- lakefinderdownload("11041300")
#' #Extract the fish survey data
#' y <- fishsurveydata(x)
#' #Select the fish catch data
#' z <-fishtable(
#'       fishsurvey = y,
#'       fishspecies = c("Bluegill","Largemouth Bass"))
#' }
#' @export

fishtable = function(fishsurvey,fishspecies,startyear = NULL,endyear = NULL){

  if(!is.null(fishsurvey)){
    if (!is.null(startyear)){
      fishsurvey = fishsurvey[lubridate::year(fishsurvey$date) >= startyear,]
    }

    if (!is.null(endyear)){
      fishsurvey = fishsurvey[lubridate::year(fishsurvey$date <= endyear),]
    }

    LOIData = NULL
    for (i in 1:length(fishspecies)){

      speciescode = mnsentinellakes::fishspeciesmetadata$Code[mnsentinellakes::fishspeciesmetadata$Name %in% fishspecies[i]]
      speciesgear = unique(mnsentinellakes::fishspeciesmetadata$Gear[mnsentinellakes::fishspeciesmetadata$Name %in% fishspecies[i]])

      if(!is.null(fishspecies[i])){
        if (!is.na(fishspecies[i])){
          LOIDatarow = fishsurvey[fishsurvey$species %in% speciescode & fishsurvey$gear %in% speciesgear,]

          if (speciesgear == "Standard gill nets"){
            #Additional data for lakes with shallow and deep gill data
            if ("Shallow gill nets" %in% fishsurvey$gear | "Deep gill nets"%in% fishsurvey$gear){
              LOIDataadd = fishsurvey[which(fishsurvey$species %in% speciescode & fishsurvey$gear %in% c("Shallow gill nets","Deep gill nets")),]
              LOIDatacombo = NULL
              for (j in unique(LOIDataadd$date)){

                LOIDataselect = LOIDataadd[which(LOIDataadd$date==j),]

                cpue = round(sum(LOIDataselect$totalcatch) / sum(LOIDataselect$gearcount),digits = 2)
                totalcatch = sum(LOIDataselect$totalcatch)
                totalweight = sum(LOIDataselect$totalweight)

                if ((mean(as.numeric(LOIDataselect$averageweight)) - (totalweight/totalcatch)) <= 0.0001){
                  averageweight = round(totalweight / totalcatch,digits = 2)
                }else{
                  averageweight = round((totalweight / totalcatch) * 0.00220462,digits = 2)
                }
                gearcount = sum(LOIDataselect$gearcount)

                LOIDatacomborow = data.frame(
                  "quartilecount" = "N/A",
                  "cpue" = cpue,
                  "totalcatch" = totalcatch,
                  "species" = unique(LOIDataselect$species),
                  "totalweight" = totalweight,
                  "quartileweight" = "N/A",
                  "averageweight" = averageweight,
                  "gearcount" = gearcount,
                  "gear" = "Shallow and Deep gill nets",
                  "date"=as.Date(j,origin = "1970-01-01"),
                  "basin_id"=unique(LOIDataselect$basin_id),
                  "ecoregion"=unique(LOIDataselect$ecoregion))

                if (LOIDatacomborow$totalweight / LOIDatacomborow$totalcatch != LOIDatacomborow$averageweight){
                  LOIDatacomborow$averageweight = round(LOIDatacomborow$totalweight / LOIDatacomborow$totalcatch * 0.00220462,digits = 2)
                }

                LOIDatacombo = rbind(LOIDatacombo,LOIDatacomborow)
              }
              LOIDatarow = rbind(LOIDatarow,LOIDatacombo)
            }
          }

          if (nrow(LOIDatarow) > 0){
            LOIDatarow = LOIDatarow
          }else{
            LOIDatarow = LOIDatarow[nrow(LOIDatarow) + 1,]
            LOIDatarow[1,1:11] = NA
            LOIDatarow$species = fishspecies[i]
          }
          LOIData = rbind(LOIData,LOIDatarow)
        }
      }
    }
    LOIData["year"] = lubridate::year(LOIData$date)
    LOIData = LOIData[!is.na(LOIData$cpue),]
    LOIData = LOIData[order(LOIData$date),]
  }else{
    LOIData= NULL
    warning("No Fish Survey Data Available")
  }
  return(LOIData)
}
