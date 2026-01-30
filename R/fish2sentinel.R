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

  basin_name = mnsentinellakes::basinid2name(unique(fishsurvey$basin_id))

  fishsurvey$quartilecount[fishsurvey$quartilecount == "N/A"] = NA
  fishsurvey$quartileweight[fishsurvey$quartileweight == "N/A"] = NA

  fishoutput=data.frame(
    "basin_name" = basin_name,
    "basin_id" = unique(fishsurvey$basin_id),
    "date" = fishsurvey$date,
    "species" = fishsurvey$species,
    "quartile_count" = fishsurvey$quartilecount,
    "cpue" = fishsurvey$cpue,
    "total_catch" = fishsurvey$totalcatch,
    "total_weight" = fishsurvey$totalweight,
    "quartile_weight" = fishsurvey$quartileweight,
    "average_weight" = fishsurvey$averageweight,
    "gear" = fishsurvey$gear,
    "gear_count" = fishsurvey$gearcount
    )
  fishoutput = suppressWarnings(dplyr::left_join(fishoutput,mnsentinellakes::fishabbreviations,by = c("species"="Code")))
  colnames(fishoutput)[names(fishoutput) == "name"] = "species_name"
  names(fishoutput) = tolower(names(fishoutput))


  fishoutput = fishoutput[,c(1,2,3,4,13,5,6,7,8,9,10,11,12)]

  fishoutput = fishoutput[order(fishoutput$date),]
  row.names(fishoutput) = NULL
  return(fishoutput)
}
