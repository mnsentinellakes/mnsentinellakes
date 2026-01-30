#' Calculate Fish CPUE Linear Trend Statistics
#'
#' This function calculates linear trend statistics using data download from the MNDNR Lakefinder website using the output of the fishtable() function.
#' @param statdata a fish species data.frame processed through the fishtable() function.
#' @param logtransform a logical indicating whether the statistics should be calculated using the natural log of the CPUE + 1 values.  Defaults to TRUE.
#' @keywords mndnr minnesota natural resources fish statistics trend analysis
#' @return a data.frame with statistics for each species' CPUE.
#' @family Fish
#' @examples
#' \dontrun{
#' #Download the data
#' x <- lakefinderdownload("21005700")
#'
#' #Select fisheries survey data
#' y <- fishsurveydata(
#'        lakefinderdata = x
#'      )
#'
#' #Extract appropriate data for each fish species
#' z <-fishtable(
#'       fishsurvey = y,
#'       fishspecies = c("Bluegill","Largemouth Bass"))
#'
#' fishtrendstats(statdata = z)
#' }
#' @export
fishtrendstats = function(statdata,logtransform = TRUE){

  basinname = mnsentinellakes::basinid2name(unique(statdata$basin_id))

  statdata$cpue = as.numeric(as.character(statdata$cpue))
  statcompile=NULL
  for (i in unique(statdata$species)){
    statdataselect = statdata[statdata$species == i,]

    if (nrow(statdataselect) > 3){
      if (logtransform == TRUE){

        xvalue = log(statdataselect$cpue + 1)
      }else{
        xvalue = statdataselect$cpue
      }
      statlm = suppressWarnings(stats::lm(xvalue ~ statdataselect$year))

      if (nrow(statdataselect) > 4){
        pearson = Hmisc::rcorr(x = statdataselect$year,y = xvalue,type = "pearson")$r[1,2]
        spearman = Hmisc::rcorr(x=statdataselect$year,y = xvalue,type = "spearman")$r[1,2]
      }else{
        pearson = NA
        spearman = NA
      }

      statdataframe = data.frame(
        "basin_name" = basinname,
        "basin_id" = unique(statdata$basin_id),
        "species" = i,
        "n_samples"= sum(statdataselect$totalcatch),
        "n_years" = nrow(statdataselect),
        "r-squared" = suppressWarnings(round(as.numeric(summary(statlm)$r.squared),digits = 5)),
        "p-value" = suppressWarnings(round(as.numeric(summary(statlm)$coefficients[2,4]),digits = 5)),
        "slope" = round(as.numeric(statlm$coefficients[2]),digits = 5),
        "pearson" = round(as.numeric(pearson),digits = 5),
        "spearman" = round(as.numeric(spearman),digits = 5),
        stringsAsFactors = FALSE
      )
    }else{
      statdataframe =data.frame(
        "basin_name" = basinname,
        "basin_id" = unique(statdata$basin_id),
        "species" = i,
        "n_samples" = NA,
        "n_years" = NA,
        "r-squared" = NA,
        "p-value" = NA,
        "slope" = NA,
        "pearson" = NA,
        "spearman" = NA,
        stringsAsFactors = FALSE
      )
    }
    statcompile = rbind(statcompile,statdataframe)
  }
  return(statcompile)
}
