#' Calculate Fish CPUE Linear Trend Statistics
#'
#' This function calculates linear trend statistics using data download from the MNDNR Lakefinder website using the output of the fishtable() function.
#' @param statdata a fish species data.frame processed through the fishtable() function.
#' @param logtransform a logical indicating whether the statistics should be calculated using the natural log of the CPUE + 1 values.  Defaults to TRUE.
#' @keywords mndnr minnesota natural resources fish statistics trend analysis
#' @return a data.frame with statistics for each species' CPUE.
#' @family Fish
#' @examples
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
#'
#' @export
fishtrendstats=function(statdata,logtransform=TRUE){
  Lakename=mnsentinellakes::mnlakesmetadata$Lake[mnsentinellakes::mnlakesmetadata$LakeId==unique(statdata$LakeId)]
  statdata$CPUE=as.numeric(as.character(statdata$CPUE))
  statcompile=NULL
  for (i in unique(statdata$species)){
    statdataselect=statdata[statdata$species==i,]

    if (nrow(statdataselect)>3){
      if (logtransform==TRUE){

        xvalue=log(statdataselect$CPUE+1)
      }else{
        xvalue=statdataselect$CPUE
      }
      statlm=suppressWarnings(stats::lm(xvalue~statdataselect$Year))

      if (nrow(statdataselect)>4){
        pearson=Hmisc::rcorr(x=statdataselect$Year,y=xvalue,type = "pearson")$r[1,2]
        spearman=Hmisc::rcorr(x=statdataselect$Year,y=xvalue,type = "spearman")$r[1,2]
      }else{
        pearson=NA
        spearman=NA
      }

      statdataframe=data.frame("Lake"=Lakename,"LakeId"=unique(statdata$LakeId),"Species"=i,
                               "n_samples"=sum(statdataselect$totalCatch),"n_years"=nrow(statdataselect),
                               "R-squared"=suppressWarnings(round(as.numeric(summary(statlm)$r.squared),digits = 5)),
                               "P-value"=suppressWarnings(round(as.numeric(summary(statlm)$coefficients[2,4]),digits = 5)),
                               "Slope"=round(as.numeric(statlm$coefficients[2]),digits = 5),"Pearson"=round(as.numeric(pearson),digits = 5),
                               "Spearman"=round(as.numeric(spearman),digits = 5),stringsAsFactors = FALSE)
    }else{
      statdataframe=data.frame("Lake"=Lakename,"LakeId"=unique(statdata$LakeId),"Species"=i,"n_samples"=NA,
                               "n_years"=NA,"R-squared"=NA,"P-value"=NA,"Slope"=NA,"Pearson"=NA,"Spearman"=NA,stringsAsFactors = FALSE)
    }
    statcompile=rbind(statcompile,statdataframe)
  }
  return(statcompile)
}
