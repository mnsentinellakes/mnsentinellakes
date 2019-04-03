#' Calculate Water Quality Linear Trend Statistics
#'
#' This function calculates linear trend statistics using data download from the MNPCA using the output of the wqmonthtable() function.
#' @param statdata a water quality data.frame processed through the wqmonthtable() function.
#' @param logtransform a logical indicating whether the statistics should be calculated using the natural log of the parameter values. Defaults to FALSE.
#' @keywords mnpca minnesota pollution water quality statistics trend analysis
#' @return a data.frame with statistics for each parameter in the water quality data.frame input.
#' @family Water Quality
#' @examples
#' #Download the data
#' x <- wqdatadownload(c("15-0010-00-100","15-0010-00-101","15-0010-00-102"))
#'
#' #Create WQ formatted table
#' y <- wqmonthtable(
#'        wqdata = x,
#'        parameters = c("Depth", "Secchi disk depth","Temperature"),
#'        months = c(7,8,9),
#'        startyear = 2008,
#'        endyear = 2018
#'      )
#'
#' wqmonthtrendstats(statdata = y)
#'
#' @export

wqmonthtrendstats=function(statdata,logtransform=FALSE){
  statcompile=NULL
  for (i in unique(statdata$Parameter)){
    statdataselect=statdata[statdata$Parameter==i,]

    if (nrow(statdataselect)>3){

      if (logtransform==TRUE){
        xvalue=log(statdataselect$Value)
      }else{
        xvalue=statdataselect$Value
      }
        statlm=suppressWarnings(stats::lm(xvalue~statdataselect$Year))

      if (nrow(statdataselect)>4){
        pearson=Hmisc::rcorr(x=statdataselect$Year,y=xvalue,type = "pearson")$r[1,2]
        spearman=Hmisc::rcorr(x=statdataselect$Year,y=xvalue,type = "spearman")$r[1,2]
      }else{
        pearson=NA
        spearman=NA
      }

      statdataframe=data.frame("Lake"=unique(statdataselect$Lake),"Parameter"=unique(statdataselect$Parameter),"n_samples"=sum(statdataselect$Sample_Size),
                               "n_years"=nrow(statdataselect),"R-squared"=suppressWarnings(round(as.numeric(summary(statlm)$r.squared),digits = 5)),
                               "P-value"=suppressWarnings(round(as.numeric(summary(statlm)$coefficients[2,4]),digits = 5)),
                               "Slope"=round(as.numeric(statlm$coefficients[2]),digits = 5),"Pearson"=round(as.numeric(pearson),digits = 5),
                               "Spearman"=round(as.numeric(spearman),digits = 5),stringsAsFactors = FALSE)
    }else{
      statdataframe=data.frame("Lake"=unique(statdataselect$Lake),"Parameter"=unique(statdataselect$Parameter),"n_samples"=NA,
                               "n_years"=NA,"R-squared"=NA,"P-value"=NA,"Slope"=NA,"Pearson"=NA,"Spearman"=NA,stringsAsFactors = FALSE)
    }
    statcompile=rbind(statcompile,statdataframe)
  }
  return(statcompile)
}
