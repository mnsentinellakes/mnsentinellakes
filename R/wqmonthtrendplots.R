#' WQ Trend Plots
#'
#' This function creates and saves water quality trend scatter plots comparing parameters for specific months across years. It will also save a .csv table with
#' trend statistics calculated with the wqmonthrendstats() function.
#' @param plotdata a water quality data.frame created with the wqmonthtable() function.
#' @param logtransform a logical indicating whether the statistics should be calulated using the natural log of the parameter values. Defaults to FALSE.
#' @param saveto designates the folder where the plots and table should be saved. Defaults to the working directory.
#' @param includemonths a logical indicating whether the months including in the analysis should be displayed on the x-axis.
#' @param ptcolor designates the color of the points in the plots. Defaults to "blue".
#' @param ptsize designates the size of the points in the plots. Defaults to 3.
#' @param pttype designates the type of the points in the plots. Defaults to 20.
#' @param addlm a logical indicating whether the linear regression line should be displayed on the plot. Defaults to TRUE.
#' @param lmcolor designates the color of the linear regression line. Defaults to "black".
#' @param lmsize designates the size of the linear regression line. Defaults to 1.
#' @param lmtype designates the type of the linear regression line. Defaults to 2.
#' @param includestats a logical indicating whether the R-squared and p-value statistics text should be displayed on the plot. Defaults to TRUE.
#' @param statstable a logical indicating whether a .csv of trend statistics should be saved. Defaults to TRUE.
#' @param maxpvalue a numeric indicating the maximum p-value limit for a parameter trend to be included in the export. Any parameters trends with a p-value above this number will not be exported. Defaults to 0.1.
#' @keywords water quality trends
#' @return .png plots and statistics .csv
#' @family Water Quality
#' @importFrom rlang .data
#' @examples
#' #Download the data
#' x <- wqdatadownload(c("15-0010-00-100","15-0010-00-101","15-0010-00-102"))
#'
#' #Process multiple parameters across multiple months within a given year range
#' y <- wqmonthtable(
#'        wqdata = x,
#'        parameters = c("Depth, Secchi disk depth","Temperature, water","pH"),
#'        months = c(7,8,9),
#'        startyear = 2008,
#'        endyear = 2018
#'      )
#'
#' #Create the trend plots
#' wqmonthtrendplots(
#'   plotdata = y
#'   )
#'
#' @export

wqmonthtrendplots=function(plotdata,logtransform=FALSE,saveto=paste0(getwd()),includemonths=TRUE,ptcolor="blue",ptsize=3,pttype=20,addlm=TRUE,lmcolor="black",
                           lmsize=1,lmtype=2,includestats=TRUE,statstable=TRUE,maxpvalue=0.1){

  saveto=paste0(saveto,"/WQ_Trends")
  dir.create(saveto,showWarnings = FALSE)
  #Loop through each parameter in the plotdata dataset
  statsdata=NULL
  for (i in unique(plotdata$Parameter)){

    # i=unique(plotdata$Parameter)[1]

    plotdataselect=plotdata[plotdata$Parameter==i,]

    #Calculate statistics for the data and if the pvalue is less then the designated maxpvalue, proceed with the plot
    plotstats=wqmonthtrendstats(plotdataselect,logtransform = logtransform)
    if (plotstats$P.value<=maxpvalue & !is.na(plotstats$P.value)){
      print(i)

      #Format and combine the stats tables

      if(includemonths==TRUE){
        plotstats["Months"]=unique(plotdataselect$Months)
        plotstats=plotstats[c(1,2,10,3,4,5,6,7,8,9)]
        month=toString(month.name[as.numeric(unlist(strsplit(gsub(" ","",as.character(unique(plotdataselect$Months))),",")))])
      }
      statsdata=rbind(statsdata,plotstats)

      #Format characters for use in labelling the plot


      lakename=as.character(unique(gsub("\\s*\\([^\\)]+\\)","",plotdataselect$Lake)))
      parameter=as.character(unique(plotdataselect$Parameter))
      units=as.character(unique(plotdataselect$units[plotdataselect$units!="(null)"]))

      if (logtransform==TRUE){
        plotdataselect$Value=log(plotdataselect$Value)
        ylabel=paste0("ln(",i," (",units,"))")
      }else{
        ylabel=paste0(i," (",units,")")
      }

      #Determine the x-axis tics to be displayed
      yearspan=max(plotdata$Year)-min(plotdata$Year)
      if (yearspan<=15){
        divisions=2
      }else if (yearspan<=30 & yearspan >15){
        divisions=5
      }else if (yearspan >30){
        divisions=10
      }
      yearrange=seq(
        from = min(plotdataselect$Year),
        to = max(plotdataselect$Year),
        by = divisions)

      #Determine if a regression line should be added and how to format it
      if (addlm==TRUE){
        addline="reg.line"
        addparameters=list(
          color = lmcolor,
          size = lmsize,
          linetype = lmtype
        )
        #If a regression line is added, determine if the r2 and pvalue should be displayed
        if (includestats==TRUE){
          b = ggpubr::stat_cor(
            ggplot2::aes(
              label = paste(.data$..rr.label..,.data$..p.label.., sep = "~`,`~")
            ),
            label.x.npc = "left",
            label.y.npc = "top"
          )
        }else{
          b=NULL
        }
      }else{
        addline=NULL
        b=NULL
      }

      if (includemonths==TRUE){
        xlabel=months
      }else{
        xlabel="Year"
      }

      #Set up the scatter plot
      a = ggpubr::ggscatter(
        data = plotdataselect,
        x = "Year",
        y = "Value",
        add = addline,
        fullrange = TRUE,
        xlab = xlabel,
        ylab = ylabel,
        color = ptcolor,
        shape = pttype,
        size = ptsize,
        add.params = addparameters,
        repel = TRUE
      )

      #Set up the title
      title = ggplot2::ggtitle(lakename)

      #Set the x-axis breaks
      xscale = ggplot2::scale_x_continuous(
        breaks = yearrange
      )

      #Create ggplot
      a+b+title+xscale

      if(includemonths==TRUE){
        monthtitle=addunderscore(month)
      }else{
        monthtitle=NULL
      }

      #Save the plots to file
      ggplot2::ggsave(
        filename = paste0(saveto,"/",addunderscore(lakename),"_",addunderscore(i),"_",monthtitle,".png")
      )
    }
  }
  #Save the statstable to file
  if(statstable==TRUE & !is.null(statsdata)){
    utils::write.csv(statsdata,paste0(saveto,"/",addunderscore(lakename),"_stats.csv"),row.names = FALSE)
  }else{
    print("No significant trends")
  }
}

