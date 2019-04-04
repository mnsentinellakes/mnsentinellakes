#' Fish Trend Plots
#'
#' This function creates and saves fish trend scatter plots comparing CPUE across surveys. It will also save a .csv table with trend statistics calculated with
#' the fishstats() function.
#' @param plotdata a water quality data.frame created with the fishtable() function.
#' @param logtransform a logical indicating whether the statistics should be calulated using the natural log of the CPUE +1 values. Defaults to TRUE.
#' @param saveto designates the folder where the plots and table should be saved. Defaults to the working directory.
#' @param ptcolor designates the color of the points in the plots. Defaults to "blue".
#' @param ptsize designates the size of the points in the plots. Defaults to 3.
#' @param pttype designates the type of the points in the plots. Defaults to 20.
#' @param addlm a logical indicating whether the linear regression line should be displayed on the plot. Defaults to TRUE.
#' @param lmcolor designates the color of the linear regression line. Defaults to "black".
#' @param lmsize designates the size of the linear regression line. Defaults to 1.
#' @param lmtype designates the type of the linear regression line. Defaults to 2.
#' @param includestats a logical indicating whether the R-squared and p-value statistics text should be displayed on the plot. Defaults to TRUE.
#' @param statstable a logical indicating whether a .csv of trend statistics should be saved. Defaults to TRUE.
#' @param maxpvalue a numeric indicating the maximum p-value limit for a species' CPUE trend to be included in the export. Any species' CPUE trends with a
#' p-value above this number will not be exported. Defaults to 0.1.
#' @keywords fish trends
#' @return .png plots and statistics .csv
#' @importFrom rlang .data
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
#' fishtrendplots(plotdata = z)
#'
#' @export

fishtrendplots=function(plotdata,logtransform=TRUE,saveto=paste0(getwd()),ptcolor="blue",ptsize=3,pttype=20,addlm=TRUE,lmcolor="black",lmsize=1,
                           lmtype=2,includestats=TRUE,statstable=TRUE,maxpvalue=0.1){
  saveto=paste0(saveto,"/Fish_Trends")
  dir.create(saveto,showWarnings = FALSE)
  #Loop through each parameter in the plotdata dataset
  statsdata=NULL
  for (i in unique(plotdata$species)){

    plotdataselect=plotdata[plotdata$species==i,]
    plotdataselect$CPUE=as.numeric(plotdataselect$CPUE)

    #Calculate statistics for the data and if the pvalue is less then the designated maxpvalue, proceed with the plot
    plotstats=fishtrendstats(plotdataselect,logtransform = TRUE)
    if (plotstats$P.value<=maxpvalue & !is.na(plotstats$P.value)){
      print(i)

      #Format and combine the stats tables

      statsdata=rbind(statsdata,plotstats)

      #Format characters for use in labelling the plot
      lakename=mnsentinellakes::mnlakesmetadata$Lake[mnsentinellakes::mnlakesmetadata$LakeId==unique(plotdata$LakeId)]
      species=as.character(mnsentinellakes::fishspeciesmetadata$Name[mnsentinellakes::fishspeciesmetadata$Code==unique(plotdataselect$species)])

      if (logtransform==TRUE){
        plotdataselect$CPUE=log(plotdataselect$CPUE+1)
        ylabel=paste0("ln(",species," CPUE +1)")
      }else{
        ylabel=paste0(species," CPUE")
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

      #Set up the scatter plot
      a = ggpubr::ggscatter(
        data = plotdataselect,
        x = "Year",
        y = "CPUE",
        add = addline,
        fullrange = TRUE,
        xlab = "Year",
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

      #Save the plots to file
      ggplot2::ggsave(
        filename = paste0(saveto,"/",addunderscore(lakename),"_",addunderscore(i),".png")
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

