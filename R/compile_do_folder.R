#' Compile DO data
#'
#' This function compiles Dissolved Oxygen data files downloaded from PME miniDot DO loggers. Use if the data where not compiled using the PME java app on the logger.
#' @param folderpath The folder where the individual files are located
#' @param savetofolder The folder where the output csv is to be saved
#' @keywords DO Dissolved Oxygen
#' @return a .csv file
#' @examples
#' \dontrun{
#' compile_do_folder(
#'   folderpath = "C:/Projects/Datasets/DO/Continuous/Greenwood/2019/7450-516388",
#'   savetopath = "C:/Projects/Datasets/DO/Continuous/Greenwood/2019")
#' }
#' @export

compile_do_folder=function(folderpath,savetofolder){

  if (!is.null(savetofolder)){

  read_do_files=function(filename){
    dofile=utils::read.delim(filename,stringsAsFactors = FALSE)
    dodata=as.data.frame(do.call(rbind,strsplit(dofile[,1],split = ",")))
    names(dodata)=as.character(unlist(dodata[2,]))
    dodata=dodata[-c(1,2),]
    return(dodata)
  }

  dofiles=list.files(folderpath,full.names = TRUE,pattern = ".txt")

  docombined=NULL
  for (i in dofiles){
    readdo=read_do_files(i)

    datetime=as.numeric(as.character(readdo$`Time (sec)`))
    datetimeutc=as.POSIXct(datetime,origin = "1970-01-01",tz="UTC")
    batteryformat=as.numeric(as.character(readdo$`  BV (Volts)`))
    tempformat=as.numeric(as.character(readdo$`  T (deg C)`))
    doformat=as.numeric(as.character(readdo$`  DO (mg/l)`))
    qformat=as.numeric(as.character(readdo$`  Q ()`))



    doreformat=data.frame("Unix.Timestamp"=as.character(datetime),"UTC_Date_._Time"=as.character(datetimeutc),
                          "Central.Standard.Time"=as.character(lubridate::with_tz(datetimeutc,'US/Central')),"Battery"=as.character(batteryformat),
                          "Temperature"=as.character(tempformat),"Dissolved.Oxygen"=as.character(doformat),"Dissolved.Oxygen.Saturation"=NA,
                          "Q"=as.character(qformat),stringsAsFactors = FALSE)

    docombined=rbind(docombined,doreformat)
  }

  dorowadd=data.frame("Unix.Timestamp"=as.character("(Second)"),"UTC_Date_._Time"=as.character("(none)"),"Central.Standard.Time"=as.character("(none)"),
                      "Battery"=as.character("(Volt)"),"Temperature"=as.character("(deg C)"),"Dissolved.Oxygen"=as.character("(mg/l)"),
                      "Dissolved.Oxygen.Saturation"=as.character("(%)"),"Q"=as.character("(none)"),stringsAsFactors = FALSE)
  dofinal=rbind(dorowadd,docombined)

  folder=unlist(strsplit(folderpath,split = "/"))[length(unlist(strsplit(folderpath,split = "/")))]
  serialnum=unlist(strsplit(folder,split="-"))[2]

  if (substr(savetofolder,(nchar(savetofolder)),nchar(savetofolder))!="/"){
    savetofolder=paste0(savetofolder,"/")
  }

  pathout=paste0(savetofolder,serialnum,".csv")

  utils::write.csv(dofinal,pathout,row.names = FALSE)
  }else{
    warning("Missing saveto")
  }
}
