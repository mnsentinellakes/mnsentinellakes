#' Write to Access Database
#'
#' A function that writes data to an Access database using the RODBC package.
#' @param data a data.frame to write to an Access database.
#' @param database file path to the Access database.
#' @param sqtable name of the table in the database.
#' @param append a logical indicating whether to append to the table or overwrite it. Default is TRUE.
#' @keywords access database read export save
#' @export

writeaccessdatabase=function(data,database,sqtable,append=TRUE){

  DB=RODBC::odbcConnectAccess2007(
    access.file = database
  )
  if (append==TRUE){
    tabledata=RODBC::sqlFetch(
      channel = DB,
      sqtable = sqtable
    )
    lasttableid=max(tabledata$ID)
    firstid=lasttableid+1
    finalid=lasttableid+nrow(data)
    data["ID"]=c(firstid:finalid)

    RODBC::sqlSave(
      channel = DB,
      tablename = sqtable,
      dat = data,
      rownames = FALSE,
      append = TRUE
    )

  }else{
    data["ID"]=seq(1:nrow(data))
    data=data[,c(ncol(data),1:(ncol(data)-1))]
    RODBC::sqlSave(
      channel = DB,
      tablename = sqtable,
      dat = data,
      rownames = FALSE
    )
  }
  RODBC::odbcClose(
    channel = DB
  )

}
