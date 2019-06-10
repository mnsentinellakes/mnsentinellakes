#' Read Access Databases
#'
#' This function opens a connection to an Access database, reads the selected table, and closes the connection using the RODBC R package.
#' @param database file path to the Access database.
#' @param sqtable name of the table in the database.
#' @keywords access database read import
#' @return data.frame
#' @family Sentinel Lakes Tools
#' @export

readaccessdatabase=function(database,sqtable){

  DB=RODBC::odbcConnectAccess2007(
    access.file = database,
    readOnlyOptimize=TRUE

  )
  output=RODBC::sqlFetch(
    channel = DB,
    sqtable = sqtable,
    as.is = TRUE
  )
  RODBC::odbcClose(
    channel = DB
  )
  return(output)
}
