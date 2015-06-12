#' Connects to the gBLASTer embedded Derby database via JDBC.
#' 
#' @param \code{db} path to the debry database folder
#' @param \code{derby.jar} path to derby.jar library, default "derby.jar"
#' @param \code{usr} username, default "gblaster"
#' @param \code{pwd} password, default "gblaster"
#' @return \code{connection} object, which represents the a connection to the database
#' @examples
#' db<-"/home/db/deby/"
#' derby.lib<-"/lib/derby.jar"
#' connect.derby(db, derby.jar=derby.lib, create=TRUE, usr="gblaster", pwd="gblaster")
#'
connect.derby<-function(db, derby.jar="derby.jar", usr="APP", pwd="gblaster"){
  
  if (!require("RJDBC")){
    install.packages("RJDBC",dep=TRUE)
  }
  
  drv <- JDBC(driverClass = "org.apache.derby.jdbc.EmbeddedDriver", classPath = derby.jar, identifier.quote="`")
  conn <- dbConnect(drv, paste("jdbc:derby:",db,";", sep = ""), usr, pwd)
  return(list(drv=drv,conn=conn))
}
