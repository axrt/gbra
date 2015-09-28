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

#' Reads and returns the genome legend from the database.
#' 
#' @param \code{conn} connection to the gBLASTer database
#' @param \code{out.file} file to save the legend
#' @examples
#' conn<-connect.derby(db, derby.jar=derby.lib, create=TRUE, usr="gblaster", pwd="gblaster")
#' save.legend(conn$conn, "legend.txt")
#' 
save.legend<-function(conn, out.file){
  if (!require("dplyr")){
    install.packages("dplyr")
  }
  legend<-dbGetQuery(conn, "select * from genomes")
  legend<- legend %>% select(ID_GENOME,NAME)
  write.table(legend, file = out.file, quote = FALSE, sep = "\t", row.names = FALSE)
  return(legend)
}



