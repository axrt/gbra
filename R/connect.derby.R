connect.derby<-function(db, derby.jar="derby.jar", create=TRUE, usr="gblaster", pwd="gblaster"){
  
  if (!require("RJDBC")){
    install.packages("RJDBC")
  }
  
  if(create){
    create<-"true"
  }else{
    create<-"false"
  }
  
  drv <- JDBC("org.apache.derby.jdbc.EmbeddedDriver", db, identifier.quote="`")
  conn <- dbConnect(drv, paste("jdbc:derby:",db,";create=",create, sep = ""), "gblaster", "gblaster")
  return(conn)
}
connect.derby(db = "/tmp/testdb", derby.jar = "/home/alext/Developer/GenBLASTer/dist/lib/derby.jar")
