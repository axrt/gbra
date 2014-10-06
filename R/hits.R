load.hits<-function(hits.file){
  hits<-read.table(file = hits.file,header = TRUE,sep = "\t",row.names = 1)
  return(hits)
}