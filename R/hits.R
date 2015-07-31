#'Use to load hits table
#'@param \code{hits.file} a path to the hits file
#'@return a \code{dataframe} with the data
#'
load.hits<-function(hits.file){
  hits<-read.table(file = hits.file,header = TRUE,sep = "\t",row.names = 1)
  return(hits)
}