load.legend<-function(legend.file){
  legend<-read.table(file = legend.file, header = TRUE, sep=",")
  legend<-data.frame(legend$name,legend$id_genomes)
  return(legend)
}
