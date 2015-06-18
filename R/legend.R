load.legend<-function(legend.file){
  legend<-read.table(file = legend.file, header = TRUE, sep=",")
  legend<-data.frame(name=legend$name,id_genomes=legend$id_genomes)
  return(legend)
}
