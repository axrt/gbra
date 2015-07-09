load.legend<-function(legend.file,sep=","){
  legend<-read.table(file = legend.file, header = FALSE, sep=sep)
  legend<-data.frame(name=legend[,2],id_genomes=legend[,1])
  return(legend)
}
