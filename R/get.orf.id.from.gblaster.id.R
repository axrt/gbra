get.orf.id.from.gblaster.id<-function(gblaster.id){
  return(as.numeric(strsplit(gblaster.id,"|",fixed = TRUE)[[1]][1]))
}