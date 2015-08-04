write.fitch<-function(dist.mtx, file){
  #write the first line, which needs the weird number of genomes
  fileConn<-file("file",open = "wt")
  writeLines(text = paste0("    ",nrow(dist.mtx)),con = fileConn, sep = "\n")
  close(fileConn)
  #write out the matrix
  fileConn<-file("file",open = "at")
  for(i in 1:nrow(genome.mle.dist)){
    writeLines(text=paste0(rownames(dist.mtx)[i],paste(dist.mtx[i,], collapse = "  ")), con = fileConn, sep="\n")
  }
  close(fileConn)
  return()
}
