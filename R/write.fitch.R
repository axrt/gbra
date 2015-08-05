#' Use to save a data.frame or a matrix (presumable a distance matrix for some corresponding data)
#' in a very weirdly formatted distance matrix file, that further may be fed to fitch.
#' @param \code{dist.mtx} is a distance matrix or data.frame
#' @param \cdoe{file} the filename for the fitch formatted output
#' @return returns nothing, void
#' @examples 
#' data <- read.table(file="data.txt")
#' dist.mtx<-as.matrix(dist(data))
#' write.fitch(dist.mtx, "dist.mtx.fitch")
write.fitch<-function(dist.mtx, file){
  #load stringr for simple string stuff 
  if(!require("stringr")){
    install.packages("stringr")
    library("stringr")
  }
  #regardless of how long the row names are, convert them to the
  #weird 12 char-prefix format, that fitch uses
  rn<-sapply(rownames(dist.mtx), str_trim)
  rn<-sapply(rn, function(i){return(str_pad(i, 12, side = "right",pad = " "))})
  #assign rownames
  #we do not have to do anything abou the colnames as the fitch format does not include 
  #column numbering
  rownames(dist.mtx)<-rn
  #write the first line, which needs the weird number of genomes
  #open connection and write the number of rows as fitch requires
  fileConn<-file(file,open = "wt")
  #write the number with 4 spaces in the beginning
  writeLines(text = paste0("    ",nrow(dist.mtx)),con = fileConn, sep = "\n")
  #clode the connetcion, which was opened with "wt", which overwrites any file the might have existed
  close(fileConn)
  #write out the matrix
  #open connection to the same file, but in append this time as we need to keep the previous line
  fileConn<-file(file,open = "at")
  #write each line in a specified with two spaces between columns weird format
  for(i in 1:nrow(genome.mle.dist)){
    writeLines(text=paste0(rownames(dist.mtx)[i],paste(dist.mtx[i,], collapse = "  ")), con = fileConn, sep="\n")
  }
  close(fileConn)
  return()
}
