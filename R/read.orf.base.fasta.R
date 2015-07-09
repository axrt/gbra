#' Use to read a protein fasta file into a data frame
#' @param \code{file} a file, that contains multiple fasta-formatted records
#' @return data.frame with tow columns, the first contains sequences, the second - sequence ids
#' @examples 
#' file<-"gBLASTer/orfs/Acidilobus_saccharovorans"
#' test.fasta.df<-read.orf.base.fasta.single(file = file)
read.orf.base.fasta.single<-function(file){
  if(!require("Biostrings")){
    library(BiocInstaller)
    biocLite("Biostrings")
    library("Biostrings")
  }
  if(!require("data.table")){
    install.packages("data.table")
    library(data.table)
  }
  message(paste("Reading file: ",file))
  fasta.df<-readAAStringSet(filepath = file)
  ids<-names(fasta.df)
  fasta.df<-data.table(fasta.df)
  fasta.df<-as.data.frame(fasta.df)
  fasta.df[,1]<-as.character(fasta.df[,1])
  fasta.df$ids=ids
  return(fasta.df)
}

#' Use to read all files from a given folder and efficiently concatenate them into one large data.frame
#' @param \code{folder} that contains EXCLUSIVELY protein fasta records
#' @return a data.frame that contains all sequences in one
#' @examples 
#' fastas<-read.orf.base.fastas(folder = "gBLASTer/orfs")
read.orf.base.fastas<-function(folder){
  
  files<-list.files(path = folder,full.names = TRUE)
  full.list<-list()
  for(i in 1:length(files)){
    df<-read.orf.base.fasta.single(files[i])
    full.list[[i]]<-df
  }
  full.df<-rbindlist(full.list)

  return(full.df)
}
