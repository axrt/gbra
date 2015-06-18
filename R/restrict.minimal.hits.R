#' Use to restrict a given data.frame with RAW(!) bitscores to a certain number of
#' non-zero bitscores per ORF.
#' @param \code{df} data.frame, presumably from read.bhs().
#' @param \code{minhit} minimal number of non-zero hits that an ORF must have in order remain in the table, default is 10.
#' @return a data.frame with only those ORFs that had non-zero hits of \code{minhit} threshold and greater.
#' @examples
#' master.table.raw<-read.bhs(bh.folder = "gBLASTer/bh")
#' master.table.raw<-as.data.frame(master.table.raw)
#' master.table.raw.10hcut<-restrict.minimal.hits<-function(df=master.table.raw)
#' 
restrict.minimal.hits<-function(df, minhit=10){
  #Checks if a row given in i has enough non-zero hits
  enough<-function(i,minhit){
    return(ifelse(test = sum(i>0)>=minhit, yes=TRUE, no=FALSE))
  }
  #filters out poor ORF rows
  enoughs<-apply(df[,3:ncol(df)],1,enough,minhit=minhit)
  return(df[enoughs,])
}