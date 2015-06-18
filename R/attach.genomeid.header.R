#' Use to attach marker symbols to numeric column names.
#' @param \code{df} data.frame, presumably RAW (!)
#' @param \code{sep} marker symbol to be attached, default is "X". Keep \code{sep} consistent throuhg analysis.
#' @return the same data.frame as \code{df}, but with column names marked with \code{sep}
#' @examples
#' master.table.raw<-read.bhs(bh.folder = "gBLASTer/bh")
#' master.table.raw<-as.data.frame(master.table.raw)
#' master.table.raw.10hcut<-restrict.minimal.hits<-function(df=master.table.raw)
#' master.table.raw.10hcut.header<-attach.genomeid.header(df=master.table.raw.10hcut)
#' 
attach.genomeid.header<-function(df, sep="X"){
  
  colnames(df)<-sapply(colnames(df),function(i){return(paste(sep,i,sep=""))})
  
  return(df)
}