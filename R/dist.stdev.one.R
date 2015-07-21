#' Normalizes the rows by the standard deviation to make the more or less correlated rows appear closer
#'@param \code{df} data, can be data.frame or a matrix
#'@return the data, where each row is normalied by its standard deviation
#'@examples 
#'library(plyr)
#'library(dplyr)
#'library(tidyr)
#'bh.data<-read.bhs(bh.folder = "gBLASTer/bh",sep="_") %>% 
#'  as.data.frame() %>%
#'  restrict.minimal.hits(minhit = 3) %>%
#'  normalize.scores() %>%
#'  attach.genomeid.header() %>%
#'  sign.bh.table()
#' data.3.6<-select.genomes(df = bh.data,g.ids = c(3,6))
#' data.3.6.stn<-dist.stdev.one(data.3.6)
dist.stdev.one<-function(df){
  
  if(!require("scales")){
    install.packages("scales")
    library("sacales")
  }
  
  row.nams<-rownames(df)
  col.nams<-colnames(df)
  
  df<-apply(df,1,function(i){
      m<-mean(i)
      r<-sapply(i,function(j){
        di<-(j-m)^2
        return(di)
      })
      m<-mean(r)
      m<-m/length(r)
      m<-sqrt(m)
      i<-sapply(i,function(j){
        return(j/m)
      })
      return(i)
  })
  df<-t(df)
  rownames(df)<-row.nams
  colnames(df)<-col.nams
  return(df)
}