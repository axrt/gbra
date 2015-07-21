#'Use to calculate distanse based on 1+spearman_correlation(each row in the dataframe)
#'@param \code{df} data, can be data.frame or a matrix
#'@return a dist object with the calculated values
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
dist.spearman.one<-function(df){

  m<-as.matrix(df)
  m.dist<-.Call("spearmanDist",m,"gbra")
  rownames(m.dist)<-rownames(df)
  colnames(m.dist)<-rownames(df)
  return(as.dist(m.dist))
}
