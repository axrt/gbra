#' Use to normalize the bitscore values in the RAW(!) table from read.bhs()
#'@param \code{raw.df} data.frame from read.bhs(); keep in mind that read.bhs() returns a data.table, not data.frame, so conversion is due
#'@return \code{data.frame} with bit score values converted to [0,1] scale
#'@example
#'master.table.raw<-read.bhs(bh.folder = "/gBLASTer/bh")
#'master.table.raw<-as.data.frame(master.table.raw)
#'master.table.norm<- normalize.scores(master.table.raw)
#'
normalize.scores<-function(raw.df){
  #needs scales for rescale()
  if(!require("scales")){
    library("scales")
  }
  #as the function assumes that the raw table is provided, it only takes the columns with actual hits [3:end_of_table]
  norm.tab<-apply(raw.df[,3:ncol(raw.df)],1,function(i){
    #all scores get rescaled in order to the self-score (which is assumed to be the maximal, however 
    #is not entirely always so because of the multiple hsp problem)
    #rescales to [0,1]
    i<-rescale(x = i,from = c(0,max(i)),to = c(0,1))
    return(i)
  })
  
  return(cbind(raw.df[,1:2],t(norm.tab)))
}