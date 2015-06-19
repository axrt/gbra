#' Use to get a list of MLE values for a combined hit data.frame
#' 
#' @param \code{df} a RAW or normalized RAW (has ORF names and ID_QUERY_GENOME column) data.frame
#' @param maximum \code{steps} until the convergence of the ML, default 50 (more than enough actually)
#' @param \code{epsilon} minimum improvement in the coordinate after which the ML is considered to have converged, default 1e-6
#' @return a list of MLEs one per each genome
#' @examples
#' master.table.raw<-read.bhs(bh.folder = "gBLASTer/bh")
#' data<-master.table.raw %>% data.frame() %>% restrict.minimal.hits(minhit = 40) %>% normalize.scores() %>% group_by(ID_QUERY_GENOME) %>% split(f=.$ID_QUERY_GENOME)
#' mles<-getMLEs(data)
#' 
getMLEs<-function(df, steps=50, epsilon=1e-6){
  
  if(!require("plyr")){
    install.packages("plyr")
  }
  if(!require("dplyr")){
    install.packages("dplyr")
  }
  
  mles<-df %>% group_by(ID_QUERY_GENOME) %>% split(f=.$ID_QUERY_GENOME)
  mles<-lapply(mles,FUN = function(i){
    message(paste("Calculating MLE for genome",i[1,2]))
    return(getMLE(df = t(i[,3:ncol(i)]),steps = steps, epsilon = epsilon))
  })
  return(mles)
}