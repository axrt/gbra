#' Use to extravt gprime MLE values from a list of MLEs
#' 
#' @param \code{mle.list} a list of MLE values (from getMLEs())
#' @return a plain data.frame of MLE gprime values
#' @examples
#' data<-master.table.raw %>% data.frame() %>% restrict.minimal.hits(minhit = 40) %>% normalize.scores() %>% group_by(ID_QUERY_GENOME) %>% split(f=.$ID_QUERY_GENOME)
#' mles.raw<-lapply(data,FUN = function(i){
#' message(paste("Calculating MLE for genome",i[1,2]))
#' return(getMLE(t(i[,3:ncol(i)])))
#' })
#' mle.df<-extract.gprimes(mles.raw)
#' 
extract.gprimes<-function(mle.list){
  mle.df<-data.frame(sapply(mle.list,function(i){return(i$gprime)}))
  return(t(mle.df))
}