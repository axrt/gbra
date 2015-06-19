#' Use to attach column names (human-readable organism names) to a plain data.frame in accordance with the legend
#' 
#' @param \code{df} plain data.frame (presumably MLE data frame)
#' @param \code{legend} a legend data.frame (presumably from load.legend())
#' @param \code{sep} symbol that comes in front of a genome id in a column, default "X"
#' @return same data.frame, but with attached column names (human-readable organism names)
#' @examples
#' legend<-load.legend(legend.file = "legend.csv")
#' mle.df<-extract.gprimes(mles.raw)
#' mle.df<-assign.names(mle.df,legend)
#' 
assign.names<-function(df,legend,sep="X"){
  
  c.n<-colnames(df)
  c.n.split<-strsplit(x = c.n,split = sep,fixed=TRUE)
  c.n.assigned<-sapply(c.n.split,function(i){
    return(legend$name[which(legend$id_genomes==i[2])])
  })
  colnames(df)<-c.n.assigned
  return(df)
}