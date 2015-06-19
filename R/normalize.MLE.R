#' Use to normalize the MLE values if those were calculated on non-normalized RAW data
#' 
#' @param \code{mle.df} a plain data.frame (no names and genome id columns).
#' @return a data.frame that has each row normalized to [0,1] by the maximum value in the row (presumably "self hit MLE")
#' @examples
#' mle.df<-extract.gprimes(mles.raw)
#' mle.df<-assign.names(mle.df,legend)
#' mle.df.norm<-normalize.MLE(mle.df = mle.df)
#' 
normalize.MLE<-function(mle.df){
  if(!require("scales")){
    install.packages("scales")
  }

  mle.norm<-t(apply(mle.df,1,function(i){
    return(rescale(i,from=c(0,max(i)),to=c(0,1)))
  }))
  
  return(data.frame(mle.norm))  
}