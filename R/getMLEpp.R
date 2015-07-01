#'
#'TODO: document
getMLEpp<-function(df, steps=50, epsilon=1e-6){
  df<-as.matrix(df);
  outlist<-.Call("getMLE",df,steps,epsilon,"gbra")
  names(outlist)<-c("gprime","update.track")
  return(outlist);
}