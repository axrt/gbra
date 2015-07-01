#'
#'TODO: document
getMLEpp<-function(df, steps=50, epsilon=1e-6){
  df<-as.matrix(df);
  return(.Call("getMLE",df,steps,epsilon,"gbra"));
}