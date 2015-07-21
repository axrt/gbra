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