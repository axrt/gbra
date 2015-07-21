system.time(data.spe<-dist.spearman.one(df = data.3.6))
system.time(
  
  data.spe.r<-apply(data.3.6,1,function(i){
    return(apply(data.3.6,1,function(j){
      
      return(cor(i,j,method="spearman"))
      
    }))
  })
  
)


data.3.6<-as.matrix(data.3.6)
head(as.matrix(data.spe))
cor(data.3.6[1,],data.3.6[2,],method="spearman")
