system.time(data.spe<-dist.spearman.one(df = data.3.6))

system.time(
  
  data.spe.r<-apply(data.3.6,1,function(i){
    return(apply(data.3.6,1,function(j){
      
      return(cor(i,j,method="spearman"))
      
    }))
  })
  
)


data.spe<-as.matrix(data.spe)
data.spe[1:3,1:3]
head(data.spe)
ncol(data.spe)
nrow(data.spe)
