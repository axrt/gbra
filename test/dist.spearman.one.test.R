data.spe<-dist.spearman.one(df = data.3.6[1:2,])
data.3.6<-as.matrix(data.3.6)
head(as.matrix(data.spe))
cor(data.3.6[1,],data.3.6[2,],method="spearman")
