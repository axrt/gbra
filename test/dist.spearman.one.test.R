data.spe<-dist.spearman.one(df = data.3.6[1:2,])
min(data.spe)
hc<-hclust(data.spe)
