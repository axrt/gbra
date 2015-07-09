library(plyr)
library(dplyr)
library(tidyr)
bh.data<-read.bhs(bh.folder = "/home/alext/Documents/Research/gBLASTer/bh",sep="_") %>% 
  as.data.frame() %>%
  restrict.minimal.hits(minhit = 3) %>%
  normalize.scores() %>%
  attach.genomeid.header() %>%
  sign.bh.table()

legend<-load.legend(legend.file = "/home/alext/Documents/Research/gBLASTer/stat/legend.csv",sep = ",")[1:44,]

data.3.6<-select.genomes(df = bh.data,g.ids = c(3,6))
data.3.6<-select(data.3.6,-c(X3,X6))

hcl<-color.clust(hclust(dist(data.3.6),method = "ward.D"),g.ids = c(3,6),colors =c("red","blue"))
plot(hcl)

gp<-gen.pca(df = data.3.6,legend = legend, g.ids = c(3,6),sep = "X")
plot(gp)

data.3.6[is.na(data.3.6)]<-0
model.3.6<-logreg.mismatch.genes(data = bh.data, org1 = "3", org2 = "6",part = 0.75)

model.3.6<-logreg.mismatch.genes(data = bh.data, org1 = "3", org2 = "6",part = 0.99)
model.3.6$fit

