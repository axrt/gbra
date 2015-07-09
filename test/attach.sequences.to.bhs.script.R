library(plyr)
library(dplyr)
library(tidyr)
bh.data<-read.bhs(bh.folder = "/home/alext/Documents/Research/gBLASTer/bh",sep="_") %>% as.data.frame()

fastas<-read.orf.base.fastas(folder = "/home/alext/Documents/Research/gBLASTer/orfs")
fastas$ids<-sapply(fastas$ids,get.orf.id.from.gblaster.id)

colnames(fastas)<-c("SEQUENCE","QUERY_ORF_ID")

total.data<-merge(x = data.table(fastas, key="QUERY_ORF_ID"), y= data.table(bh.data,key="QUERY_ORF_ID"), by="QUERY_ORF_ID")

colnames(total.data)
write.table(x=total.data, file="/home/alext/Documents/Research/gBLASTer/output/total.data", sep="\t",quote = FALSE, row.names = FALSE, col.names = TRUE)
nrow(fastas)-nrow(total.data)
setdiff(x = fastas$QUERY_ORF_ID,y = total.data$QUERY_ORF_ID)
