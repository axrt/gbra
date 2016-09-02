library(dplyr)
library(tidyr)
bh.folder<-"/home/alext/Documents/Research/gBLASTer/bh"
bh.files<-list.files(path = bh.folder, pattern = ".short")
genome.ids<-strsplit(x = bh.files, split = "_", fixed = TRUE)

read.short.file<-function(qgen.id, tgen.id, bh.folder){
  
  table.short<-fread(input = paste(bh.folder,paste(qgen.id,"VS",tgen.id,"80.0.short", sep = "_"),sep = "/"), 
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE, data.table = FALSE, select=c("QUERY_ORF_ID", "COMULATIVE_BITSCORE")) %>%
    mutate(ID_QUERY_GENOME=qgen.id, ID_TARGET_GENOME=tgen.id)
  
  return(table.short)
}

system.time(all.tables<-lapply(genome.ids, function(i){return(read.short.file(i[1],i[3],bh.folder))}))
system.time(test.table<-rbind_all(all.tables) %>% spread(ID_TARGET_GENOME,COMULATIVE_BITSCORE,fill = 0))

test.table<-read.bhs(bh.folder = bh.folder)

