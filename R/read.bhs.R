

read.bhs<-function(bh.folder, ext=".short", sep="_"){
  
  bh.files<-list.files(path = bh.folder, pattern = ext)
  genome.ids<-strsplit(x = bh.files, split = sep, fixed = TRUE)
  all.tables<-lapply(genome.ids, function(i){
    
    return(read.bh.file(i[1],i[3],bh.folder))
    })
  bh.table<-rbind_all(all.tables) %>% spread(ID_TARGET_GENOME,COMULATIVE_BITSCORE,fill = 0)
  return(bh.table)
  
}

read.bh.file<-function(qgen.id, tgen.id, bh.folder, ending="80.0.short", sep="_", margin="VS"){
  if(!require("dplyr")){
    install.packages("dplyr")
  }
  if(!require("tidyr")){
    install.packages("tidyr")
  }
  if(!require("data.table")){
    install.packages("data.table")
  }
  input<-paste(bh.folder,paste(qgen.id, margin, tgen.id, ending, sep = sep),sep = "/")
  message(paste("Reading", input))
  table.bh<-fread(input = input, showProgress = TRUE,
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE, data.table = TRUE, ,select=c("QUERY_ORF_ID", "COMULATIVE_BITSCORE")) %>%
    mutate(ID_QUERY_GENOME=qgen.id, ID_TARGET_GENOME=tgen.id)
  
  return(table.bh)
}