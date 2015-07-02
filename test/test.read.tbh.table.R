test.file<-"/home/alext/Documents/Research/gBLASTer/research/24_10_45.tbh"
test.table.sequences<-read.tbh.table(file = test.file, sequences = TRUE)
test.table.no.sequences<-read.tbh.table(file = test.file)

test.files<-read.tbh.tables(folder = "/home/alext/Documents/Research/gBLASTer/research",sequences = FALSE)


winner<-function(i) {
  me <- mean(i)
  if (me > 0) {
    return("B")
  }else if (me < 0) {
    return("A")
  }else{
    return("X")
  }
}
test.grouping<-test.files %>% 
  
  group_by(A_genome_id,B_genome_id) %>%
  
  do(
    cbind(.[,1],.[,4],win=apply(.,1,function(i){
      
      if(i[6]>i[9]){
        return(1)
      }else if(i[6]<i[9]){
        return(-1)
      }else{
        return(0)
      }
    })
    )
    
  ) %>%

  summarize(win=winner(win))

library(xlsx)

write.table(x = test.grouping, file = "loki.txt",quote = FALSE,sep = "\t",row.names = FALSE)

