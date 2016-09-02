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
      
      if(i[3]>i[6]){
        return(1)
      }else if(i[3]<i[6]){
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

test.grouping.bacteria<-test.grouping %>% filter(A_genome_id%in%c(1,2))
table(test.grouping.bacteria$win)
table(test.grouping$win)
  
g24vs.all<-test.files %>% filter(A_genome_id==24) %>% group_by(B_genome_id) %>% do({
  data.frame(wicox.pval=wilcox.test(.$A...B_bitScore, .$B...C_bitScore)$"p.value")
})

gall.vs.all<-test.files %>% group_by(A_genome_id,B_genome_id) %>% do({
  data.frame(wicox.pval=wilcox.test(.$A...B_bitScore, .$B...C_bitScore)$"p.value")
})

summary(gall.vs.all$wicox.pval)

mean.scores<-test.files %>% group_by(A_genome_id,B_genome_id) %>% summarize(
  mean.AB=mean(A...B_bitScore),sd.AB=sd(A...B_bitScore),mean.BC=mean(B...C_bitScore),sd.BC=sd(B...C_bitScore))

test.files %>% filter(A_genome_id==24, A_orf_id==8268183)
  