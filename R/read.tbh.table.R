read.tbh.table<-function(file, sequences=FALSE){
  if(!require(plyr)){
    install.packages(dplyr)
  }
  if(!require(plyr)){
    install.packages(dplyr)
  }
  
  tab<-read.table(file = file,header=TRUE,stringsAsFactors = FALSE)
  
  if(!sequences){
    tab<-tab %>% select(-c(A_sequence,B_sequence,C_sequence))
  }
  return(tab)
}