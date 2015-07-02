read.tbh.tables<-function(folder,sequences){
  files<-list.files(path = folder, pattern ="\\.tbh",full.names = TRUE)
  df<-read.tbh.table(file = files[1],sequences = sequences)
  for(i in 2:length(files)){
    df<-rbind(df,read.tbh.table(file = files[i],sequences = sequences))
  }
  return(df)
}