append.cluster<- function(data, sep="X"){
  
  data %>% genome.ids.in.coocked.df ->genome.ids
  if(genome.ids %>% length != 2){
    stop("Please select only two genomes from the dataframe, select.genome.data function may be used for that.")
  }
  
  data$cluster<- grepl(pattern = paste0("\\b", genome.ids[1], sep), x = rownames(data), perl = TRUE)
  return(data)
}