genome.ids.in.coocked.df<- function(data, sep="X"){
  return(
    sapply(rownames(data), function(x){
      return(
        strsplit(x = x, split = sep)[[1]][1] %>% as.numeric
      )
    }) %>% unique %>% sort
  )
}