create.model.driver<- function(legend, sep="_", ext=".rda"){
  
  len<- legend %>% nrow
  
  drv.mtx<- sapply(1:(len-1), function(i){
    return(
      sapply((i+1):len, function(j){
        return(c(legend$id_genomes[i],legend$id_genomes[j]))
      })
    )
  }) %>% unlist() %>% matrix(nrow=2) %>% t %>% data.frame
  
  drv.mtx$file<- sapply(1:nrow(drv.mtx), function(i){
    return(
      paste(drv.mtx[i,1], drv.mtx[i,2], "model", sep = sep) %>% paste0(ext)
      )
  })
  
  return(
    drv.mtx
  )
  
}