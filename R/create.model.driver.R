create.model.driver<- function(legend, sep="_", ext=".rda"){
  
  len<- legend %>% nrow
  
  drv.mtx<- sapply(1:(len-1), function(i){
    return(
      sapply((i+1):len, function(j){
        return(c(i,j))
      })
    )
  }) %>% unlist() %>% matrix(ncol=2) %>% data.frame
  
  drv.mtx$file<- sapply(1:nrow(drv.mtx), function(i){
    return(
      paste(drv.mtx[i,1], drv.mtx[i,2], "model", ext, sep=sep)
      )
  })
  
  return(
    drv.mtx
  )
  
}