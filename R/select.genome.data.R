#'Use this to select 
#'todo: document
select.genome.data<- function(data, orgs, exclude=TRUE){
  
  ids<- which(colnames(data)%in% sapply(orgs, function(x){
    return(
      paste("X",x,sep="", collapse = "")
    )
  })
  )
  
  if(exclude){
    return(data[,-ids,drop=FALSE])
  }else{
    return(data[,ids,drop=FALSE])
  }
  
}