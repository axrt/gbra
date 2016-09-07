#'
#'todo: document
select.genome.data<- function(data, orgs){
  
  to.exclude<- -which(colnames(data)%in% sapply(orgs, function(x){
   return(
           paste("X",x,sep="", collapse = "")
         )
  })
  )
  return(data[,to.exclude,drop=FALSE])
}