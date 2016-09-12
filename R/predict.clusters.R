predict.clusters<- function(model, data.rest, cut=0.5){
  
  predicted.clusters<- predict(model, data.rest, type = "response")
  data.rest$predicted<- sapply(predicted.clusters, function(x){
    return(
      x>cut
      )
  })
  return(
    data.rest
  )
}