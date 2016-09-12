get.rsq<- function(model){
  
  null.fit<-glm(formula = "cluster ~ 1", data = model$data, family="binomial")
  return(
    1-logLik(model)/logLik(null.fit)
  )
  
}