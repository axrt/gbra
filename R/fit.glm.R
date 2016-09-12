fit.glm<- function(partitioned.data){
  
  partitioned.data %>% get.glm.formula ->f
  model<- glm(formula = f, data=partitioned.data, family="binomial")
  return(
    model
    )
}