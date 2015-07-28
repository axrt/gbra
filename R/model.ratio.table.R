model.ratio.table<-function(model,predict.data){
  #predict the rest of the data
  glm.probs<-predict(model,predict.data,type = "response")
  #assign a row to either one, or other cluster
  glm.probs.groups<-sapply(glm.probs,function(i){if(i>=cut){return(TRUE)}else{return(FALSE)}})
  #calculate the tp/fp/tn/fn
  glm.table<-table(glm.probs.groups,predict.data$cluster)
  return(glm.table)
}