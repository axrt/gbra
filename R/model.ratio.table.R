#'Use to calculate tp/tn/fp/fn ratio for a given logistic regression, which uses the given data to predict
#'@param \code{model} logistic regression model
#'@param \code{predict.data} data to use for model assessment
#'@param \code{cut} probability margin, default is 0.5 (equally probable to get in both clusters)
#'@return a \code{table} funciton output
model.ratio.table<-function(model,predict.data, cut=0.5){
  #predict the rest of the data
  glm.probs<-predict(model,predict.data,type = "response")
  #assign a row to either one, or other cluster
  glm.probs.groups<-sapply(glm.probs,function(i){if(i>=cut){return(TRUE)}else{return(FALSE)}})
  #calculate the tp/fp/tn/fn
  glm.table<-table(glm.probs.groups,predict.data$cluster)
  return(glm.table)
}