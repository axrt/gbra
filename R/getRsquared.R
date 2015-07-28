#'Use to get a pseudo Rsquared value for a logistic regression model
#'@param \code{model} logistic regression model object
#'@return R squared value, calculated as 1-logLik(model)/logLik(null.fit), where null.fit is a model of cluster ~ 1
#'
getRsquared<-function(model){
  if(!require("stats")){
    install.packages("stats")
    library("stats")
  }
  #calculate null fit
  null.fit<-glm(formula = "cluster ~ 1", data = model$data, family="binomial")
  #calculate the R^2 of the model
  r.squared=1-logLik(model)/logLik(null.fit)
  return(r.squared)  
}