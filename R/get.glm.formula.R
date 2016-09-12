get.glm.formula<- function(data){
  return(
    paste("cluster ~",
          paste(colnames(data[1:ncol(data)-1]),
                collapse = "+"))
  )
}