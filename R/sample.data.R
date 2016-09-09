#'Gets a part of the data chosen randomly in ammounts indicated in \code{part}
#'@param \code{data} nonempty dataframe
#'@param \code{part} 0.75 (75%) by default, the ammount of data to get randomly
#'@return dataframe of randomly chosen part of the given dataframe
#'@examples
#'data.sampled <- sample.data(data=df, part=0.6)
sample.data<-function(data, part=0.75){
  if(!require("plyr")){
    install.packages("plyr")
    library("plyr")
  }
  if(!require("dplyr")){
    install.packages("dplyr")
    library("dplyr")
  }
  sampled.data<-data %>%
    mutate(names=rownames(data)) %>% 
    group_by(cluster) %>% 
    do({
    .[sample(x = nrow(.),size = nrow(.)*part,replace = FALSE),]
    })
  rownames(sampled.data)<-sampled.data$names
  
  return(
    select(sampled.data,-c(names))
  )
}