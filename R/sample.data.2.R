#'Gets a part of the data chosen randomly in ammounts indicated in \code{part}
#'@param \code{data} nonempty dataframe
#'@param \code{part} 0.75 (75%) by default, the ammount of data to get randomly
#'@return a list of dataframes, one randomly chosen part of the given dataframe,
#'the other one - the rest of the initial dataframe.
#'@examples
#'data.sampled <- sample.data(data=df, part=0.6)
sample.data.2<- function(data, part=0.75){

  sampled.data<-data %>%
    mutate(names=rownames(data)) %>% 
    group_by(cluster) %>% 
    do({
      df<-.
      return(df[sample(x = 1:nrow(df), size = nrow(df)*part,replace = FALSE),])
    }) %>% data.frame
  
  rest.of.data<- data[!rownames(data)%in%sampled.data$names,] 
  rownames(sampled.data)<- sampled.data$names
  
  return(
    list(
      train=sampled.data[,!colnames(sampled.data) %in% c("names")],
      validate=rest.of.data
    )
  )
}