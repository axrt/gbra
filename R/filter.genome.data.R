#'NOTE: this will not check of any of the organisma exist in the data.frame, so make sure the \code{orgs} are consistent
#'@param \code{data} a "coocked" data frame where colnames are as follows: X1..Xn, where n is the ids of genomes, rownames are: 1X0..mXn, where m is the largest orf_id
#'@param \code{orgs} a vector of organism ids that will be selected
#'@return
filter.genome.data<- function(data, orgs){
  return(
    data[grepl(pattern = paste("\\b(",
                               paste(orgs, collapse ="|"),")X",
                               sep = "",collapse = ""),
               x = rownames(data),
               perl = TRUE),
         ]
  )
}