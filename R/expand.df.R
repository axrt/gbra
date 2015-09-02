#' Use this function to extract ID_QUERY_GENOME and QUERY_ORF_ID as separate columns from a data.frame, 
#' where the rownames are like "1X11", where 1 is the query genome and 11 is the query orf ids respectively.
#' @param \code{short.df} is the data.frame, where the ID_QUERY_GENOME and QUERY_ORF_ID are cooked into rownames (as explained above)
#' @para \code{sep} separator symbol, default is "X"
#' @return a \code{data.frame} with ID_QUERY_GENOME and QUERY_ORF_ID columns
#' @examples
#' 
expand.df<- function(short.df, sep="X"){
  short.df.colnames<- colnames(short.df)
  short.df.rownames<- rownames(short.df)
  short.df.names.split<- strsplit(x = short.df.rownames,split = sep, fixed = TRUE)
  short.df$QUERY_ORF_ID<- as.numeric(sapply(short.df.names.split, function(x){
    return(x[[1]][2])
  }))
  short.df$ID_QUERY_GENOME<-as.numeric(sapply(short.df.names.split, function(x){
    return(x[[1]][1])
  }))
  short.df<- short.df[,c("ID_QUERY_GENOME","QUERY_ORF_ID",short.df.colnames)]
  return(short.df)
}