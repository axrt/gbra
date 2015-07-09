select.row<-function(name,g.ids,sep){
  split<-strsplit(x = name,split = sep)
  if(as.numeric(split[[1]][1]) %in% g.ids){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

select.genomes<-function(df,g.ids,sep="X"){
  return(df[sapply(row.names(df),select.row,g.ids,sep),])
}

