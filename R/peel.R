peel.out.genomes<-function(x,v){ 
  split<-strsplit(x = x,split = "X")
  if(as.numeric(split[[1]][1]) %in% v){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

peel.out.groups<-function(x,v,nams){
  g<-sapply(x,function(i){
    split<-strsplit(x = i,split = "X")
    
    val<-as.numeric(split[[1]][1])
    if(val %in% v){
      return(nams[val,1])
    }else{
      return(NULL)  
    }
  })
  return(unlist(lapply(g[!unlist(lapply(g, is.null))],'[[',1)))
}

peel.out.nicely<-function(df,vec,names.master){
  require(ggbiplot)
  return(nice.pca(df = df[sapply(row.names(df),peel.out.genomes,vec),],groups = peel.out.groups(row.names(df),vec,names.master)))
}

peel.out.subgroup<-function(df,vec){

  df.out<-df[sapply(row.names(df),peel.out.genomes,vec),]
  return(df.out)
}