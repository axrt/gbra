gen.pca<-function(df, legend, g.ids, sep, var.axes=FALSE, scale.=FALSE, circle=TRUE, ...){
  
  list.of.packages <- c("devtools")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)){
    install.packages(new.packages)
  }
  require(devtools)
  list.of.packages <- c("ggbiplot")
  if(length(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])){
    install_github("vqv/ggbiplot")
  }
  require(ggbiplot)
  
  pca<-prcomp(df,center=TRUE,scale.=scale.) 
  g <- ggbiplot(pca, obs.scale = 1, var.scale = 1, groups = clip.legend(row.names(df), g.ids, legend, sep), ellipse = TRUE,circle =circle,var.axes = var.axes,...)
  g <- g + scale_color_discrete(name = '')
  g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
  return(g)
}



#do not export
clip.legend<-function(node, g.ids, legend, sep){
  g<-sapply(node,function(i){
    split<-strsplit(x = i,split = sep)
    
    val<-as.numeric(split[[1]][1])
    if(val %in% g.ids){
      return(legend[val,1])
    }else{
      return(NULL)  
    }
  })
  return(unlist(lapply(g[!unlist(lapply(g, is.null))],'[[',1)))
}