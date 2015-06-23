gen.pca<-function(df, legend, g.ids, sep, trim.names=20, var.axes=FALSE, scale.=FALSE, circle=TRUE, ...){
  check.ggbiplot.loaded()
  groups<-clip.legend(row.names(df),g.ids = g.ids,legend = legend,sep = sep)
  colnames(df)<-filter(legend,!id_genomes %in% g.ids)$name
  g<-nice.pca(df = df,legend = legend, groups = groups, var.axes = var.axes, scale. = scale., circle = circle, ... )
  return(g)
}


kmeans.pca<-function(df, legend, g.ids, sep, trim.names=20, var.axes=FALSE, scale.=FALSE, circle=TRUE, ...){
  check.ggbiplot.loaded()
  kmeans.clust<-data.frame(unlist(kmeans(x = dist(df),centers = length(g.ids))$cluster))
  kmeans.groups<-data.frame(sapply(kmeans.clust[,1],function(i){return(legend[match(x = g.ids[i],legend[,2]),1])}))
  true.groups<-data.frame(clip.legend(row.names(df), g.ids, legend, sep))
  cross.groups<-sapply(1:nrow(df),function(i,true.groups,kmeans.groups){
    if(true.groups[i,1]==kmeans.groups[i,1]){
      return(substr(x = toString(true.groups[i,1]),start=1,stop=trim.names))
    }else{
      return(paste(substr(x = toString(true.groups[i,1]),start=1,stop =trim.names),substr(x = toString(kmeans.groups[i,1]),start=1,stop =trim.names),sep = "\n<->\n"))
    }
  },true.groups,kmeans.groups)
  g<-nice.pca(df = df,legend = legend, groups = cross.groups, var.axes = var.axes, scale. = scale., circle = circle, ... )
  return(g)
}

#do not export
clip.legend<-function(node, g.ids, legend, sep){
  g<-sapply(node,function(i){
    split<-strsplit(x = i,split = sep)
    
    val<-as.numeric(split[[1]][1])
    if(val %in% g.ids){
      return(legend[match(x=val,table = legend[,2]),1])
    }else{
      return(NULL)  
    }
  })
  return(unlist(lapply(g[!unlist(lapply(g, is.null))],'[[',1)))
}

check.ggbiplot.loaded<-function(){
 
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
  
}
nice.pca<-function(df, legend, groups, var.axes=FALSE, scale.=FALSE, circle=TRUE, ...){
  pca<-prcomp(df,center=TRUE,scale.=scale.) 
  g <- ggbiplot(pca, obs.scale = 1, var.scale = 1, groups = groups, ellipse = TRUE,circle =circle,var.axes = var.axes, ...)
  g <- g + scale_color_discrete(name = '')
  g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
}