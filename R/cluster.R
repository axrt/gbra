color.clust<-function(gen.hclust, g.ids, colors, sep){
  dnd<-as.dendrogram(object = gen.hclust)
  dnd<-dendrapply(X = dnd,FUN = color.dnd, g.ids, colors, sep)
  return(dnd)
}

color.dnd<-function(node, g.ids, colors, sep){
  if(is.leaf(node)){
    a<-attributes(node)
    label<-attr(node,"label")
    attr(node,"nodePar")<-c(a$nodePar,lab.col=col)
    cls<-colors[match(strsplit(x = label,split = sep)[[1]][1],g.ids)]
    attr(node,"nodePar")<-list(lab.col=cls,col=cls,pch=15)
  }
  return(node)
}