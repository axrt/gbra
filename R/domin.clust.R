domin.clust<-function(gen.hclust, g.ids, legend, num.clusters, critical.ratio, sep){
  
  ctre<-cutree(tree = gen.hclust, k = num.clusters)
  #If the group matches cluster
  names.split=strsplit(x = names(ctre),split = sep)
  groups.mtx<-sapply(1:num.clusters,function(i){
    li<-sapply(1:length(ctre),function(j){
      if(i==ctre[j]){
        return(toString(legend[match(x=as.numeric(names.split[[j]][1]),table = legend[,2]),1]))
      }else{
        return("NotInCluster")
      }
    })
    return(li)
  })
  cluster.summary<-sapply(1:ncol(groups.mtx),function(i){
    return(summary.factor(groups.mtx[,i]))
  })
  cluster.summary<-sapply(cluster.summary,function(i){
    return(i/length(ctre))
  })
  return(cluster.summary)
}