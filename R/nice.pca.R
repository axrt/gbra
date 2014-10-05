nice.pca<-function(df,groups,var.axes=FALSE,scale.=FALSE,circle=TRUE,...){
  pca<-prcomp(df,center=TRUE,scale.=scale.)  
  g <- ggbiplot(pca, obs.scale = 1, var.scale = 1, groups = groups, ellipse = TRUE,circle =circle,var.axes = var.axes,...)
  g <- g + scale_color_discrete(name = '')
  g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
return(g)
}
