clo.lab<-function(n){
    if(is.leaf(n)){
          a<-attributes(n)
             col<-tree.colors[cut.clust[which(names(cut.clust)==a$label)]]
             attr(n,"nodePar")<-c(a$nodePar,lab.col=col)
         }
       return(n)
}

tree.colors<-c("red","green","blue","orange")
clust<-hclust(dist(table),method="ward.D2")
cut.clust<-cutree(clust,4)
table<-read.table(file="/Users/alext/Downloads/19_1.txt",header = TRUE, row.names=1,sep="\t")

dendro.clust<-dendrapply(as.dendrogram(clust),clo.lab)
plot(dendro.clust, main="BOOBS!")

clo.a<-function(n,){
  if(is.leaf(n)){
    a<-attributes(n)
    label<-attr(n,"label")
    attr(n,"nodePar")<-c(a$nodePar,lab.col=col)
    attr(n,"nodePar")<-list(lab.col=ifelse(test = grepl("3X",label),yes= "red",no="blue"))
  }
  return(n)
}


clust<-hclust(dist(table),method="average")
dendro.clust.a<-dendrapply(as.dendrogram(clust),clo.a)
plot(dendro.clust.a, main="BOOBS!")

matrix<-read.table(file="/Users/alext/Downloads/shit.out",header = TRUE,row.names = 1,sep="\t")

clust<-hclust(cor.dist,method="average")
dendro.clust.a<-dendrapply(as.dendrogram(clust),clo.a)
plot(dendro.clust.a, main="BOOBS!")


table<-read.table(file="/Users/alext/Downloads/temp.txt",header = TRUE, row.names=1,sep="\t")
clust<-hclust(dist(table),method="ward.D")
dendro.clust.a<-dendrapply(as.dendrogram(clust),clo.a)
plot(dendro.clust.a, main="BOOBS!")


has.a<-function(n){
  if(grepl("A",n)==1){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

both.nonzer<-function(a,b){
  if(a==0||b==0){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

elem.nonzer<-function(i,x,y){
  return(both.nonzer(a = x[[i]],b = y[[i]]))
}

f<-function(x,y){
  
  driver<-sapply(1:length(x),elem.nonzer,x,y)
  if(sum(driver)<5){
    return(200)
  }
    x<-x[driver]
    y<-y[driver]
  return((1-sp.dist(x,y))*100)
}




sp.dist<-function(x,y){
  c<-(cor(x = x,y=y,use = "everything",method = "spearman"))
  if(is.na(c)||is.nan(c)){
    print("x:")dat
    print(x)
    print("y:")
    print(y)
  }
  return(c) 
}

get.rid.of.nan<-function(dist,replacement){
  m<-as.matrix(dist)
  m[is.na(m)]<-replacement
  m[is.nan(m)]<-replacement
  return(as.dist(m))
}


table.noa<-table[sapply(X = row.names(table),FUN = has.a),]
clust<-hclust(dist(table.noa),method="ward.D")
dendro.clust.a<-dendrapply(as.dendrogram(clust),clo.a)
plot(dendro.clust.a, main="Ward.D")

all.genomes<-read.table(file="/home/alext/Documents/gBlaster/research/R/all_genomes-2.out",header = TRUE, row.names=1,sep="\t")
leaf.colors<-colours()[seq(from = 547,by = 5,length.out = 21)]
leaf.colors<-colours()[c(646,636,503,454,461,362,142,258,153,251,610,94,411,450,251,552,556,251,229,204,251)]

clo.a.multi<-function(n){
  if(is.leaf(n)){
    a<-attributes(n)
    label<-attr(n,"label")
    attr(n,"nodePar")<-c(a$nodePar,lab.col=col)
    attr(n,"nodePar")<-list(lab.col=leaf.colors[[as.numeric(strsplit(x = label,split = "X",fixed = TRUE)[[1]][1])+1]])
  }
  return(n)
}

clust<-hclust(dist(all.genomes),method="ward")
dendro.clust.a<-dendrapply(as.dendrogram(clust),clo.a.multi)
plot(dendro.clust.a, main="Ward my ass")

pdf(file = "/home/alext/Documents/gBlaster/research/R/pic/aex.pdf", width=100,height=100)
par(cex=0.5,cex.lab=1,font=10)
plot(dendro.hclust.dist.axe.ward, main="Ward my ass")
dev.off()
par(cex=1,cex.lab=0.01,font=1)

clo.a<-function(n){
  if(is.leaf(n)){
    a<-attributes(n)
    label<-attr(n,"label")
    attr(n,"nodePar")<-c(a$nodePar,lab.col=col)
    label.color<-"white"
    
    if((grepl("6X",label)==1) && (grepl("16X",label)==0)){
      label.color<-colours()[552]
    }else if((grepl("8X",label)==1) && (grepl("18X",label)==0)){
      label.color<-colours()[503]
    }else if((grepl("3X",label)==1) && (grepl("13X",label)==0)){
      label.color<-colours()[142]
    }else if(grepl("16X",label)==1){
      label.color<-colours()[258]
    }else if(grepl("15X",label)==1){
      label.color<-colours()[454]
    }
    
    attr(n,"nodePar")<-list(lab.col=label.color)
  }
  return(n)
}
