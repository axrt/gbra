#Sample script from http://www.r-bloggers.com/circlizing-numbers/
if (!require("Rmpfr")){
  install.packages("Rmpfr")
}
if (!require("circlize")){
  install.packages("circlize")
}
if (!require("RColorBrewer")){
  install.packages("RColorBrewer")
}
library(RColorBrewer)
CreateAdjacencyMatrix <- function(x){
  s<-gsub(".", "", x,fixed = TRUE)
  m<-matrix(0, 10, 10)
  for (i in 1:(nchar(s)-1)){
   m[as.numeric(substr(s, i, i))+1, as.numeric(substr(s, i+1, i+1))+1]<-m[as.numeric(substr(s, i, i))+1, as.numeric(substr(s, i+1, i+1))+1]+1
  }
  rownames(m) <- 0:9
  colnames(m) <- 0:9
  return(m)
}
m1<-CreateAdjacencyMatrix(formatMpfr(Const("pi",2000)))
m2<-CreateAdjacencyMatrix(formatMpfr(Const("gamma",2000)))
m3<-CreateAdjacencyMatrix(formatMpfr(Const("catalan",2000)))
m4<-CreateAdjacencyMatrix(formatMpfr(Const("log2",2000)))
jpeg(filename = "Chords.jpg", width = 800, height = 800, quality = 100)
par(mfrow=c(2,2), mar = c(1, 1, 1, 1))
chordDiagram(m1, grid.col = "darkgreen",
             col = colorRamp2(quantile(m1, seq(0, 1, by = 0.25)), brewer.pal(5,"Greens")),
             transparency = 0.4, annotationTrack = c("name", "grid"))
chordDiagram(m2, grid.col = "mediumpurple4",
             col = colorRamp2(quantile(m2, seq(0, 1, by = 0.25)), brewer.pal(5,"Purples")),
             transparency = 0.4, annotationTrack = c("name", "grid"))
chordDiagram(m3, grid.col = "midnightblue",
             col = colorRamp2(quantile(m3, seq(0, 1, by = 0.25)), brewer.pal(5,"Blues")),
             transparency = 0.4, annotationTrack = c("name", "grid"))
chordDiagram(m4, grid.col = "red3",
             col = colorRamp2(quantile(m4, seq(0, 1, by = 0.25)), brewer.pal(5,"Reds")),
             transparency = 0.4, annotationTrack = c("name", "grid"))
dev.off()