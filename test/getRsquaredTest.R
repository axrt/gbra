model.file<-"/home/alext/Documents/Research/gBLASTer/stat/data/1_2.rda"
model<-load(model.file)
r.sq<-getRsquared(l.m.g$fit)
as.numeric(r.sq)
