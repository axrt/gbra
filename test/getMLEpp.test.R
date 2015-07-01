mtx<-matrix(c(rep(1,3),rep(4,3),rep(5,3)),ncol=3)
gprime.r<-getMLE(df = data.frame(mtx))$gprime
