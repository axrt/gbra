mtx<-matrix(c(rep(1,3),rep(2,3),rep(4,3),rep(5,3)),ncol=4)
gprime.r<-getMLE(df = data.frame(mtx))$gprime
gprime.pp<-getMLEpp(df =  data.frame(mtx))$gprime
