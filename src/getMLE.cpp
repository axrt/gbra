#include "getMLE.h"

// [[Rcpp::export]]
RcppExport SEXP getMLE(SEXP mtx, SEXP steps, SEXP epsilon){
  
  NumericMatrix mt(mtx);
  const NumericVector st(steps);
  const NumericVector eps(epsilon);
  const size_t stps=st[0];
  const double epsil=eps[0];
  
  const NumericVector * gprimePrior=E(mt);
  NumericVector * gprime= new NumericVector(*gprimePrior);
  delete gprimePrior;

  
  NumericMatrix track(stps,2);
  fillInMatrix(track,0);
  
  double gDist(sumDist(mt,*gprime));
  double deltaG=0;
  R_len_t counter=0;
  
  while ((counter < stps) && (deltaG > epsil)){

    track(counter,0)=gDist;
    track(counter,1)=deltaG;
    
    NumericMatrix * devisable = new NumericMatrix(mt.nrow(),mt.ncol());
    for(int i=0;i<mt.nrow();i++){
      NumericVector row=mt(i,_);
      const NumericVector * ddr = diffDistRatio(row,*gprime);
      (*devisable)(i,_)=*ddr;
      delete ddr;
    }
    
    const NumericVector * rs=rowSums(*devisable);
    delete devisable;
    
    NumericMatrix * devider = new NumericMatrix(mt.nrow(),mt.ncol());
    for(int i=0;i<mt.nrow();i++){
      NumericVector row=mt(i,_);
      const NumericVector * ddr = diffDistInverse(row,*gprime);
      (*devisable)(i,_)=*ddr;
      delete ddr;
    }
    
    const double suDiv=sumMatrix(*devider);
    delete devider;
    
    for(int i=0;i<gprime->size();i++){
      (*gprime)[i]=(*rs)[i]/suDiv;
    }
    delete rs;
    
    gDist=double(sumDist(mt,*gprime));
    deltaG=fabs(track(counter,0)-gDist);
    counter++;
  }
  
  List ret;
  ret.push_back(*gprime);
  delete gprime;
  ret.push_back(track);
  
  return ret;
}



inline const NumericVector * E (const NumericMatrix& mtx){
  
  const NumericVector * rm=rowMeans(mtx);
  const NumericVector * jt=jitter(*rm);
  delete rm;
  
  return jt;
}

inline const NumericVector * jitter(const NumericVector& nmv){
  
  NumericVector * jittered = new NumericVector(nmv.size());
  
  for(int i=0;i<nmv.size();i++){
    const size_t d=nmv[i]*10;
    (*jittered)[i]=nmv[i]+(float)(rand() % d)/500-(float)d/1000;
  }
  
  return jittered;
}

inline const NumericVector * rowMeans (const NumericMatrix& mtx){
  
  NumericVector * rm=new NumericVector(mtx.nrow());
  NumericMatrix mt(mtx);
  for(int i=0;i<mtx.nrow();i++){
    (*rm)[i] = meanRow(mt(i,_));
  }
  
  return rm;
}

inline const double meanRow(const NumericVector& v){
  const double su=std::accumulate(v.begin(),v.end(),0.0);
  return su/v.size();
}

inline const double mean(const NumericVector& nmv){
  R_len_t sum=0;
  for(int i=0;i<nmv.size();i++){
    sum+=nmv[i];
  }
  return sum/nmv.size();
}

inline void fillInMatrix(NumericMatrix& mtx, const double fill){
  const R_len_t msize=mtx.nrow()*mtx.ncol();
  for(int i=0;i<msize;i++){
    mtx[i]=fill;
  }
}

inline const double diffDist(const NumericVector& iv, const NumericVector& gprime){
  NumericVector distSq(gprime.size());
  for(int i=0;i<gprime.size();i++){
      distSq[i]=pow(iv[i]-gprime[i],2);
  }
  double sm=std::accumulate(distSq.begin(),distSq.end(),0.0);
  return sqrt(sm);
}

inline const double sumDist(const NumericMatrix& mtx, const NumericVector& gprime){
  double su=0;
  for(int i=0;i<mtx.ncol();i++){
    su+=diffDist(mtx(_,i),gprime);
  }
  return su;
}

inline const NumericVector * diffDistRatio(const NumericVector& iv, const NumericVector& gprime){
  
  NumericVector * diffDistV=new NumericVector(iv.size());
  for(int i=0;i<iv.size();i++){
    (*diffDistV)[i]=iv[i]/diffDist(iv,gprime);
  }
  
  return diffDistV;
}

inline const NumericVector * diffDistInverse(const NumericVector& iv, const NumericVector& gprime){
  
  NumericVector * diffDistInv=new NumericVector(iv.size());
  for(int i=0;i<iv.size();i++){
    (*diffDistInv)[i]=1/diffDist(iv,gprime);
  }
  
  return diffDistInv;
}

inline const NumericVector * rowSums(const NumericMatrix& mtx){
  NumericVector * rs = new NumericVector(mtx.nrow());
  for(int i=0;i<mtx.nrow();i++){
    double s=0;
    for(int j=0;j<mtx.ncol();j++){
      s+=mtx(i,j);
    }
    (*rs)[i]=s;
  }
  return rs;
}

inline const double sumMatrix(const NumericMatrix&mtx){
  double su=0;
  for(int i=0;i<mtx.nrow();i++){
    for(int j=0;j<mtx.ncol();j++){
      su+=mtx(i,j);
    }
  }
  return su;
}







