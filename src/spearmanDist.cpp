#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

inline static const bool greater(double a, double b){
  return a <= b;
}

inline static const std::vector<double> * ranks(std::vector<double> &sorted){
  std::vector<double> * rank=new std::vector<double>(sorted.size());
 
  return rank;
} 

inline static const double spearmanRho(std::vector<double> &x, std::vector<double> &y){
  
  std::sort(x.begin(), x.end(), greater);
  std::sort(y.begin(), y.end(), greater);
  
  const std::vector<double> * xr=ranks(x);
  const std::vector<double> * yr=ranks(y);
  
  std::vector<double> ds;
  for(int i=0;i<xr->size();i++){
    ds.push_back(pow((*xr)[i]-(*yr)[i],2));
  }
  delete xr;
  delete yr;
  const double dsqareds=std::accumulate(ds.begin(),ds.end(),0);
  const double nsquared=pow(ds.size(),2);
  
  const double rho = 1-(6*dsqareds)/(ds.size()*(nsquared-1));  
  return(rho);
}

// [[Rcpp::export]]
RcppExport SEXP spearmanDist(SEXP mtx){
  
  NumericMatrix mt(mtx);
  const int nrow=mt.nrow();
  
  NumericMatrix distMatrix(nrow,nrow);
 
  for(int i=0; i<nrow; i++){
    for(int j=i; j<nrow; j++){
       if(i==j){
         distMatrix(i,j)=1;
       }else{
         NumericVector nx=(mt(i,_));
         std::vector<double> x =Rcpp::as<std::vector<double> >(nx);
         NumericVector ny=mt(j,_);
         std::vector<double> y =Rcpp::as<std::vector<double> >(ny);
         const double rho=spearmanRho(x, y);
         distMatrix(i,j)=rho;
         distMatrix(j,i)=rho;  
       }      
    }
  }
  
  return wrap(distMatrix);
}

