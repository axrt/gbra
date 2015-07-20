#include <Rcpp.h>
#include <math.h>
#include <algorithm> 
using namespace Rcpp;

struct val_rank{
  double val;
  double rank;
};

static bool greater(val_rank& a, val_rank& b){
  return a.val <= b.val;
}

inline const std::vector<double> * ranks(std::vector<val_rank&> &sorted){
  
  std::vector<double> * rank=new std::vector<double>();
  
   for(int i=0;i<sorted.size()-1;i++){
   double r=i+1;
   int c=i;
   while(i<sorted.size()-1){
     if(sorted.at(i).val!=sorted.at(i+1).val){
       if(i+1==sorted.size()-1){
         rank->push_back(sorted.at(i+1).rank);
       }
       break;
     }
     r+=sorted.at(i).rank;
     i++;
   }
   int diff=i-c+1;
     r=r/diff;
     for(int j=0;j<diff;j++){
       rank->push_back(r);
   }
  }
  return rank;
} 

inline const double spearmanRho(std::vector<val_rank&> &x, std::vector<val_rank&> &y){

  std::stable_sort(x.begin(), x.end(), greater);
  std::stable_sort(y.begin(), y.end(), greater);
  
  const std::vector<double> * xr=ranks(x);
  const std::vector<double> * yr=ranks(y);
  
  std::vector<double> ds;
  for(int i=0;i<xr->size();i++){
    ds.push_back(pow(xr->at(i)-yr->at(i),2));
  }
  delete xr;
  delete yr;
  const double dsqareds=std::accumulate(ds.begin(),ds.end(),0);
  const double nsquared=pow(ds.size(),2);
  
  const double rho = 1-(6*dsqareds)/(ds.size()*(nsquared-1));  
  return rho;
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
         std::vector<val_rank&> x;
         for(int k=0;k<mt.ncol();k++){
           val_rank d;
           d.val=mt(i,k);
           d.rank=0;
           x.push_back(d);
         }
         std::vector<val_rank&> y;
         for(int k=0;k<mt.ncol();k++){
           val_rank d;
           d.val=mt(i,k);
           d.rank=0;
           y.push_back(d);
         }
         const double rho=spearmanRho(x, y);
         distMatrix(i,j)=rho;
         distMatrix(j,i)=rho;  
       }      
    }
  }
  
  return wrap(distMatrix);
}

