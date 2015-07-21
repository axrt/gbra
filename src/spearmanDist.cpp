#include <Rcpp.h>
#include <math.h>
#include <algorithm> 
using namespace Rcpp;

struct val_rank{
  int number;
  double val;
  double rank;
};

inline const double meanOfRanks(std::vector<val_rank*>&vec){
  int ra=0;
  for(int i=0;i<vec.size();i++){
    ra+=vec.at(i)->rank;
  }
  return ra/vec.size();
}

inline const bool greater(val_rank* a, val_rank* b){
  return a->val <= b->val;
}

inline const bool initialOrder(val_rank* a, val_rank* b){
  return a->number <= b->number;
}

inline std::vector<val_rank*> &ranks(std::vector<val_rank*> &sorted){
  
   for(int i=0;i<sorted.size()-1;i++){
   double r=i+1;
   int c=i;
   while(i<sorted.size()-1){
     if(sorted.at(i)->val!=sorted.at(i+1)->val){
       if(i+1==sorted.size()-1){
         sorted.at(i+1)->rank=i+2;
       }
       break;
     }
     r+=i+2;
     i++;
   }
   int diff=i-c+1;
     r=r/diff;
     for(int j=c;j<=i;j++){
       sorted.at(j)->rank=r;
   }
  }
  return sorted;
} 

inline const double spearmanRho(std::vector<val_rank*> &x, std::vector<val_rank*> &y){

  std::stable_sort(x.begin(), x.end(), greater);
  std::stable_sort(y.begin(), y.end(), greater);
  
  for(int i=0;i<x.size();i++){
   Rcout<<x.at(i)->number<<'\t'<<x.at(i)->rank<<'\t'<<x.at(i)->val<<std::endl;  
  }
  Rcout<<std::endl;
  Rcout<<std::endl;
  
  std::vector<val_rank*>& xr=ranks(x);
  std::vector<val_rank*>& yr=ranks(y);
  
  for(int i=0;i<xr.size();i++){
    Rcout<<xr.at(i)->number<<'\t'<<xr.at(i)->rank<<'\t'<<xr.at(i)->val<<std::endl;  
  }
  Rcout<<std::endl;
  Rcout<<std::endl;
  
  std::stable_sort(xr.begin(), xr.end(), initialOrder);
  std::stable_sort(yr.begin(), yr.end(), initialOrder);
  
  for(int i=0;i<xr.size();i++){
    Rcout<<xr.at(i)->number<<'\t'<<xr.at(i)->rank<<'\t'<<xr.at(i)->val<<std::endl;  
  }
  Rcout<<std::endl;
  Rcout<<std::endl;
  
  const double meanX=meanOfRanks(xr);
  const double meanY=meanOfRanks(yr);
  
  std::vector<double> ds;
  std::vector<double> dsqx;
  std::vector<double> dsqy;
  for(int i=0;i<xr.size();i++){
    const double xi=xr.at(i)->rank-meanX;
    const double yi=yr.at(i)->rank-meanY;
    ds.push_back(xi*yi);
    dsqx.push_back(pow(xi,2));
    dsqy.push_back(pow(yi,2));
  }
  const double dsum=std::accumulate(ds.begin(),ds.end(),0);
  const double dsqsumx=std::accumulate(dsqx.begin(),dsqx.end(),0);
  const double dsqsumy=std::accumulate(dsqy.begin(),dsqy.end(),0);
  const double denominator=sqrt(dsqsumx*dsqsumy);
  
  const double rho = dsum/denominator;  
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
         std::vector<val_rank*> x;
         for(int k=0;k<mt.ncol();k++){
           val_rank * d=new val_rank;
           d->number=k;
           d->val=mt(i,k);
           d->rank=0;
           x.push_back(d);
         }
         std::vector<val_rank*> y;
         for(int k=0;k<mt.ncol();k++){
           val_rank * d=new val_rank;
           d->number=k;
           d->val=mt(j,k);
           d->rank=0;
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

