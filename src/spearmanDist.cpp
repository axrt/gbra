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
  
  std::vector<val_rank*> xs(x);
  std::vector<val_rank*> ys(y);
  
  std::stable_sort(xs.begin(), xs.end(), greater);
  std::stable_sort(ys.begin(), ys.end(), greater);
  
  ranks(xs);
  ranks(ys);
  
  const double meanX=meanOfRanks(x);
  const double meanY=meanOfRanks(y);
  
  std::vector<double> ds;
  std::vector<double> dsqx;
  std::vector<double> dsqy;
  
  for(int i=0;i<x.size();i++){
    const double xi=x.at(i)->rank-meanX;
    const double yi=y.at(i)->rank-meanY;
    ds.push_back(xi*yi);
    dsqx.push_back(pow(xi,2));
    dsqy.push_back(pow(yi,2));
  }
  const double dsum=std::accumulate(ds.begin(),ds.end(),0);
  const double dsqsumx=std::accumulate(dsqx.begin(),dsqx.end(),0);
  const double dsqsumy=std::accumulate(dsqy.begin(),dsqy.end(),0);
  const double denominator=sqrt(dsqsumx*dsqsumy);
  
  const double rho = dsum/denominator;  
  return 1-rho; //cuz that's the distance in fact
}

// [[Rcpp::export]]
RcppExport SEXP spearmanDist(SEXP mtx){
  
  NumericMatrix mt(mtx);
  const int nrow=mt.nrow();
  const int ncol=mt.ncol();
  
  NumericMatrix distMatrix(nrow,nrow);
  
  std::vector<val_rank*> x;
  std::vector<val_rank*> y;
  for(int i=0;i<ncol;i++){
    val_rank * dx=new val_rank;
    x.push_back(dx);
    val_rank * dy=new val_rank;
    y.push_back(dy);
  }
  
  for(int i=0; i<nrow; i++){
    for(int j=i; j<nrow; j++){
       if(i==j){
         distMatrix(i,j)=1;
       }else{
         
         for(int k=0;k<ncol;k++){
           val_rank * dx = x.at(k);
           dx->number=k;
           dx->val=mt(i,k);
           dx->rank=0;
         }
         
         for(int k=0;k<ncol;k++){
           val_rank * dy = y.at(k);
           dy->number=k;
           dy->val=mt(j,k);
           dy->rank=0;
         }
         const double rho=spearmanRho(x, y);
         distMatrix(i,j)=rho;
         distMatrix(j,i)=rho;  
       }      
    }
  }
  for(int i=0;i<ncol;i++){
    delete x.at(i);
    delete y.at(i);
  }
  
  return wrap(distMatrix);
}

