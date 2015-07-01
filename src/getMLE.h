#include <Rcpp.h>
#include <stdio.h>
#include <stdlib.h> 
#include <cmath> 
using namespace Rcpp;

inline const double mean(const NumericVector& nmv);
inline const NumericVector * rowMeans (const NumericMatrix& mtx);
inline const NumericVector * rowSums (const NumericMatrix& mtx);
inline const NumericVector * jitter(const NumericVector& nmv);
inline const NumericVector * E (const NumericMatrix& mtx);
inline void fillInMatrix(NumericMatrix& mtx, const double fill);
inline const double diffDist(const NumericVector& iv, const NumericVector& gprime);
inline const double sumDist(const NumericMatrix& mtx, const NumericVector& gprime);
inline const NumericVector * diffDistRatio(const NumericVector& iv, const NumericVector& gprime);
inline const NumericVector * diffDistInverse(const NumericVector& iv, const NumericVector& gprime);
inline const double sumMatrix(const NumericMatrix&mtx);
inline const double meanRow(const NumericVector& v);