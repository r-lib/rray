#include "mtrx_types.h"
#include <Rcpp.h>

// Broadcast sum
// [[Rcpp::export]]
xt::rarray<double> mtrx_add(xt::rarray<double> x, xt::rarray<double> y) {
  xt::rarray<double> res = x + y;
  return res;
}

// Broadcast subtraction
// [[Rcpp::export]]
xt::rarray<double> mtrx_subtract(xt::rarray<double> x, xt::rarray<double> y) {
  xt::rarray<double> res = x - y;
  return res;
}

// Broadcast multiply
// [[Rcpp::export]]
xt::rarray<double> mtrx_multiply(xt::rarray<double> x, xt::rarray<double> y) {
  xt::rarray<double> res = x * y;
  return res;
}


// Broadcast divide
// [[Rcpp::export]]
xt::rarray<double> mtrx_divide(xt::rarray<double> x, xt::rarray<double> y) {
  xt::rarray<double> res = x * y;
  return res;
}
