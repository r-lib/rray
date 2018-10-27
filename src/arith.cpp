#include "mtrx.h"
#include <Rcpp.h>

// [[Rcpp::plugins(cpp14)]]

//' Broadcast sum
//'
//' @param x,y Numeric vectors, matrices, or arrays
//'
//' @export
// [[Rcpp::export]]
xt::rarray<double> cpp_mtrx_add(xt::rarray<double> x, xt::rarray<double> y) {
  xt::rarray<double> res = x + y;
  return res;
}

//' Broadcast subtraction
//'
//' @param x,y Numeric vectors, matrices, or arrays
//'
//' @export
// [[Rcpp::export]]
xt::rarray<double> cpp_mtrx_subtract(xt::rarray<double> x, xt::rarray<double> y) {
  xt::rarray<double> res = x - y;
  return res;
}
