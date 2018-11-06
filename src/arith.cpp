#include "mtrx_types.h"
#include <Rcpp.h>
using namespace Rcpp;

// Broadcast sum
// [[Rcpp::export]]
xt::rarray<double> rray_add_cpp(xt::rarray<double> x, xt::rarray<double> y) {
  xt::rarray<double> res = x + y;
  return res;
}

// Broadcast subtraction
// [[Rcpp::export]]
xt::rarray<double> rray_subtract_cpp(xt::rarray<double> x, xt::rarray<double> y) {
  xt::rarray<double> res = x - y;
  return res;
}

// Broadcast multiply
// [[Rcpp::export]]
xt::rarray<double> rray_multiply_cpp(xt::rarray<double> x, xt::rarray<double> y) {
  xt::rarray<double> res = x * y;
  return res;
}


// Broadcast divide
// [[Rcpp::export]]
xt::rarray<double> rray_divide_cpp(xt::rarray<double> x, xt::rarray<double> y) {
  xt::rarray<double> res = x / y;
  return res;
}
