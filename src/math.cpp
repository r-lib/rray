#include "mtrx_types.h"
#include "xtensor/xmath.hpp"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
xt::rarray<double> rray_atan_cpp(xt::rarray<double> x) {
  return(xt::atan(x));
}
