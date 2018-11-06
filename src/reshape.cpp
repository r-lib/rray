#include <mtrx_types.h>
#include <Rcpp.h>
using namespace Rcpp;

using shape_type = std::vector<std::size_t>;

// [[Rcpp::export]]
xt::rarray<double> rray_reshape_cpp(xt::rarray<double> x, IntegerVector shape) {
  shape_type new_shape = as<shape_type>(shape);
  x.reshape(new_shape);
  return(x);
}
