#include <rray_types.h>
#include <Rcpp.h>
using namespace Rcpp;

using shape_type = std::vector<std::size_t>;

// [[Rcpp::export]]
xt::rarray<double> rray_broadcast_cpp(xt::rarray<double> x, IntegerVector shape) {
  shape_type new_shape = as<shape_type>(shape);
  xt::rarray<double> res = xt::broadcast(x, new_shape);
  return(res);
}

// [[Rcpp::export]]
bool rray_broadcast_shape_cpp(IntegerVector src_shape, IntegerVector dest_shape) {
  shape_type new_from_shape = as<shape_type>(src_shape);
  shape_type new_to_shape = as<shape_type>(dest_shape);
  bool new_shape = xt::broadcast_shape(new_from_shape, new_to_shape);
  return(new_shape);
}

