#include <rray_types.h>
#include <Rcpp.h>
using namespace Rcpp;

using shape_type = std::vector<std::size_t>;

// [[Rcpp::export]]
xt::rarray<double> rray_reshape_cpp(xt::rarray<double> x, IntegerVector shape) {
  shape_type new_shape = as<shape_type>(shape);
  x.reshape(new_shape);
  return(x);
}

// this also has an argument for axis to squeeze so it doesnt do them all

// [[Rcpp::export]]
xt::rarray<double> rray_squeeze_cpp(xt::rarray<double> x, IntegerVector axis) {

  // better way?
  LogicalVector axis_na = LogicalVector::create(IntegerVector::is_na(axis[0]));
  bool axis_na_bool = as<bool>(axis_na);

  xt::rarray<double> x_sq;

  if(axis_na_bool) {
    x_sq = xt::squeeze(x);
  }
  else {
    x_sq = xt::squeeze(x, axis);
  }

  return x_sq;
}
