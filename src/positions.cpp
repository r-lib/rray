#include <xtensor/xsort.hpp>
#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<T> rray__sort_impl(const xt::rarray<T>& x, Rcpp::IntegerVector axis) {
  std::ptrdiff_t xt_axis = Rcpp::as<std::ptrdiff_t>(axis);
  xt::rarray<T> res = xt::sort(x, xt_axis);
  return res;
}

// [[Rcpp::export]]
Rcpp::RObject rray__sort(Rcpp::RObject x, Rcpp::IntegerVector axis) {
  DISPATCH_UNARY_ONE(rray__sort_impl, x, axis);
}

// -----------------------------------------------------------------------------
