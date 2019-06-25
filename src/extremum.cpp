#include <rray.h>
#include <dispatch.h>
#include <type2.h>
#include <cast.h>

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<T> rray__maximum_impl(const xt::rarray<T>& x,
                                 const xt::rarray<T>& y) {

  auto views = rray__increase_dims_view2(x, y);
  auto x_view = std::get<0>(views);
  auto y_view = std::get<1>(views);

  return xt::maximum(x_view, y_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__maximum(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY_MATH(rray__maximum_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<T> rray__minimum_impl(const xt::rarray<T>& x,
                                 const xt::rarray<T>& y) {

  auto views = rray__increase_dims_view2(x, y);
  auto x_view = std::get<0>(views);
  auto y_view = std::get<1>(views);

  return xt::minimum(x_view, y_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__minimum(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY_MATH(rray__minimum_impl, x, y);
}
