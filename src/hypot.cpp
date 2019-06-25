#include <rray.h>
#include <dispatch.h>
#include <cast.h>
#include <type2.h>

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__hypot_impl(const xt::rarray<T>& x,
                                    const xt::rarray<T>& y) {

  auto views = rray__increase_dims_view2(x, y);
  auto x_view = std::get<0>(views);
  auto y_view = std::get<1>(views);

  return xt::hypot(x_view, y_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__hypot(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY_MATH(rray__hypot_impl, x, y);
}
