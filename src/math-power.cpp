#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__square_impl(const xt::rarray<T>& x) {
  return xt::square(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__square(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__square_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__cube_impl(const xt::rarray<T>& x) {
  return xt::cube(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__cube(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__cube_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__sqrt_impl(const xt::rarray<T>& x) {
  return xt::sqrt(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__sqrt(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__sqrt_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__cbrt_impl(const xt::rarray<T>& x) {
  return xt::cbrt(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__cbrt(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__cbrt_impl, x);
}

// -----------------------------------------------------------------------------
