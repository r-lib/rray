#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__sin_impl(const xt::rarray<T>& x) {
  return xt::sin(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__sin(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__sin_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__cos_impl(const xt::rarray<T>& x) {
  return xt::cos(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__cos(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__cos_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__tan_impl(const xt::rarray<T>& x) {
  return xt::tan(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__tan(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__tan_impl, x);
}

// -----------------------------------------------------------------------------


template <typename T>
xt::rarray<double> rray__asin_impl(const xt::rarray<T>& x) {
  return xt::asin(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__asin(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__asin_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__acos_impl(const xt::rarray<T>& x) {
  return xt::acos(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__acos(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__acos_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__atan_impl(const xt::rarray<T>& x) {
  return xt::atan(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__atan(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__atan_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__atan2_impl(const xt::rarray<T>& y, const xt::rarray<T>& x) {
  return xt::atan2(y, x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__atan2(Rcpp::RObject y, Rcpp::RObject x) {
  DISPATCH_BINARY(rray__atan2_impl, y, x);
}
