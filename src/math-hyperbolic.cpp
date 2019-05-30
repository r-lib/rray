#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__sinh_impl(const xt::rarray<T>& x) {
  return xt::sinh(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__sinh(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__sinh_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__cosh_impl(const xt::rarray<T>& x) {
  return xt::cosh(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__cosh(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__cosh_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__tanh_impl(const xt::rarray<T>& x) {
  return xt::tanh(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__tanh(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__tanh_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__asinh_impl(const xt::rarray<T>& x) {
  return xt::asinh(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__asinh(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__asinh_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__acosh_impl(const xt::rarray<T>& x) {
  return xt::acosh(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__acosh(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__acosh_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__atanh_impl(const xt::rarray<T>& x) {
  return xt::atanh(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__atanh(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__atanh_impl, x);
}

// -----------------------------------------------------------------------------
