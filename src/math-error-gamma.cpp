#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__erf_impl(const xt::rarray<T>& x) {
  return xt::erf(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__erf(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__erf_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__erfc_impl(const xt::rarray<T>& x) {
  return xt::erfc(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__erfc(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__erfc_impl, x);
}

// -----------------------------------------------------------------------------

// xt::tgamma() == base::gamma()

template <typename T>
xt::rarray<double> rray__gamma_impl(const xt::rarray<T>& x) {
  return xt::tgamma(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__gamma(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__gamma_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__lgamma_impl(const xt::rarray<T>& x) {
  return xt::lgamma(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__lgamma(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__lgamma_impl, x);
}
