#include <rray.h>
#include <dispatch.h>

// Even though there are opportunities for these to return integers (i.e.
// you are passed an integer or logical) both base R and C++ return doubles
// so we do too. One case where you have to return a double is with NaN, even
// though all of these functions return integers in theory

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__ceiling_impl(const xt::rarray<T>& x) {
  return xt::ceil(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__ceiling(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__ceiling_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__floor_impl(const xt::rarray<T>& x) {
  return xt::floor(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__floor(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__floor_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__trunc_impl(const xt::rarray<T>& x) {
  return xt::trunc(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__trunc(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__trunc_impl, x);
}

// -----------------------------------------------------------------------------

// For round(), use base R's version that also supports `digits`
// xtensor's does not, since C++ does not
// https://xtensor.readthedocs.io/en/latest/api/nearint_operations.html#_CPPv4I0EN2xt5roundEDaRR1E
// http://www.cplusplus.com/reference/cmath/round/

// -----------------------------------------------------------------------------

// Not caring about nearbyint() or rint() for now, they seem to rely on
// a rounding direction controlled by fegetround() which we don't care about
// http://www.cplusplus.com/reference/cmath/rint/

// -----------------------------------------------------------------------------
