#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

// All return doubles. this does so with only 1 allocation (so does base R)

template <typename T>
xt::rarray<double> rray__exp_impl(const xt::rarray<T>& x) {
  return xt::exp(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__exp(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__exp_impl, x);
}

// -----------------------------------------------------------------------------
