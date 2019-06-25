#include <rray.h>
#include <dispatch.h>
#include <type2.h>
#include <cast.h>

// Enforced that low/high are the same inner type as `x`, but they are scalar values

template <typename T>
xt::rarray<T> rray__clip_impl(const xt::rarray<T>& x,
                              Rcpp::RObject low,
                              Rcpp::RObject high) {

  int x_type = TYPEOF(SEXP(x));

  if (x_type == REALSXP) {
    double xt_low = Rcpp::as<double>(low);
    double xt_high = Rcpp::as<double>(high);

    return xt::clip(x, xt_low, xt_high);
  }
  else if (x_type == INTSXP) {
    int xt_low = Rcpp::as<int>(low);
    int xt_high = Rcpp::as<int>(high);

    return xt::clip(x, xt_low, xt_high);
  }
  else if (x_type == LGLSXP) {
    bool xt_low = Rcpp::as<bool>(low);
    bool xt_high = Rcpp::as<bool>(high);

    return xt::clip(x, xt_low, xt_high);
  }

  // Should never get called
  error_unknown_type();
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__clip(Rcpp::RObject x, Rcpp::RObject low, Rcpp::RObject high) {

  if (r_is_null(x)) {
    return x;
  }

  Rcpp::RObject out;
  DISPATCH_UNARY_TWO(out, rray__clip_impl, x, low, high);
  rray__set_dim_names(out, rray__dim_names(x));
  return out;
}

