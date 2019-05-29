#include <rray.h>
#include <dispatch.h>

// All return doubles. this does so with only 1 allocation (so does base R)

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__exp_impl(const xt::rarray<T>& x) {
  return xt::exp(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__exp(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__exp_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__exp2_impl(const xt::rarray<T>& x) {
  return xt::exp2(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__exp2(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__exp2_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__expm1_impl(const xt::rarray<T>& x) {
  return xt::expm1(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__expm1(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__expm1_impl, x);
}

// -----------------------------------------------------------------------------

// Only perform the base calculation if we have to, for speed

template <typename T>
xt::rarray<double> rray__log_impl(const xt::rarray<T>& x, Rcpp::RObject base) {

  xt::rarray<double> res;

  // Natural log
  if (r_is_null(base)) {
    res = xt::log(x);
  }
  // Based log
  else {
    double base_dbl = Rcpp::as<double>(base);
    res = xt::log(x) / std::log(base_dbl);
  }

  return res;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__log(Rcpp::RObject x, Rcpp::RObject base) {
  Rcpp::RObject out;
  DISPATCH_UNARY_ONE(out, rray__log_impl, x, base);
  rray__set_dim_names(out, rray__dim_names(x));
  return out;
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__log2_impl(const xt::rarray<T>& x) {
  return xt::log2(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__log2(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__log2_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__log10_impl(const xt::rarray<T>& x) {
  return xt::log10(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__log10(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__log10_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__log1p_impl(const xt::rarray<T>& x) {
  return xt::log1p(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__log1p(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__log1p_impl, x);
}

// -----------------------------------------------------------------------------




