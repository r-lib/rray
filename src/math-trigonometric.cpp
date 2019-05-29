#include <rray.h>
#include <dispatch.h>
#include <cast.h>
#include <utils.h>

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__sin_impl(const xt::rarray<T>& x) {
  return xt::sin(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__sin(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__sin_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__cos_impl(const xt::rarray<T>& x) {
  return xt::cos(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__cos(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__cos_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__tan_impl(const xt::rarray<T>& x) {
  return xt::tan(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__tan(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__tan_impl, x);
}

// -----------------------------------------------------------------------------


template <typename T>
xt::rarray<double> rray__asin_impl(const xt::rarray<T>& x) {
  return xt::asin(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__asin(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__asin_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__acos_impl(const xt::rarray<T>& x) {
  return xt::acos(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__acos(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__acos_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__atan_impl(const xt::rarray<T>& x) {
  return xt::atan(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__atan(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__atan_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__atan2_impl(const xt::rarray<T>& y, const xt::rarray<T>& x) {

  auto views = rray__increase_dims_view2(y, x);
  auto y_view = std::get<0>(views);
  auto x_view = std::get<1>(views);

  return xt::atan2(y_view, x_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__atan2(Rcpp::RObject y, Rcpp::RObject x) {
  if (r_is_null(x) || r_is_null(y)) {
    return R_NilValue;
  }

  Rcpp::List new_dim_names = rray__dim_names2(x, y);

  // Always cast to numeric!
  x = vec__cast_inner(x, rray_shared_empty_dbl);
  y = vec__cast_inner(y, rray_shared_empty_dbl);

  Rcpp::RObject out;
  DISPATCH_BINARY_SIMPLE(out, rray__atan2_impl, x, y);

  rray__set_dim_names(out, new_dim_names);
  return out;
}
