#include <rray.h>
#include <dispatch.h>
#include <type2.h>
#include <cast.h>

// -----------------------------------------------------------------------------

// TODO - handle any integer overflow somehow?

template <typename T>
xt::rarray<T> rray__multiply_add_impl(const xt::rarray<T>& x,
                                      const xt::rarray<T>& y,
                                      const xt::rarray<T>& z) {

  // Common dim
  Rcpp::IntegerVector x_dim = rray__dim(SEXP(x));
  Rcpp::IntegerVector y_dim = rray__dim(SEXP(y));
  Rcpp::IntegerVector z_dim = rray__dim(SEXP(z));

  Rcpp::IntegerVector dim = rray__dim2(rray__dim2(x_dim, y_dim), z_dim);

  const int& dim_n = dim.size();

  auto x_view = rray__increase_dims_view(x, dim_n);
  auto y_view = rray__increase_dims_view(y, dim_n);
  auto z_view = rray__increase_dims_view(z, dim_n);

  return xt::fma(x_view, y_view, z_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__multiply_add(Rcpp::RObject x, Rcpp::RObject y, Rcpp::RObject z) {

  Rcpp::IntegerVector x_dim = rray__dim(x);
  Rcpp::IntegerVector y_dim = rray__dim(y);
  Rcpp::IntegerVector z_dim = rray__dim(z);

  Rcpp::IntegerVector dim = rray__dim2(rray__dim2(x_dim, y_dim), z_dim);

  Rcpp::List resized_x_dim_names = rray__resize_dim_names(rray__dim_names(x), dim);
  Rcpp::List resized_y_dim_names = rray__resize_dim_names(rray__dim_names(y), dim);
  Rcpp::List resized_z_dim_names = rray__resize_dim_names(rray__dim_names(z), dim);

  Rcpp::List new_dim_names;

  new_dim_names = rray__coalesce_dim_names(
    resized_x_dim_names,
    resized_y_dim_names
  );

  new_dim_names = rray__coalesce_dim_names(
    new_dim_names,
    resized_z_dim_names
  );

  Rcpp::RObject type = vec__type_inner2(vec__type_inner2(x, y), z);
  x = vec__cast_inner(x, type);
  y = vec__cast_inner(y, type);
  z = vec__cast_inner(z, type);

  Rcpp::RObject out;
  DISPATCH_TRINARY_NO_LOGICAL(out, rray__multiply_add_impl, x, y, z);

  rray__set_dim_names(out, new_dim_names);

  return out;
}

// -----------------------------------------------------------------------------

// - for doubles, this should return a double, not an integer.
// this is so that `rray_sign(NaN)` correctly returns `NaN`
// - for ints, we can return an int. base R does not.
//   e.g. `storage.mode(sign(1L))`

template <typename T>
Rcpp::RObject rray__sign_impl(const xt::rarray<T>& x) {
  xt::rarray<T> res = xt::sign(x);
  return Rcpp::as<Rcpp::RObject>(res);
}

Rcpp::RObject rray__sign_impl(const xt::rarray<rlogical>& x) {
  xt::rarray<int> res = xt::sign(x);
  return Rcpp::as<Rcpp::RObject>(res);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__sign(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__sign_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__fmod_impl(const xt::rarray<T>& x,
                                   const xt::rarray<T>& y) {

  auto views = rray__increase_dims_view2(x, y);
  auto x_view = std::get<0>(views);
  auto y_view = std::get<1>(views);

  return xt::fmod(x_view, y_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__fmod(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY_MATH(rray__fmod_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__remainder_impl(const xt::rarray<T>& x,
                                        const xt::rarray<T>& y) {

  auto views = rray__increase_dims_view2(x, y);
  auto x_view = std::get<0>(views);
  auto y_view = std::get<1>(views);

  return xt::remainder(x_view, y_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__remainder(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY_MATH(rray__remainder_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<T> rray__maximum_impl(const xt::rarray<T>& x,
                                 const xt::rarray<T>& y) {

  auto views = rray__increase_dims_view2(x, y);
  auto x_view = std::get<0>(views);
  auto y_view = std::get<1>(views);

  return xt::maximum(x_view, y_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__maximum(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY_MATH(rray__maximum_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<T> rray__minimum_impl(const xt::rarray<T>& x,
                                 const xt::rarray<T>& y) {

  auto views = rray__increase_dims_view2(x, y);
  auto x_view = std::get<0>(views);
  auto y_view = std::get<1>(views);

  return xt::minimum(x_view, y_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__minimum(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY_MATH(rray__minimum_impl, x, y);
}

// -----------------------------------------------------------------------------

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

