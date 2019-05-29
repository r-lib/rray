#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

// TODO - handle any integer overflow somehow?

template <typename T>
xt::rarray<T> rray__multiply_add_impl(const xt::rarray<T>& x,
                                      const xt::rarray<T>& y,
                                      const xt::rarray<T>& z) {

  // Common dim
  Rcpp::IntegerVector tmp_dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  Rcpp::IntegerVector dim = rray__dim2(tmp_dim, rray__dim(SEXP(z)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);
  auto z_view = rray__increase_dims_view(z, dims);

  return xt::fma(x_view, y_view, z_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__multiply_add(Rcpp::RObject x, Rcpp::RObject y, Rcpp::RObject z) {
  DISPATCH_TRINARY_NO_LOGICAL(rray__multiply_add_impl, x, y, z);
}

// -----------------------------------------------------------------------------

// logicals return integers. this does so with only 1 allocation (so does base R)

template <typename T>
Rcpp::RObject rray__abs_impl(const xt::rarray<T>& x) {
  xt::rarray<T> res = xt::abs(x);
  return Rcpp::as<Rcpp::RObject>(res);
}

Rcpp::RObject rray__abs_impl(const xt::rarray<rlogical>& x) {
  xt::rarray<int> res = xt::abs(x);
  return Rcpp::as<Rcpp::RObject>(res);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__abs(Rcpp::RObject x) {
  DISPATCH_UNARY_MATH(rray__abs_impl, x);
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

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return xt::fmod(x_view, y_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__fmod(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__fmod_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__remainder_impl(const xt::rarray<T>& x,
                                        const xt::rarray<T>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return xt::remainder(x_view, y_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__remainder(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__remainder_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<T> rray__maximum_impl(const xt::rarray<T>& x,
                                 const xt::rarray<T>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return xt::maximum(x_view, y_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__maximum(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__maximum_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<T> rray__minimum_impl(const xt::rarray<T>& x,
                                 const xt::rarray<T>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return xt::minimum(x_view, y_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__minimum(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__minimum_impl, x, y);
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
  DISPATCH_UNARY_TWO_SIMPLE(out, rray__clip_impl, x, low, high);
  rray__set_dim_names(out, rray__dim_names(x));
  return out;
}

