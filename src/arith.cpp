#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__add_impl(const xt::rarray<T>& x,
                             const xt::rarray<T>& y,
                             Rcpp::List new_dim_names) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  xt::rarray<T> res = x_view + y_view;
  Rf_setAttrib(SEXP(res), R_DimNamesSymbol, new_dim_names);

  return Rcpp::as<Rcpp::RObject>(res);
}

// Logicals return integers
Rcpp::RObject rray__add_impl(const xt::rarray<rlogical>& x,
                             const xt::rarray<rlogical>& y,
                             Rcpp::List new_dim_names) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  xt::rarray<int> res = x_view + y_view;
  Rf_setAttrib(SEXP(res), R_DimNamesSymbol, new_dim_names);

  return Rcpp::as<Rcpp::RObject>(res);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__add(Rcpp::RObject x, Rcpp::RObject y, Rcpp::List new_dim_names) {
  DISPATCH_BINARY_ONE(rray__add_impl, x, y, new_dim_names);
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__subtract_impl(const xt::rarray<T>& x,
                                  const xt::rarray<T>& y,
                                  Rcpp::List new_dim_names) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  xt::rarray<T> res = x_view - y_view;
  Rf_setAttrib(SEXP(res), R_DimNamesSymbol, new_dim_names);

  return Rcpp::as<Rcpp::RObject>(res);
}

// Logicals return integers
Rcpp::RObject rray__subtract_impl(const xt::rarray<rlogical>& x,
                                  const xt::rarray<rlogical>& y,
                                  Rcpp::List new_dim_names) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  xt::rarray<int> res = x_view - y_view;
  Rf_setAttrib(SEXP(res), R_DimNamesSymbol, new_dim_names);

  return Rcpp::as<Rcpp::RObject>(res);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__subtract(Rcpp::RObject x, Rcpp::RObject y, Rcpp::List new_dim_names) {
  DISPATCH_BINARY_ONE(rray__subtract_impl, x, y, new_dim_names);
}

// -----------------------------------------------------------------------------

// Should always take and return a numeric result

xt::rarray<double> rray__divide_impl(const xt::rarray<double>& x,
                                     const xt::rarray<double>& y,
                                     Rcpp::List new_dim_names) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  xt::rarray<double> res = x_view / y_view;
  Rf_setAttrib(SEXP(res), R_DimNamesSymbol, new_dim_names);

  return res;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__divide(Rcpp::RObject x, Rcpp::RObject y, Rcpp::List new_dim_names) {
  DISPATCH_BINARY_ONE(rray__divide_impl, x, y, new_dim_names);
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__multiply_impl(const xt::rarray<T>& x,
                                  const xt::rarray<T>& y,
                                  Rcpp::List new_dim_names) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  xt::rarray<T> res = x_view * y_view;
  Rf_setAttrib(SEXP(res), R_DimNamesSymbol, new_dim_names);

  return Rcpp::as<Rcpp::RObject>(res);
}

// Logicals return integers
Rcpp::RObject rray__multiply_impl(const xt::rarray<rlogical>& x,
                                  const xt::rarray<rlogical>& y,
                                  Rcpp::List new_dim_names) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  xt::rarray<int> res = x_view * y_view;
  Rf_setAttrib(SEXP(res), R_DimNamesSymbol, new_dim_names);

  return Rcpp::as<Rcpp::RObject>(res);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__multiply(Rcpp::RObject x, Rcpp::RObject y, Rcpp::List new_dim_names) {
  DISPATCH_BINARY_ONE(rray__multiply_impl, x, y, new_dim_names);
}

// -----------------------------------------------------------------------------

// On the R side, there is a guarantee that both inputs are double.

Rcpp::RObject rray__pow_impl(const xt::rarray<double>& x,
                             const xt::rarray<double>& y,
                             Rcpp::List new_dim_names) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  xt::rarray<double> res = xt::pow(x_view, y_view);
  Rf_setAttrib(SEXP(res), R_DimNamesSymbol, new_dim_names);

  return Rcpp::as<Rcpp::RObject>(res);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__pow(Rcpp::RObject x, Rcpp::RObject y, Rcpp::List new_dim_names) {
  DISPATCH_BINARY_ONE(rray__pow_impl, x, y, new_dim_names);
}

// -----------------------------------------------------------------------------

// Not implementing `rray__modulus()` as it ONLY works with integers
// and crashes when dividing by 0. It is essentially a less stable
// `rray_fmod()` which always returns a double, and can handle the
// dividing by 0 case.

// -----------------------------------------------------------------------------

// Nothing to do in the case of double / integer x

template <typename T>
Rcpp::RObject rray__identity_impl(const xt::rarray<T>& x) {
  return SEXP(x);
}

// Logicals as integers
Rcpp::RObject rray__identity_impl(const xt::rarray<rlogical>& x) {
  xt::rarray<int> xt_res = xt::operator+(x);
  Rcpp::RObject res = rray__restore_dim_names(xt_res, x);
  return res;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__identity(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__identity_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__opposite_impl(const xt::rarray<T>& x) {
  xt::rarray<T> xt_res = xt::operator-(x);
  Rcpp::RObject res = rray__restore_dim_names(xt_res, x);
  return res;
}

// Logicals as integers
Rcpp::RObject rray__opposite_impl(const xt::rarray<rlogical>& x) {
  xt::rarray<int> xt_res = xt::operator-(x);
  Rcpp::RObject res = rray__restore_dim_names(xt_res, x);
  return res;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__opposite(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__opposite_impl, x);
}
