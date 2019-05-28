#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__greater_impl(const xt::rarray<T>& x,
                                 const xt::rarray<T>& y,
                                 Rcpp::List new_dim_names) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  xt::rarray<rlogical> res = x_view > y_view;
  Rf_setAttrib(SEXP(res), R_DimNamesSymbol, new_dim_names);

  return Rcpp::as<Rcpp::RObject>(res);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__greater(Rcpp::RObject x, Rcpp::RObject y, Rcpp::List new_dim_names) {
  DISPATCH_BINARY_ONE(rray__greater_impl, x, y, new_dim_names);
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__greater_equal_impl(const xt::rarray<T>& x,
                                       const xt::rarray<T>& y,
                                       Rcpp::List new_dim_names) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  xt::rarray<rlogical> res = x_view >= y_view;
  Rf_setAttrib(SEXP(res), R_DimNamesSymbol, new_dim_names);

  return Rcpp::as<Rcpp::RObject>(res);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__greater_equal(Rcpp::RObject x, Rcpp::RObject y, Rcpp::List new_dim_names) {
  DISPATCH_BINARY_ONE(rray__greater_equal_impl, x, y, new_dim_names);
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__lesser_impl(const xt::rarray<T>& x,
                                const xt::rarray<T>& y,
                                Rcpp::List new_dim_names) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  xt::rarray<rlogical> res = x_view < y_view;
  Rf_setAttrib(SEXP(res), R_DimNamesSymbol, new_dim_names);

  return Rcpp::as<Rcpp::RObject>(res);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__lesser(Rcpp::RObject x, Rcpp::RObject y, Rcpp::List new_dim_names) {
  DISPATCH_BINARY_ONE(rray__lesser_impl, x, y, new_dim_names);
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__lesser_equal_impl(const xt::rarray<T>& x,
                                      const xt::rarray<T>& y,
                                      Rcpp::List new_dim_names) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  xt::rarray<rlogical> res = x_view <= y_view;
  Rf_setAttrib(SEXP(res), R_DimNamesSymbol, new_dim_names);

  return Rcpp::as<Rcpp::RObject>(res);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__lesser_equal(Rcpp::RObject x, Rcpp::RObject y, Rcpp::List new_dim_names) {
  DISPATCH_BINARY_ONE(rray__lesser_equal_impl, x, y, new_dim_names);
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__equal_impl(const xt::rarray<T>& x,
                               const xt::rarray<T>& y,
                               Rcpp::List new_dim_names) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  xt::rarray<rlogical> res = xt::equal(x_view, y_view);
  Rf_setAttrib(SEXP(res), R_DimNamesSymbol, new_dim_names);

  return Rcpp::as<Rcpp::RObject>(res);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__equal(Rcpp::RObject x, Rcpp::RObject y, Rcpp::List new_dim_names) {
  DISPATCH_BINARY_ONE(rray__equal_impl, x, y, new_dim_names);
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__not_equal_impl(const xt::rarray<T>& x,
                                   const xt::rarray<T>& y,
                                   Rcpp::List new_dim_names) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  xt::rarray<rlogical> res = xt::not_equal(x_view, y_view);
  Rf_setAttrib(SEXP(res), R_DimNamesSymbol, new_dim_names);

  return Rcpp::as<Rcpp::RObject>(res);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__not_equal(Rcpp::RObject x, Rcpp::RObject y, Rcpp::List new_dim_names) {
  DISPATCH_BINARY_ONE(rray__not_equal_impl, x, y, new_dim_names);
}

// -----------------------------------------------------------------------------
// Strict equality

// - No broadcasting is performed (the shape is part of the check)
// - At the R level, the types are allowed to be different and a FALSE is
//   returned in those cases before we get here. This checks the actual values
//   and shape

template <typename T>
xt::rarray<rlogical> rray__all_equal_impl(const xt::rarray<T>& x,
                                          const xt::rarray<T>& y) {
  xt::rarray<rlogical> res = (x == y);
  return res;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__all_equal(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__all_equal_impl, x, y);
}

// -----------------------------------------------------------------------------
// Strict in-equality

template <typename T>
xt::rarray<rlogical> rray__any_not_equal_impl(const xt::rarray<T>& x,
                                              const xt::rarray<T>& y) {
  xt::rarray<rlogical> res = (x != y);
  return res;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__any_not_equal(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__any_not_equal_impl, x, y);
}

// -----------------------------------------------------------------------------
