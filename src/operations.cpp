#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<rlogical> rray__gt_impl(const xt::rarray<T>& x, const xt::rarray<T>& y) {
  const int& dims = rray__dims2(rray__dims(SEXP(x)), rray__dims(SEXP(y)));

  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return x_view > y_view;
}

// [[Rcpp::export]]
Rcpp::RObject rray__gt(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__gt_impl, x, y);
}

// -----------------------------------------------------------------------------


template <typename T>
xt::rarray<rlogical> rray__gte_impl(const xt::rarray<T>& x, const xt::rarray<T>& y) {
  const int& dims = rray__dims2(rray__dims(SEXP(x)), rray__dims(SEXP(y)));

  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return x_view >= y_view;
}

// [[Rcpp::export]]
Rcpp::RObject rray__gte(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__gte_impl, x, y);
}

// -----------------------------------------------------------------------------
