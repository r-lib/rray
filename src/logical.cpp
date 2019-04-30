#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<rlogical> rray__logical_and_impl(const xt::rarray<T>& x, const xt::rarray<T>& y) {
  const int& dims = rray__dims2(rray__dims(SEXP(x)), rray__dims(SEXP(y)));

  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return xt::operator&&(x_view, y_view);
}

// [[Rcpp::export]]
Rcpp::RObject rray__logical_and(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__logical_and_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<rlogical> rray__logical_or_impl(const xt::rarray<T>& x, const xt::rarray<T>& y) {
  const int& dims = rray__dims2(rray__dims(SEXP(x)), rray__dims(SEXP(y)));

  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return xt::operator||(x_view, y_view);
}

// [[Rcpp::export]]
Rcpp::RObject rray__logical_or(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__logical_or_impl, x, y);
}

// -----------------------------------------------------------------------------

// Only logical in and out
// Use rray_cast_inner() at the R level first

xt::rarray<rlogical> rray__logical_not_impl(const xt::rarray<rlogical>& x) {
  return xt::operator!(x);
}

// [[Rcpp::export]]
Rcpp::RObject rray__logical_not(const xt::rarray<rlogical>& x) {
  return Rcpp::as<Rcpp::RObject>(rray__logical_not_impl(x));
}

// -----------------------------------------------------------------------------

// TODO - This should support axes / keep dims

xt::rarray<rlogical> rray__any_impl(const xt::rarray<rlogical>& x) {
  return xt::any(x);
}

// [[Rcpp::export]]
Rcpp::RObject rray__any(const xt::rarray<rlogical>& x) {
  return Rcpp::as<Rcpp::RObject>(rray__any_impl(x));
}
