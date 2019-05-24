#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__square_impl(const xt::rarray<T>& x) {
  return xt::square(xt::cast<double>(x));
}

xt::rarray<double> rray__square_impl(const xt::rarray<double>& x) {
  return xt::square(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__square(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__square_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__cube_impl(const xt::rarray<T>& x) {
  return xt::cube(xt::cast<double>(x));
}

xt::rarray<double> rray__cube_impl(const xt::rarray<double>& x) {
  return xt::cube(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__cube(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__cube_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__sqrt_impl(const xt::rarray<T>& x) {
  return xt::sqrt(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__sqrt(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__sqrt_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__cbrt_impl(const xt::rarray<T>& x) {
  return xt::cbrt(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__cbrt(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__cbrt_impl, x);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__hypot_impl(const xt::rarray<T>& x,
                                    const xt::rarray<T>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return xt::hypot(x_view, y_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__hypot(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__hypot_impl, x, y);
}
