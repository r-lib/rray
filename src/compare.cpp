#include <rray.h>
#include <dispatch.h>

// TODO - Depending on the adjustment made in this pr, we might be able
// to remove the early exits calls. Currently xtensor does not match the
// numpy behavior
// https://github.com/QuantStack/xtensor/issues/1562

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<rlogical> rray__greater_impl(const xt::rarray<T>& x, const xt::rarray<T>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));

  // If any dimension is size 0, return an empty logical array with common dim
  if (Rcpp::is_true(Rcpp::any(dim == 0))) {
    xt::rarray<rlogical> res(Rcpp::as<std::vector<std::size_t>>(dim));
    return(res);
  }

  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return x_view > y_view;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__greater(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__greater_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<rlogical> rray__greater_equal_impl(const xt::rarray<T>& x, const xt::rarray<T>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));

  // If any dimension is size 0, return an empty logical array with common dim
  if (Rcpp::is_true(Rcpp::any(dim == 0))) {
    xt::rarray<rlogical> res(Rcpp::as<std::vector<std::size_t>>(dim));
    return(res);
  }

  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return x_view >= y_view;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__greater_equal(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__greater_equal_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<rlogical> rray__lesser_impl(const xt::rarray<T>& x, const xt::rarray<T>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));

  // If any dimension is size 0, return an empty logical array with common dim
  if (Rcpp::is_true(Rcpp::any(dim == 0))) {
    xt::rarray<rlogical> res(Rcpp::as<std::vector<std::size_t>>(dim));
    return(res);
  }

  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return x_view < y_view;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__lesser(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__lesser_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<rlogical> rray__lesser_equal_impl(const xt::rarray<T>& x, const xt::rarray<T>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));

  // If any dimension is size 0, return an empty logical array with common dim
  if (Rcpp::is_true(Rcpp::any(dim == 0))) {
    xt::rarray<rlogical> res(Rcpp::as<std::vector<std::size_t>>(dim));
    return(res);
  }

  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return x_view <= y_view;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__lesser_equal(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__lesser_equal_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<rlogical> rray__equal_impl(const xt::rarray<T>& x, const xt::rarray<T>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));

  // If any dimension is size 0, return an empty logical array with common dim
  if (Rcpp::is_true(Rcpp::any(dim == 0))) {
    xt::rarray<rlogical> res(Rcpp::as<std::vector<std::size_t>>(dim));
    return(res);
  }

  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return xt::equal(x_view, y_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__equal(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__equal_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<rlogical> rray__not_equal_impl(const xt::rarray<T>& x, const xt::rarray<T>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));

  // If any dimension is size 0, return an empty logical array with common dim
  if (Rcpp::is_true(Rcpp::any(dim == 0))) {
    xt::rarray<rlogical> res(Rcpp::as<std::vector<std::size_t>>(dim));
    return(res);
  }

  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return xt::not_equal(x_view, y_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__not_equal(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__not_equal_impl, x, y);
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
