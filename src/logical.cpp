#include <rray.h>
#include <dispatch.h>

// TODO - Depending on the adjustment made in this pr, we might be able
// to remove the early exits calls. Currently xtensor does not match the
// numpy behavior
// https://github.com/QuantStack/xtensor/issues/1562

// -----------------------------------------------------------------------------

xt::rarray<rlogical> rray__logical_and_impl(const xt::rarray<rlogical>& x,
                                            const xt::rarray<rlogical>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));

  // If any dimension is size 0, return an empty logical array with common dim
  if (Rcpp::is_true(Rcpp::any(dim == 0))) {
    xt::rarray<rlogical> res(Rcpp::as<std::vector<std::size_t>>(dim));
    return(res);
  }

  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return xt::operator&&(x_view, y_view);
}

// [[Rcpp::export]]
Rcpp::RObject rray__logical_and(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__logical_and_impl, x, y);
}

// -----------------------------------------------------------------------------

xt::rarray<rlogical> rray__logical_or_impl(const xt::rarray<rlogical>& x,
                                           const xt::rarray<rlogical>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));

  // If any dimension is size 0, return an empty logical array with common dim
  if (Rcpp::is_true(Rcpp::any(dim == 0))) {
    xt::rarray<rlogical> res(Rcpp::as<std::vector<std::size_t>>(dim));
    return(res);
  }

  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return xt::operator||(x_view, y_view);
}

// [[Rcpp::export]]
Rcpp::RObject rray__logical_or(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__logical_or_impl, x, y);
}

// -----------------------------------------------------------------------------

xt::rarray<rlogical> rray__logical_not_impl(const xt::rarray<rlogical>& x) {
  return xt::operator!(x);
}

// [[Rcpp::export]]
Rcpp::RObject rray__logical_not(const xt::rarray<rlogical>& x) {
  return Rcpp::as<Rcpp::RObject>(rray__logical_not_impl(x));
}

// -----------------------------------------------------------------------------

// TODO - replace with native xt::any() if this issue is resolved
// https://github.com/QuantStack/xtensor/issues/1555
// Note that this is slower than it needs to be because it doesn't early exit

xt::rarray<rlogical> rray__any_impl(const xt::rarray<rlogical>& x, Rcpp::RObject axes) {

  // If `axes = NULL` we can rely on `xt::any()` which finds a "global" any value
  if (r_is_null(axes)) {
    auto x_global_any = xt::any(x);
    xt::rarray<rlogical> res = rray__keep_dims_view(x_global_any, rray__dim(SEXP(x)), axes);
    return res;
  }

  // If `axes != NULL` we implement our own custom reducer
  auto any_reducer = xt::make_xreducer_functor(
    [](bool a, bool b) { return a | b; },
    xt::const_value<bool>(0)
  );
  std::vector<std::size_t> xt_axes = Rcpp::as<std::vector<std::size_t>>(axes);

  auto x_reduced = xt::reduce(any_reducer, x, xt_axes);

  xt::rarray<rlogical> res = rray__keep_dims_view(x_reduced, rray__dim(SEXP(x)), axes);

  return res;
}

// [[Rcpp::export]]
Rcpp::RObject rray__any(const xt::rarray<rlogical>& x, Rcpp::RObject axes) {
  return Rcpp::as<Rcpp::RObject>(rray__any_impl(x, axes));
}
