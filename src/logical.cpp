#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<rlogical> rray__logical_and_impl(const xt::rarray<T>& x, const xt::rarray<T>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
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

template <typename T>
xt::rarray<rlogical> rray__logical_or_impl(const xt::rarray<T>& x, const xt::rarray<T>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
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
