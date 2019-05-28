#include <rray.h>
#include <dispatch.h>

// Required for any() and all()
#include <xtensor/xarray.hpp>

// -----------------------------------------------------------------------------

xt::rarray<rlogical> rray__logical_and_impl(const xt::rarray<rlogical>& x,
                                            const xt::rarray<rlogical>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return xt::operator&&(x_view, y_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__logical_and(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__logical_and_impl, x, y);
}

// -----------------------------------------------------------------------------

xt::rarray<rlogical> rray__logical_or_impl(const xt::rarray<rlogical>& x,
                                           const xt::rarray<rlogical>& y) {

  Rcpp::IntegerVector dim = rray__dim2(rray__dim(SEXP(x)), rray__dim(SEXP(y)));
  const int& dims = dim.size();
  auto x_view = rray__increase_dims_view(x, dims);
  auto y_view = rray__increase_dims_view(y, dims);

  return xt::operator||(x_view, y_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__logical_or(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_BINARY(rray__logical_or_impl, x, y);
}

// -----------------------------------------------------------------------------

xt::rarray<rlogical> rray__logical_not_impl(const xt::rarray<rlogical>& x) {
  return xt::operator!(x);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__logical_not(const xt::rarray<rlogical>& x) {
  return Rcpp::as<Rcpp::RObject>(rray__logical_not_impl(x));
}

// -----------------------------------------------------------------------------

// TODO - replace with native xt::any() if this issue is resolved
// https://github.com/QuantStack/xtensor/issues/1555
// Note that this is slower than it needs to be because it doesn't early exit

// TODO - it also currently fails with multiple axes when one is size 0
// https://github.com/QuantStack/xtensor/issues/1563

xt::rarray<rlogical> rray__any_impl(const xt::rarray<rlogical>& x, Rcpp::RObject axes) {

  // If `axes = NULL` we can rely on `xt::any()` which finds a "global" any value
  if (r_is_null(axes)) {
    xt::xarray<bool> x_global_any = xt::any(x);
    xt::rarray<rlogical> res = rray__keep_dims_view(x_global_any, rray__dim(SEXP(x)), axes);
    return res;
  }

  // If `axes != NULL` we implement our own custom reducer
  auto any_reducer = xt::make_xreducer_functor(
    [](bool a, bool b) { return a || b; },
    xt::const_value<bool>(false)
  );
  std::vector<std::size_t> xt_axes = Rcpp::as<std::vector<std::size_t>>(axes);

  // Currently required to materialize into a xarray<bool> before going to
  // rarray<rlogical>
  xt::xarray<bool> x_reduced = xt::reduce(any_reducer, x, xt_axes);

  xt::rarray<rlogical> res = rray__keep_dims_view(x_reduced, rray__dim(SEXP(x)), axes);

  return res;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__any(const xt::rarray<rlogical>& x, Rcpp::RObject axes) {
  return Rcpp::as<Rcpp::RObject>(rray__any_impl(x, axes));
}

// -----------------------------------------------------------------------------

// TODO - replace with native xt::all() if this issue is resolved
// https://github.com/QuantStack/xtensor/issues/1555
// Note that this is slower than it needs to be because it doesn't early exit

// TODO - it also currently fails with multiple axes when one is size 0
// https://github.com/QuantStack/xtensor/issues/1563

xt::rarray<rlogical> rray__all_impl(const xt::rarray<rlogical>& x, Rcpp::RObject axes) {

  // If `axes = NULL` we can rely on `xt::all()` which finds a "global" all value
  if (r_is_null(axes)) {
    xt::xarray<bool> x_global_all = xt::all(x);
    xt::rarray<rlogical> res = rray__keep_dims_view(x_global_all, rray__dim(SEXP(x)), axes);
    return res;
  }

  // If `axes != NULL` we implement our own custom reducer
  auto all_reducer = xt::make_xreducer_functor(
    [](bool a, bool b) { return a && b; },
    xt::const_value<bool>(true)
  );
  std::vector<std::size_t> xt_axes = Rcpp::as<std::vector<std::size_t>>(axes);

  // Currently required to materialize into a xarray<bool> before going to
  // rarray<rlogical>
  xt::xarray<bool> x_reduced = xt::reduce(all_reducer, x, xt_axes);

  xt::rarray<rlogical> res = rray__keep_dims_view(x_reduced, rray__dim(SEXP(x)), axes);

  return res;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__all(const xt::rarray<rlogical>& x, Rcpp::RObject axes) {
  return Rcpp::as<Rcpp::RObject>(rray__all_impl(x, axes));
}

// -----------------------------------------------------------------------------

// Special kind of dispatch for `rray__if_else()`, first value is always logical

template <typename T>
xt::rarray<T> rray__if_else_impl(const xt::rarray<rlogical>& condition,
                                 const xt::rarray<T>& true_,
                                 const xt::rarray<T>& false_) {

  // Common dim
  Rcpp::IntegerVector tmp_dim = rray__dim2(rray__dim(SEXP(condition)), rray__dim(SEXP(true_)));
  Rcpp::IntegerVector dim = rray__dim2(tmp_dim, rray__dim(SEXP(false_)));
  const int& dims = dim.size();
  auto condition_view = rray__increase_dims_view(condition, dims);
  auto true_view = rray__increase_dims_view(true_, dims);
  auto false_view = rray__increase_dims_view(false_, dims);

  return xt::where(condition_view, true_view, false_view);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__if_else(Rcpp::RObject condition,
                            Rcpp::RObject true_,
                            Rcpp::RObject false_) {

  // TODO - Is this right?
  if (r_is_null(condition) || r_is_null(true_) || r_is_null(false_)) {
    return Rcpp::as<Rcpp::RObject>(R_NilValue);
  }

  int condition_type = TYPEOF(condition);
  int true_type = TYPEOF(true_);
  int false_type = TYPEOF(false_);

  if (true_type != false_type) {
    Rcpp::stop("`true` and `false` must have the same type.");
  }

  if (condition_type != LGLSXP) {
    Rcpp::stop("`condition` must be a logical.");
  }

  if (true_type == REALSXP) {
    return Rcpp::as<Rcpp::RObject>(
      rray__if_else_impl(
        xt::rarray<rlogical>(condition),
        xt::rarray<double>(true_),
        xt::rarray<double>(false_)
      )
    );
  }
  else if (true_type == INTSXP) {
    return Rcpp::as<Rcpp::RObject>(
      rray__if_else_impl(
        xt::rarray<rlogical>(condition),
        xt::rarray<int>(true_),
        xt::rarray<int>(false_)
      )
    );
  }
  else if (true_type == LGLSXP) {
    return Rcpp::as<Rcpp::RObject>(
      rray__if_else_impl(
        xt::rarray<rlogical>(condition),
        xt::rarray<rlogical>(true_),
        xt::rarray<rlogical>(false_)
      )
    );
  }

  error_unknown_type();
}

// -----------------------------------------------------------------------------
