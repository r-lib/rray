#include <rray.h>
#include <dispatch.h>
#include <cast.h>
#include <type2.h>
#include <utils.h>

// Required for any() and all()
#include <xtensor/xarray.hpp>

// -----------------------------------------------------------------------------

#define LOGICAL_IMPL(FUN, X, Y)                                  \
  auto views = rray__increase_dims_view2(X, Y);                  \
  auto x_view = std::get<0>(views);                              \
  auto y_view = std::get<1>(views);                              \
                                                                 \
  xt::rarray<rlogical> res = FUN(x_view, y_view);                \
                                                                 \
  return Rcpp::as<Rcpp::RObject>(res)

// -----------------------------------------------------------------------------

#define DISPATCH_LOGICAL(FUN, X, Y)                              \
  if (r_is_null(X)) {                                            \
    X = rray_shared_empty_lgl;                                   \
  }                                                              \
                                                                 \
  if (r_is_null(Y)) {                                            \
    Y = rray_shared_empty_lgl;                                   \
  }                                                              \
                                                                 \
  Rcpp::List new_dim_names = rray__dim_names2(X, Y);             \
                                                                 \
  X = vec__cast_inner(X, rray_shared_empty_lgl);                 \
  Y = vec__cast_inner(Y, rray_shared_empty_lgl);                 \
                                                                 \
  Rcpp::RObject out;                                             \
  DISPATCH_BINARY(out, FUN, X, Y);                               \
                                                                 \
  out.attr("dimnames") = new_dim_names;                          \
                                                                 \
  return out

// -----------------------------------------------------------------------------

Rcpp::RObject rray__logical_and_impl(const xt::rarray<rlogical>& x,
                                     const xt::rarray<rlogical>& y) {
  LOGICAL_IMPL(xt::operator&&, x, y);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__logical_and(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_LOGICAL(rray__logical_and_impl, x, y);
}

// -----------------------------------------------------------------------------

Rcpp::RObject rray__logical_or_impl(const xt::rarray<rlogical>& x,
                                    const xt::rarray<rlogical>& y) {
  LOGICAL_IMPL(xt::operator||, x, y);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__logical_or(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_LOGICAL(rray__logical_or_impl, x, y);
}

// -----------------------------------------------------------------------------

Rcpp::RObject rray__logical_not_impl(const xt::rarray<rlogical>& x) {
  xt::rarray<rlogical> out = xt::operator!(x);
  return Rcpp::as<Rcpp::RObject>(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__logical_not(Rcpp::RObject x) {

  if (r_is_null(x)) {
    return rray_shared_empty_lgl;
  }

  x = vec__cast_inner(x, rray_shared_empty_lgl);

  Rcpp::RObject out = rray__logical_not_impl(xt::rarray<rlogical>(x));
  out.attr("dimnames") = rray__dim_names(x);

  return out;
}

// -----------------------------------------------------------------------------

// TODO - replace with native xt::any() if this issue is resolved
// https://github.com/QuantStack/xtensor/issues/1555
// Note that this is slower than it needs to be because it doesn't early exit

// TODO - it also currently fails with multiple axes when one is size 0
// https://github.com/QuantStack/xtensor/issues/1563

Rcpp::RObject rray__any_impl(const xt::rarray<rlogical>& x, Rcpp::RObject axes) {

  // Currently required to materialize into a xarray<bool> before going to rarray<rlogical>
  xt::xarray<bool> x_reduced;
  xt::rarray<rlogical> out;

  // If `axes = NULL` we can rely on `xt::any()` which finds a "global" any value
  // If `axes != NULL` we implement our own custom reducer
  if (r_is_null(axes)) {
    x_reduced = xt::any(x);
  }
  else {
    auto any_reducer = xt::make_xreducer_functor(
      [](bool a, bool b) { return a || b; },
      xt::const_value<bool>(false)
    );

    std::vector<std::size_t> xt_axes = Rcpp::as<std::vector<std::size_t>>(axes);

    x_reduced = xt::reduce(any_reducer, x, xt_axes);
  }

  out = rray__keep_dims_view(x_reduced, rray__dim(SEXP(x)), axes);
  return Rcpp::as<Rcpp::RObject>(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__any(Rcpp::RObject x, Rcpp::RObject axes) {

  if (r_is_null(x)) {
    x = rray_shared_empty_lgl;
  }

  Rcpp::RObject x_cast = vec__cast_inner(x, rray_shared_empty_lgl);

  Rcpp::RObject out = rray__any_impl(xt::rarray<rlogical>(x_cast), axes);

  rray__resize_and_set_dim_names(out, x);

  return out;
}

// -----------------------------------------------------------------------------

// TODO - replace with native xt::all() if this issue is resolved
// https://github.com/QuantStack/xtensor/issues/1555
// Note that this is slower than it needs to be because it doesn't early exit

// TODO - it also currently fails with multiple axes when one is size 0
// https://github.com/QuantStack/xtensor/issues/1563

Rcpp::RObject rray__all_impl(const xt::rarray<rlogical>& x,
                             Rcpp::RObject axes) {

  // Currently required to materialize into a xarray<bool> before going to rarray<rlogical>
  xt::xarray<bool> x_reduced;
  xt::rarray<rlogical> out;

  // If `axes = NULL` we can rely on `xt::all()` which finds a "global" all value
  // If `axes != NULL` we implement our own custom reducer
  if (r_is_null(axes)) {
    x_reduced = xt::all(x);
  }
  else {
    auto all_reducer = xt::make_xreducer_functor(
      [](bool a, bool b) { return a && b; },
      xt::const_value<bool>(true)
    );

    std::vector<std::size_t> xt_axes = Rcpp::as<std::vector<std::size_t>>(axes);

    x_reduced = xt::reduce(all_reducer, x, xt_axes);
  }

  out = rray__keep_dims_view(x_reduced, rray__dim(SEXP(x)), axes);
  return Rcpp::as<Rcpp::RObject>(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__all(Rcpp::RObject x, Rcpp::RObject axes) {

  if (r_is_null(x)) {
    x = rray_shared_empty_lgl;
  }

  Rcpp::RObject x_cast = vec__cast_inner(x, rray_shared_empty_lgl);

  Rcpp::RObject out = rray__all_impl(xt::rarray<rlogical>(x_cast), axes);

  rray__resize_and_set_dim_names(out, x);

  return out;
}

// -----------------------------------------------------------------------------

// Special kind of dispatch for `rray__if_else()`, first value is always logical

template <typename T>
xt::rarray<T> rray__if_else_impl(const xt::rarray<T>& true_,
                                 const xt::rarray<T>& false_,
                                 Rcpp::RObject condition) {

  const xt::rarray<rlogical>& condition_ = SEXP(condition);

  Rcpp::IntegerVector dim_condition = rray__dim(condition);
  Rcpp::IntegerVector dim_true = rray__dim(SEXP(true_));
  Rcpp::IntegerVector dim_false = rray__dim(SEXP(false_));

  Rcpp::IntegerVector dim = rray__dim2(rray__dim2(dim_condition, dim_true), dim_false);

  const int& dim_n = dim.size();

  auto condition_view = rray__increase_dims_view(condition_, dim_n);
  auto true_view = rray__increase_dims_view(true_, dim_n);
  auto false_view = rray__increase_dims_view(false_, dim_n);

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

  Rcpp::List new_dim_names = rray__dim_names2(true_, false_);

  condition = vec__cast_inner(condition, rray_shared_empty_lgl);

  Rcpp::RObject type = vec__ptype_inner2(true_, false_);
  true_ = vec__cast_inner(true_, type);
  false_ = vec__cast_inner(false_, type);

  Rcpp::RObject out;
  DISPATCH_BINARY_ONE(out, rray__if_else_impl, true_, false_, condition);

  out.attr("dimnames") = new_dim_names;

  return out;
}

// -----------------------------------------------------------------------------
