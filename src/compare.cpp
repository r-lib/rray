#include <rray.h>
#include <dispatch.h>
#include <cast.h>
#include <utils.h>
#include <type2.h>

// -----------------------------------------------------------------------------

#define COMPARE_IMPL(FUN, X, Y)                                \
  auto views = rray__increase_dims_view2(X, Y);                \
  auto x_view = std::get<0>(views);                            \
  auto y_view = std::get<1>(views);                            \
                                                               \
  xt::rarray<rlogical> res = FUN(x_view, y_view);              \
                                                               \
  return Rcpp::as<Rcpp::RObject>(res)

// -----------------------------------------------------------------------------

#define DISPATCH_COMPARE(FUN, X, Y)                            \
  if (r_is_null(X)) {                                          \
    X = rray_shared_empty_lgl;                                 \
  }                                                            \
                                                               \
  if (r_is_null(Y)) {                                          \
    Y = rray_shared_empty_lgl;                                 \
  }                                                            \
                                                               \
  Rcpp::List new_dim_names = rray__dim_names2(X, Y);           \
                                                               \
  Rcpp::RObject type = vec__ptype_inner2(X, Y);                \
  X = vec__cast_inner(X, type);                                \
  Y = vec__cast_inner(Y, type);                                \
                                                               \
  Rcpp::RObject out;                                           \
  DISPATCH_BINARY(out, FUN, X, Y);                             \
                                                               \
  out.attr("dimnames") = new_dim_names;                        \
                                                               \
  return out

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__greater_impl(const xt::rarray<T>& x,
                                 const xt::rarray<T>& y) {
  COMPARE_IMPL(xt::operator>, x, y);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__greater(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_COMPARE(rray__greater_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__greater_equal_impl(const xt::rarray<T>& x,
                                       const xt::rarray<T>& y) {
  COMPARE_IMPL(xt::operator>=, x, y);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__greater_equal(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_COMPARE(rray__greater_equal_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__lesser_impl(const xt::rarray<T>& x,
                                const xt::rarray<T>& y) {
  COMPARE_IMPL(xt::operator<, x, y);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__lesser(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_COMPARE(rray__lesser_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__lesser_equal_impl(const xt::rarray<T>& x,
                                      const xt::rarray<T>& y) {
  COMPARE_IMPL(xt::operator<=, x, y);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__lesser_equal(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_COMPARE(rray__lesser_equal_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__equal_impl(const xt::rarray<T>& x,
                               const xt::rarray<T>& y) {
  COMPARE_IMPL(xt::equal, x, y);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__equal(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_COMPARE(rray__equal_impl, x, y);
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__not_equal_impl(const xt::rarray<T>& x,
                                   const xt::rarray<T>& y) {
  COMPARE_IMPL(xt::not_equal, x, y);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__not_equal(Rcpp::RObject x, Rcpp::RObject y) {
  DISPATCH_COMPARE(rray__not_equal_impl, x, y);
}

// -----------------------------------------------------------------------------
// Strict equality

// - No broadcasting is performed (the shape is part of the check)

template <typename T>
xt::rarray<rlogical> rray__all_equal_impl(const xt::rarray<T>& x,
                                          const xt::rarray<T>& y) {
  xt::rarray<rlogical> res = (x == y);
  return res;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__all_equal(Rcpp::RObject x, Rcpp::RObject y) {

  Rcpp::RObject x_class = x.attr("class");
  Rcpp::RObject y_class = y.attr("class");

  // The class is part of the check (rray vs array)
  if (!r_identical(x_class, y_class)) {
    return Rcpp::LogicalVector::create(false);
  }

  // The type is part of the check (1 vs 1L)
  if (TYPEOF(x) != TYPEOF(y)) {
    return Rcpp::LogicalVector::create(false);
  }

  // Two NULLs are a special case
  if (r_is_null(x) && r_is_null(y)) {
    return Rcpp::LogicalVector::create(true);
  }

  Rcpp::RObject out;
  DISPATCH_BINARY(out, rray__all_equal_impl, x, y);

  return out;
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

  Rcpp::RObject x_class = x.attr("class");
  Rcpp::RObject y_class = y.attr("class");

  // The class is part of the check (rray vs array)
  if (!r_identical(x_class, y_class)) {
    return Rcpp::LogicalVector::create(true);
  }

  // The type is part of the check (1 vs 1L)
  if (TYPEOF(x) != TYPEOF(y)) {
    return Rcpp::LogicalVector::create(true);
  }

  // Two NULLs are a special case
  if (r_is_null(x) && r_is_null(y)) {
    return Rcpp::LogicalVector::create(false);
  }

  Rcpp::RObject out;
  DISPATCH_BINARY(out, rray__any_not_equal_impl, x, y);

  return out;
}

// -----------------------------------------------------------------------------
