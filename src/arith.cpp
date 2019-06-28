#include <rray.h>
#include <dispatch.h>
#include <cast.h>
#include <type2.h>
#include <utils.h>

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__add_impl(const xt::rarray<T>& x,
                             const xt::rarray<T>& y) {

  auto views = rray__increase_dims_view2(x, y);
  auto x_view = std::get<0>(views);
  auto y_view = std::get<1>(views);

  xt::rarray<T> res = x_view + y_view;

  return Rcpp::as<Rcpp::RObject>(res);
}

// Logicals return integers
Rcpp::RObject rray__add_impl(const xt::rarray<rlogical>& x,
                             const xt::rarray<rlogical>& y) {

  auto views = rray__increase_dims_view2(x, y);
  auto x_view = std::get<0>(views);
  auto y_view = std::get<1>(views);

  xt::rarray<int> res = x_view + y_view;

  return Rcpp::as<Rcpp::RObject>(res);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__add(Rcpp::RObject x, Rcpp::RObject y) {

  if (r_is_null(x)) {
    x = rray_shared_empty_int;
  }

  if (r_is_null(y)) {
    y = rray_shared_empty_int;
  }

  Rcpp::List new_dim_names = rray__dim_names2(x, y);

  Rcpp::RObject type = vec__ptype_inner2(x, y);
  x = vec__cast_inner(x, type);
  y = vec__cast_inner(y, type);

  Rcpp::RObject out;
  DISPATCH_BINARY(out, rray__add_impl, x, y);

  out.attr("dimnames") = new_dim_names;

  return out;
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__subtract_impl(const xt::rarray<T>& x,
                                  const xt::rarray<T>& y) {

  auto views = rray__increase_dims_view2(x, y);
  auto x_view = std::get<0>(views);
  auto y_view = std::get<1>(views);

  xt::rarray<T> res = x_view - y_view;

  return Rcpp::as<Rcpp::RObject>(res);
}

// Logicals return integers
Rcpp::RObject rray__subtract_impl(const xt::rarray<rlogical>& x,
                                  const xt::rarray<rlogical>& y) {

  auto views = rray__increase_dims_view2(x, y);
  auto x_view = std::get<0>(views);
  auto y_view = std::get<1>(views);

  xt::rarray<int> res = x_view - y_view;

  return Rcpp::as<Rcpp::RObject>(res);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__subtract(Rcpp::RObject x, Rcpp::RObject y) {

  if (r_is_null(x)) {
    x = rray_shared_empty_int;
  }

  if (r_is_null(y)) {
    y = rray_shared_empty_int;
  }

  Rcpp::List new_dim_names = rray__dim_names2(x, y);

  Rcpp::RObject type = vec__ptype_inner2(x, y);
  x = vec__cast_inner(x, type);
  y = vec__cast_inner(y, type);

  Rcpp::RObject out;
  DISPATCH_BINARY(out, rray__subtract_impl, x, y);

  out.attr("dimnames") = new_dim_names;

  return out;
}

// -----------------------------------------------------------------------------

// Should always take and return a numeric result

Rcpp::RObject rray__divide_impl(const xt::rarray<double>& x,
                                const xt::rarray<double>& y) {

  auto views = rray__increase_dims_view2(x, y);
  auto x_view = std::get<0>(views);
  auto y_view = std::get<1>(views);

  xt::rarray<double> res = x_view / y_view;

  return Rcpp::as<Rcpp::RObject>(res);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__divide(Rcpp::RObject x, Rcpp::RObject y) {

  if (r_is_null(x)) {
    x = rray_shared_empty_dbl;
  }

  if (r_is_null(y)) {
    y = rray_shared_empty_dbl;
  }

  Rcpp::List new_dim_names = rray__dim_names2(x, y);

  x = vec__cast_inner(x, rray_shared_empty_dbl);
  y = vec__cast_inner(y, rray_shared_empty_dbl);

  Rcpp::RObject out;
  DISPATCH_BINARY(out, rray__divide_impl, x, y);

  out.attr("dimnames") = new_dim_names;

  return out;
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__multiply_impl(const xt::rarray<T>& x,
                                  const xt::rarray<T>& y) {

  auto views = rray__increase_dims_view2(x, y);
  auto x_view = std::get<0>(views);
  auto y_view = std::get<1>(views);

  xt::rarray<T> res = x_view * y_view;

  return Rcpp::as<Rcpp::RObject>(res);
}

// Logicals return integers
Rcpp::RObject rray__multiply_impl(const xt::rarray<rlogical>& x,
                                  const xt::rarray<rlogical>& y) {

  auto views = rray__increase_dims_view2(x, y);
  auto x_view = std::get<0>(views);
  auto y_view = std::get<1>(views);

  xt::rarray<int> res = x_view * y_view;

  return Rcpp::as<Rcpp::RObject>(res);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__multiply(Rcpp::RObject x, Rcpp::RObject y) {

  if (r_is_null(x)) {
    x = rray_shared_empty_int;
  }

  if (r_is_null(y)) {
    y = rray_shared_empty_int;
  }

  Rcpp::List new_dim_names = rray__dim_names2(x, y);

  Rcpp::RObject type = vec__ptype_inner2(x, y);
  x = vec__cast_inner(x, type);
  y = vec__cast_inner(y, type);

  Rcpp::RObject out;
  DISPATCH_BINARY(out, rray__multiply_impl, x, y);

  out.attr("dimnames") = new_dim_names;

  return out;
}

// -----------------------------------------------------------------------------

// On the R side, there is a guarantee that both inputs are double.

Rcpp::RObject rray__pow_impl(const xt::rarray<double>& x,
                             const xt::rarray<double>& y) {

  auto views = rray__increase_dims_view2(x, y);
  auto x_view = std::get<0>(views);
  auto y_view = std::get<1>(views);

  xt::rarray<double> res = xt::pow(x_view, y_view);

  return Rcpp::as<Rcpp::RObject>(res);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__pow(Rcpp::RObject x, Rcpp::RObject y) {

  if (r_is_null(x)) {
    x = rray_shared_empty_dbl;
  }

  if (r_is_null(y)) {
    y = rray_shared_empty_dbl;
  }

  Rcpp::List new_dim_names = rray__dim_names2(x, y);

  x = vec__cast_inner(x, rray_shared_empty_dbl);
  y = vec__cast_inner(y, rray_shared_empty_dbl);

  Rcpp::RObject out;
  DISPATCH_BINARY(out, rray__pow_impl, x, y);

  out.attr("dimnames") = new_dim_names;

  return out;
}

// -----------------------------------------------------------------------------

// Nothing to do in the case of double / integer x

// Logicals as integers
Rcpp::RObject rray__identity_impl(const xt::rarray<rlogical>& x) {
  xt::rarray<int> out = xt::operator+(x);
  return Rcpp::as<Rcpp::RObject>(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__identity(Rcpp::RObject x) {

  if (r_is_null(x)) {
    error_unknown_type();
  }

  const int& x_type = TYPEOF(x);

  if (x_type == REALSXP || x_type == INTSXP) {
    return x;
  }

  Rcpp::RObject out = rray__identity_impl(xt::rarray<rlogical>(x));
  out.attr("dimnames") = rray__dim_names(x);

  return out;
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__opposite_impl(const xt::rarray<T>& x) {
  xt::rarray<T> out = xt::operator-(x);
  return Rcpp::as<Rcpp::RObject>(out);
}

// Logicals as integers
Rcpp::RObject rray__opposite_impl(const xt::rarray<rlogical>& x) {
  xt::rarray<int> out = xt::operator-(x);
  return Rcpp::as<Rcpp::RObject>(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__opposite(Rcpp::RObject x) {

  Rcpp::RObject out;
  DISPATCH_UNARY(out, rray__opposite_impl, x);

  out.attr("dimnames") = rray__dim_names(x);

  return out;
}
