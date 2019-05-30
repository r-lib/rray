// this header seems necessary for full_like() along with xbuilder.hpp
#include <xtensor/xarray.hpp>
#include <xtensor/xbuilder.hpp>

#include <rray.h>
#include <dispatch.h>
#include <cast.h>

// -----------------------------------------------------------------------------

// Type of `value` has to be the same as the type of T

template <typename T>
Rcpp::RObject rray__full_like_impl(const xt::rarray<T>& x,
                                   Rcpp::RObject value) {

  using c_type = typename xt::r_detail::get_underlying_value_type_r<T>::type;
  c_type xt_value;

  int x_type = TYPEOF(SEXP(x));

  if (x_type == REALSXP) {
    xt_value = REAL(value)[0];
  }
  else if (x_type == INTSXP) {
    xt_value = INTEGER(value)[0];
  }
  else if (x_type == LGLSXP) {
    xt_value = LOGICAL(value)[0];
  }
  else {
    Rcpp::stop("should never be reached, but pleases clang.");
  }

  xt::rarray<T> out = xt::full_like(x, xt_value);

  return Rcpp::as<Rcpp::RObject>(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__full_like(Rcpp::RObject x, Rcpp::RObject value) {

  if (r_is_null(x)) {
    return x;
  }

  R_xlen_t value_len = Rf_xlength(value);
  if (value_len != 1) {
    Rcpp::stop("`value` must have length 1, not %i.", value_len);
  }

  value = vec__cast_inner(value, x);

  Rcpp::RObject out;
  DISPATCH_UNARY_ONE(out, rray__full_like_impl, x, value);

  return out;
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<T> rray__diag_impl(const xt::rarray<T>& x, const int& offset) {

  if (x.dimension() > 1) {
    Rcpp::stop("`x` must be 1D, not %iD.", x.dimension());
  }

  xt::rarray<T> out = xt::diag(x, offset);
  return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__diag(Rcpp::RObject x, const int& offset) {

  if (r_is_null(x)) {
    return x;
  }

  Rcpp::RObject out;
  DISPATCH_UNARY_ONE(out, rray__diag_impl, x, offset);

  return out;
}

// -----------------------------------------------------------------------------

// TODO - Blocked by https://github.com/QuantStack/xtensor/issues/1570

// template <typename T>
// xt::rarray<T> rray__pull_diagonal_impl(const xt::rarray<T>& x,
//                                        int offset,
//                                        std::size_t axis_1,
//                                        std::size_t axis_2) {
//   return xt::diagonal(x, offset, axis_1, axis_2);
// }
//
// // [[Rcpp::export(rng = false)]]
// Rcpp::RObject rray__pull_diagonal(Rcpp::RObject x,
//                                   int offset,
//                                   std::size_t axis_1,
//                                   std::size_t axis_2) {
//   DISPATCH_UNARY_THREE(rray__pull_diagonal_impl, x, offset, axis_1, axis_2);
// }

// -----------------------------------------------------------------------------

// TODO - eye / eye_square are row major only?
// https://github.com/QuantStack/xtensor-r/issues/90

// TODO - eye_square with negative k not working?

// // [[Rcpp::export(rng = false)]]
// SEXP rray_eye_cpp(const std::vector<std::size_t> shape, int k = 0) {
//   xt::rarray<int> res = xt::eye<int>(shape, k);
//   return res;
// }
//
// // [[Rcpp::export(rng = false)]]
// SEXP rray_eye_square_cpp(std::size_t n, int k = 0) {
//   xt::rarray<int> res = xt::eye<int>(n, k);
//   return res;
// }

// -----------------------------------------------------------------------------
