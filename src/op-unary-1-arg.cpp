#include <rray_types.h>
// this header seems necessary for full_like() rather than xbuilder.hpp
#include <xtensor/xarray.hpp>
#include <xtensor/xsort.hpp>
#include <tools/errors.hpp>
#include <tools/utils.hpp>
#include <Rcpp.h>
using namespace Rcpp;
using namespace rray;

// -----------------------------------------------------------------------------
// Helper

template <typename T>
xt::rarray<T> as_r_idx(xt::rarray<T> x) {
  return x + 1;
}

// -----------------------------------------------------------------------------
// Broadcast

template <typename T>
SEXP rray_broadcast_cpp(const xt::rarray<T>& x, SEXP arg) {
  rray::dim_t dim = as<std::vector<std::size_t>>(arg);
  const xt::rarray<T>& res = xt::broadcast(x, dim);
  return(res);
}

// -----------------------------------------------------------------------------
// Sort / arg*

// sort and argsort seem somewhat broken.
// https://github.com/QuantStack/xtensor-r/issues/88

template <typename T>
SEXP rray_sort_cpp(xt::rarray<T> x, SEXP arg) {
  std::ptrdiff_t axis = as<std::ptrdiff_t>(arg);
  xt::rarray<T> res = xt::sort(x, axis);
  return res;
}

template <typename T>
SEXP rray_argsort_cpp(xt::rarray<T> x, SEXP arg) {
  std::ptrdiff_t axis = as<std::ptrdiff_t>(arg);
  xt::rarray<int> res = xt::argsort(x, axis);
  return as_r_idx(res);
  return res;
}

// Waiting on the ability to specify the layout for argmax / argmin
// especially when arg = NULL
// https://github.com/QuantStack/xtensor-r/issues/89

template <typename T>
SEXP rray_argmax_cpp(xt::rarray<T> x, SEXP arg) {

  if (Rf_isNull(arg)) {
    xt::rarray<int> res = xt::argmax(x);
    return as_r_idx(res);
  }

  std::ptrdiff_t axis = *INTEGER(arg);
  xt::rarray<int> res = xt::argmax(x, axis);
  return as_r_idx(res);
}

template <typename T>
SEXP rray_argmin_cpp(xt::rarray<T> x, SEXP arg) {

  if (Rf_isNull(arg)) {
    xt::rarray<int> res = xt::argmin(x);
    return as_r_idx(res);
  }

  std::ptrdiff_t axis = *INTEGER(arg);
  xt::rarray<int> res = xt::argmin(x, axis);
  return as_r_idx(res);
}

// -----------------------------------------------------------------------------
// Builders

// type of arg has to be the same as the type of T

template <typename T>
SEXP rray_full_like_cpp(const xt::rarray<T>& x, SEXP arg) {

  // Coerce arg to the underlying type T
  // (rlogical is really int so we need the underlying int type)
  using underlying_type = typename xt::r_detail::get_underlying_value_type_r<T>::type;
  underlying_type fill_value = as<underlying_type>(arg);

  const xt::rarray<T>& res = xt::full_like(x, fill_value);
  return res;
}

// -----------------------------------------------------------------------------
// Accumulators

// To prevent overflow, return doubles
// TODO - https://github.com/QuantStack/xtensor/issues/1333

template <typename T>
SEXP rray_cumsum_cpp(const xt::rarray<T>& x, SEXP arg) {

  if (Rf_isNull(arg)) {
    const xt::rarray<double>& res = xt::cumsum(x);
    return res;
  }

  std::ptrdiff_t axis = as<std::ptrdiff_t>(arg);
  const xt::rarray<double>& res = xt::cumsum(x, axis);
  return res;
}

template <typename T>
SEXP rray_cumprod_cpp(const xt::rarray<T>& x, SEXP arg) {

  if (Rf_isNull(arg)) {
    const xt::rarray<double>& res = xt::cumprod(x);
    return res;
  }

  std::ptrdiff_t axis = as<std::ptrdiff_t>(arg);
  const xt::rarray<double>& res = xt::cumprod(x, axis);
  return res;
}

// -----------------------------------------------------------------------------
// Switch on the op

template <typename T1>
SEXP rray_op_unary_1_arg_cpp_impl(std::string op, xt::rarray<T1> x, SEXP arg) {

  switch(str2int(op.c_str())) {

  // ---------------------------------------------------------------------------
  // Broadcast

  case str2int("broadcast"): {
    return rray_broadcast_cpp(x, arg);
  }

  // ---------------------------------------------------------------------------
  // Sort / arg*

  case str2int("sort"): {
    return rray_sort_cpp(x, arg);
  }

  case str2int("argsort"): {
    return rray_argsort_cpp(x, arg);
  }

  case str2int("argmax"): {
    return rray_argmax_cpp(x, arg);
  }

  case str2int("argmin"): {
    return rray_argmin_cpp(x, arg);
  }

  // ---------------------------------------------------------------------------
  // Builders

  case str2int("full_like"): {
    return rray_full_like_cpp(x, arg);
  }

  // ---------------------------------------------------------------------------
  // Accumulator

  case str2int("cumsum"): {
    return rray_cumsum_cpp(x, arg);
  }

  case str2int("cumprod"): {
    return rray_cumprod_cpp(x, arg);
  }

  default: {
    stop("Unknown unary operation.");
  }

  }

}

// -----------------------------------------------------------------------------
// Switch on the type of x

// [[Rcpp::export]]
SEXP rray_op_unary_1_arg_cpp(std::string op, SEXP x, SEXP arg) {

  // Switch on X
  switch(TYPEOF(x)) {

  case REALSXP: {
    return rray_op_unary_1_arg_cpp_impl(op, xt::rarray<double>(x), arg);
  }

  case INTSXP: {
    return rray_op_unary_1_arg_cpp_impl(op, xt::rarray<int>(x), arg);
  }

  case LGLSXP: {
    return rray_op_unary_1_arg_cpp_impl(op, xt::rarray<rlogical>(x), arg);
  }

  default: {
    error_unknown_type();
  }

  }

}
