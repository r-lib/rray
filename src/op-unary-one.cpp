// Both required for cumsum() / cumprod()
#include <xtensor/xarray.hpp>
#include <xtensor/xview.hpp>

#include <rray.h>
#include <tools/tools.h>

// -----------------------------------------------------------------------------
// Accumulators

// To prevent overflow, return doubles

template <typename T>
SEXP rray_cumsum_cpp(const xt::rarray<T>& x, SEXP arg) {

  if (Rf_isNull(arg)) {
    const xt::rarray<double>& res = xt::cumsum(x);
    return res;
  }

  std::ptrdiff_t axis = Rcpp::as<std::ptrdiff_t>(arg);
  const xt::rarray<double>& res = xt::cumsum(x, axis);
  return res;
}

template <typename T>
SEXP rray_cumprod_cpp(const xt::rarray<T>& x, SEXP arg) {

  if (Rf_isNull(arg)) {
    const xt::rarray<double>& res = xt::cumprod(x);
    return res;
  }

  std::ptrdiff_t axis = Rcpp::as<std::ptrdiff_t>(arg);
  const xt::rarray<double>& res = xt::cumprod(x, axis);
  return res;
}

// -----------------------------------------------------------------------------
// Manipulation

template <typename T>
SEXP rray_flip_cpp(const xt::rarray<T>& x, SEXP arg) {
  std::size_t axis = Rcpp::as<std::size_t>(arg);
  xt::rarray<T> res = xt::flip(x, axis);
  return res;
}

// -----------------------------------------------------------------------------
// Switch on the op

template <typename T1>
SEXP rray_op_unary_one_cpp_impl(std::string op, const xt::rarray<T1>& x, SEXP arg) {

  switch(str2int(op.c_str())) {

  // ---------------------------------------------------------------------------
  // Accumulator

  case str2int("cumsum"): {
    return rray_cumsum_cpp(x, arg);
  }

  case str2int("cumprod"): {
    return rray_cumprod_cpp(x, arg);
  }

  // ---------------------------------------------------------------------------
  // Manipulation

  case str2int("flip"): {
    return rray_flip_cpp(x, arg);
  }

  default: {
    Rcpp::stop("Unknown unary operation.");
  }

  }

}

// -----------------------------------------------------------------------------
// Switch on the type of x

// [[Rcpp::export(rng = false)]]
SEXP rray_op_unary_one_cpp(std::string op, SEXP x, SEXP arg) {

  if (Rf_isNull(x)) {
    return(R_NilValue);
  }

  // Switch on X
  switch(TYPEOF(x)) {

  case REALSXP: {
    return rray_op_unary_one_cpp_impl(op, xt::rarray<double>(x), arg);
  }

  case INTSXP: {
    return rray_op_unary_one_cpp_impl(op, xt::rarray<int>(x), arg);
  }

  case LGLSXP: {
    return rray_op_unary_one_cpp_impl(op, xt::rarray<rlogical>(x), arg);
  }

  default: {
    error_unknown_type();
  }

  }

}
