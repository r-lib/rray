#include <xtensor/xstrided_view.hpp>
#include <xtensor/xmanipulation.hpp>
#include <tools/errors.hpp>
#include <tools/utils.hpp>
#include <rray.h>
using namespace Rcpp;
using namespace rray;

// -----------------------------------------------------------------------------
// Manipulation

template <typename T>
SEXP rray_rotate_cpp(const xt::rarray<T>& x, SEXP arg1, SEXP arg2, SEXP arg3) {

  // Axes
  std::ptrdiff_t from = as<std::ptrdiff_t>(arg1);
  std::ptrdiff_t to = as<std::ptrdiff_t>(arg2);
  std::array<std::ptrdiff_t, 2> axes = {from, to};

  // Number of rotations
  int n = as<int>(arg3);

  if (n == 1) {
    const xt::rarray<T>& res = xt::rot90<1>(x, axes);
    return res;
  }
  else if (n == 2) {
    const xt::rarray<T>& res = xt::rot90<2>(x, axes);
    return res;
  }
  else if (n == 3) {
    const xt::rarray<T>& res = xt::rot90<3>(x, axes);
    return res;
  }
  else {
    Rcpp::stop("`n` must be 1, 2, or 3.");
  }

}

// -----------------------------------------------------------------------------
// Switch on the op

template <typename T1>
SEXP rray_op_unary_three_cpp_impl(std::string op, const xt::rarray<T1>& x, SEXP arg1, SEXP arg2, SEXP arg3) {

  switch(str2int(op.c_str())) {

  // ---------------------------------------------------------------------------
  // Manipulation

  case str2int("rotate"): {
    return rray_rotate_cpp(x, arg1, arg2, arg3);
  }

  default: {
    stop("Unknown operation.");
  }

  }

}

// -----------------------------------------------------------------------------
// Switch on the type of x

// [[Rcpp::export]]
SEXP rray_op_unary_three_cpp(std::string op, SEXP x, SEXP arg1, SEXP arg2, SEXP arg3) {

  if (Rf_isNull(x)) {
    return(R_NilValue);
  }

  // Switch on X
  switch(TYPEOF(x)) {

  case REALSXP: {
    return rray_op_unary_three_cpp_impl(op, xt::rarray<double>(x), arg1, arg2, arg3);
  }

  case INTSXP: {
    return rray_op_unary_three_cpp_impl(op, xt::rarray<int>(x), arg1, arg2, arg3);
  }

  case LGLSXP: {
    return rray_op_unary_three_cpp_impl(op, xt::rarray<rlogical>(x), arg1, arg2, arg3);
  }

  default: {
    error_unknown_type();
  }

  }

}
