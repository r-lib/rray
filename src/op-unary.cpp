#include <rray_types.h>
#include <xtensor/xmath.hpp>
#include <tools/errors.hpp>
#include <tools/utils.hpp>
#include <Rcpp.h>
using namespace Rcpp;
using namespace rray;

// Math functions like atan() should always return doubles. Otherwise, if
// xt::rarray<T> is used, a logical will be recoerced to a logical and that
// is not what we want.
// Even base R returns doubles: class(atan(0L)) == "numeric"

template <typename T>
SEXP rray_atan_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::atan(x);
  return res;
}

// -----------------------------------------------------------------------------
// Switch on the op

template <typename T1>
SEXP rray_op_unary_cpp_impl(std::string op, xt::rarray<T1> x) {

  switch(str2int(op.c_str())) {

  case str2int("atan"): {
    return rray_atan_cpp(x);
  }

  default: {
    stop("Unknown unary operation.");
  }

  }

}

// -----------------------------------------------------------------------------
// Switch on the type of x

// [[Rcpp::export]]
SEXP rray_op_unary_cpp(std::string op, SEXP x) {

  // Switch on X
  switch(TYPEOF(x)) {

  case REALSXP: {
    auto x_rray = xt::rarray<double>(x);
    return rray_op_unary_cpp_impl(op, x_rray);
  }

  case INTSXP: {
    auto x_rray = xt::rarray<int>(x);
    return rray_op_unary_cpp_impl(op, x_rray);
  }

  case LGLSXP: {
    auto x_rray = xt::rarray<rlogical>(x);
    return rray_op_unary_cpp_impl(op, x_rray);
  }

  default: {
    error_unknown_type();
  }

  }

}
