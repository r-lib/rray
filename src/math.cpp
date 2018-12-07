#include <rray_types.h>
#include <xtensor/xmath.hpp>
#include <tools/errors.hpp>
#include <tools/utils.hpp>
#include <Rcpp.h>
using namespace Rcpp;

// Math functions like atan() should always return doubles. Otherwise, if
// xt::rarray<T> is used, a logical will be recoerced to a logical and that
// is not what we want.
// Even base R returns doubles: class(atan(0L)) == "numeric"

template <typename T>
xt::rarray<double> rray_atan_cpp(xt::rarray<T> x) {
  return(xt::atan(x));
}

// -----------------------------------------------------------------------------
// Switch on the op

template <typename T1>
SEXP rray_unary_op_cpp_impl(std::string op, xt::rarray<T1> x) {

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
SEXP rray_unary_op_cpp(std::string op, SEXP x) {

  // I can't figure out any cleaner way to do this.

  // Switch on X
  switch(TYPEOF(x)) {

    case REALSXP: {
      auto res1 = xt::rarray<double>(x);
      return rray_unary_op_cpp_impl(op, res1);
    }

    case INTSXP: {
      auto res1 = xt::rarray<int>(x);
      return rray_unary_op_cpp_impl(op, res1);
    }

    case LGLSXP: {
      auto res1 = xt::rarray<rlogical>(x);
      return rray_unary_op_cpp_impl(op, res1);
    }

    default: {
      error_unknown_type();
    }

  }

}
