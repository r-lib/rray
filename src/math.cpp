#include "rray_types.h"
#include "xtensor/xmath.hpp"
#include <Rcpp.h>
using namespace Rcpp;

template <typename T>
xt::rarray<T> rray_atan_cpp(xt::rarray<T> x) {
  return(xt::atan(x));
}

// -----------------------------------------------------------------------------
// Helper for switching on the string op

constexpr unsigned int str2int(const char* str, int h = 0) {
  return !str[h] ? 5381 : (str2int(str, h+1) * 33) ^ str[h];
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
      auto res1 = Rcpp::as<xt::rarray<double>>(x);
      return rray_unary_op_cpp_impl(op, res1);
    }

    case INTSXP: {
      auto res1 = Rcpp::as<xt::rarray<int>>(x);
      return rray_unary_op_cpp_impl(op, res1);
    }

    default: {
      stop("Incompatible SEXP encountered; only accepts REALSXPs and INTSXPs.");
    }

  }

}
