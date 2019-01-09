#include <rray_types.h>
#include <xtensor/xsort.hpp>
#include <tools/errors.hpp>
#include <tools/utils.hpp>
#include <Rcpp.h>
using namespace Rcpp;
using namespace rray;

// -----------------------------------------------------------------------------
// Sort

// This seems somewhat broken

// template <typename T>
// SEXP rray_sort_cpp(xt::rarray<T> x, SEXP arg) {
//   std::ptrdiff_t axis = *INTEGER(arg);
//   xt::rarray<T> res = xt::sort(x, axis);
//   return res;
// }

// -----------------------------------------------------------------------------
// Switch on the op

template <typename T1>
SEXP rray_op_unary_1_arg_cpp_impl(std::string op, xt::rarray<T1> x, SEXP arg) {

  switch(str2int(op.c_str())) {

  // case str2int("sort"): {
  //   return rray_sort_cpp(x, arg);
  // }

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
