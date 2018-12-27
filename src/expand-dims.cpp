#include <rray_types.h>
#include <tools/errors.hpp>
#include <Rcpp.h>
using namespace Rcpp;

// Call xt::squeeze() but always use xt::check_policy::full()
// which throws an error if you are trying to drop a dimension
// with >1 element. You pretty much never want this so we don't
// expose that option.

template <typename T>
xt::rarray<T> rray_expand_dims_cpp_impl(const xt::rarray<T>& x, std::size_t axis) {
  xt::rarray<T> res = xt::expand_dims(x, axis);
  return res;
}

// -----------------------------------------------------------------------------
// Switch on the type of x

// [[Rcpp::export]]
SEXP rray_expand_dims_cpp(SEXP x, std::size_t axis) {

  // Switch on X
  switch(TYPEOF(x)) {

  case REALSXP: {
    auto x_rray = xt::rarray<double>(x);
    return rray_expand_dims_cpp_impl(x_rray, axis);
  }

  case INTSXP: {
    auto x_rray = xt::rarray<int>(x);
    return rray_expand_dims_cpp_impl(x_rray, axis);
  }

  case LGLSXP: {
    auto x_rray = xt::rarray<rlogical>(x);
    return rray_expand_dims_cpp_impl(x_rray, axis);
  }

  default: {
    rray::error_unknown_type();
  }

  }

}
