#include <rray_types.h>
#include <tools/errors.hpp>
#include <Rcpp.h>
using namespace Rcpp;
using namespace rray;

template <typename T>
xt::rarray<T> rray_reshape_cpp_impl(const xt::rarray<T>& x, rray::dim_t dim) {
  // needs a copy, otherwise modifying x
  xt::rarray<T> res(x);
  res.reshape(dim);
  return(res);
}

// -----------------------------------------------------------------------------
// Switch on the type of x

// [[Rcpp::export]]
SEXP rray_reshape_cpp(SEXP x, rray::dim_t dim) {

  // Switch on X
  switch(TYPEOF(x)) {

    case REALSXP: {
      auto x_rray = xt::rarray<double>(x);
      return rray_reshape_cpp_impl(x_rray, dim);
    }

    case INTSXP: {
      auto x_rray = xt::rarray<int>(x);
      return rray_reshape_cpp_impl(x_rray, dim);
    }

    case LGLSXP: {
      auto x_rray = xt::rarray<rlogical>(x);
      return rray_reshape_cpp_impl(x_rray, dim);
    }

    default: {
      error_unknown_type();
    }

  }

}
