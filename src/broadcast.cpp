#include <rray_types.h>
#include <tools/errors.hpp>
#include <Rcpp.h>
using namespace Rcpp;
using namespace rray;

template <typename T>
SEXP rray_broadcast_cpp_impl(const xt::rarray<T>& x, rray::dim_t dim) {
  const xt::rarray<T>& res = xt::broadcast(x, dim);
  return(res);
}

// [[Rcpp::export]]
SEXP rray_broadcast_cpp(SEXP x, rray::dim_t dim) {

  switch(TYPEOF(x)) {

    case REALSXP: {
      const xt::rarray<double>& res = xt::rarray<double>(x);
      return rray_broadcast_cpp_impl(res, dim);
    }

    case INTSXP: {
      const xt::rarray<int>& res = xt::rarray<int>(x);
      return rray_broadcast_cpp_impl(res, dim);
    }

    case LGLSXP: {
      const xt::rarray<rlogical>& res = xt::rarray<rlogical>(x);
      return rray_broadcast_cpp_impl(res, dim);
    }

    default: {
      error_unknown_type();
    }

  }

}
