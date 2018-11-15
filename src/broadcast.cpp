#include <rray_types.h>
#include <Rcpp.h>
using namespace Rcpp;

using shape_type = std::vector<std::size_t>;

template <typename T>
SEXP rray_broadcast_cpp_impl(const xt::rarray<T>& x, const IntegerVector& dim) {
  shape_type new_shape = as<shape_type>(dim);
  const xt::rarray<T>& res = xt::broadcast(x, new_shape);
  return(res);
}

// [[Rcpp::export]]
SEXP rray_broadcast_cpp(SEXP x, const IntegerVector& dim) {

  switch(TYPEOF(x)) {

    case REALSXP: {
      const xt::rarray<double>& res = xt::rarray<double>(x);
      return rray_broadcast_cpp_impl(res, dim);
    }

    case INTSXP: {
      const xt::rarray<int>& res = xt::rarray<int>(x);
      return rray_broadcast_cpp_impl(res, dim);
    }

    default: {
      stop("Incompatible SEXP encountered; only accepts lists with REALSXPs, INTSXPs and LGLSXPs.");
    }

  }

}
