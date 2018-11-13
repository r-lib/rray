#include <rray_types.h>
#include <Rcpp.h>
using namespace Rcpp;

using shape_type = std::vector<std::size_t>;

template <typename T>
xt::rarray<T> rray_broadcast_cpp_impl(xt::rarray<T> x, IntegerVector dim) {
  shape_type new_shape = as<shape_type>(dim);
  xt::rarray<T> res = xt::broadcast(x, new_shape);
  return(res);
}

// [[Rcpp::export]]
SEXP rray_broadcast_cpp(SEXP x, IntegerVector dim) {

  switch(TYPEOF(x)) {

    case REALSXP: {
      auto res = Rcpp::as<xt::rarray<double>>(x);
      return rray_broadcast_cpp_impl(res, dim);
    }

    case INTSXP: {
      auto res = Rcpp::as<xt::rarray<int>>(x);
      return rray_broadcast_cpp_impl(res, dim);
    }

    default: {
      stop("Incompatible SEXP encountered; only accepts lists with REALSXPs, INTSXPs and LGLSXPs.");
    }

  }

}
