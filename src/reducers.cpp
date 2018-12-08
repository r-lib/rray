#include <rray_types.h>
#include <tools/errors.hpp>
#include <tools/utils.hpp>
#include <Rcpp.h>
using namespace Rcpp;
using namespace rray;

template <typename T>
xt::rarray<T> rray_sum_cpp(const xt::rarray<T>& x, rray::axes_t axes) {
  const xt::rarray<T>& res = xt::sum(x, axes);
  return res;
}

// -----------------------------------------------------------------------------
// Switch on the op

template <typename T>
SEXP rray_reducer_cpp_impl(std::string op, xt::rarray<T> x, rray::axes_t axes) {

  switch(str2int(op.c_str())) {

    case str2int("sum"): {
      return rray_sum_cpp(x, axes);
    }

    default: {
      stop("Unknown reducing operation.");
    }

  }

}

// -----------------------------------------------------------------------------
// Switch on the type of x

// [[Rcpp::export]]
SEXP rray_reducer_cpp(std::string op, SEXP x, rray::axes_t axes) {

  // Switch on X
  switch(TYPEOF(x)) {

    case REALSXP: {
      auto x_rray = Rcpp::as<xt::rarray<double>>(x);
      return rray_reducer_cpp_impl(op, x_rray, axes);
    }

    case INTSXP: {
      auto x_rray = Rcpp::as<xt::rarray<int>>(x);
      return rray_reducer_cpp_impl(op, x_rray, axes);
    }

    case LGLSXP: {
      auto x_rray = Rcpp::as<xt::rarray<rlogical>>(x);
      return rray_reducer_cpp_impl(op, x_rray, axes);
    }

    default: {
      rray::error_unknown_type();
    }

  }

}
