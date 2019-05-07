// this header seems necessary for full_like() rather than xbuilder.hpp
#include <xtensor/xarray.hpp>
#include <xtensor/xmath.hpp>

#include <rray.h>
#include <tools/tools.h>

// -----------------------------------------------------------------------------
// Builder

// Due to some complications with rlogical, we have to
// call full_like(x, 1) manually. Same for zeros.

template <typename T>
SEXP rray_ones_like_cpp(const xt::rarray<T>& x) {

  using underlying_type = typename xt::r_detail::get_underlying_value_type_r<T>::type;
  underlying_type fill_value = (underlying_type)1;

  const xt::rarray<T>& res = xt::full_like(x, fill_value);
  return res;
}

template <typename T>
SEXP rray_zeros_like_cpp(const xt::rarray<T>& x) {

  using underlying_type = typename xt::r_detail::get_underlying_value_type_r<T>::type;
  underlying_type fill_value = (underlying_type)0;

  const xt::rarray<T>& res = xt::full_like(x, fill_value);
  return res;
}

// -----------------------------------------------------------------------------
// Switch on the op

template <typename T1>
SEXP rray_op_unary_cpp_impl(std::string op, xt::rarray<T1> x) {

  switch(str2int(op.c_str())) {

  // ---------------------------------------------------------------------------
  // Builders

  case str2int("ones_like"): {
    return rray_ones_like_cpp(x);
  }

  case str2int("zeros_like"): {
    return rray_zeros_like_cpp(x);
  }

  default: {
    Rcpp::stop("Unknown unary operation.");
  }

  }

}

// -----------------------------------------------------------------------------
// Switch on the type of x

// [[Rcpp::export(rng = false)]]
SEXP rray_op_unary_cpp(std::string op, SEXP x) {

  if (Rf_isNull(x)) {
    return(R_NilValue);
  }

  // Switch on X
  switch(TYPEOF(x)) {

  case REALSXP: {
    return rray_op_unary_cpp_impl(op, xt::rarray<double>(x));
  }

  case INTSXP: {
    return rray_op_unary_cpp_impl(op, xt::rarray<int>(x));
  }

  case LGLSXP: {
    return rray_op_unary_cpp_impl(op, xt::rarray<rlogical>(x));
  }

  default: {
    error_unknown_type();
  }

  }

}
