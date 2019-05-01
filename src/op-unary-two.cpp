#include <xtensor/xstrided_view.hpp>
#include <xtensor/xmanipulation.hpp>

#include <rray.h>
#include <tools/tools.h>

// -----------------------------------------------------------------------------
// Manipulation

template <typename T>
SEXP rray_split_cpp(const xt::rarray<T>& x, SEXP arg1, SEXP arg2) {

  std::size_t n = Rcpp::as<std::size_t>(arg1);
  std::size_t axis = Rcpp::as<std::size_t>(arg2);

  auto res = xt::split(x, n, axis);

  Rcpp::List out(n);

  // For whatever reason, we have to cast to rarray<T> first
  // before going to SEXP
  for (int i = 0; i < n; ++i) {
    xt::rarray<T> res_i = res[i];
    out[i] = res_i;
  }

  return out;
}

// -----------------------------------------------------------------------------
// Switch on the op

template <typename T1>
SEXP rray_op_unary_two_cpp_impl(std::string op, xt::rarray<T1> x, SEXP arg1, SEXP arg2) {

  switch(str2int(op.c_str())) {

  // ---------------------------------------------------------------------------
  // Manipulation

  case str2int("split"): {
    return rray_split_cpp(x, arg1, arg2);
  }

  default: {
    Rcpp::stop("Unknown unary operation.");
  }

  }

}

// -----------------------------------------------------------------------------
// Switch on the type of x

// [[Rcpp::export(rng = false)]]
SEXP rray_op_unary_two_cpp(std::string op, SEXP x, SEXP arg1, SEXP arg2) {

  if (Rf_isNull(x)) {
    return(R_NilValue);
  }

  // Switch on X
  switch(TYPEOF(x)) {

  case REALSXP: {
    return rray_op_unary_two_cpp_impl(op, xt::rarray<double>(x), arg1, arg2);
  }

  case INTSXP: {
    return rray_op_unary_two_cpp_impl(op, xt::rarray<int>(x), arg1, arg2);
  }

  case LGLSXP: {
    return rray_op_unary_two_cpp_impl(op, xt::rarray<rlogical>(x), arg1, arg2);
  }

  default: {
    error_unknown_type();
  }

  }

}
