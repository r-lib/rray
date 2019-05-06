// this header seems necessary for full_like() rather than xbuilder.hpp
#include <xtensor/xarray.hpp>
#include <xtensor/xmath.hpp>

#include <rray.h>
#include <tools/tools.h>

// -----------------------------------------------------------------------------
// Operators

template <typename T>
SEXP rray_all_cpp(xt::rarray<T> x) {
  Rcpp::LogicalVector res = Rcpp::LogicalVector::create(xt::all(x));
  return res;
}

// -----------------------------------------------------------------------------
// Math - Basic

template <typename T>
SEXP rray_sign_cpp(xt::rarray<T> x) {
  const xt::rarray<int>& res = xt::sign(x);
  return res;
}

// -----------------------------------------------------------------------------
// Math - Exponential

template <typename T>
SEXP rray_exp_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::exp(x);
  return res;
}

template <typename T>
SEXP rray_exp2_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::exp2(x);
  return res;
}

template <typename T>
SEXP rray_expm1_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::expm1(x);
  return res;
}

template <typename T>
SEXP rray_log_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::log(x);
  return res;
}

template <typename T>
SEXP rray_log2_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::log2(x);
  return res;
}

template <typename T>
SEXP rray_log10_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::log10(x);
  return res;
}

template <typename T>
SEXP rray_log1p_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::log1p(x);
  return res;
}

// -----------------------------------------------------------------------------
// Math - Power

template <typename T>
SEXP rray_square_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::square(x);
  return res;
}

template <typename T>
SEXP rray_cube_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::cube(x);
  return res;
}

template <typename T>
SEXP rray_sqrt_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::sqrt(x);
  return res;
}

template <typename T>
SEXP rray_cbrt_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::cbrt(x);
  return res;
}

// -----------------------------------------------------------------------------
// Math - Trigonometric

template <typename T>
SEXP rray_sin_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::sin(x);
  return res;
}

template <typename T>
SEXP rray_cos_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::cos(x);
  return res;
}

template <typename T>
SEXP rray_tan_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::tan(x);
  return res;
}

template <typename T>
SEXP rray_asin_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::asin(x);
  return res;
}
template <typename T>
SEXP rray_acos_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::acos(x);
  return res;
}

template <typename T>
SEXP rray_atan_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::atan(x);
  return res;
}

// -----------------------------------------------------------------------------
// Math - Hyperbolic

template <typename T>
SEXP rray_sinh_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::sinh(x);
  return res;
}

template <typename T>
SEXP rray_cosh_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::cosh(x);
  return res;
}

template <typename T>
SEXP rray_tanh_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::tanh(x);
  return res;
}

template <typename T>
SEXP rray_asinh_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::asinh(x);
  return res;
}

template <typename T>
SEXP rray_acosh_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::acosh(x);
  return res;
}

template <typename T>
SEXP rray_atanh_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::atanh(x);
  return res;
}

// -----------------------------------------------------------------------------
// Math - Error / Gamma

template <typename T>
SEXP rray_erf_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::erf(x);
  return res;
}

template <typename T>
SEXP rray_erfc_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::erfc(x);
  return res;
}

template <typename T>
SEXP rray_tgamma_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::tgamma(x);
  return res;
}

template <typename T>
SEXP rray_lgamma_cpp(xt::rarray<T> x) {
  const xt::rarray<double>& res = xt::lgamma(x);
  return res;
}

// -----------------------------------------------------------------------------
// Math - Nearest integer

template <typename T>
SEXP rray_ceil_cpp(xt::rarray<T> x) {
  const xt::rarray<int>& res = xt::ceil(x);
  return res;
}

template <typename T>
SEXP rray_floor_cpp(xt::rarray<T> x) {
  const xt::rarray<int>& res = xt::floor(x);
  return res;
}

template <typename T>
SEXP rray_trunc_cpp(xt::rarray<T> x) {
  const xt::rarray<int>& res = xt::trunc(x);
  return res;
}

template <typename T>
SEXP rray_round_cpp(xt::rarray<T> x) {
  const xt::rarray<int>& res = xt::round(x);
  return res;
}

template <typename T>
SEXP rray_nearbyint_cpp(xt::rarray<T> x) {
  const xt::rarray<int>& res = xt::nearbyint(x);
  return res;
}

template <typename T>
SEXP rray_rint_cpp(xt::rarray<T> x) {
  const xt::rarray<int>& res = xt::rint(x);
  return res;
}

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
  // Operators

  case str2int("all"): {
    return rray_all_cpp(x);
  }

  // ---------------------------------------------------------------------------
  // Math - Basic

  case str2int("sign"): {
    return rray_sign_cpp(x);
  }

  // ---------------------------------------------------------------------------
  // Math - Exponential

  case str2int("exp"): {
    return rray_exp_cpp(x);
  }

  case str2int("exp2"): {
    return rray_exp2_cpp(x);
  }

  case str2int("expm1"): {
    return rray_expm1_cpp(x);
  }

  case str2int("log"): {
    return rray_log_cpp(x);
  }

  case str2int("log2"): {
    return rray_log2_cpp(x);
  }

  case str2int("log10"): {
    return rray_log10_cpp(x);
  }

  case str2int("log1p"): {
    return rray_log1p_cpp(x);
  }

  // ---------------------------------------------------------------------------
  // Math - Exponential

  case str2int("square"): {
    return rray_square_cpp(x);
  }

  case str2int("cube"): {
    return rray_cube_cpp(x);
  }

  case str2int("sqrt"): {
    return rray_sqrt_cpp(x);
  }

  case str2int("cbrt"): {
    return rray_cbrt_cpp(x);
  }

  // ---------------------------------------------------------------------------
  // Math - Trigonometric

  case str2int("sin"): {
    return rray_sin_cpp(x);
  }

  case str2int("cos"): {
    return rray_cos_cpp(x);
  }

  case str2int("tan"): {
    return rray_tan_cpp(x);
  }

  case str2int("asin"): {
    return rray_asin_cpp(x);
  }

  case str2int("acos"): {
    return rray_acos_cpp(x);
  }

  case str2int("atan"): {
    return rray_atan_cpp(x);
  }

  // ---------------------------------------------------------------------------
  // Math - Hyperbolic

  case str2int("sinh"): {
    return rray_sinh_cpp(x);
  }

  case str2int("cosh"): {
    return rray_cosh_cpp(x);
  }

  case str2int("tanh"): {
    return rray_tanh_cpp(x);
  }

  case str2int("asinh"): {
    return rray_asinh_cpp(x);
  }

  case str2int("acosh"): {
    return rray_acosh_cpp(x);
  }

  case str2int("atanh"): {
    return rray_atanh_cpp(x);
  }

  // ---------------------------------------------------------------------------
  // Math - Error / Gamma

  case str2int("erf"): {
    return rray_erf_cpp(x);
  }

  case str2int("erfc"): {
    return rray_erfc_cpp(x);
  }

  case str2int("tgamma"): {
    return rray_tgamma_cpp(x);
  }

  case str2int("lgamma"): {
    return rray_lgamma_cpp(x);
  }

  // ---------------------------------------------------------------------------
  // Math - Nearest integer

  case str2int("ceil"): {
    return rray_ceil_cpp(x);
  }

  case str2int("floor"): {
    return rray_floor_cpp(x);
  }

  case str2int("trunc"): {
    return rray_trunc_cpp(x);
  }

  case str2int("round"): {
    return rray_round_cpp(x);
  }

  case str2int("nearbyint"): {
    return rray_nearbyint_cpp(x);
  }

  case str2int("rint"): {
    return rray_rint_cpp(x);
  }

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
