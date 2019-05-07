#include <rray.h>
#include <tools/tools.h>

// -----------------------------------------------------------------------------
// Operators

template <typename T1, typename T2>
SEXP rray_equality_cpp(const xt::rarray<T1>& x, const xt::rarray<T2>& y) {
  Rcpp::LogicalVector res = Rcpp::LogicalVector::create(x == y);
  return res;
}

template <typename T1, typename T2>
SEXP rray_inequality_cpp(const xt::rarray<T1>& x, const xt::rarray<T2>& y) {
  Rcpp::LogicalVector res = Rcpp::LogicalVector::create(x != y);
  return res;
}

// -----------------------------------------------------------------------------
// Math - Basic

template <typename T1, typename T2>
SEXP rray_fmod_cpp(const xt::rarray<T1>& x, const xt::rarray<T2>& y) {

  using common_type = typename std::common_type<
    typename xt::r_detail::get_underlying_value_type_r<T1>::type,
    typename xt::r_detail::get_underlying_value_type_r<T2>::type
  >::type;

  const xt::rarray<common_type>& res = xt::fmod(x, y);
  return res;
}

template <typename T1, typename T2>
SEXP rray_remainder_cpp(const xt::rarray<T1>& x, const xt::rarray<T2>& y) {

  using common_type = typename std::common_type<
    typename xt::r_detail::get_underlying_value_type_r<T1>::type,
    typename xt::r_detail::get_underlying_value_type_r<T2>::type
  >::type;

  const xt::rarray<common_type>& res = xt::remainder(x, y);
  return res;
}

template <typename T1, typename T2>
SEXP rray_maximum_cpp(const xt::rarray<T1>& x, const xt::rarray<T2>& y) {

  using common_type = typename std::common_type<
    typename xt::r_detail::get_underlying_value_type_r<T1>::type,
    typename xt::r_detail::get_underlying_value_type_r<T2>::type
  >::type;

  const xt::rarray<common_type>& res = xt::maximum(x, y);
  return res;
}

template <typename T1, typename T2>
SEXP rray_minimum_cpp(const xt::rarray<T1>& x, const xt::rarray<T2>& y) {

  using common_type = typename std::common_type<
    typename xt::r_detail::get_underlying_value_type_r<T1>::type,
    typename xt::r_detail::get_underlying_value_type_r<T2>::type
  >::type;

  const xt::rarray<common_type>& res = xt::minimum(x, y);
  return res;
}

template <typename T1, typename T2>
SEXP rray_fdim_cpp(const xt::rarray<T1>& x, const xt::rarray<T2>& y) {

  using common_type = typename std::common_type<
    typename xt::r_detail::get_underlying_value_type_r<T1>::type,
    typename xt::r_detail::get_underlying_value_type_r<T2>::type
  >::type;

  const xt::rarray<common_type>& res = xt::fdim(x, y);
  return res;
}

// -----------------------------------------------------------------------------
// Math - Power

template <typename T1, typename T2>
SEXP rray_hypot_cpp(const xt::rarray<T1>& x, const xt::rarray<T2>& y) {

  using common_type = typename std::common_type<
    typename xt::r_detail::get_underlying_value_type_r<T1>::type,
    typename xt::r_detail::get_underlying_value_type_r<T2>::type
  >::type;

  const xt::rarray<common_type>& res = xt::hypot(x, y);
  return res;
}

// -----------------------------------------------------------------------------
// Switch on the op

template <typename T1, typename T2>
SEXP rray_op_binary_cpp_impl(const std::string& op,
                             const xt::rarray<T1>& x,
                             const xt::rarray<T2>& y) {

  switch(str2int(op.c_str())) {

  // ---------------------------------------------------------------------------
  // Operators

  case str2int("equality"): {
    return rray_equality_cpp(x, y);
  }

  case str2int("inequality"): {
    return rray_inequality_cpp(x, y);
  }

  // ---------------------------------------------------------------------------
  // Math - Basic

  case str2int("fmod"): {
    return rray_fmod_cpp(x, y);
  }

  case str2int("remainder"): {
    return rray_remainder_cpp(x, y);
  }

  case str2int("maximum"): {
    return rray_maximum_cpp(x, y);
  }

  case str2int("minimum"): {
    return rray_minimum_cpp(x, y);
  }

  case str2int("fdim"): {
    return rray_fdim_cpp(x, y);
  }

  // ---------------------------------------------------------------------------
  // Math - Power

  case str2int("hypot"): {
    return rray_hypot_cpp(x, y);
  }

  default: {
    Rcpp::stop("Unknown binary operation.");
  }

  }

}

// -----------------------------------------------------------------------------
// Switch on the types of y

template<typename T1>
SEXP rray_op_binary_cpp_y(const std::string& op,
                          const xt::rarray<T1>& x_rray,
                          SEXP y) {

  switch(TYPEOF(y)) {

  case REALSXP: {
    return rray_op_binary_cpp_impl(op, x_rray, xt::rarray<double>(y));
  }

  case INTSXP: {
    return rray_op_binary_cpp_impl(op, x_rray, xt::rarray<int>(y));
  }

  case LGLSXP: {
    return rray_op_binary_cpp_impl(op, x_rray, xt::rarray<rlogical>(y));
  }

  default: {
    error_unknown_type();
  }

  }

}

// -----------------------------------------------------------------------------
// Switch on the types of x

// [[Rcpp::export(rng = false)]]
SEXP rray_op_binary_cpp(const std::string& op,
                        SEXP x,
                        SEXP y) {

  if (Rf_isNull(x) || Rf_isNull(y)) {
    return(R_NilValue);
  }

  switch(TYPEOF(x)) {

  case REALSXP: {
    return rray_op_binary_cpp_y(op, xt::rarray<double>(x), y);
  }

  case INTSXP: {
    return rray_op_binary_cpp_y(op, xt::rarray<int>(x), y);
  }

  case LGLSXP: {
    return rray_op_binary_cpp_y(op, xt::rarray<rlogical>(x), y);
  }

  default: {
    error_unknown_type();
  }

  }

}
