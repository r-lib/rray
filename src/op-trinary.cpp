#include <rray_types.h>
#include <tools/errors.hpp>
#include <tools/utils.hpp>
#include <Rcpp.h>
using namespace Rcpp;
using namespace rray;

// -----------------------------------------------------------------------------
// Trinary operations

template <typename T1, typename T2, typename T3>
SEXP rray_fma_cpp(const xt::rarray<T1>& x,
                  const xt::rarray<T2>& y,
                  const xt::rarray<T3>& z) {

  using value_type_T1 = xt::r_detail::get_underlying_value_type_r<T1>;
  using value_type_T2 = xt::r_detail::get_underlying_value_type_r<T2>;
  using value_type_T3 = xt::r_detail::get_underlying_value_type_r<T3>;

  using common_type = typename std::common_type<typename value_type_T1::type,
                                                typename value_type_T2::type,
                                                typename value_type_T3::type>::type;

  const xt::rarray<common_type>& res = xt::fma(x, y, z);

  return res;
}

// -----------------------------------------------------------------------------
// Switch on the op

template <typename T1, typename T2, typename T3>
SEXP rray_op_trinary_cpp_impl(const std::string& op,
                              const xt::rarray<T1>& x,
                              const xt::rarray<T2>& y,
                              const xt::rarray<T3>& z) {

  switch(str2int(op.c_str())) {

  case str2int("fma"): {
    return rray_fma_cpp(x, y, z);
  }

  default: {
    stop("Unknown trinary operation.");
  }

  }

}

// -----------------------------------------------------------------------------
// Switch on the types of z

template<typename T1, typename T2>
SEXP rray_op_trinary_cpp_z(const std::string& op,
                           const xt::rarray<T1>& x_rray,
                           const xt::rarray<T2>& y_rray,
                           SEXP z) {

  switch(TYPEOF(z)) {

  case REALSXP: {
    const xt::rarray<double>& z_rray = xt::rarray<double>(z);
    return rray_op_trinary_cpp_impl(op, x_rray, y_rray, z_rray);
  }

  case INTSXP: {
    const xt::rarray<int>& z_rray = xt::rarray<int>(z);
    return rray_op_trinary_cpp_impl(op, x_rray, y_rray, z_rray);
  }

  case LGLSXP: {
    const xt::rarray<rlogical>& z_rray = xt::rarray<rlogical>(z);
    return rray_op_trinary_cpp_impl(op, x_rray, y_rray, z_rray);
  }

  default: {
    error_unknown_type();
  }

  }

}

// -----------------------------------------------------------------------------
// Switch on the types of y

template<typename T1>
SEXP rray_op_trinary_cpp_y(const std::string& op,
                           const xt::rarray<T1>& x_rray,
                           SEXP y,
                           SEXP z) {

  switch(TYPEOF(y)) {

  case REALSXP: {
    const xt::rarray<double>& y_rray = xt::rarray<double>(y);
    return rray_op_trinary_cpp_z(op, x_rray, y_rray, z);
  }

  case INTSXP: {
    const xt::rarray<int>& y_rray = xt::rarray<int>(y);
    return rray_op_trinary_cpp_z(op, x_rray, y_rray, z);
  }

  case LGLSXP: {
    const xt::rarray<rlogical>& y_rray = xt::rarray<rlogical>(y);
    return rray_op_trinary_cpp_z(op, x_rray, y_rray, z);
  }

  default: {
    error_unknown_type();
  }

  }

}

// -----------------------------------------------------------------------------
// Switch on the types of x

// [[Rcpp::export]]
SEXP rray_op_trinary_cpp(const std::string& op,
                         SEXP x,
                         SEXP y,
                         SEXP z) {

  switch(TYPEOF(x)) {

  case REALSXP: {
    const xt::rarray<double>& x_rray = xt::rarray<double>(x);
    return rray_op_trinary_cpp_y(op, x_rray, y, z);
  }

  case INTSXP: {
    const xt::rarray<int>& x_rray = xt::rarray<int>(x);
    return rray_op_trinary_cpp_y(op, x_rray, y, z);
  }

  case LGLSXP: {
    const xt::rarray<rlogical>& x_rray = xt::rarray<rlogical>(x);
    return rray_op_trinary_cpp_y(op, x_rray, y, z);
  }

  default: {
    error_unknown_type();
  }

  }

}
