#include <rray_types.h>
#include <tools/errors.hpp>
#include <tools/utils.hpp>
#include <Rcpp.h>
using namespace Rcpp;
using namespace rray;

// -----------------------------------------------------------------------------
// Core arithmetic operations

template <typename T1, typename T2>
SEXP rray_add_cpp(const xt::rarray<T1>& x, const xt::rarray<T2>& y) {

  // get_underlying_value_type_r<T1> returns T1 (double, int) in all cases except T1 = rlogical
  // where it returns bool. This was needed to go R logical <-> xtensor of bools
  // because R logicals are int32 values.

  using value_type_T1 = xt::r_detail::get_underlying_value_type_r<T1>;
  using value_type_T2 = xt::r_detail::get_underlying_value_type_r<T2>;

  using common_type = typename std::common_type<typename value_type_T1::type,
                                                typename value_type_T2::type>::type;

  const xt::rarray<common_type>& res = x + y;
  return res;
}

template <typename T1, typename T2>
SEXP rray_subtract_cpp(const xt::rarray<T1>& x, const xt::rarray<T2>& y) {

  using value_type_T1 = xt::r_detail::get_underlying_value_type_r<T1>;
  using value_type_T2 = xt::r_detail::get_underlying_value_type_r<T2>;

  using common_type = typename std::common_type<typename value_type_T1::type,
                                                typename value_type_T2::type>::type;

  const xt::rarray<common_type>& res = x - y;
  return res;
}

template <typename T1, typename T2>
SEXP rray_multiply_cpp(const xt::rarray<T1>& x, const xt::rarray<T2>& y) {

  using value_type_T1 = xt::r_detail::get_underlying_value_type_r<T1>;
  using value_type_T2 = xt::r_detail::get_underlying_value_type_r<T2>;

  using common_type = typename std::common_type<typename value_type_T1::type,
                                                typename value_type_T2::type>::type;

  const xt::rarray<common_type>& res = x * y;
  return res;
}

template <typename T1, typename T2>
SEXP rray_divide_cpp(const xt::rarray<T1>& x, const xt::rarray<T2>& y) {

  using value_type_T1 = xt::r_detail::get_underlying_value_type_r<T1>;
  using value_type_T2 = xt::r_detail::get_underlying_value_type_r<T2>;

  using common_type = typename std::common_type<typename value_type_T1::type,
                                                typename value_type_T2::type>::type;

  const xt::rarray<common_type>& res = x / y;
  return res;
}

// -----------------------------------------------------------------------------
// Switch on the op

template <typename T1, typename T2>
SEXP rray_binary_op_cpp_impl(const std::string& op, const xt::rarray<T1>& x, const xt::rarray<T2>& y) {

  switch(str2int(op.c_str())) {

    case str2int("+"): {
      return rray_add_cpp(x, y);
    }

    case str2int("-"): {
      return rray_subtract_cpp(x, y);
    }

    case str2int("*"): {
      return rray_multiply_cpp(x, y);
    }

    case str2int("/"): {
      return rray_divide_cpp(x, y);
    }

    default: {
      stop("Unknown binary arithmetic operation.");
    }

  }

}

// -----------------------------------------------------------------------------
// Switch on the types of x and y

// [[Rcpp::export]]
SEXP rray_binary_op_cpp(const std::string& op, SEXP x, SEXP y) {

  // I can't figure out any cleaner way to do this.

  switch(TYPEOF(x)) {

    case REALSXP: {
      const xt::rarray<double>& x_rray = xt::rarray<double>(x);

      switch(TYPEOF(y)) {

        case REALSXP: {
          const xt::rarray<double>& y_rray = xt::rarray<double>(y);
          return rray_binary_op_cpp_impl(op, x_rray, y_rray);
        }

        case INTSXP: {
          const xt::rarray<int>& y_rray = xt::rarray<int>(y);
          return rray_binary_op_cpp_impl(op, x_rray, y_rray);
        }

        case LGLSXP: {
          const xt::rarray<rlogical>& y_rray = xt::rarray<rlogical>(y);
          return rray_binary_op_cpp_impl(op, x_rray, y_rray);
        }

        default: {
          error_unknown_type();
        }

      } // End Y switch

    } // End REALSXP X case

    case INTSXP: {
      const xt::rarray<int>& x_rray = xt::rarray<int>(x);

      // Switch on Y
      switch(TYPEOF(y)) {

        case REALSXP: {
          const xt::rarray<double>& y_rray = xt::rarray<double>(y);
          return rray_binary_op_cpp_impl(op, x_rray, y_rray);
        }

        case INTSXP: {
          const xt::rarray<int>& y_rray = xt::rarray<int>(y);
          return rray_binary_op_cpp_impl(op, x_rray, y_rray);
        }

        case LGLSXP: {
          const xt::rarray<rlogical>& y_rray = xt::rarray<rlogical>(y);
          return rray_binary_op_cpp_impl(op, x_rray, y_rray);
        }

        default: {
          error_unknown_type();
        }

      } // End Y switch

    } // End INTSXP X case

    case LGLSXP: {

      const xt::rarray<rlogical>& x_rray = xt::rarray<rlogical>(x);

      // Switch on Y
      switch(TYPEOF(y)) {

        case REALSXP: {
          const xt::rarray<double>& y_rray = xt::rarray<double>(y);
          return rray_binary_op_cpp_impl(op, x_rray, y_rray);
        }

        case INTSXP: {
          const xt::rarray<int>& y_rray = xt::rarray<int>(y);
          return rray_binary_op_cpp_impl(op, x_rray, y_rray);
        }

        case LGLSXP: {
          const xt::rarray<rlogical>& y_rray = xt::rarray<rlogical>(y);
          return rray_binary_op_cpp_impl(op, x_rray, y_rray);
        }

        default: {
          error_unknown_type();
        }

      } // End Y switch

    } // End LGLSXP X case

    default: {
      error_unknown_type();
    }

  } // End X switch

}
