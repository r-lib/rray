#include <rray_types.h>
#include <tools/errors.hpp>
#include <tools/utils.hpp>
#include <xtensor/xreducer.hpp>
#include <xtensor/xarray.hpp>
#include <Rcpp.h>
using namespace Rcpp;
using namespace rray;

// How does this work?
// 1) rray_custom_reducer_cpp() takes x, a function with 2 args, the axes, and the return type
// 2) Based on the types of x and the return type, it calls something along the lines of:
//    rray_custom_reducer_impl<TYPEOFX, RETURNTYPE>(x, f, axes);
// 3) rray_custom_reducer_impl
//    - coerces x to an rarray
//    - creates a binary_functor using f and all of the type info
//    - then makes an xreducer_functor out of that and calls reduce using it
// 4) binary_functor this is where the magic happens
//    it lets you use scoping so you can initialize it with "things" you'd like
//    it to have available when the function is called (by the overloaded () operator)
//    - So we give it the function and the SEXPTYPE return type in the constructor
//    - And then we template on the Cpp element and return type
//    - Each call to the overloaded () operator calls f and coerces the result
//      to RET_T if it can be, otherwise error


// Functor allows a kind of scoping so r_functor(left, right)
// internally knows about the R function f
template <typename ELEM_T, typename RET_T>
class binary_functor {

  private:
    Rcpp::Function f;
    SEXPTYPE RTYPE;
    R_xlen_t good_length = 1;

  public:
    binary_functor(Rcpp::Function f_, SEXPTYPE RTYPE_) : f(f_), RTYPE(RTYPE_) {  }

    // Also pulling the first element of the result, even if it had more than
    // one thing. Should do checking instead.
    RET_T operator () (const ELEM_T& x, const ELEM_T& y) const {

      SEXP f_res = f(x, y);

      // Type check
      if (TYPEOF(f_res) != RTYPE) {
        const char* good_type = Rf_type2char(RTYPE);
        const char* bad_type = Rf_type2char(TYPEOF(f_res));
        Rf_errorcall(
          R_NilValue,
          "`.f` should return length 1 objects of class `%s`, not class `%s`.",
          good_type,
          bad_type
        );
      }

      // Length check
      if (Rf_xlength(f_res) != good_length) {
        Rf_errorcall(
          R_NilValue,
          "`.f` should return objects of length 1, not %i.",
          Rf_xlength(f_res)
        );
      }

      // Clever conversion to replace explicit use of
      // REAL() or INTEGER()
      RET_T res = ((RET_T *)dataptr(f_res))[0];

      return(res);
    }
};


template <typename ELEM_R_T, typename RET_R_T>
SEXP rray_custom_reducer_impl(SEXP x, Rcpp::Function f, rray::axes_t axes) {

  // Underlying Cpp type. This helper is necessary because it converts
  // rlogical->int
  using elem_t = typename xt::r_detail::get_underlying_value_type_r<ELEM_R_T>::type;
  using ret_t  = typename xt::r_detail::get_underlying_value_type_r<RET_R_T>::type;

  // Pull the SEXPTYPE of the return value from the cpp type
  // The helper is necessary because rlogical->LGLSXP has been specially registered
  static constexpr int ret_sexptype = Rcpp::traits::r_sexptype_traits<RET_R_T>::rtype;

  // Create the rarray with the type matching x
  const xt::rarray<ELEM_R_T>& x_rray = xt::rarray<ELEM_R_T>(x);

  // Create the functor
  auto r_functor = binary_functor<elem_t, ret_t>(f, ret_sexptype);
  auto xr_functor = xt::make_xreducer_functor(r_functor);

  // Temporary solution until xtensor-r#76 is resolved
  // Currently it is problematic to assign directly to an rarray from xt::reduce()
  // Can't use xarray<RET_R_T> because it doesnt know about rlogical
  xt::xarray<ret_t> temp_res = xt::reduce(xr_functor, x_rray, axes);
  xt::rarray<RET_R_T> res = temp_res;

  return(res);
}

// [[Rcpp::export]]
SEXP rray_custom_reducer_cpp(SEXP x, Rcpp::Function f, rray::axes_t axes, SEXP type_) {

  // Collect the char from the string type ("double", "integer", "logical")
  const char* type = CHAR(Rf_asChar(type_));

  // Switch on X
  switch(TYPEOF(x)) {

    // This means ELEM_T = double
    case REALSXP: {

      // Switch on return type:
      // "double" = REALSXP
      // "integer" = INTSXP
      // "logical" = LGLSXP
      switch(str2int(type)) {

        case str2int("double"): {
          return rray_custom_reducer_impl<double, double>(x, f, axes);
        }

        case str2int("integer"): {
          return rray_custom_reducer_impl<double, int>(x, f, axes);
        }

        case str2int("logical"): {
          return rray_custom_reducer_impl<double, rlogical>(x, f, axes);
        }

        default: {
          rray::error_unknown_type();
        }

      }

    }

    // This means ELEM_T = int
    case INTSXP: {
      switch(str2int(type)) {

        case str2int("double"): {
          return rray_custom_reducer_impl<int, double>(x, f, axes);
        }

        case str2int("integer"): {
          return rray_custom_reducer_impl<int, int>(x, f, axes);
        }

        case str2int("logical"): {
          return rray_custom_reducer_impl<int, rlogical>(x, f, axes);
        }

        default: {
          rray::error_unknown_type();
        }

      }
    }

    // This means ELEM_T = int
    case LGLSXP: {
      switch(str2int(type)) {

        case str2int("double"): {
          return rray_custom_reducer_impl<rlogical, double>(x, f, axes);
        }

        case str2int("integer"): {
          return rray_custom_reducer_impl<rlogical, int>(x, f, axes);
        }

        case str2int("logical"): {
          return rray_custom_reducer_impl<rlogical, rlogical>(x, f, axes);
        }

        default: {
          rray::error_unknown_type();
        }

      }
    }

    default: {
      error_unknown_type();
    }

  }
}
