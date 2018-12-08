#include <rray_types.h>
#include <tools/errors.hpp>
#include <tools/utils.hpp>
#include <xtensor/xreducer.hpp>
#include <Rcpp.h>
using namespace Rcpp;
using namespace rray;

// Functor allows a kind of scoping so r_functor(left, right)
// internally knows about the R function f
template <typename T>
class r_binary_functor {
  private:
    Rcpp::Function f;
  public:
    r_binary_functor(Rcpp::Function f_) : f(f_) {  }

    // currently not general enough to _return_ anything except a double
    // we don't know what the users function returns (double, int, logical, ...)

    // Also pulling the first element of the result, even if it had more than
    // one thing. Should do checking instead.
    double operator () (const T& x, const T& y) const {
      auto f_res = f(x, y);
      double dbl_res = REAL(f_res)[0];
      return(dbl_res);
    }
};

template <typename T>
xt::rarray<double> rray_custom_reducer_impl(xt::rarray<double> x, Rcpp::Function f, rray::axes_t axes) {

  auto r_functor = r_binary_functor<T>(f);
  auto xr_functor = xt::make_xreducer_functor(r_functor);
  xt::rarray<double> res = xt::reduce(xr_functor, x, axes);

  return(res);
}

// TODO: Not actually dispatching to multiple types right now
// as i cant figure out how to do it!

// [[Rcpp::export]]
SEXP rray_custom_reducer_cpp(SEXP x, Rcpp::Function f, rray::axes_t axes) {

  // Switch on X
  switch(TYPEOF(x)) {

    case REALSXP: {
      auto x_rray = xt::rarray<double>(x);
      return rray_custom_reducer_impl<double>(x_rray, f, axes);
    }

    case INTSXP: {
      auto x_rray = xt::rarray<double>(x);
      return rray_custom_reducer_impl<double>(x_rray, f, axes);
    }

    case LGLSXP: {
      auto x_rray = xt::rarray<double>(x);
      return rray_custom_reducer_impl<double>(x_rray, f, axes);
    }

    default: {
      error_unknown_type();
    }

  }
}

// template <typename T>
// xt::rarray<T> rray_custom_reducer_impl(xt::rarray<T> x, Rcpp::Function f, rray::axes_t axes) {
//
//   auto r_fun = [&f](auto const & left, auto const & right) {
//     SEXP res = f(left, right);
//     double res2 = REAL(res)[0];
//     return(res2);
//   };
//
//   auto r_functor = xt::make_xreducer_functor(r_fun);
//
//   xt::rarray<double> res = xt::reduce(r_functor, x, axes);
//
//   return(res);
//
// }

// // [[Rcpp::export]]
// xt::rarray<double> rray_custom_reducer_cpp(xt::rarray<double> x, Rcpp::Function f, rray::axes_t axes) {
//
//   auto r_fun = [&f](auto const & left, auto const & right) {
//     SEXP res = f(left, right);
//     double res2 = REAL(res)[0];
//     return(res2);
//   };
//
//   auto r_functor = xt::make_xreducer_functor(r_fun);
//
//   xt::rarray<double> res = xt::reduce(r_functor, x, axes);
//
//   return(res);
// }



