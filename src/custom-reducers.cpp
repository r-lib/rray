#include <rray_types.h>
#include <tools/errors.hpp>
#include <tools/utils.hpp>
#include <xtensor/xreducer.hpp>
#include <Rcpp.h>
using namespace Rcpp;
using namespace rray;

// [[Rcpp::export]]
xt::rarray<double> rray_custom_reducer_cpp(xt::rarray<double> x, Rcpp::Function f, rray::axes_t axes) {

  auto r_fun = [&f](auto const & left, auto const & right) {
    SEXP res = f(left, right);
    double res2 = REAL(res)[0];
    return(res2);
  };

  auto r_functor = xt::make_xreducer_functor(r_fun);

  xt::rarray<double> res = xt::reduce(r_functor, x, axes);

  return(res);
}



