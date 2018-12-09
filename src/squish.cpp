#include <rray_types.h>
#include <Rcpp.h>
#include <xtensor/xio.hpp>
using namespace Rcpp;
using namespace rray;

// [[Rcpp::export]]
void rray_squish_cpp(const xt::rarray<double>& x, std::vector<std::size_t> dim) {
  xt::rarray<double> xx(x);
  auto xx_split = xt::split(xx, 2, 0);
  xt::rarray<double> xx1 = xx_split[0];
  std::cout << xx1 << std::endl;
}

// [[Rcpp::export]]
xt::rarray<double> rray_concatenate_cpp(const xt::rarray<double>& x, const xt::rarray<double>& y, std::size_t axis) {
  xt::rarray<double> res = xt::concatenate(xt::xtuple(x, y), axis);
  return(res);
}
