#include <tools/errors.hpp>
#include <tools/utils.hpp>
#include <rray.h>
using namespace Rcpp;
using namespace rray;

// [[Rcpp::export]]
SEXP rray_ones_cpp(const std::vector<std::size_t>& shape) {
  xt::rarray<int> res = xt::ones<int>(shape);
  return res;
}

// [[Rcpp::export]]
SEXP rray_zeros_cpp(const std::vector<std::size_t>& shape) {
  xt::rarray<int> res = xt::zeros<int>(shape);
  return res;
}

// TODO - eye / eye_square are row major only?
// https://github.com/QuantStack/xtensor-r/issues/90

// TODO - eye_square with negative k not working?

// [[Rcpp::export]]
SEXP rray_eye_cpp(const std::vector<std::size_t> shape, int k = 0) {
  xt::rarray<int> res = xt::eye<int>(shape, k);
  return res;
}

// [[Rcpp::export]]
SEXP rray_eye_square_cpp(std::size_t n, int k = 0) {
  xt::rarray<int> res = xt::eye<int>(n, k);
  return res;
}
