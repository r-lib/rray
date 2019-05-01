// this header seems necessary for full_like() along with xbuilder.hpp
#include <xtensor/xarray.hpp>
#include <xtensor/xbuilder.hpp>

#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

// Type of `value` has to be the same as the type of T

template <typename T>
xt::rarray<T> rray__full_like_impl(const xt::rarray<T>& x,
                                   Rcpp::RObject value) {

  // This has also been enforced at the R level
  using underlying_type = typename xt::r_detail::get_underlying_value_type_r<T>::type;
  underlying_type xt_fill_value = Rcpp::as<underlying_type>(value);

  xt::rarray<T> out = xt::full_like(x, xt_fill_value);
  return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__full_like(Rcpp::RObject x, Rcpp::RObject value) {
  DISPATCH_UNARY_ONE(rray__full_like_impl, x, value);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<T> rray__diag_impl(const xt::rarray<T>& x, int k) {

  if (x.dimension() > 1) {
    Rcpp::stop("`x` must be 1D, not %iD.", x.dimension());
  }

  xt::rarray<T> out = xt::diag(x, k);
  return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__diag(Rcpp::RObject x, int k) {
  DISPATCH_UNARY_ONE(rray__diag_impl, x, k);
}

// -----------------------------------------------------------------------------

// [[Rcpp::export(rng = false)]]
SEXP rray_ones_cpp(const std::vector<std::size_t>& shape) {
  xt::rarray<int> res = xt::ones<int>(shape);
  return res;
}

// -----------------------------------------------------------------------------

// [[Rcpp::export(rng = false)]]
SEXP rray_zeros_cpp(const std::vector<std::size_t>& shape) {
  xt::rarray<int> res = xt::zeros<int>(shape);
  return res;
}

// -----------------------------------------------------------------------------

// TODO - eye / eye_square are row major only?
// https://github.com/QuantStack/xtensor-r/issues/90

// TODO - eye_square with negative k not working?

// [[Rcpp::export(rng = false)]]
SEXP rray_eye_cpp(const std::vector<std::size_t> shape, int k = 0) {
  xt::rarray<int> res = xt::eye<int>(shape, k);
  return res;
}

// [[Rcpp::export(rng = false)]]
SEXP rray_eye_square_cpp(std::size_t n, int k = 0) {
  xt::rarray<int> res = xt::eye<int>(n, k);
  return res;
}

// -----------------------------------------------------------------------------
