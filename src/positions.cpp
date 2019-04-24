#include <xtensor/xsort.hpp>
#include <rray.h>
#include <dispatch.h>
#include <tools/tools.h>

// -----------------------------------------------------------------------------

template <class E>
inline auto rray__keep_dims_view(E&& x,
                                 Rcpp::IntegerVector dim,
                                 Rcpp::RObject axis) {

  using vec_size_t = typename std::vector<std::size_t>;
  Rcpp::IntegerVector dim_view;

  if (Rf_isNull(axis)) {
    dim_view = Rcpp::IntegerVector(dim.size(), 1);;
  }
  else {
    dim_view = Rcpp::clone(dim);
    int axis_int = Rcpp::as<int>(axis);
    dim_view[axis_int] = 1;
  }

  const vec_size_t& dim_view_vec = Rcpp::as<vec_size_t>(dim_view);

  auto out = xt::reshape_view(x, dim_view_vec, xt::layout_type::column_major);

  return out;
}

template <class E>
inline auto rray__as_r_idx(E&& x) {
  return x + 1;
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<T> rray__sort_impl(const xt::rarray<T>& x, std::ptrdiff_t axis) {
  xt::rarray<T> res = xt::sort(x, axis);
  return res;
}

// [[Rcpp::export]]
Rcpp::RObject rray__sort(Rcpp::RObject x, std::ptrdiff_t axis) {
  DISPATCH_UNARY_ONE(rray__sort_impl, x, axis);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<int> rray__argsort_impl(const xt::rarray<T>& x, std::ptrdiff_t axis) {
  xt::rarray<int> res = xt::argsort(x, axis);
  return rray::as_r_idx(res);
}

// [[Rcpp::export]]
Rcpp::RObject rray__argsort(Rcpp::RObject x, std::ptrdiff_t axis) {
  DISPATCH_UNARY_ONE(rray__argsort_impl, x, axis);
}

// -----------------------------------------------------------------------------

// Note: if axis != "leading axis" then a copy of `x` is made
// https://github.com/QuantStack/xtensor/blob/7f1d93b5dd5b405a74febe70d2a1515611b1e2a6/include/xtensor/xsort.hpp#L867
// Notable difference between:
// x <- matrix(1:5000, 5000, 5000) + 0L
// profmem::profmem(rray__argmax(x, 1)) # not leading axis, slow
// profmem::profmem(rray__argmax(x, 0)) # leading axis, fast

// Note: This might throw false alarm syntax errors in rstudio on the
// auto x_reshape = rray__keep_dims_view() lines

// Note: xtensor's argmax() drops dimensions no matter what. It also iterates
// over objects in an interesting way. For a 3D object, computing the argmax
// over axis = 2 first looks at the first row of the 1st element in the 3rd dim
// then looks at the first row of the 2nd element in the third dim.

template <typename T>
xt::rarray<int> rray__argmax_impl(xt::rarray<T> x, Rcpp::RObject axis) {

  if (Rf_isNull(axis)) {
    // purposefully do column major here
    auto x_argmax = xt::argmax<xt::layout_type::column_major>(x);
    auto x_reshape = rray__keep_dims_view(x_argmax, rray__dim(SEXP(x)), axis);
    auto out = rray__as_r_idx(x_reshape);
    return out;
  }

  std::size_t xt_axis = Rcpp::as<std::size_t>(axis);
  // purposefully do row major here
  auto x_argmax = xt::argmax<xt::layout_type::row_major>(x, xt_axis);
  auto x_reshape = rray__keep_dims_view(x_argmax, rray__dim(SEXP(x)), axis);
  auto out = rray__as_r_idx(x_reshape);
  return out;
}

// [[Rcpp::export]]
Rcpp::RObject rray__argmax(Rcpp::RObject x, Rcpp::RObject axis) {
  DISPATCH_UNARY_ONE(rray__argmax_impl, x, axis);
}

// template <typename T>
// xt::rarray<int> rray__unravel_indices_impl(const xt::rarray<T>& x, Rcpp::RObject shape) {
//   const std::vector<int>& idx = Rcpp::as<std::vector<int>>(x);
//   xt::rarray<int> x_idx = xt::ravel_indices(idx, x.shape(), xt::layout_type::column_major);
//   return x_idx;
// }
//
// // [[Rcpp::export]]
// Rcpp::RObject rray__unravel_indices(Rcpp::RObject x, Rcpp::RObject shape) {
//   DISPATCH_UNARY_ONE(rray__unravel_indices_impl, x, shape);
// }

