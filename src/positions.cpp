#include <xtensor/xsort.hpp>
#include <rray.h>
#include <dispatch.h>
#include <tools/tools.h>

// TODO - Until this is fixed:
// https://github.com/QuantStack/xtensor-r/issues/113
#include <xtensor/xarray.hpp>

// -----------------------------------------------------------------------------

// Might get a false alarm on the xt::reshape_view()

template <typename T>
xt::rarray<T> rray__sort_impl(const xt::rarray<T>& x, Rcpp::RObject axis) {

  using vec_size_t = typename std::vector<std::size_t>;
  xt::rarray<T> res;

  if (r_is_null(axis)) {
    auto flat_res = xt::sort(x, xt::placeholders::xtuph());
    const vec_size_t& shape = Rcpp::as<vec_size_t>(rray__dim(SEXP(x)));
    res = xt::reshape_view(flat_res, shape, xt::layout_type::column_major);
  }
  else {
    std::ptrdiff_t xt_axis = Rcpp::as<std::ptrdiff_t>(axis);
    res = xt::sort(x, xt_axis);
  }

  return res;
}

// [[Rcpp::export]]
Rcpp::RObject rray__sort(Rcpp::RObject x, Rcpp::RObject axis) {
  DISPATCH_UNARY_ONE(rray__sort_impl, x, axis);
}

// -----------------------------------------------------------------------------

// TODO - simplify this after this is fixed
// https://github.com/QuantStack/xtensor-r/issues/113

/// TODO - Flattening argsort() is broken for column major xarrays
// (and by extension rarrays)
// https://github.com/QuantStack/xtensor/issues/1537

// template <typename T>
// xt::rarray<int> rray__sort_pos_impl(const xt::rarray<T>& x, Rcpp::RObject axis) {
//
//   if (r_is_null(axis)) {
//     // TODO - Temporarily go through xarray for argsort() to work
//     using underlying_type = typename xt::r_detail::get_underlying_value_type_r<T>::type;
//     xt::xarray<underlying_type, xt::layout_type::column_major> x_xarray(x);
//
//     auto x_sort = xt::argsort(x_xarray, xt::placeholders::xtuph());
//     auto x_r_idx = rray__as_r_idx(x_sort);
//     xt::xarray<int, xt::layout_type::column_major> out_xarray(x_r_idx);
//     xt::rarray<int> out(out_xarray);
//
//     return out;
//   }
//
//   // TODO - Temporarily go through xarray for argsort() to work
//   using underlying_type = typename xt::r_detail::get_underlying_value_type_r<T>::type;
//   xt::xarray<underlying_type, xt::layout_type::column_major> x_xarray(x);
//
//   std::ptrdiff_t xt_axis = Rcpp::as<std::ptrdiff_t>(axis);
//   auto x_sort = xt::argsort(x_xarray, xt_axis);
//   auto x_r_idx = rray__as_r_idx(x_sort);
//   xt::xarray<int, xt::layout_type::column_major> out_xarray(x_r_idx);
//   xt::rarray<int> out(out_xarray);
//
//   return out;
// }
//
// // [[Rcpp::export]]
// Rcpp::RObject rray__sort_pos(Rcpp::RObject x, Rcpp::RObject axis) {
//   DISPATCH_UNARY_ONE(rray__sort_pos_impl, x, axis);
// }

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
xt::rarray<int> rray__max_pos_impl(xt::rarray<T> x, Rcpp::RObject axis) {

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
Rcpp::RObject rray__max_pos(Rcpp::RObject x, Rcpp::RObject axis) {
  DISPATCH_UNARY_ONE(rray__max_pos_impl, x, axis);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<int> rray__min_pos_impl(xt::rarray<T> x, Rcpp::RObject axis) {

  if (Rf_isNull(axis)) {
    // purposefully do column major here
    auto x_argmin = xt::argmin<xt::layout_type::column_major>(x);
    auto x_reshape = rray__keep_dims_view(x_argmin, rray__dim(SEXP(x)), axis);
    auto out = rray__as_r_idx(x_reshape);
    return out;
  }

  std::size_t xt_axis = Rcpp::as<std::size_t>(axis);
  // purposefully do row major here
  auto x_argmin = xt::argmin<xt::layout_type::row_major>(x, xt_axis);
  auto x_reshape = rray__keep_dims_view(x_argmin, rray__dim(SEXP(x)), axis);
  auto out = rray__as_r_idx(x_reshape);
  return out;
}

// [[Rcpp::export]]
Rcpp::RObject rray__min_pos(Rcpp::RObject x, Rcpp::RObject axis) {
  DISPATCH_UNARY_ONE(rray__min_pos_impl, x, axis);
}
