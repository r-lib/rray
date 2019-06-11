#include <xtensor/xsort.hpp>
#include <rray.h>
#include <dispatch.h>
#include <tools/tools.h>

// TODO - Until this is fixed:
// https://github.com/QuantStack/xtensor-r/issues/113
#include <xtensor/xarray.hpp>

// -----------------------------------------------------------------------------

// Remove dim names along the axis you sort over,
// but keep meta names. If `axis == NULL`, remove
// all dimension names

Rcpp::List sort_dim_names(const Rcpp::List& dim_names, Rcpp::RObject axis) {

  Rcpp::List new_dim_names;

  if(r_is_null(axis)) {
    new_dim_names = rray__new_empty_dim_names(dim_names.size());
    new_dim_names.names() = dim_names.names();
  }
  else {
    Rcpp::IntegerVector int_axis = Rcpp::as<Rcpp::IntegerVector>(axis);
    new_dim_names = Rf_shallow_duplicate(dim_names);
    new_dim_names[int_axis] = rray__new_empty_dim_names(1);
  }

  return new_dim_names;
}

template <typename T>
Rcpp::RObject rray__sort_impl(const xt::rarray<T>& x, Rcpp::RObject axis) {

  using vec_size_t = typename std::vector<std::size_t>;
  xt::rarray<T> out;

  if (r_is_null(axis)) {
    auto flat_res = xt::sort(x, xt::placeholders::xtuph());
    const vec_size_t& shape = Rcpp::as<vec_size_t>(rray__dim(SEXP(x)));
    out = xt::reshape_view<xt::layout_type::column_major>(flat_res, shape);
  }
  else {
    std::ptrdiff_t xt_axis = Rcpp::as<std::ptrdiff_t>(axis);
    out = xt::sort(x, xt_axis);
  }

  return Rcpp::as<Rcpp::RObject>(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__sort(Rcpp::RObject x, Rcpp::RObject axis) {
  if (r_is_null(x)) {
    return x;
  }

  Rcpp::RObject out;
  DISPATCH_UNARY_ONE(out, rray__sort_impl, x, axis);

  rray__set_dim_names(out, sort_dim_names(rray__dim_names(x), axis));

  return out;
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
// // [[Rcpp::export(rng = false)]]
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

template <typename T>
Rcpp::RObject rray__max_pos_impl(const xt::rarray<T>& x, Rcpp::RObject axis) {

  Rcpp::IntegerVector dim = rray__dim(SEXP(x));

  if (r_is_null(axis)) {
    auto x_argmax = xt::argmax<xt::layout_type::column_major>(x);
    auto x_reshape = rray__keep_dims_view(x_argmax, dim, axis);
    xt::rarray<int> out = rray__as_r_idx(x_reshape);
    return Rcpp::as<Rcpp::RObject>(out);
  }

  std::size_t xt_axis = Rcpp::as<std::size_t>(axis);
  auto x_argmax = xt::argmax<xt::layout_type::column_major>(x, xt_axis);
  auto x_reshape = rray__keep_dims_view(x_argmax, dim, axis);
  xt::rarray<int> out = rray__as_r_idx(x_reshape);

  return Rcpp::as<Rcpp::RObject>(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__max_pos(Rcpp::RObject x, Rcpp::RObject axis) {

  if (r_is_null(x)) {
    return x;
  }

  Rcpp::RObject out;
  DISPATCH_UNARY_ONE(out, rray__max_pos_impl, x, axis);

  rray__resize_and_set_dim_names(out, x);

  return out;
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__min_pos_impl(xt::rarray<T> x, Rcpp::RObject axis) {

  Rcpp::IntegerVector dim = rray__dim(SEXP(x));

  if (r_is_null(axis)) {
    auto x_argmax = xt::argmin<xt::layout_type::column_major>(x);
    auto x_reshape = rray__keep_dims_view(x_argmax, dim, axis);
    xt::rarray<int> out = rray__as_r_idx(x_reshape);
    return Rcpp::as<Rcpp::RObject>(out);
  }

  std::size_t xt_axis = Rcpp::as<std::size_t>(axis);
  auto x_argmax = xt::argmin<xt::layout_type::column_major>(x, xt_axis);
  auto x_reshape = rray__keep_dims_view(x_argmax, dim, axis);
  xt::rarray<int> out = rray__as_r_idx(x_reshape);

  return Rcpp::as<Rcpp::RObject>(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__min_pos(Rcpp::RObject x, Rcpp::RObject axis) {

  if (r_is_null(x)) {
    return x;
  }

  Rcpp::RObject out;
  DISPATCH_UNARY_ONE(out, rray__min_pos_impl, x, axis);

  rray__resize_and_set_dim_names(out, x);

  return out;
}
