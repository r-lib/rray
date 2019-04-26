#include <rray.h>
#include <tools/tools.h>
#include <dispatch.h>
#include <subset-tools.h>

// Required for the assignment step
#include <xtensor/xarray.hpp>

template <typename T>
auto rray__increase_dims_view(const xt::rarray<T>& x, const int& dims) {

  using vec_size_t = typename std::vector<std::size_t>;

  // Reshape `x` to have the dimensionality of `dims`
  Rcpp::IntegerVector x_dim = rray__dim(SEXP(x));

  Rcpp::IntegerVector x_view_dim = rray__increase_dims(x_dim, dims);
  const vec_size_t& xt_view_dim = Rcpp::as<vec_size_t>(x_view_dim);

  auto x_view = xt::reshape_view(x, xt_view_dim, xt::layout_type::column_major);

  return x_view;
}

template <typename T>
auto rray__subset_assign_strided(const xt::rarray<T>& x,
                                 Rcpp::List indexer,
                                 Rcpp::RObject value) {

  // Convert `value` to an rarray
  const xt::rarray<T>& xt_value = SEXP(value);

  // Reshape `xt_value` to have the dimensionality of `x`
  const int& x_dims = rray__dims(SEXP(x));
  auto xt_value_view = rray__increase_dims_view(xt_value, x_dims);

  // NOTE - Request a copy of `x` that we can assign to
  // Currently, `x` is a `const&` that we can't modify directly
  xt::rarray<T> x_assignable = x;

  xt::xstrided_slice_vector sv = build_strided_slice_vector(indexer);
  auto x_subset_view = xt::strided_view(x_assignable, sv);

  x_subset_view = xt_value_view;

  return x_assignable;
}

template <typename T>
auto rray__subset_assign_dynamic(const xt::rarray<T>& x,
                                 Rcpp::List indexer,
                                 Rcpp::RObject value) {

  // Convert `value` to an rarray
  const xt::rarray<T>& xt_value = SEXP(value);

  // Reshape `xt_value` to have the dimensionality of `x`
  const int& x_dims = rray__dims(SEXP(x));
  auto xt_value_view = rray__increase_dims_view(xt_value, x_dims);

  // NOTE - Request a copy of `x` that we can assign to
  // Currently, `x` is a `const&` that we can't modify directly
  xt::rarray<T> x_assignable = x;

  xt::xdynamic_slice_vector sv = build_dynamic_slice_vector(indexer);
  auto x_subset_view = xt::dynamic_view(x_assignable, sv);

  x_subset_view = xt_value_view;

  return 1;
}

template <typename T>
xt::rarray<T> rray__subset_assign_impl(const xt::rarray<T>& x,
                                       Rcpp::List indexer,
                                       Rcpp::RObject value) {

  xt::rarray<T> out;

  if (is_stridable(indexer)) {
    out = rray__subset_assign_strided(x, indexer, value);
  }
  else {
    out = rray__subset_assign_dynamic(x, indexer, value);
  }

  return out;
}

// [[Rcpp::export]]
Rcpp::RObject rray__subset_assign(Rcpp::RObject x, Rcpp::List indexer, Rcpp::RObject value) {
  DISPATCH_UNARY_TWO(rray__subset_assign_impl, x, indexer, value);
}
