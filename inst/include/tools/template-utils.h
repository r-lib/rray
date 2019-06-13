#ifndef rray_template_utils_h
#define rray_template_utils_h

#include <rray.h>

template <class E>
inline auto rray__keep_dims_view(E&& x,
                                 Rcpp::IntegerVector dim,
                                 Rcpp::RObject axes) {

  using vec_size_t = typename std::vector<std::size_t>;
  Rcpp::IntegerVector dim_view;

  if (Rf_isNull(axes)) {
    dim_view = Rcpp::IntegerVector(dim.size(), 1);;
  }
  else {
    // Must clone because we alter the value
    // Otherwise we could end up altering the dim
    // of a real object.
    dim_view = Rcpp::clone(dim);

    Rcpp::IntegerVector axes_vec = Rcpp::as<Rcpp::IntegerVector>(axes);
    for (Rcpp::IntegerVector::iterator it = axes_vec.begin(); it != axes_vec.end(); ++it) {
      dim_view[*it] = 1;
    }
  }

  const vec_size_t& dim_view_vec = Rcpp::as<vec_size_t>(dim_view);

  auto out = xt::reshape_view<xt::layout_type::column_major>(x, dim_view_vec);

  return out;
}

template <typename T>
inline auto rray__increase_dims_view(const xt::rarray<T>& x, const int& dim_n) {

  using vec_size_t = typename std::vector<std::size_t>;

  // Reshape `x` to have the dimensionality of `dim_n`
  Rcpp::IntegerVector x_dim = rray__dim(SEXP(x));

  Rcpp::IntegerVector x_view_dim = rray__increase_dims(x_dim, dim_n);
  const vec_size_t& xt_view_dim = Rcpp::as<vec_size_t>(x_view_dim);

  auto x_view = xt::reshape_view<xt::layout_type::column_major>(x, xt_view_dim);

  return x_view;
}

// Validates that x and y can be broadcast together by finding their
// common dim, then creates reshape views on both to ensure their dimensionality
// is correct to work with xtensor. Returns them as a tuple.

template <typename T>
inline auto rray__increase_dims_view2(const xt::rarray<T>& x,
                                      const xt::rarray<T>& y) {

  Rcpp::IntegerVector x_dim = rray__dim(SEXP(x));
  Rcpp::IntegerVector y_dim = rray__dim(SEXP(y));

  Rcpp::IntegerVector dim = rray__dim2(x_dim, y_dim);
  const int& dim_n = dim.size();

  using x_reshaped_t = decltype(rray__increase_dims_view(x, dim_n));
  using y_reshaped_t = decltype(rray__increase_dims_view(y, dim_n));

  auto x_view = rray__increase_dims_view(x, dim_n);
  auto y_view = rray__increase_dims_view(y, dim_n);

  return std::tuple<x_reshaped_t, y_reshaped_t>(x_view, y_view);
}

// Validate that `x` is immediately broadcastable to the dimensions of `to`

template <class E1, class E2>
void rray__validate_broadcastable_to(E1&& x, E2&& to) {
  auto x_shape = x.shape();
  Rcpp::IntegerVector x_dim(x_shape.begin(), x_shape.end());

  auto to_shape = to.shape();
  Rcpp::IntegerVector to_dim(to_shape.begin(), to_shape.end());

  rray__validate_broadcastable_to_dim(x_dim, to_dim);
}

template <class E>
inline auto rray__as_r_idx(E&& x) {
  return x + 1;
}

#endif
