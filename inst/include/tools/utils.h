#ifndef rray_utils_h
#define rray_utils_h

#include <rray.h>

// Helper for switching on the string op
constexpr unsigned int str2int(const char* str, int h = 0) {
  return !str[h] ? 5381 : (str2int(str, h+1) * 33) ^ str[h];
}

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
    // Must clone because we alter the value
    // Otherwise we could end up altering the dim
    // of a real object.
    dim_view = Rcpp::clone(dim);
    int axis_int = Rcpp::as<int>(axis);
    dim_view[axis_int] = 1;
  }

  const vec_size_t& dim_view_vec = Rcpp::as<vec_size_t>(dim_view);

  auto out = xt::reshape_view(x, dim_view_vec, xt::layout_type::column_major);

  return out;
}

template <typename T>
inline auto rray__increase_dims_view(const xt::rarray<T>& x, const int& dims) {

  using vec_size_t = typename std::vector<std::size_t>;

  // Reshape `x` to have the dimensionality of `dims`
  Rcpp::IntegerVector x_dim = rray__dim(SEXP(x));

  Rcpp::IntegerVector x_view_dim = rray__increase_dims(x_dim, dims);
  const vec_size_t& xt_view_dim = Rcpp::as<vec_size_t>(x_view_dim);

  auto x_view = xt::reshape_view(x, xt_view_dim, xt::layout_type::column_major);

  return x_view;
}

template <class E>
inline auto rray__as_r_idx(E&& x) {
  return x + 1;
}

#endif
