#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

// rray__reshape_dim_names() is essentially rray__resize_dim_names(), but
// it rather than leaving the meta names in place no matter what, it removes
// them if the axis size changes. This is appropriate for a reshape.

Rcpp::List rray__reshape_dim_names(const Rcpp::List& dim_names,
                                   const Rcpp::IntegerVector& dim,
                                   const Rcpp::IntegerVector& new_dim) {

  Rcpp::List new_dim_names = rray__resize_dim_names(dim_names, new_dim);

  if (r_is_null(new_dim_names.names())) {
    return new_dim_names;
  }

  const R_xlen_t n = new_dim.size();
  const R_xlen_t n_iter = std::min(n, dim.size());
  Rcpp::CharacterVector new_meta_names(n);
  Rcpp::CharacterVector old_meta_names = new_dim_names.names();

  // Remove meta names if the `dim` changed
  for (R_xlen_t i = 0; i < n_iter; ++i) {
    if (dim[i] == new_dim[i]) {
      new_meta_names[i] = old_meta_names[i];
    }
  }

  new_dim_names.names() = new_meta_names;

  return new_dim_names;
}

template <typename T>
xt::rarray<T> rray__reshape_impl(const xt::rarray<T>& x,
                                 const Rcpp::IntegerVector& dim) {

  using vec_size_t = typename std::vector<std::size_t>;
  const vec_size_t& xt_dim = Rcpp::as<vec_size_t>(dim);

  xt::rarray<T> out = xt::reshape_view<xt::layout_type::column_major>(x, xt_dim);

  return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__reshape(Rcpp::RObject x, const Rcpp::IntegerVector& dim) {

  if (r_is_null(x)) {
    return x;
  }

  // Early exit, no reshape needed (no copy made)
  const Rcpp::IntegerVector& x_dim = rray__dim(x);
  if (r_identical(x_dim, dim)) {
    return x;
  }

  rray__validate_reshape(x, dim);

  Rcpp::RObject out;
  DISPATCH_UNARY_ONE(out, rray__reshape_impl, x, dim);

  // Potentially going down in dimensionality, but this is fine
  Rcpp::List new_dim_names = rray__reshape_dim_names(rray__dim_names(x), x_dim, dim);
  rray__set_dim_names(out, new_dim_names);

  return out;
}

// -----------------------------------------------------------------------------
