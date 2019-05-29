#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

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
  Rcpp::IntegerVector x_dim = rray__dim(x);
  if (r_identical(x_dim, dim)) {
    return x;
  }

  rray__validate_reshape(x, dim);

  Rcpp::RObject out;
  DISPATCH_UNARY_ONE(out, rray__reshape_impl, x, dim);

  // Potentially going down in dimensionality, but this is fine
  rray__reshape_and_set_dim_names(out, x);

  return out;
}

// -----------------------------------------------------------------------------
