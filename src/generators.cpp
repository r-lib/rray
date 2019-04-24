#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------

// Note: Don't treat `x` as a `const rarray&`. We have the option
// to early exit and return x without modification. Using a const
// reference would force the copy on the way out.

template <typename T>
xt::rarray<T> rray__reshape_impl(xt::rarray<T> x, Rcpp::IntegerVector dim) {

  using vec_size_t = typename std::vector<std::size_t>;

  // Early exit, no reshape needed (no copy made)
  if (r_identical(rray__dim(SEXP(x)), dim)) {
    return x;
  }

  rray__validate_reshape(SEXP(x), dim);

  const vec_size_t& xt_dim = Rcpp::as<vec_size_t>(dim);

  xt::rarray<T> out = xt::reshape_view(x, xt_dim, xt::layout_type::column_major);

  return out;
}

// [[Rcpp::export]]
Rcpp::RObject rray__reshape(Rcpp::RObject x, Rcpp::IntegerVector dim) {
  DISPATCH_UNARY_ONE(rray__reshape_impl, x, dim);
}

// -----------------------------------------------------------------------------
