#include <rray.h>
#include <dispatch.h>

template <typename T>
xt::rarray<T> rray__broadcast_impl(const xt::rarray<T>& x,
                                   Rcpp::IntegerVector dim) {

  using vec_size_t = typename std::vector<std::size_t>;

  Rcpp::IntegerVector x_dim = rray__dim(SEXP(x));
  int dims = dim.size();

  // Cheap early exit
  if (r_identical(x_dim, dim)) {
    return(x);
  }

  // Must reshape to match dimensionality first b/c of QuantStack/xtensor-r#57
  auto x_view = rray__increase_dims_view(x, dims);

  rray__validate_broadcastable(x_dim, dim);

  const vec_size_t& dim_vec = Rcpp::as<vec_size_t>(dim);
  xt::rarray<T> res = xt::broadcast(x_view, dim_vec);

  return(res);
}

// [[Rcpp::export]]
Rcpp::RObject rray__broadcast(Rcpp::RObject x, Rcpp::IntegerVector dim) {
  DISPATCH_UNARY_ONE(rray__broadcast_impl, x, dim);
}

