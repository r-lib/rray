#include <rray.h>
#include <dispatch.h>

template <typename T>
Rcpp::RObject rray__broadcast_impl(const xt::rarray<T>& x,
                                   Rcpp::IntegerVector dim) {

  using vec_size_t = typename std::vector<std::size_t>;
  const vec_size_t& dim_vec = Rcpp::as<vec_size_t>(dim);

  int dim_n = dim.size();

  auto x_view = rray__increase_dims_view(x, dim_n);

  xt::rarray<T> out = xt::broadcast(x_view, dim_vec);

  return SEXP(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__broadcast(Rcpp::RObject x, Rcpp::IntegerVector dim) {

  if (r_is_null(x)) {
    return x;
  }

  Rcpp::IntegerVector x_dim = rray__dim(x);

  // Cheap early exit
  if (r_identical(x_dim, dim)) {
    return x;
  }

  rray__validate_broadcastable_to_dim(x_dim, dim);

  Rcpp::RObject out;
  DISPATCH_UNARY_ONE(out, rray__broadcast_impl, x, dim);

  rray__resize_and_set_dim_names(out, x);

  return out;
}

