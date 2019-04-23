#include <rray.h>
#include <dispatch.h>

template <typename T>
xt::rarray<T> rray__broadcast_impl(const xt::rarray<T>& x,
                                   Rcpp::IntegerVector dim) {

  using vec_size_t = typename std::vector<std::size_t>;

  Rcpp::IntegerVector x_dim = rray_dim(SEXP(x));
  int dims = dim.size();

  // Cheap early exit
  if (r_identical(x_dim, dim)) {
    return(x);
  }

  // Match dimensionality before comparison
  x_dim = rray_increase_dims(x_dim, dims);

  // 3 cases where broadcasting works:
  // - Dimensions are the same (no change is made)
  // - Dimension of x is 1 (broadcast to new dimension)
  // - New dimension is 0 (no change is made)
  Rcpp::LogicalVector ok = (x_dim == dim | x_dim == 1 | dim == 0);
  if (Rcpp::is_true(Rcpp::any(!ok))) {
    Rcpp::stop("Non-broadcastable dimensions.");
  }

  // Must reshape to match dimensionality first b/c of QuantStack/xtensor-r#57
  const vec_size_t& x_view_dim = Rcpp::as<vec_size_t>(x_dim);
  auto x_view = xt::reshape_view(x, x_view_dim);

  const vec_size_t& dim_vec = Rcpp::as<vec_size_t>(dim);
  xt::rarray<T> res = xt::broadcast(x_view, dim_vec);

  return(res);
}

// [[Rcpp::export]]
Rcpp::RObject rray__broadcast(Rcpp::RObject x, Rcpp::IntegerVector dim) {
  DISPATCH_UNARY_ONE(rray__broadcast_impl, x, dim);
}

