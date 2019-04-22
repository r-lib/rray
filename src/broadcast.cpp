#include <rray.h>
#include <dispatch.h>

// -----------------------------------------------------------------------------
// Broadcast

template <typename T>
const xt::rarray<T> rray__broadcast_impl(const xt::rarray<T>& x, SEXP arg) {
  std::vector<std::size_t> dim = Rcpp::as<std::vector<std::size_t>>(arg);
  const xt::rarray<T> res = xt::broadcast(x, dim);
  return(res);
}

// [[Rcpp::export]]
SEXP rray__broadcast(SEXP x, SEXP dim) {
  DISPATCH_UNARY_ONE(rray__broadcast_impl, x, dim);
}

