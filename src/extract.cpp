#include <dispatch.h>
#include <subset-tools.h>

// Used in rray__extract_impl()
#include <xtensor/xnoalias.hpp>

// Required for xt::flatten() which uses an xtensor_adaptor()
#include <xtensor/xadapt.hpp>

template <typename T>
Rcpp::RObject rray__extract_impl(const xt::rarray<T>& x, Rcpp::List indexer) {

  xt::rarray<T> out;

  if (is_stridable(indexer)) {
    auto x_view = xt::strided_view(x, build_strided_slice_vector(indexer));
    xt::noalias(out) = xt::flatten<xt::layout_type::column_major>(x_view);
  }
  else {
    auto x_view = xt::dynamic_view(x, build_dynamic_slice_vector(indexer));
    xt::noalias(out) = xt::flatten<xt::layout_type::column_major>(x_view);
  }

  return Rcpp::as<Rcpp::RObject>(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__extract(Rcpp::RObject x, Rcpp::List indexer) {
  Rcpp::RObject out;
  DISPATCH_UNARY_ONE(out, rray__extract_impl, x, indexer);
  rray__set_dim_names(out, rray__new_empty_dim_names(1));
  return out;
}
