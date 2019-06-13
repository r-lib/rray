#include <rray.h>
#include <tools/tools.h>
#include <dispatch.h>
#include <subset-tools.h>

// Required for the assignment step
#include <xtensor/xarray.hpp>

// For assignment speed
#include <xtensor/xnoalias.hpp>

template <typename T>
Rcpp::RObject rray__subset_assign_impl(const xt::rarray<T>& x,
                                       Rcpp::List indexer,
                                       Rcpp::RObject value_) {

  xt::rarray<T> value(value_);

  // Reshape `xt_value` to have the dimensionality of `x`
  const int& x_dim_n = rray__dim_n(SEXP(x));
  auto value_view = rray__increase_dims_view(value, x_dim_n);

  // Request a copy of `x` that we can assign to
  // `x` comes in as a `const&` that we can't modify directly
  // (and we don't want to change this. We will have to copy `x` at
  // some point for the assignment. It makes sense to do it here.)
  xt::rarray<T> out = x;

  if (is_stridable(indexer)) {
    xt::xstrided_slice_vector sv = build_strided_slice_vector(indexer);
    auto xt_out_subset_view = xt::strided_view(out, sv);
    rray__validate_broadcastable_to(value_view, xt_out_subset_view);
    xt::noalias(xt_out_subset_view) = value_view;
  }
  else {
    xt::xdynamic_slice_vector sv = build_dynamic_slice_vector(indexer);
    auto xt_out_subset_view = xt::dynamic_view(out, sv);
    rray__validate_broadcastable_to(value_view, xt_out_subset_view);
    xt::noalias(xt_out_subset_view) = value_view;
  }

  return Rcpp::as<Rcpp::RObject>(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__subset_assign(Rcpp::RObject x,
                                  Rcpp::List indexer,
                                  Rcpp::RObject value) {
  Rcpp::RObject out;
  DISPATCH_UNARY_TWO(out, rray__subset_assign_impl, x, indexer, value);

  rray__set_dim_names(out, rray__dim_names(x));

  return out;
}
