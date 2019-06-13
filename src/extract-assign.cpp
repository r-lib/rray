#include <rray.h>
#include <tools/tools.h>
#include <dispatch.h>
#include <subset-tools.h>

// Required for the assignment step
#include <xtensor/xarray.hpp>

// Required for xt::flatten()
#include <xtensor/xadapt.hpp>

// Faster assignment
#include <xtensor/xnoalias.hpp>

template <typename T>
Rcpp::RObject rray__extract_assign_impl(const xt::rarray<T>& x,
                                        Rcpp::List indexer,
                                        Rcpp::RObject value_) {

  // Copy `x` to get the output container
  xt::rarray<T> out = x;
  xt::rarray<T> value(value_);

  if (is_stridable(indexer)) {
    xt::xstrided_slice_vector sv = build_strided_slice_vector(indexer);
    auto xt_out_subset_view = xt::strided_view(out, sv);
    auto xt_out_extract_view = xt::flatten<xt::layout_type::column_major>(xt_out_subset_view);
    rray__validate_broadcastable_to(value, xt_out_extract_view);
    xt::noalias(xt_out_extract_view) = value;
  }
  else {
    xt::xdynamic_slice_vector sv = build_dynamic_slice_vector(indexer);
    auto xt_out_subset_view = xt::dynamic_view(out, sv);
    auto xt_out_extract_view = xt::flatten<xt::layout_type::column_major>(xt_out_subset_view);
    rray__validate_broadcastable_to(value, xt_out_extract_view);
    xt::noalias(xt_out_extract_view) = value;
  }

  return Rcpp::as<Rcpp::RObject>(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__extract_assign(Rcpp::RObject x,
                                   Rcpp::List indexer,
                                   Rcpp::RObject value) {
  Rcpp::RObject out;
  DISPATCH_UNARY_TWO(out, rray__extract_assign_impl, x, indexer, value);

  rray__set_dim_names(out, rray__dim_names(x));

  return out;
}
