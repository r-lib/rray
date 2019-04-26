#ifndef rray_subset_tools_h
#define rray_subset_tools_h

#include <xtensor-r/rarray.hpp>
#include <xtensor/xstrided_view.hpp>
#include <xtensor/xdynamic_view.hpp>

// -----------------------------------------------------------------------------
// Subsetting helpers that are used across multiple subsetting files

bool is_stridable(Rcpp::List x);

xt::xstrided_slice_vector build_strided_slice_vector(Rcpp::List indexer);

xt::xdynamic_slice_vector build_dynamic_slice_vector(Rcpp::List indexer);

#endif
