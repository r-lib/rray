#include <rray.h>
#include <subset-tools.h>

// This check is done after contiguous vectors have been converted to lists of ranges

bool is_stridable(Rcpp::List x) {
  bool stridable = true;
  int x_size = x.size();

  for (int i = 0; i < x_size; ++i) {

    Rcpp::RObject idx = x[i];

    if (r_is_missing(idx)) {
      continue;
    }

    // Can't check if its a list with Rf_isList() or .inherits("list")
    // as that doesn't really catch it
    if (TYPEOF(idx) == VECSXP) {
      continue;
    }

    stridable = false;
    break;
  }

  return stridable;
}

xt::xstrided_slice_vector build_strided_slice_vector(Rcpp::List indexer) {

  xt::xstrided_slice_vector sv({});
  int n = indexer.size();

  for (int i = 0; i < n; ++i) {

    Rcpp::RObject index = indexer[i];

    if (r_is_missing(index)) {
      sv.emplace_back(xt::all());
      continue;
    }

    // It was contiguous, and is now a list of the start/stop positions
    int start = *INTEGER(VECTOR_ELT(index, 0));
    int stop = *INTEGER(VECTOR_ELT(index, 1));
    sv.emplace_back(xt::range(start, stop));
  }

  return sv;
}

xt::xdynamic_slice_vector build_dynamic_slice_vector(Rcpp::List indexer) {

  xt::xdynamic_slice_vector sv({});
  int n = indexer.size();

  for (int i = 0; i < n; ++i) {

    Rcpp::RObject index = indexer[i];

    if (r_is_missing(index)) {
      sv.emplace_back(xt::all());
      continue;
    }

    // It was contiguous, and is now a list of the start/stop positions
    if (TYPEOF(index) == VECSXP) {
      int start = *INTEGER(VECTOR_ELT(index, 0));
      int stop = *INTEGER(VECTOR_ELT(index, 1));
      sv.emplace_back(xt::range(start, stop));
      continue;
    }

    // Else it is a non-contiguous IntegerVector
    // Use `int` rather than `size_t` to prevent the indices being copied
    std::vector<int> slice = Rcpp::as<std::vector<int>>(index);
    sv.emplace_back(xt::keep(slice));
  }

  return sv;
}
