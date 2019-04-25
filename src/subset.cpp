#include <xtensor/xdynamic_view.hpp>
#include <xtensor/xstrided_view.hpp>
#include <xtensor/xarray.hpp>
#include <rray.h>
#include <tools/tools.h>
#include <dispatch.h>


// [[Rcpp::export]]
bool is_any_na_int(Rcpp::List x) {
  int x_size = x.size();

  for (int i = 0; i < x_size; ++i) {
    if (TYPEOF(x[i]) != INTSXP) {
      continue;
    }

    Rcpp::IntegerVector x_i = Rcpp::as<Rcpp::IntegerVector>(x[i]);
    int x_i_size = x_i.size();

    for (int j = 0; j < x_i_size; ++j) {
      if (x_i[j] == NA_INTEGER) {
        return true;
      }
    }

  }

  return false;
}

// xt::range() only works with increasing contiguous ranges right now xtensor#1542
// It _does_ actually work with decreasing contiguous ranges
// you can do `xt::range(5, 3, -1)` or something like that
// but to select the first element you have to do xt::range(5, _, -1)`
// and make sure to do `using namespace xt::placeholders;`

// [[Rcpp::export]]
bool is_contiguous_increasing(Rcpp::RObject x) {

  if (!Rf_isInteger(x)) {
    return false;
  }

  Rcpp::IntegerVector x_int = Rcpp::as<Rcpp::IntegerVector>(x);

  bool contiguous = true;
  int x_size = x_int.size();

  if (x_size == 0) {
    return false;
  }

  if (x_size == 1) {
    return x_int[0] != NA_INTEGER ? true : false;
  }

  // Only checking for increasing contiguous (1) not decreasing (-1)
  for (int i = 1; i < x_size; ++i) {
    if (x_int[i] - x_int[i - 1] != 1) {
      contiguous = false;
      break;
    }
  }

  return contiguous;
}

// This check is done after contiguous vectors have been converted to lists of ranges

// [[Rcpp::export]]
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

// -----------------------------------------------------------------------------

Rcpp::CharacterVector subset_names_with_range(Rcpp::CharacterVector names,
                                              int start,
                                              int stop) {

  int i = start;
  int n = stop - start;
  Rcpp::CharacterVector new_names(n);

  while(i < stop) {
    new_names[i - start] = names[i];
    i++;
  }

  return new_names;
}

// [[Rcpp::export]]
Rcpp::List subset_dim_names(Rcpp::List dim_names, Rcpp::List indexer) {

  int n_names = dim_names.size();
  int n_indexer = indexer.size();

  Rcpp::List out(n_names);
  out.names() = dim_names.names();

  for (int i = 0; i < n_indexer; ++i) {

    // No dim names
    if (r_is_null(dim_names[i])) {
      continue;
    }

    Rcpp::CharacterVector names = dim_names[i];

    // Select everything
    if (r_is_missing(indexer[i])) {
      out[i] = names;
      continue;
    }

    // Select with range
    if (TYPEOF(indexer[i]) == VECSXP) {
      int start = *INTEGER(VECTOR_ELT(indexer[i], 0));
      int stop = *INTEGER(VECTOR_ELT(indexer[i], 1));
      out[i] = subset_names_with_range(names, start, stop);
      continue;
    }

    // Select with non-contiguous int vector
    if (TYPEOF(indexer[i]) == INTSXP) {
      Rcpp::IntegerVector int_index = Rcpp::as<Rcpp::IntegerVector>(indexer[i]);
      out[i] = names[int_index];
      continue;
    }

  }

  return out;
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<T> rray__subset_strided(const xt::rarray<T>& x, Rcpp::List indexer) {

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

  xt::rarray<T> out = xt::strided_view(x, sv);
  return out;
}

template <typename T>
xt::rarray<T> rray__subset_dynamic(const xt::rarray<T>& x, Rcpp::List indexer) {

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

  xt::rarray<T> out = xt::dynamic_view(x, sv);
  return out;
}

template <typename T>
xt::rarray<T> rray__subset_impl(const xt::rarray<T>& x, Rcpp::List indexer) {

  xt::rarray<T> out;

  if (is_stridable(indexer)) {
    out = rray__subset_strided(x, indexer);
  }
  else {
    out = rray__subset_dynamic(x, indexer);
  }

  return out;
}

// [[Rcpp::export]]
Rcpp::RObject rray__subset(Rcpp::RObject x, Rcpp::List indexer) {
  DISPATCH_UNARY_ONE(rray__subset_impl, x, indexer);
}
