#include <xtensor/xdynamic_view.hpp>
#include <xtensor/xstrided_view.hpp>
#include <xtensor/xarray.hpp>
#include <rray.h>
#include <tools/tools.h>
#include <dispatch.h>

// xt::range() only works with increasing contiguous ranges right now xtensor#1542

// [[Rcpp::export]]
bool is_contiguous_increasing(Rcpp::RObject x) {

  if (!Rf_isInteger(x)) {
    return false;
  }

  Rcpp::IntegerVector x_int = Rcpp::as<Rcpp::IntegerVector>(x);

  bool contiguous = true;
  int x_size = x_int.size();

  // Only checking for increasing contiguous (1) not decreasing (-1)
  for (int i = 1; i < x_size; ++i) {
    if (x_int[i] - x_int[i-1] != 1) {
      contiguous = false;
      break;
    }
  }

  return contiguous;
}

Rcpp::List slice_range_to_lst(Rcpp::RObject x) {
  Rcpp::List x_lst = Rcpp::as<Rcpp::List>(x);

  int start = Rcpp::as<int>(x_lst[0]);
  int stop = Rcpp::as<int>(x_lst[1]) + 1;

  return Rcpp::List::create(start, stop);
}

Rcpp::List contiguous_increasing_range_to_lst(Rcpp::RObject x) {
  Rcpp::IntegerVector x_int = Rcpp::as<Rcpp::IntegerVector>(x);
  return Rcpp::List::create(x_int[0], x_int[x_int.size() - 1] + 1);
}

// [[Rcpp::export]]
Rcpp::List convert_ranges(Rcpp::List x) {

  int x_size = x.size();

  Rcpp::List out(x_size);

  for (int i = 0; i < x_size; ++i) {

    Rcpp::RObject idx = x[i];

    if (r_is_missing(idx)) {
      out[i] = idx;
    }
    else if (idx.inherits("vctrs_slice_range")) {
      out[i] = slice_range_to_lst(idx);
    }
    else if (is_contiguous_increasing(idx)) {
      out[i] = contiguous_increasing_range_to_lst(idx);
    }
    // non-contiguous
    else {
      out[i] = idx;
    }

  }

  return out;
}

// This check is done after contiguous vectors have been
// converted to lists of ranges
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

template <typename T>
xt::rarray<T> rray__subset_strided(const xt::rarray<T>& x, Rcpp::List slice_indices) {

  xt::xstrided_slice_vector sv({});
  int n = slice_indices.size();

  for (int i = 0; i < n; ++i) {

    Rcpp::RObject slice_index = slice_indices[i];

    if (r_is_missing(slice_index)) {
      sv.emplace_back(xt::all());
    }
    // It was contiguous, and is now a list of the start/stop positions
    else {
      Rcpp::List slice_index_lst = Rcpp::as<Rcpp::List>(slice_index);
      int start = Rcpp::as<int>(slice_index_lst[0]);
      int stop = Rcpp::as<int>(slice_index_lst[1]);
      sv.emplace_back(xt::range(start, stop));
    }

  }

  xt::rarray<T> out = xt::strided_view(x, sv);
  return out;
}

template <typename T>
xt::rarray<T> rray__subset_dynamic(const xt::rarray<T>& x, Rcpp::List slice_indices) {

  xt::xdynamic_slice_vector sv({});
  int n = slice_indices.size();

  for (int i = 0; i < n; ++i) {

    Rcpp::RObject slice_index = slice_indices[i];

    if (r_is_missing(slice_index)) {
      sv.emplace_back(xt::all());
    }
    // It was contiguous, and is now a list of the start/stop positions
    else if (TYPEOF(slice_index) == VECSXP) {
      Rcpp::List slice_index_lst = Rcpp::as<Rcpp::List>(slice_index);
      int start = Rcpp::as<int>(slice_index_lst[0]);
      int stop = Rcpp::as<int>(slice_index_lst[1]);
      sv.emplace_back(xt::range(start, stop));
    }
    // Else it is a non-contiguous IntegerVector
    else {
      // Use `int` rather than `size_t` to prevent the indices being copied
      std::vector<int> slice = Rcpp::as<std::vector<int>>(slice_index);
      sv.emplace_back(xt::keep(slice));
    }

  }

  xt::rarray<T> out = xt::dynamic_view(x, sv);
  return out;
}

template <typename T>
xt::rarray<T> rray__subset_impl(const xt::rarray<T>& x, Rcpp::List slice_indices) {

  xt::rarray<T> out;
  Rcpp::List converted_indices = convert_ranges(slice_indices);

  if (is_stridable(converted_indices)) {
    out = rray__subset_strided(x, converted_indices);
  }
  else {
    out = rray__subset_dynamic(x, converted_indices);
  }

  return out;
}

// [[Rcpp::export]]
Rcpp::RObject rray__subset(Rcpp::RObject x, Rcpp::List slice_indices) {
  DISPATCH_UNARY_ONE(rray__subset_impl, x, slice_indices);
}
