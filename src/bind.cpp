#include <rray.h>
#include <dispatch.h>

#include <xtensor/xstrided_view.hpp>
#include <xtensor/xarray.hpp>

#include <xtensor/xio.hpp>

// -----------------------------------------------------------------------------

// [[Rcpp::export(rng = false)]]
int compute_dims(Rcpp::List args, const int& axis) {

  int dims = 1;
  const int& n_args = args.size();
  const int& implied_dims = axis + 1;

  for (int i = 0; i < n_args; ++i) {
    dims = rray__dims2(rray__dims(args[i]), dims);
  }

  // Allows for going up in dimensionality
  dims = std::max(dims, implied_dims);

  return dims;
}

// -----------------------------------------------------------------------------

// Updates `has_axis_dim_names` if it is currently false, but this `axis_names`
// is not null.
void check_all_or_nothing_name_consistency(Rcpp::RObject axis_names,
                                           bool& has_axis_dim_names) {

  if (r_is_null(axis_names)) {
    if (has_axis_dim_names) {
      Rcpp::stop("There are at least some axis names, but this one is NULL.");
    }
  }
  else {
    if (!has_axis_dim_names) {
      has_axis_dim_names = true;
    }
  }

}

// Loop through `all_axis_dim_names`, which is a list of character vectors
// and combine them into 1 character vector
Rcpp::CharacterVector combine_axis_dim_names(Rcpp::List all_axis_dim_names,
                                             const int& axis,
                                             Rcpp::IntegerVector dim) {

  const int& n_args = all_axis_dim_names.size();
  Rcpp::CharacterVector axis_dim_names(dim[axis]);

  int pos = 0;

  for (int i = 0; i < n_args; ++i) {

    Rcpp::CharacterVector partial_axis_dim_names = all_axis_dim_names[i];
    int n_names = Rf_xlength(partial_axis_dim_names);

    for (int j = 0; j < n_names; ++j) {
      axis_dim_names[pos] = partial_axis_dim_names[j];
      pos++;
    }

  }

  return axis_dim_names;
}

// If all elements are null, nothing to do
bool has_all_null_dim_names(const Rcpp::List& dim_names) {

  const int& n_arg_i_dim_names = dim_names.size();

  bool all_null = true;

  for (int j = 0; j < n_arg_i_dim_names; ++j) {

    if (r_is_null(dim_names[j])) {
      continue;
    }

    all_null = false;
    break;
  }

  return all_null;
}

// [[Rcpp::export(rng = false)]]
Rcpp::List compute_bind_dim_names(const Rcpp::List& arg_dim_names,
                                  const int& axis,
                                  const Rcpp::IntegerVector& dim) {

  const int& n_args = arg_dim_names.size();
  const int& dims = dim.size();

  Rcpp::List new_dim_names = rray__new_empty_dim_names(dims);
  Rcpp::List arg_axis_dim_names(n_args);

  bool has_axis_dim_names = false;

  for (int i = 0; i < n_args; ++i) {

    Rcpp::List arg_i_dim_names = arg_dim_names[i];

    // Store axis dim names if applicable
    if (arg_i_dim_names.size() >= axis) {
      Rcpp::RObject arg_i_axis_names = arg_i_dim_names[axis];
      check_all_or_nothing_name_consistency(arg_i_axis_names, has_axis_dim_names);
      arg_axis_dim_names[i] = arg_i_axis_names;
    }

    // Check this after the name consistency check
    if (has_all_null_dim_names(arg_i_dim_names)) {
      continue;
    }

    // Extend names to the right dimensionality
    arg_i_dim_names = rray__reshape_dim_names(arg_i_dim_names, dim);

    // Coalesce names and meta names
    new_dim_names = rray__coalesce_dim_names(new_dim_names, arg_i_dim_names);
  }

  // Each arg has axis names so we need to combine them
  if (has_axis_dim_names) {
    new_dim_names[axis] = combine_axis_dim_names(arg_axis_dim_names, axis, dim);
  }
  // No arg has axis names
  else {
    new_dim_names[axis] = R_NilValue;
  }

  return new_dim_names;
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__bind_impl(Rcpp::List args,
                              const int& axis,
                              Rcpp::List arg_dim_names) {

  const int& n_args = args.size();

  const int& dims = compute_dims(args, axis);

  // ---------------------------------------------------------------------------
  // TODO - could this block be extracted into a function that returns a
  // list of `out_dim` and `out_axis_locs`?

  std::vector<std::size_t> out_axis_locs(n_args + 1);
  out_axis_locs[0] = 0;
  int loc = 0;
  int out_axis_size = 0;
  Rcpp::IntegerVector out_dim(dims, 1);

  for (int i = 0; i < n_args; ++i) {
    // Must clone, otherwise directly affecting the args `dim`
    Rcpp::IntegerVector arg_dim_i = Rcpp::clone(rray__dim(args[i]));
    arg_dim_i = rray__increase_dims(arg_dim_i, dims);

    int arg_axis_size_i = arg_dim_i[axis];
    arg_dim_i[axis] = 0;

    out_dim = rray__dim2(out_dim, arg_dim_i);
    out_axis_size = out_axis_size + arg_axis_size_i;

    out_axis_locs[i + 1] = loc + arg_axis_size_i - 1;
    loc = loc + arg_axis_size_i;
  }

  out_dim[axis] = out_axis_size;

  // ---------------------------------------------------------------------------

  // Allocate an empty container of type `T` and shape `out_dim`
  const std::vector<std::size_t>& shape = Rcpp::as<std::vector<std::size_t>>(out_dim);
  xt::rarray<T> xt_out(shape);

  // Initialize slice vector with `all()`
  xt::xstrided_slice_vector sv(dims, xt::all());

  for (int i = 0; i < n_args; ++i) {

    // Update slice vector positions
    std::size_t start = out_axis_locs[i];
    std::size_t end = out_axis_locs[i + 1] + 1;
    sv[axis] = xt::range(start, end);

    // Bump so it becomes the correct next start position
    out_axis_locs[i + 1]++;

    // Reshape view on args[i]
    xt::rarray<T> arg_i = args[i];
    auto arg_i_view = rray__increase_dims_view(arg_i, dims);

    xt::strided_view(xt_out, sv) = arg_i_view;
  }

  Rcpp::RObject out = SEXP(xt_out);

  Rf_setAttrib(out, R_DimNamesSymbol, compute_bind_dim_names(arg_dim_names, axis, out_dim));

  return out;
}


// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__bind(Rcpp::RObject proxy,
                         Rcpp::List args,
                         const int& axis,
                         Rcpp::List arg_dim_names) {

  if (Rf_isNull(proxy)) {
    return R_NilValue;
  }

  // Switch on out
  switch(TYPEOF(proxy)) {

  case REALSXP: {
    return rray__bind_impl<double>(args, axis, arg_dim_names);
  }

  case INTSXP: {
    return rray__bind_impl<int>(args, axis, arg_dim_names);
  }

  case LGLSXP: {
    return rray__bind_impl<rlogical>(args, axis, arg_dim_names);
  }

  default: {
    error_unknown_type();
  }

  }

}
