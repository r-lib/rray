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

// If all elements are null, and there are no names, nothing to do
bool has_only_null_elements_and_no_names(const Rcpp::List& x) {

  bool all_null = true;
  const int& dims = x.size();
  Rcpp::RObject names = x.names();

  if (!r_is_null(names)) {
    all_null = false;
    return all_null;
  }

  for (int j = 0; j < dims; ++j) {

    if (r_is_null(x[j])) {
      continue;
    }

    all_null = false;
    break;
  }

  return all_null;
}

bool is_empty_string(const Rcpp::String& x) {
  bool res = (x == "");
  return res;
}

void add_outer_names(Rcpp::CharacterVector& new_axis_names,
                     const Rcpp::CharacterVector& outer_names,
                     const Rcpp::IntegerVector& axis_sizes) {

  int pos = 0;
  const Rcpp::String& dot_dot = "..";
  const int& n_sizes = axis_sizes.size();

  for (int i = 0; i < n_sizes; ++i) {

    Rcpp::String outer_name = outer_names[i];
    const int& size = axis_sizes[i];

    // No outer name, keep existing result
    if (is_empty_string(outer_name)) {
      pos = pos + size;
      continue;
    }

    // Outer name, and size is 1
    // Either `outer` or `outer..name`
    if (size == 1) {

      Rcpp::String new_axis_name = new_axis_names[pos];

      if (is_empty_string(new_axis_name)) {
        new_axis_names[pos] = outer_name;
      }
      else {
        new_axis_name.push_front(dot_dot);
        new_axis_name.push_front(outer_name);
        new_axis_names[pos] = new_axis_name;
      }

      pos++;
      continue;
    }

    // Outer name available, and size > 1
    // Either `outer..n` or `outer..name`
    for (int j = 0; j < size; ++j) {

      Rcpp::String new_axis_name = new_axis_names[pos];

      if (is_empty_string(new_axis_name)) {
        new_axis_name.push_back(outer_name);
        new_axis_name.push_back(j + 1);
        new_axis_names[pos] = new_axis_name;
      }
      else {
        new_axis_name.push_front(dot_dot);
        new_axis_name.push_front(outer_name);
        new_axis_names[pos] = new_axis_name;
      }

      pos++;
    }

  }

}

// Loop through `lst_of_axis_names`, which is a list of character vectors
// or NULL and combine them into 1 character vector
Rcpp::RObject combine_axis_names(const Rcpp::List& lst_of_axis_names,
                                 const Rcpp::IntegerVector& axis_sizes,
                                 const Rcpp::RObject& outer_names) {

  const bool& has_outer_names = !r_is_null(outer_names);

  if (has_only_null_elements_and_no_names(lst_of_axis_names) && !has_outer_names) {
    return R_NilValue;
  }

  int pos = 0;
  const int& n_args = lst_of_axis_names.size();
  const int& size = Rcpp::sum(axis_sizes);
  Rcpp::CharacterVector new_axis_names(size);

  for (int i = 0; i < n_args; ++i) {

    if (r_is_null(lst_of_axis_names[i])) {
      pos = pos + axis_sizes[i];
      continue;
    }

    const Rcpp::CharacterVector& axis_names = lst_of_axis_names[i];
    const R_xlen_t& n_names = Rf_xlength(axis_names);

    for (int j = 0; j < n_names; ++j) {
      new_axis_names[pos] = axis_names[j];
      pos++;
    }

  }

  if (has_outer_names) {
    add_outer_names(new_axis_names, Rcpp::as<Rcpp::CharacterVector>(outer_names), axis_sizes);
  }

  return new_axis_names;
}

// [[Rcpp::export(rng = false)]]
Rcpp::List compute_bind_dim_names(const Rcpp::List& lst_of_dim_names,
                                  const int& axis,
                                  const Rcpp::IntegerVector& dim,
                                  const Rcpp::IntegerVector& axis_sizes) {

  const int& n_args = lst_of_dim_names.size();
  const int& dims = dim.size();

  Rcpp::List new_dim_names = rray__new_empty_dim_names(dims);
  Rcpp::List lst_of_axis_names(n_args);

  Rcpp::RObject outer_names = lst_of_dim_names.names();

  for (int i = 0; i < n_args; ++i) {

    Rcpp::List dim_names = lst_of_dim_names[i];

    // Store axis dim names or NULL
    if (dim_names.size() >= axis + 1) {
      lst_of_axis_names[i] = dim_names[axis];
    }
    else {
      lst_of_axis_names[i] = R_NilValue;
    }

    if (has_only_null_elements_and_no_names(dim_names)) {
      continue;
    }

    // Call this after storing axis names, as it will set
    // names to null if they don't match the dim size
    dim_names = rray__reshape_dim_names(dim_names, dim);

    // Coalesce names and meta names
    new_dim_names = rray__coalesce_dim_names(new_dim_names, dim_names);
  }

  new_dim_names[axis] = combine_axis_names(lst_of_axis_names, axis_sizes, outer_names);

  return new_dim_names;
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__bind_impl(const Rcpp::List& args,
                              const int& axis,
                              const Rcpp::List& lst_of_dim_names) {

  const int& n_args = args.size();

  const int& dims = compute_dims(args, axis);

  // ---------------------------------------------------------------------------
  // TODO - could this block be extracted into a function that returns a
  // list of `out_dim` and `out_axis_locs` and `axis_sizes`?

  std::vector<std::size_t> out_axis_locs(n_args + 1);
  out_axis_locs[0] = 0;
  int loc = 0;
  int out_axis_size = 0;
  Rcpp::IntegerVector out_dim(dims, 1);
  Rcpp::IntegerVector axis_sizes(n_args);

  for (int i = 0; i < n_args; ++i) {
    // Must clone, otherwise directly affecting the args `dim`
    Rcpp::IntegerVector arg_dim_i = Rcpp::clone(rray__dim(args[i]));
    arg_dim_i = rray__increase_dims(arg_dim_i, dims);

    axis_sizes[i] = arg_dim_i[axis];
    arg_dim_i[axis] = 0;

    out_dim = rray__dim2(out_dim, arg_dim_i);
    out_axis_size = out_axis_size + axis_sizes[i];

    out_axis_locs[i + 1] = loc + axis_sizes[i] - 1;
    loc = loc + axis_sizes[i];
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

  const Rcpp::List& new_dim_names = compute_bind_dim_names(
    lst_of_dim_names,
    axis,
    out_dim,
    axis_sizes
  );

  Rf_setAttrib(out, R_DimNamesSymbol, new_dim_names);

  return out;
}


// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__bind(Rcpp::RObject proxy,
                         const Rcpp::List& args,
                         const int& axis,
                         const Rcpp::List& lst_of_dim_names) {

  // Dispatch on proxy type
  switch(TYPEOF(proxy)) {
    case REALSXP: return rray__bind_impl<double>(args, axis, lst_of_dim_names);
    case INTSXP: return rray__bind_impl<int>(args, axis, lst_of_dim_names);
    case LGLSXP: return rray__bind_impl<rlogical>(args, axis, lst_of_dim_names);
    default: error_unknown_type();
  }

}
