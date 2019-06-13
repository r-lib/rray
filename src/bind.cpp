#include <rray.h>
#include <dispatch.h>
#include <cast.h>

#include <xtensor/xstrided_view.hpp>
#include <xtensor/xarray.hpp>
#include <xtensor/xnoalias.hpp>

// -----------------------------------------------------------------------------

int compute_dimensionality(Rcpp::List args, const int& axis) {

  int dim_n = 1;
  const int& n_args = args.size();
  const int& implied_dim_n = axis + 1;

  for (int i = 0; i < n_args; ++i) {
    dim_n = std::max(rray__dim_n(args[i]), dim_n);
  }

  // Allows for going up in dimensionality
  dim_n = std::max(dim_n, implied_dim_n);

  return dim_n;
}

// -----------------------------------------------------------------------------

// If all elements are null, and there are no names, nothing to do
bool has_only_null_elements_and_no_names(const Rcpp::List& x) {

  bool all_null = true;
  const int& dim_n = x.size();
  Rcpp::RObject names = x.names();

  if (!r_is_null(names)) {
    all_null = false;
    return all_null;
  }

  for (int j = 0; j < dim_n; ++j) {

    if (r_is_null(x[j])) {
      continue;
    }

    all_null = false;
    break;
  }

  return all_null;
}

// -----------------------------------------------------------------------------

bool is_empty_string(const Rcpp::String& x) {
  bool res = (x == "");
  return res;
}

// Attach outer names to existing `new_axis_names`
void add_outer_names(Rcpp::CharacterVector& new_axis_names,
                     const Rcpp::CharacterVector& outer_names,
                     const Rcpp::IntegerVector& axis_sizes) {

  int pos = 0;
  const Rcpp::String& dot_dot = "..";
  const int& n_sizes = axis_sizes.size();

  for (int i = 0; i < n_sizes; ++i) {

    const Rcpp::String& outer_name = outer_names[i];
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
    // Either `outer..(j+1)` or `outer..name`
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

  // Early exit in the most common case - no names, no outer names
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
    add_outer_names(
      new_axis_names,
      Rcpp::as<Rcpp::CharacterVector>(outer_names),
      axis_sizes
    );
  }

  return new_axis_names;
}

Rcpp::List compute_bind_dim_names(const Rcpp::List& lst_of_dim_names,
                                  const int& axis,
                                  const Rcpp::IntegerVector& dim,
                                  const Rcpp::IntegerVector& axis_sizes) {

  const int& n_args = lst_of_dim_names.size();
  const int& dim_n = dim.size();

  Rcpp::List new_dim_names = rray__new_empty_dim_names(dim_n);
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
    dim_names = rray__resize_dim_names(dim_names, dim);

    // Coalesce names and meta names
    new_dim_names = rray__coalesce_dim_names(new_dim_names, dim_names);
  }

  new_dim_names[axis] = combine_axis_names(
    lst_of_axis_names,
    axis_sizes,
    outer_names
  );

  return new_dim_names;
}

// -----------------------------------------------------------------------------

Rcpp::List compute_out_info(const Rcpp::List& args,
                            const int& axis,
                            const int& dim_n) {

  const int& n_args = args.size();

  int loc = 0;
  int out_axis_size = 0;

  Rcpp::IntegerVector out_dim(dim_n, 1);
  Rcpp::IntegerVector axis_starts(n_args);
  Rcpp::IntegerVector axis_ends(n_args);
  Rcpp::IntegerVector axis_sizes(n_args);

  for (int i = 0; i < n_args; ++i) {
    // Must clone, otherwise directly affecting the args `dim`
    Rcpp::IntegerVector arg_dim_i = Rcpp::clone(rray__dim(args[i]));
    arg_dim_i = rray__increase_dims(arg_dim_i, dim_n);

    int axis_size_i = arg_dim_i[axis];
    axis_sizes[i] = axis_size_i;
    arg_dim_i[axis] = 0;

    out_dim = rray__dim2(out_dim, arg_dim_i);
    out_axis_size = out_axis_size + axis_size_i;

    axis_starts[i] = loc;
    loc = loc + axis_size_i;
    axis_ends[i] = loc;
  }

  out_dim[axis] = out_axis_size;

  return Rcpp::List::create(
    Rcpp::Named("out_dim") = out_dim,
    Rcpp::Named("axis_starts") = axis_starts,
    Rcpp::Named("axis_ends") = axis_ends,
    Rcpp::Named("axis_sizes") = axis_sizes
  );
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__bind_impl(const Rcpp::List& args,
                              const int& axis,
                              const Rcpp::List& lst_of_dim_names) {

  const int& n_args = args.size();
  const int& dim_n = compute_dimensionality(args, axis);

  Rcpp::List out_info = compute_out_info(args, axis, dim_n);

  Rcpp::IntegerVector out_dim = out_info["out_dim"];
  Rcpp::IntegerVector axis_starts = out_info["axis_starts"];
  Rcpp::IntegerVector axis_ends = out_info["axis_ends"];
  Rcpp::IntegerVector axis_sizes = out_info["axis_sizes"];

  // Allocate an empty container of type `T` and shape `out_dim`
  const std::vector<std::size_t>& shape = Rcpp::as<std::vector<std::size_t>>(out_dim);
  xt::rarray<T> xt_out(shape);

  // Initialize slice vector with `all()`
  xt::xstrided_slice_vector sv(dim_n, xt::all());

  for (int i = 0; i < n_args; ++i) {

    // Update slice vector positions
    sv[axis] = xt::range(axis_starts[i], axis_ends[i]);

    // Reshape view on args[i]
    xt::rarray<T> arg_i = args[i];
    auto arg_i_view = rray__increase_dims_view(arg_i, dim_n);

    auto xt_view = xt::strided_view(xt_out, sv);
    xt::noalias(xt_view) = arg_i_view;
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
                         Rcpp::List args,
                         const int& axis) {

  R_len_t n_args = args.size();
  Rcpp::List lst_of_dim_names(n_args);

  // Attach outer names
  lst_of_dim_names.names() = args.names();

  for (int i = 0; i < n_args; ++i) {
    lst_of_dim_names[i] = rray__dim_names(args[i]);
    args[i] = vec__cast_inner(args[i], proxy);
  }

  // Dispatch on proxy type
  switch(TYPEOF(proxy)) {
    case REALSXP: return rray__bind_impl<double>(args, axis, lst_of_dim_names);
    case INTSXP: return rray__bind_impl<int>(args, axis, lst_of_dim_names);
    case LGLSXP: return rray__bind_impl<rlogical>(args, axis, lst_of_dim_names);
    default: error_unknown_type();
  }

}
