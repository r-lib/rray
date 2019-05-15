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

template <typename T>
Rcpp::RObject rray__bind_impl(xt::rarray<T> out,
                              Rcpp::List args,
                              const int& axis,
                              const int& dims) {

  const int& n_args = args.size();

  //const int& dims = compute_dims(args, axis);

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

  // Immediately resize `out` container to the `out_dim` shape
  out.resize(out_dim);

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

    xt::strided_view(out, sv) = arg_i_view;
  }

  return Rcpp::as<Rcpp::RObject>(out);
}


// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__bind(Rcpp::RObject out,
                         Rcpp::List args,
                         const int& axis,
                         const int& dims) {

  if (Rf_isNull(out)) {
    return R_NilValue;
  }

  // Switch on out
  switch(TYPEOF(out)) {

  case REALSXP: {
    return rray__bind_impl(xt::rarray<double>(out), args, axis, dims);
  }

  case INTSXP: {
    return rray__bind_impl(xt::rarray<int>(out), args, axis, dims);
  }

  case LGLSXP: {
    return rray__bind_impl(xt::rarray<rlogical>(out), args, axis, dims);
  }

  default: {
    error_unknown_type();
  }

  }

}
