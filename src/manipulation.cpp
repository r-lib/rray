#include <rray.h>
#include <dispatch.h>

// Required for xt::flatten() which uses an xtensor_adaptor()
#include <xtensor/xadapt.hpp>

// -----------------------------------------------------------------------------

// xt_split() splits `e` along `axes`. For example, it does:
// x <- array(1:8, c(2, 2, 2))
// rray_split(x, c(1, 2)) # (0, 1) cpp axes
// x[1, 1, all()]
// x[2, 1, all()]
// x[1, 2, all()]
// x[2, 2, all()]

// No `n` for now.
// Assume `n` splits it so that the step = 1

// idx updating adapted from
// https://stackoverflow.com/questions/6588487/is-there-a-way-to-iterate-over-an-n-dimensional-array-where-n-is-variable-with

bool any_not_null_along_axes(const Rcpp::List& x_dim_names,
                             const std::vector<std::size_t>& axes) {
  int n_axes = axes.size();

  for (int i = 0; i < n_axes; ++i) {
    if (!r_is_null(x_dim_names[axes[i]])) {
      return true;
    }
  }

  return false;
}

Rcpp::List split_dim_names(const Rcpp::List& x_dim_names,
                           const std::vector<std::size_t>& axes,
                           const std::vector<int>& idxs) {

  // Clone b/c we know from `any_not_null_along_axes()` that at least 1
  // axis requires splitting
  Rcpp::List new_dim_names = Rcpp::clone(x_dim_names);

  int n_axes = axes.size();

  for (int i = 0; i < n_axes; ++i) {

    std::size_t axis = axes[i];

    // No dim names along this axis
    if (r_is_null(new_dim_names[axis])) {
      continue;
    }

    int idx = idxs[i];

    // Subset using idx
    Rcpp::CharacterVector new_axis_names = new_dim_names[axis];
    new_axis_names = new_axis_names[idx];

    new_dim_names[axis] = new_axis_names;
  }

  return new_dim_names;
}

template <typename T>
Rcpp::List rray__split_impl(const xt::rarray<T>& x,
                            const std::vector<std::size_t>& axes) {

  // Initially fill the slice vector with all()
  xt::xstrided_slice_vector sv(x.dimension(), xt::all());

  // Construct the result vector of strided view
  std::vector<decltype(xt::strided_view(x, sv))> result;

  int n_axes = axes.size();

  // Container for keeping track of current axes indices
  std::vector<int> idxs(n_axes);

  // Get dim names
  Rcpp::List x_dim_names = rray__dim_names(SEXP(x));
  bool requires_name_subsetting = any_not_null_along_axes(x_dim_names, axes);

  // Get the shape of the axes
  auto shape = x.shape();
  std::vector<std::size_t> shape_axes(n_axes);
  for (int i = 0; i < n_axes; ++i) {
    shape_axes[i] = shape[axes[i]];
  }

  // essentially same as prod(shape_axes)
  int n_elems = std::accumulate(
    begin(shape_axes), end(shape_axes),
    1, std::multiplies<int>()
  );

  Rcpp::List out(n_elems);

  for (int n = 0; n < n_elems; ++n) {

    // Add strided view
    for (int i = 0; i < n_axes; ++i) {
      sv[axes[i]] = xt::range(idxs[i], (idxs[i] + 1));
    }

    xt::rarray<T> x_view = xt::strided_view(x, sv);
    out[n] = x_view;

    if (requires_name_subsetting) {
      Rcpp::List new_dim_names = split_dim_names(x_dim_names, axes, idxs);
      Rf_setAttrib(out[n], R_DimNamesSymbol, new_dim_names);
    }
    else {
      Rf_setAttrib(out[n], R_DimNamesSymbol, x_dim_names);
    }

    // Update idxs
    for (int j = 0; j < n_axes; ++j) {
      idxs[j]++;
      if (idxs[j] < shape_axes[j]) break;
      idxs[j] = 0;
    }

  }

  return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__split(Rcpp::RObject x,
                          const std::vector<std::size_t>& axes) {
  DISPATCH_UNARY_ONE(rray__split_impl, x, axes);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<T> rray__rotate_impl(const xt::rarray<T>& x,
                                std::ptrdiff_t from,
                                std::ptrdiff_t to,
                                int n) {

  // Axes
  std::array<std::ptrdiff_t, 2> axes = {from, to};

  if (n == 1) {
    xt::rarray<T> res = xt::rot90<1>(x, axes);
    return res;
  }
  else if (n == 2) {
    xt::rarray<T> res = xt::rot90<2>(x, axes);
    return res;
  }
  else if (n == 3) {
    xt::rarray<T> res = xt::rot90<3>(x, axes);
    return res;
  }
  else {
    Rcpp::stop("`n` must be 1, 2, or 3.");
  }

}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__rotate(Rcpp::RObject x, std::ptrdiff_t from, std::ptrdiff_t to, int n) {
  DISPATCH_UNARY_THREE(rray__rotate_impl, x, from, to, n);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<T> rray__transpose_impl(const xt::rarray<T>& x,
                                   Rcpp::RObject permutation) {

  using ptrdiff_vec_t = typename std::vector<std::ptrdiff_t>;

  if (r_is_null(permutation)) {
    xt::rarray<T> res = xt::transpose(x);
    return res;
  }

  ptrdiff_vec_t xt_permutation = Rcpp::as<ptrdiff_vec_t>(permutation);

  return xt::transpose(x, xt_permutation, xt::check_policy::full());
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__transpose(Rcpp::RObject x, Rcpp::RObject permutation) {
  DISPATCH_UNARY_ONE(rray__transpose_impl, x, permutation);
}

// -----------------------------------------------------------------------------

// Call xt::squeeze() but always use xt::check_policy::full()
// which throws an error if you are trying to drop a dimension
// with >1 element. You pretty much never want this so we don't
// expose that option.

// xt::squeeze() docs say it takes `axis` but its really `axes`

template <typename T>
xt::rarray<T> rray__squeeze_impl(const xt::rarray<T>& x, std::vector<std::size_t> axes) {
  return xt::squeeze(x, axes, xt::check_policy::full());
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__squeeze(Rcpp::RObject x, std::vector<std::size_t> axes) {
  DISPATCH_UNARY_ONE(rray__squeeze_impl, x, axes);
}

// -----------------------------------------------------------------------------

// [[Rcpp::export(rng = false)]]
Rcpp::List rray__expand_dim_names(const Rcpp::List& dim_names,
                                  const std::size_t& axis) {

  const int& n_dim_names = dim_names.size();
  const int& n_res_dim_names = n_dim_names + 1;

  Rcpp::List new_dim_names = rray__new_empty_dim_names(n_res_dim_names);

  int offset = 0;
  for (int i = 0; i < n_dim_names; ++i) {
    if (i == axis) {
      offset = 1;
    }
    new_dim_names[i + offset] = dim_names[i];
  }

  return new_dim_names;
}

template <typename T>
Rcpp::RObject rray__expand_dims_impl(const xt::rarray<T>& x, std::size_t axis) {

  xt::rarray<T> xt_out = xt::expand_dims(x, axis);

  Rcpp::List new_dim_names = rray__expand_dim_names(rray__dim_names(SEXP(x)), axis);

  Rcpp::RObject out = SEXP(xt_out);
  out.attr("dimnames") = new_dim_names;

  return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__expand_dims(Rcpp::RObject x, std::size_t axis) {
  DISPATCH_UNARY_ONE(rray__expand_dims_impl, x, axis);
}

// -----------------------------------------------------------------------------

Rcpp::List rev_axis_names(const Rcpp::List& dim_names, std::size_t axis) {

  if (r_is_null(dim_names[axis])) {
    return dim_names;
  }

  // Shallow duplicate the list since we are only changing 1 element
  Rcpp::List new_dim_names = Rf_shallow_duplicate(dim_names);

  // Deep copy the element we are altering
  Rcpp::CharacterVector new_axis_names = Rf_duplicate(new_dim_names[axis]);

  std::reverse(new_axis_names.begin(), new_axis_names.end());

  new_dim_names[axis] = new_axis_names;

  return new_dim_names;
}

template <typename T>
Rcpp::RObject rray__flip_impl(const xt::rarray<T>& x, std::size_t axis) {

  xt::rarray<T> xt_out = xt::flip(x, axis);

  Rcpp::List new_dim_names = rev_axis_names(rray__dim_names(SEXP(x)), axis);

  Rcpp::RObject out = SEXP(xt_out);
  out.attr("dimnames") = new_dim_names;

  return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__flip(Rcpp::RObject x, std::size_t axis) {
  DISPATCH_UNARY_ONE(rray__flip_impl, x, axis);
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__flatten_impl(const xt::rarray<T>& x) {
  xt::rarray<T> xt_out = xt::flatten<xt::layout_type::column_major>(x);
  Rcpp::RObject out = SEXP(xt_out);
  rray__reshape_and_set_dim_names(out, SEXP(x));
  return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__flatten(Rcpp::RObject x) {
  DISPATCH_UNARY(rray__flatten_impl, x);
}
