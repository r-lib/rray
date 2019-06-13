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

  if (r_is_null(x)) {
    return x;
  }

  Rcpp::List out;
  DISPATCH_UNARY_ONE(out, rray__split_impl, x, axes);

  return out;
}

// -----------------------------------------------------------------------------

// times == 1 or 3
// - `to` names are reversed
// - Swap position of `from` and `to` names
void rotate_1_3(Rcpp::List new_dim_names,
                const Rcpp::List& dim_names,
                const std::ptrdiff_t& from,
                const std::ptrdiff_t& to) {

  // Always update `to` names
  new_dim_names[to] = dim_names[from];

  // Only reverse if `to` names are not NULL, but always set them in the `from` slot
  if (r_is_null(dim_names[to])) {
    new_dim_names[from] = dim_names[to];
  }
  else {
    Rcpp::CharacterVector new_to_names = Rf_duplicate(dim_names[to]);
    std::reverse(new_to_names.begin(), new_to_names.end());
    new_dim_names[from] = new_to_names;
  }

}

// times == 2
// - `to` and `from` names are reversed
// - Names are left in the same position
void rotate_2(Rcpp::List new_dim_names,
              const Rcpp::List& dim_names,
              const std::ptrdiff_t& from,
              const std::ptrdiff_t& to) {

  // Reverse `from` names, performing a full duplicate of the original names
  if (!r_is_null(dim_names[from])) {
    Rcpp::CharacterVector new_from_names = Rf_duplicate(dim_names[from]);
    std::reverse(new_from_names.begin(), new_from_names.end());
    new_dim_names[from] = new_from_names;
  }

  // Reverse `to` names, performing a full duplicate of the original names
  if (!r_is_null(dim_names[to])) {
    Rcpp::CharacterVector new_to_names = Rf_duplicate(dim_names[to]);
    std::reverse(new_to_names.begin(), new_to_names.end());
    new_dim_names[to] = new_to_names;
  }

}

Rcpp::List rotate_dim_names(Rcpp::List dim_names,
                            const std::ptrdiff_t& from,
                            const std::ptrdiff_t& to,
                            const int& times) {

  // Only shallow duplicate names so we don't copy non-rotated names
  Rcpp::List new_dim_names = Rf_shallow_duplicate(dim_names);

  if (times == 1 || times == 3) {
    rotate_1_3(new_dim_names, dim_names, from, to);
  }
  else {
    rotate_2(new_dim_names, dim_names, from, to);
  }

  return new_dim_names;
}

template <typename T>
Rcpp::RObject rray__rotate_impl(const xt::rarray<T>& x,
                                const std::ptrdiff_t& from,
                                const std::ptrdiff_t& to,
                                const int& times) {

  // Axes
  std::array<std::ptrdiff_t, 2> axes = {from, to};
  xt::rarray<T> out;

  if (times == 1) {
    out = xt::rot90<1>(x, axes);
  }
  else if (times == 2) {
    out = xt::rot90<2>(x, axes);
  }
  else if (times == 3) {
    out = xt::rot90<3>(x, axes);
  }
  else {
    Rcpp::stop("`n` must be 1, 2, or 3.");
  }

  return Rcpp::as<Rcpp::RObject>(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__rotate(Rcpp::RObject x,
                           const std::ptrdiff_t& from,
                           const std::ptrdiff_t& to,
                           const int& times) {

  if (r_is_null(x)) {
    return x;
  }

  Rcpp::RObject out;
  DISPATCH_UNARY_THREE(out, rray__rotate_impl, x, from, to, times);

  out.attr("dimnames") = rotate_dim_names(rray__dim_names(x), from, to, times);

  return out;
}

// -----------------------------------------------------------------------------

Rcpp::List transpose_dim_names(const Rcpp::List& dim_names,
                               const Rcpp::RObject& permutation) {

  const int& n_dim_names = dim_names.size();
  Rcpp::List new_dim_names = rray__new_empty_dim_names(n_dim_names);
  Rcpp::IntegerVector int_permutation;

  // Default permutation is a reversal of the sequence along the dimensionality
  if (r_is_null(permutation)) {
    int_permutation = Rcpp::seq_len(n_dim_names) - 1;
    std::reverse(int_permutation.begin(), int_permutation.end());
  }
  else {
    int_permutation = permutation;
  }

  // Reorder dim names
  for (int i = 0; i < n_dim_names; ++i) {
    new_dim_names[i] = dim_names[int_permutation[i]];
  }

  // Reorder meta names
  if (!r_is_null(dim_names.names())) {
    Rcpp::CharacterVector meta_names = dim_names.names();
    Rcpp::CharacterVector new_meta_names(n_dim_names);

    for (int i = 0; i < n_dim_names; ++i) {
      new_meta_names[i] = meta_names[int_permutation[i]];
    }

    new_dim_names.names() = new_meta_names;
  }

  return new_dim_names;
}

template <typename T>
Rcpp::RObject rray__transpose_impl(const xt::rarray<T>& x,
                                   const Rcpp::RObject& permutation) {

  using ptrdiff_vec_t = typename std::vector<std::ptrdiff_t>;

  xt::rarray<T> out;

  if (r_is_null(permutation)) {
    out = xt::transpose(x);
  }
  else {
    ptrdiff_vec_t xt_permutation = Rcpp::as<ptrdiff_vec_t>(permutation);
    out = xt::transpose(x, xt_permutation, xt::check_policy::full());
  }

  return Rcpp::as<Rcpp::RObject>(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__transpose(Rcpp::RObject x, Rcpp::RObject permutation) {

  Rcpp::RObject out;
  DISPATCH_UNARY_ONE(out, rray__transpose_impl, x, permutation);

  out.attr("dimnames") = transpose_dim_names(rray__dim_names(x), permutation);

  return out;
}

// -----------------------------------------------------------------------------

Rcpp::IntegerVector compute_complement(const int& dim_n,
                                       const std::vector<std::size_t>& axes) {

  std::vector<std::size_t> axes_seq(dim_n);
  for (std::size_t i = 0; i < dim_n; ++i) {
    axes_seq[i] = i;
  }

  // Output container with with the maximum possible size
  std::vector<std::size_t> axes_complement(dim_n);
  std::vector<std::size_t>::iterator it;

  // Set difference
  it = std::set_difference(
    axes_seq.begin(), axes_seq.end(),
    axes.begin(), axes.end(),
    axes_complement.begin()
  );

  // Resize to shrink the container
  axes_complement.resize(it - axes_complement.begin());

  return Rcpp::wrap(axes_complement);
}

Rcpp::List squeeze_dim_names(const Rcpp::List& dim_names,
                             const std::vector<std::size_t>& axes) {

  const int& dim_n = dim_names.size();
  const bool& squeezing_every_axis = axes.size() == dim_n;

  // Normally, names are pulled from the non `axes` axes
  if (!squeezing_every_axis) {
    Rcpp::IntegerVector axes_complement = compute_complement(dim_n, axes);
    return dim_names[axes_complement];
  }

  // But in this case we are squeezing every axis!
  // (only possible if all axes have size 1)
  // So we take the names from the first dimension with names
  for (int i = 0; i < dim_n; ++i) {
    if (!r_is_null(dim_names[i])) {
      return dim_names[i];
    }
  }

  // If no dimensions have names, and we are squeezing them all,
  // return 1 empty dim name
  return rray__new_empty_dim_names(1);
}

// Call xt::squeeze() but always use xt::check_policy::full()
// which throws an error if you are trying to drop a dimension
// with >1 element. You pretty much never want this so we don't
// expose that option.

// xt::squeeze() docs say it takes `axis` but its really `axes`

template <typename T>
Rcpp::RObject rray__squeeze_impl(const xt::rarray<T>& x,
                                 const std::vector<std::size_t>& axes) {
  xt::rarray<T> out = xt::squeeze(x, axes, xt::check_policy::full());
  return Rcpp::as<Rcpp::RObject>(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__squeeze(Rcpp::RObject x,
                            const std::vector<std::size_t>& axes) {

  Rcpp::RObject out;
  DISPATCH_UNARY_ONE(out, rray__squeeze_impl, x, axes);

  rray__set_dim_names(out, squeeze_dim_names(rray__dim_names(x), axes));

  return out;
}

// -----------------------------------------------------------------------------

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
Rcpp::RObject rray__expand_impl(const xt::rarray<T>& x, const std::size_t& axis) {
  xt::rarray<T> out = xt::expand_dims(x, axis);
  return Rcpp::as<Rcpp::RObject>(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__expand(Rcpp::RObject x, const std::size_t& axis) {

  if (r_is_null(x)) {
    return x;
  }

  Rcpp::RObject out;
  DISPATCH_UNARY_ONE(out, rray__expand_impl, x, axis);

  rray__set_dim_names(out, rray__expand_dim_names(rray__dim_names(x), axis));

  return out;
}

// -----------------------------------------------------------------------------

Rcpp::List rev_axis_names(const Rcpp::List& dim_names, const std::size_t& axis) {

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
Rcpp::RObject rray__flip_impl(const xt::rarray<T>& x, const std::size_t& axis) {
  xt::rarray<T> out = xt::flip(x, axis);
  return Rcpp::as<Rcpp::RObject>(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__flip(Rcpp::RObject x, const std::size_t& axis) {

  if (r_is_null(x)) {
    return x;
  }

  Rcpp::RObject out;
  DISPATCH_UNARY_ONE(out, rray__flip_impl, x, axis);

  rray__set_dim_names(out, rev_axis_names(rray__dim_names(x), axis));

  return out;
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__flatten_impl(const xt::rarray<T>& x) {
  xt::rarray<T> out = xt::flatten<xt::layout_type::column_major>(x);
  return Rcpp::as<Rcpp::RObject>(out);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__flatten(Rcpp::RObject x) {

  if (r_is_null(x)) {
    return x;
  }

  Rcpp::RObject out;
  DISPATCH_UNARY(out, rray__flatten_impl, x);

  rray__resize_and_set_dim_names(out, x);

  return out;
}
