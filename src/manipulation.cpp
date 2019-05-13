#include <rray.h>
#include <dispatch.h>

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

template <class E>
auto xt_split(E& e, const std::vector<std::size_t>& axes) {

  // Initially fill the slice vector with all()
  xt::xstrided_slice_vector sv(e.dimension(), xt::all());

  // Construct the result vector of strided view
  std::vector<decltype(xt::strided_view(e, sv))> result;

  int n_axes = axes.size();

  // Container for keeping track of current axes indices
  std::vector<int> idxs(n_axes);

  // Get the shape of the axes
  auto shape = e.shape();
  std::vector<std::size_t> shape_axes(n_axes);
  for (int i = 0; i < n_axes; ++i) {
    shape_axes[i] = shape[axes[i]];
  }

  // essentially same as prod(shape_axes)
  int n_elems = std::accumulate(
    begin(shape_axes), end(shape_axes),
    1, std::multiplies<int>()
  );

  for (int n = 0; n < n_elems; ++n) {

    // Add strided view
    for (int i = 0; i < n_axes; ++i) {
      sv[axes[i]] = xt::range(idxs[i], (idxs[i] + 1));
    }
    result.emplace_back(xt::strided_view(e, sv));

    // Update idxs
    for (int j = 0; j < n_axes; ++j) {
      idxs[j]++;
      if (idxs[j] < shape_axes[j]) break;
      idxs[j] = 0;
    }

  }

  return result;
}

// [[Rcpp::export(rng = false)]]
Rcpp::List rray__new_empty_dim_names(int n) {
  return Rcpp::List(n);
}

bool has_dim(const Rcpp::RObject& x) {
  return x.hasAttribute("dim");
}

// [[Rcpp::export(rng = false)]]
Rcpp::List rray__dim_names(const Rcpp::RObject& x) {

  Rcpp::RObject x_dim_names;

  if (has_dim(x)) {
    x_dim_names = x.attr("dimnames");

    if (x_dim_names.isNULL()) {
      x_dim_names = rray__new_empty_dim_names(rray__dims(x));
    }
  }
  else {
    x_dim_names = x.attr("names");

    if (x_dim_names.isNULL()) {
      x_dim_names = rray__new_empty_dim_names(rray__dims(x));
    }
    else {
      // character vector -> list
      x_dim_names = Rcpp::List::create(x_dim_names);
    }
  }

  return Rcpp::as<Rcpp::List>(x_dim_names);
}

Rcpp::List split_dim_names(const Rcpp::List& dim_names,
                           const std::vector<std::size_t>& axes) {

  Rcpp::List dim_names_copy = Rcpp::clone(dim_names);

  for (std::size_t axis : axes) {
    dim_names_copy[axis] = R_NilValue;
  }

  return dim_names_copy;
}

template <typename T>
Rcpp::List rray__split_impl(const xt::rarray<T>& x,
                            const std::vector<std::size_t>& axes) {

  auto res = xt_split(x, axes);
  int n = res.size();

  Rcpp::List x_dim_names = rray__dim_names(SEXP(x));
  Rcpp::List new_dim_names = split_dim_names(x_dim_names, axes);

  Rcpp::List out(n);

  for (int i = 0; i < n; ++i) {
    xt::rarray<T> res_i = res[i];
    out[i] = res_i;
    Rf_setAttrib(out[i], R_DimNamesSymbol, new_dim_names);
  }

  return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__split(Rcpp::RObject x, const std::vector<std::size_t>& axes) {
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

template <typename T>
xt::rarray<T> rray__expand_dims_impl(const xt::rarray<T>& x, std::size_t axis) {
  return xt::expand_dims(x, axis);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__expand_dims(Rcpp::RObject x, std::size_t axis) {
  DISPATCH_UNARY_ONE(rray__expand_dims_impl, x, axis);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<T> rray__flip_impl(const xt::rarray<T>& x, std::size_t axis) {
  return xt::flip(x, axis);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__flip(Rcpp::RObject x, std::size_t axis) {
  DISPATCH_UNARY_ONE(rray__flip_impl, x, axis);
}


