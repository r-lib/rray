#include <rray.h>
#include <dispatch.h>
#include <tools/tools.h>

// Required for xt::evaluation_strategy::immediate
#include <xtensor/xarray.hpp>

// -----------------------------------------------------------------------------

#define DISPATCH_REDUCER(FUN, X, AXES)                         \
  if (r_is_null(X)) {                                          \
    return X;                                                  \
  }                                                            \
                                                               \
  Rcpp::RObject out;                                           \
  DISPATCH_UNARY_ONE(out, FUN, X, AXES);                       \
                                                               \
  rray__resize_and_set_dim_names(out, X);                      \
                                                               \
  return out

#define REDUCER_IMPL(FUN, X, AXES)                                            \
  if (r_is_null(AXES)) {                                                      \
    return FUN(X, xt::keep_dims | xt::evaluation_strategy::immediate);        \
  }                                                                           \
                                                                              \
  using size_vec = typename std::vector<std::size_t>;                         \
  size_vec xt_axes = Rcpp::as<size_vec>(AXES);                                \
                                                                              \
  return FUN(X, xt_axes, xt::keep_dims | xt::evaluation_strategy::immediate)

// -----------------------------------------------------------------------------

// guard against integer overflow

template <typename T>
xt::rarray<double> rray__sum_impl(const xt::rarray<T>& x, Rcpp::RObject axes) {
  REDUCER_IMPL(xt::sum, x, axes);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__sum(Rcpp::RObject x, Rcpp::RObject axes) {
  DISPATCH_REDUCER(rray__sum_impl, x, axes);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__prod_impl(const xt::rarray<T>& x, Rcpp::RObject axes) {
  REDUCER_IMPL(xt::prod, x, axes);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__prod(Rcpp::RObject x, Rcpp::RObject axes) {
  DISPATCH_REDUCER(rray__prod_impl, x, axes);
}

// -----------------------------------------------------------------------------

template <typename T>
xt::rarray<double> rray__mean_impl(const xt::rarray<T>& x, Rcpp::RObject axes) {
  REDUCER_IMPL(xt::mean, x, axes);
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__mean(Rcpp::RObject x, Rcpp::RObject axes) {
  DISPATCH_REDUCER(rray__mean_impl, x, axes);
}

// -----------------------------------------------------------------------------

// TODO - Blocked by
// https://github.com/r-lib/rray/issues/42

// template <typename T>
// xt::rarray<double> rray__variance_impl(const xt::rarray<T>& x, Rcpp::RObject axes) {
//
//   if (r_is_null(axes)) {
//     return xt::variance(x, xt::keep_dims);
//   }
//
//   using size_vec = typename std::vector<std::size_t>;
//   size_vec xt_axes = Rcpp::as<size_vec>(axes);
//
//   return xt::variance(x, xt_axes, xt::keep_dims);
// }
//
// // [[Rcpp::export(rng = false)]]
// Rcpp::RObject rray__variance(Rcpp::RObject x, Rcpp::RObject axes) {
//   DISPATCH_UNARY_ONE(rray__variance_impl, x, axes);
// }

// -----------------------------------------------------------------------------

// TODO - Blocked by
// https://github.com/r-lib/rray/issues/42

// template <typename T>
// xt::rarray<double> rray__std_dev_impl(const xt::rarray<T>& x, Rcpp::RObject axes) {
//
//   if (r_is_null(axes)) {
//     return xt::stddev(x, xt::keep_dims);
//   }
//
//   using size_vec = typename std::vector<std::size_t>;
//   size_vec xt_axes = Rcpp::as<size_vec>(axes);
//
//   return xt::stddev(x, xt_axes, xt::keep_dims);
// }
//
// // [[Rcpp::export(rng = false)]]
// Rcpp::RObject rray__std_dev(Rcpp::RObject x, Rcpp::RObject axes) {
//   DISPATCH_UNARY_ONE(rray__std_dev_impl, x, axes);
// }

// -----------------------------------------------------------------------------

// Documentation for `infinite_check()` and `infinite_output()`:
// - `infinite_check()` checks to see if `rray_max()` or `rray_min()` are
// going to return infinite values based on `x`'s dimensions combined with
// the `axes` we are reducing over.
// - `infinite_output()` constructs the correct Inf/-Inf output if `infinite_check()`
// returned true.
// - When would this happen? Consider:
// (0, 1) -> (1, 1)
// rray_max(matrix(numeric(), 0, 1), 1)
// A value has to be filled in, and base R chooses -Inf: max(), but xtensor
// uses the lowest finite value of type T. So we get around this by returning
// our own value if we detect this case. On the other hand, this is fine
// and no special handling is needed by us.
// (0, 1) -> (0, 1)
// rray_max(matrix(numeric(), 0, 1), 2)

template <typename T>
bool infinite_check(const xt::rarray<T>& x,
                    Rcpp::Nullable<Rcpp::IntegerVector> axes) {

  bool is_infinite = false;
  Rcpp::IntegerVector dim = rray__dim(SEXP(x));
  Rcpp::IntegerVector r_axes;

  if (axes.isNull()) {
    r_axes = Rcpp::seq_along(dim) - 1;
  }
  else {
    r_axes = axes;
  }

  for (int axis : r_axes) {
    if (dim[axis] == 0) {
      is_infinite = true;
      break;
    }
  }

  return is_infinite;
}

template <typename T>
Rcpp::RObject infinite_output(const xt::rarray<T>& x,
                              Rcpp::Nullable<Rcpp::IntegerVector> axes,
                              bool is_max) {

  Rcpp::IntegerVector dim = Rcpp::clone(rray__dim(SEXP(x)));
  Rcpp::IntegerVector r_axes;

  if (axes.isNull()) {
    r_axes = Rcpp::seq_along(dim) - 1;
  }
  else {
    r_axes = axes;
  }

  for (int axis : r_axes) {
    dim[axis] = 1;
  }

  xt::rarray<double> xt_inf = is_max ? R_NegInf : R_PosInf;
  xt::rarray<double> xt_inf_out = xt::broadcast(xt_inf, dim);
  Rcpp::RObject inf_out = SEXP(xt_inf_out);

  return inf_out;
}

template <typename T>
Rcpp::RObject rray__max_impl(const xt::rarray<T>& x,
                             Rcpp::Nullable<Rcpp::IntegerVector> axes) {

  if (infinite_check(x, axes)) {
    return infinite_output(x, axes, true);
  }

  if (r_is_null(axes)) {
    xt::rarray<T> xt_out = xt::amax(x, xt::keep_dims | xt::evaluation_strategy::immediate);
    Rcpp::RObject out = SEXP(xt_out);
    return out;
  }

  using size_vec = typename std::vector<std::size_t>;
  size_vec xt_axes = Rcpp::as<size_vec>(axes);

  xt::rarray<T> xt_out = xt::amax(x, xt_axes, xt::keep_dims | xt::evaluation_strategy::immediate);
  Rcpp::RObject out = SEXP(xt_out);

  return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__max(Rcpp::RObject x,
                        Rcpp::Nullable<Rcpp::IntegerVector> axes) {
  DISPATCH_REDUCER(rray__max_impl, x, axes);
}

// -----------------------------------------------------------------------------

template <typename T>
Rcpp::RObject rray__min_impl(const xt::rarray<T>& x,
                             Rcpp::Nullable<Rcpp::IntegerVector> axes) {

  if (infinite_check(x, axes)) {
    return infinite_output(x, axes, false);
  }

  if (r_is_null(axes)) {
    xt::rarray<T> xt_out = xt::amin(x, xt::keep_dims | xt::evaluation_strategy::immediate);
    Rcpp::RObject out = SEXP(xt_out);
    return out;
  }

  using size_vec = typename std::vector<std::size_t>;
  size_vec xt_axes = Rcpp::as<size_vec>(axes);

  xt::rarray<T> xt_out = xt::amin(x, xt_axes, xt::keep_dims | xt::evaluation_strategy::immediate);
  Rcpp::RObject out = SEXP(xt_out);

  return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::RObject rray__min(Rcpp::RObject x,
                        Rcpp::Nullable<Rcpp::IntegerVector> axes) {
  DISPATCH_REDUCER(rray__min_impl, x, axes);
}

// -----------------------------------------------------------------------------
