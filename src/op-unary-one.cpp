// this header seems necessary for full_like() rather than xbuilder.hpp
#include <xtensor/xarray.hpp>
#include <xtensor/xsort.hpp>
#include <xtensor/xview.hpp>

#include <rray.h>
#include <tools/tools.h>
using namespace rray;

// -----------------------------------------------------------------------------
// Broadcast

Rcpp::IntegerVector rray_increase_dims(const Rcpp::IntegerVector& dim,
                                       const int& dims) {

  int current_dims = dim.size();

  // Early exit
  if (current_dims == dims) {
    return dim;
  }

  if (current_dims > dims) {
    Rcpp::stop("Cannot decrease dimensions.");
  }

  // At this point, we know we are missing dims
  int n_missing_dims = dims - current_dims;

  // Copy dim since we change it
  Rcpp::IntegerVector out = Rcpp::clone(dim);

  for (int i = 0; i < n_missing_dims; ++i) {
    out.push_back(1);
  }

  return out;
}

// (15 is equal to the default settings of identical())
bool r_identical(SEXP x, SEXP y) {
  return R_compute_identical(x, y, 15);
}

template <typename T>
SEXP rray_broadcast_cpp(const xt::rarray<T>& x, SEXP arg) {

  using vec_size_t = typename std::vector<std::size_t>;

  Rcpp::IntegerVector x_dim = rray_dim(SEXP(x));
  Rcpp::IntegerVector to_dim = Rcpp::as<Rcpp::IntegerVector>(arg);
  int to_dims = to_dim.size();

  // Cheap early exit
  if (r_identical(x_dim, to_dim)) {
    return(x);
  }

  // Match dimensionality before comparison
  x_dim = rray_increase_dims(x_dim, to_dims);

  // 3 cases where broadcasting works:
  // - Dimensions are the same (no change is made)
  // - Dimension of x is 1 (broadcast to new dimension)
  // - New dimension is 0 (no change is made)
  Rcpp::LogicalVector ok = (x_dim == to_dim | x_dim == 1 | to_dim == 0);
  if (Rcpp::is_true(Rcpp::any(!ok))) {
    Rcpp::stop("Non-broadcastable dimensions.");
  }

  // Must reshape to match dimensionality first b/c of QuantStack/xtensor-r#57
  const vec_size_t& x_view_dim = Rcpp::as<vec_size_t>(x_dim);
  auto x_view = xt::reshape_view(x, x_view_dim);

  const vec_size_t& to_dim_xt = Rcpp::as<vec_size_t>(to_dim);
  xt::rarray<T> res = xt::broadcast(x_view, to_dim_xt);

  return(res);
}

// -----------------------------------------------------------------------------
// Sort / arg*

template <typename T>
SEXP rray_sort_cpp(xt::rarray<T> x, SEXP arg) {
  std::ptrdiff_t axis = Rcpp::as<std::ptrdiff_t>(arg);
  xt::rarray<T> res = xt::sort(x, axis);
  return res;
}

template <typename T>
SEXP rray_argsort_cpp(xt::rarray<T> x, SEXP arg) {
  std::ptrdiff_t axis = Rcpp::as<std::ptrdiff_t>(arg);
  xt::rarray<int> res = xt::argsort(x, axis);
  return as_r_idx(res);
}

template <typename T>
SEXP rray_argmax_cpp(xt::rarray<T> x, SEXP arg) {

  if (Rf_isNull(arg)) {
    xt::rarray<int> res = xt::argmax<xt::layout_type::column_major>(x);
    return as_r_idx(res);
  }

  std::size_t axis = Rcpp::as<std::size_t>(arg);
  xt::rarray<int> res = xt::argmax<xt::layout_type::column_major>(x, axis);
  return as_r_idx(res);
}

template <typename T>
SEXP rray_argmin_cpp(xt::rarray<T> x, SEXP arg) {

  if (Rf_isNull(arg)) {
    xt::rarray<int> res = xt::argmin<xt::layout_type::column_major>(x);
    return as_r_idx(res);
  }

  std::size_t axis = Rcpp::as<std::size_t>(arg);
  xt::rarray<int> res = xt::argmin<xt::layout_type::column_major>(x, axis);
  return as_r_idx(res);
}

// -----------------------------------------------------------------------------
// Builders

// type of arg has to be the same as the type of T

template <typename T>
SEXP rray_full_like_cpp(const xt::rarray<T>& x, SEXP arg) {

  // Coerce arg to the underlying type T
  // (rlogical is really int so we need the underlying int type)
  using underlying_type = typename xt::r_detail::get_underlying_value_type_r<T>::type;
  underlying_type fill_value = Rcpp::as<underlying_type>(arg);

  const xt::rarray<T>& res = xt::full_like(x, fill_value);
  return res;
}

template <typename T>
SEXP rray_diag_cpp(const xt::rarray<T>& x, SEXP arg) {
  int k = Rcpp::as<int>(arg);
  const xt::rarray<T>& res = xt::diag(x, k);
  return res;
}

// -----------------------------------------------------------------------------
// Accumulators

// To prevent overflow, return doubles

template <typename T>
SEXP rray_cumsum_cpp(const xt::rarray<T>& x, SEXP arg) {

  if (Rf_isNull(arg)) {
    const xt::rarray<double>& res = xt::cumsum(x);
    return res;
  }

  std::ptrdiff_t axis = Rcpp::as<std::ptrdiff_t>(arg);
  const xt::rarray<double>& res = xt::cumsum(x, axis);
  return res;
}

template <typename T>
SEXP rray_cumprod_cpp(const xt::rarray<T>& x, SEXP arg) {

  if (Rf_isNull(arg)) {
    const xt::rarray<double>& res = xt::cumprod(x);
    return res;
  }

  std::ptrdiff_t axis = Rcpp::as<std::ptrdiff_t>(arg);
  const xt::rarray<double>& res = xt::cumprod(x, axis);
  return res;
}

// -----------------------------------------------------------------------------
// Generators

template <typename T>
SEXP rray_reshape_cpp(const xt::rarray<T>& x, SEXP arg) {

  std::vector<std::size_t> dim = Rcpp::as<std::vector<std::size_t>>(arg);

  // needs a copy, otherwise modifying x
  xt::rarray<T> res(x);

  res.reshape(dim);

  return(res);
}

// -----------------------------------------------------------------------------
// Manipulation

template <typename T>
SEXP rray_transpose_cpp(const xt::rarray<T>& x, SEXP arg) {

  if (Rf_isNull(arg)) {
    const xt::rarray<T>& res = xt::transpose(x);
    return res;
  }

  std::vector<std::ptrdiff_t> permutation = Rcpp::as<std::vector<std::ptrdiff_t>>(arg);

  const xt::rarray<T>& res = xt::transpose(x, permutation, xt::check_policy::full());
  return res;
}

// Call xt::squeeze() but always use xt::check_policy::full()
// which throws an error if you are trying to drop a dimension
// with >1 element. You pretty much never want this so we don't
// expose that option.

// xt::squeeze() docs say it takes `axis` but its really `axes`

template <typename T>
SEXP rray_squeeze_cpp(const xt::rarray<T>& x, SEXP arg) {
  std::vector<std::size_t> axes = Rcpp::as<std::vector<std::size_t>>(arg);
  xt::rarray<T> res = xt::squeeze(x, axes, xt::check_policy::full());
  return res;
}

template <typename T>
SEXP rray_expand_dims_cpp(const xt::rarray<T>& x, SEXP arg) {
  std::size_t axis = Rcpp::as<std::size_t>(arg);
  xt::rarray<T> res = xt::expand_dims(x, axis);
  return res;
}

template <typename T>
SEXP rray_flip_cpp(const xt::rarray<T>& x, SEXP arg) {
  std::size_t axis = Rcpp::as<std::size_t>(arg);
  xt::rarray<T> res = xt::flip(x, axis);
  return res;
}

// -----------------------------------------------------------------------------
// Switch on the op

template <typename T1>
SEXP rray_op_unary_one_cpp_impl(std::string op, const xt::rarray<T1>& x, SEXP arg) {

  switch(str2int(op.c_str())) {

  // ---------------------------------------------------------------------------
  // Broadcast

  case str2int("broadcast"): {
    return rray_broadcast_cpp(x, arg);
  }

  // ---------------------------------------------------------------------------
  // Sort / arg*

  case str2int("sort"): {
    return rray_sort_cpp(x, arg);
  }

  case str2int("argsort"): {
    return rray_argsort_cpp(x, arg);
  }

  case str2int("argmax"): {
    return rray_argmax_cpp(x, arg);
  }

  case str2int("argmin"): {
    return rray_argmin_cpp(x, arg);
  }

  // ---------------------------------------------------------------------------
  // Builders

  case str2int("full_like"): {
    return rray_full_like_cpp(x, arg);
  }

  case str2int("diag"): {
    return rray_diag_cpp(x, arg);
  }

  // ---------------------------------------------------------------------------
  // Accumulator

  case str2int("cumsum"): {
    return rray_cumsum_cpp(x, arg);
  }

  case str2int("cumprod"): {
    return rray_cumprod_cpp(x, arg);
  }

  // ---------------------------------------------------------------------------
  // Generators

  case str2int("reshape"): {
    return rray_reshape_cpp(x, arg);
  }

  // ---------------------------------------------------------------------------
  // Manipulation

  case str2int("transpose"): {
    return rray_transpose_cpp(x, arg);
  }

  case str2int("squeeze"): {
    return rray_squeeze_cpp(x, arg);
  }

  case str2int("expand_dims"): {
    return rray_expand_dims_cpp(x, arg);
  }

  case str2int("flip"): {
    return rray_flip_cpp(x, arg);
  }

  default: {
    Rcpp::stop("Unknown unary operation.");
  }

  }

}

// -----------------------------------------------------------------------------
// Switch on the type of x

// [[Rcpp::export]]
SEXP rray_op_unary_one_cpp(std::string op, SEXP x, SEXP arg) {

  if (Rf_isNull(x)) {
    return(R_NilValue);
  }

  // Switch on X
  switch(TYPEOF(x)) {

  case REALSXP: {
    return rray_op_unary_one_cpp_impl(op, xt::rarray<double>(x), arg);
  }

  case INTSXP: {
    return rray_op_unary_one_cpp_impl(op, xt::rarray<int>(x), arg);
  }

  case LGLSXP: {
    return rray_op_unary_one_cpp_impl(op, xt::rarray<rlogical>(x), arg);
  }

  default: {
    error_unknown_type();
  }

  }

}
