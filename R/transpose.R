#' Transpose an array
#'
#' `rray_transpose()` transposes `x` along axes defined by `permutation`. By
#' default, a standard transpose is performed, which is equivalent to
#' permuting along the reversed dimensions of `x`.
#'
#' @param x A vector, matrix, array, or rray.
#'
#' @param permutation This should be some permutation of `1:n` with `n`
#' being the number of dimensions of `x`. If `NULL`, the reverse of `1:n` is
#' used, which is the normal transpose.
#'
#' @details
#'
#' Unlike `t()`, using `rray_transpose()` on a vector does not transpose it,
#' as it is a 1D object, and the consistent result of transposing a
#' 1D object is itself.
#'
#' `t.vctrs_rray()` is powered by `rray_transpose()`, which means that it also
#' has this property if you use `t(<rray>)` with a 1D rray.
#'
#' There is an `aperm()` method for `rray` objects as well. Unlike base R,
#' it currently does not accept character strings for `perm`.
#'
#' @examples
#'
#' x <- rray(
#'  1:6,
#'  c(3, 2),
#'  dim_names = list(rows = c("r1", "r2", "r3"), cols = c("c1", "c2"))
#' )
#'
#' # A standard transpose
#' rray_transpose(x)
#'
#' # Identical to
#' rray_transpose(x, rev(1:2))
#'
#' x_3d <- rray_broadcast(x, c(3, 2, 2))
#'
#' # transpose here is like setting
#' # `permutation = c(3, 2, 1)`
#' # so the result should change _shape_ like:
#' # (3, 2, 2) -> (2, 2, 3)
#' rray_transpose(x_3d)
#'
#' # This transposes the "inner" matrices
#' # (flips the first and second dimension)
#' # and leaves the 3rd dimension alone
#' rray_transpose(x_3d, c(2, 1, 3))
#'
#' # ---------------------------------------------------------------------------
#' # Difference from base R
#'
#' # Coerces 1:5 into a 2D matrix, then transposes
#' t(1:5)
#'
#' # Leaves it as a 1D array and does nothing
#' rray_transpose(1:5)
#'
#' # rray_transpose() powers t.vctrs_rray()
#' # so you get the same behavior when you
#' # pass t() an rray
#' t(rray(1:5))
#'
#' @export
rray_transpose <- function(x, permutation = NULL) {

  dims <- rray_dims(x)
  permutation <- vec_cast(permutation, integer())
  validate_permutation(permutation, dims)
  validate_axes(permutation, x, nm = "permutation")

  out <- rray__transpose(x, as_cpp_idx(permutation))

  vec_cast_container(out, x)
}

#' @rdname rray_transpose
#' @export
t.vctrs_rray <- function(x) {

  if (rray_dims(x) > 2L) {
    glubort(
      "`t()` only works with 1D and 2D objects, do you need `rray_transpose()`?"
    )
  }

  rray_transpose(x)
}

#' @export
aperm.vctrs_rray <- function(a, perm = NULL, ...) {
  rray_transpose(a, permutation = perm)
}

validate_permutation <- function(permutation, dims) {

  if (is.null(permutation)) {
    return(invisible(permutation))
  }

  perm_size <- vec_size(permutation)

  if (perm_size != dims) {
    glubort(
      "`permutation` must have size {dims} to permute `x`, not {perm_size}."
    )
  }

  perm_dups <- duplicated(permutation)

  if (any(perm_dups)) {
    dups <- permutation[perm_dups]
    dups <- glue::glue_collapse(dups, ", ")

    glubort(
      "`permutation` contains the following axes more than once: {dups}."
    )
  }

  invisible(permutation)
}
