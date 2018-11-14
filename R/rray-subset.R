#' @export
`[.vctrs_rray` <- function(x, i, j, ..., drop = FALSE) {
  x <- as_array(x)
  d <- vec_dims(x)

  # for differentiating x[1] from x[1,]
  n_real_args <- nargs() - !missing(drop)

  dots <- dots_list(..., .preserve_empty = TRUE)

  # x[i], or just x[]
  if (n_real_args <= 2) {

    # x[i]
    if (!is_missing(i)) {

      abort("Use `x[,j]` to select columns, not `x[j]`.")

    }
    # x[]
    else {
      # nothing
    }

    return(as_rray(x))
  }

  # at this point one of these is returned:
  # x[i,] x[,j], x[i,j], x[i,,...], x[i,j,...], x[,,...], x[,j,...]

  # x[,j] or x[i,j] or x[i,j,...] or x[,j,...]
  if (!is_missing(j)) {
    if (d == 1) abort("incorrect number of dimensions")
    miss_args <- rep(list(missing_arg()), d - 2)
    x <- eval_bare(expr(x[, j, !!!miss_args, drop = FALSE]))
  }

  # x[i,j,...], x[i,,...], x[,,...], x[,j,...]
  if (!is_empty(dots)) {
    n_dots <- length(dots)
    if (d <= (1 + n_dots)) abort("incorrect number of dimensions")
    miss_args <- rep(list(missing_arg()), d - 2 - n_dots)
    x <- eval_bare(expr(x[, , !!!c(dots, miss_args), drop = FALSE]))
  }

  # x[i,], x[i,j], x[i,,...], x[i,j,...]
  if (!is_missing(i)) {

    if(is_integerish(i)) {
      i <- vec_cast(i, integer())
    }

    x <- vec_slice(x, i)
  }

  as_rray(x)
}

#' @export
`[[.vctrs_rray` <- function(x, i, j, ..., exact = TRUE) {
  x <- as_array(x)
  dots <- dots_list(..., .preserve_empty = TRUE)

  if (!is_missing(i)) {

    if (!is_missing(j)) {

      if (!is_empty(dots)) {

        eval_bare(expr(x[[i, j, !!! dots]]))

      } else {

        x[[i, j]]

      }
    }
    else {

      x[[i]]

    }

  }
  else {
    # throw right error
    x[[]]
  }

}

#' @export
head.vctrs_rray <- function (x, n = 6L, ...) {

  n_size <- vec_size(n)
  if (vec_size(n) != 1L) {
    glubort("`n` must be size 1, not {n_size}.")
  }

  n <- vec_cast(n, integer())

  x_size <- vec_size(x)

  if (n < 0L) {
    n <- max(x_size + n, 0L)
  }
  else {
    n <- min(n, x_size)
  }

  x[seq_len(n),]
}

#' @export
tail.vctrs_rray <- function(x, n = 6L, ...) {

  n_size <- vec_size(n)
  if (vec_size(n) != 1L) {
    glubort("`n` must be size 1, not {n_size}.")
  }

  n <- vec_cast(n, integer())

  x_size <- vec_size(x)

  if (n < 0L) {
    n <- max(x_size + n, 0L)
  }
  else {
    n <- min(n, x_size)
  }

  x[seq.int(to = x_size, length.out = n),]
}
