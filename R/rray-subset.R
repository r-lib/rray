#' @export
`[.vctrs_rray` <- function(x, i, j, ..., drop = FALSE) {
  x <- as_array(x)
  d <- vec_dims(x)

  # for differentiating x[1] from x[1,]
  n_real_args <- nargs() - !missing(drop)

  dots <- dots_list(..., .preserve_empty = TRUE)

  if (!missing(drop)) {
    rlang::warn("`drop` is ignored.")
  }

  # x[i], or just x[]
  if (n_real_args <= 2) {

    # x[i]
    if (!is_missing(i)) {

      if(is_integerish(i)) {
        i <- vec_cast(i, integer())
      }

      miss_args <- rep(list(missing_arg()), d - 2)
      x <- eval_bare(expr(x[, i, !!!miss_args, drop = FALSE]))

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
    miss_args <- rep(list(missing_arg()), d - 2)
    x <- eval_bare(expr(x[, j, !!!miss_args, drop = FALSE]))
  }

  # x[i,j,...], x[i,,...], x[,,...], x[,j,...]
  if (!is_empty(dots)) {
    miss_args <- rep(list(missing_arg()), d - 2 - length(dots))
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

  if (!is_true(exact)) {
    rlang::warn("`exact` is ignored.")
  }

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
