#' @export
`[.vctrs_mtrx` <- function(x, i, j, drop = FALSE) {
  x <- as_matrix(x)

  # for differentiating x[1] from x[1,]
  n_real_args <- nargs() - !missing(drop)

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

      x <- x[, i, drop = FALSE]

    }
    # x[]
    else {
      # nothing
    }

    return(as_mtrx(x))
  }

  # at this point one of these is returned:
  # x[i,] x[,j] or x[i,j]

  # x[,j] or x[i,j]
  if (!is_missing(j)) {
    x <- x[, j, drop = FALSE]
  }

  # x[i,] or x[i,j]
  if (!is_missing(i)) {

    if(is_integerish(i)) {
      i <- vec_cast(i, integer())
    }

    x <- vec_slice(x, i)
  }

  as_mtrx(x)
}

#' @export
`[[.vctrs_mtrx` <- function(x, i, j, exact = TRUE) {
  x <- as_matrix(x)

  if (!is_true(exact)) {
    rlang::warn("`exact` is ignored.")
  }

  if (!is_missing(i)) {

    if (!is_missing(j)) {
      x[[i, j]]
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
