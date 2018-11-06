validate_reshape <- function(from, to) {

  validate_dim(to)

  size_from <- prod(from)
  size_to   <- prod(to)

  if (size_from != size_to) {
    glubort(
      "The size you are reshaping from ({size_from}) ",
      "must be equal to the size you are reshaping to ({size_to})."
    )
  }
}

validate_dim <- function(dim) {
  if (!is_integerish(dim)) {
    abort("`dim` must be an integer vector.")
  }

  if (!all(dim > 0L)) {
    abort("`dim` must be a positive vector.")
  }
}
