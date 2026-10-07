locate_oracle <- function(x, axis, fn, na_rm) {
  lane <- function(values) {
    if (!na_rm && anyNA(values)) {
      return(NA_integer_)
    }

    out <- fn(values)
    if (length(out)) out else NA_integer_
  }

  dimensions <- dim(x)
  out_dimensions <- dimensions
  out_dimensions[[axis]] <- 1L
  margins <- setdiff(seq_along(dimensions), axis)

  if (length(margins) == 0L) {
    return(array(lane(x), out_dimensions))
  }

  array(apply(x, margins, lane), out_dimensions)
}
