slice_range <- function(start, stop) {
  start <- vec_cast(start, integer())
  stop <- vec_cast(stop, integer())

  if (start > stop) {
    abort("`start` must be less than or equal to `stop`.")
  }

  if (start < 1L) {
    abort("`start` must be greater than or equal to 1.")
  }

  new_slice_range(start, stop)
}

new_slice_range <- function(start, stop) {

  if (!is.integer(start)) {
    abort("`start` must be a single integer.")
  }

  if (!length(start) == 1L) {
    abort("`start` must be a single integer.")
  }

  if (!is.integer(stop)) {
    abort("`stop` must be a single integer.")
  }

  if (!length(stop) == 1L) {
    abort("`stop` must be a single integer.")
  }

  new_rcrd(list(start = start, stop = stop), class = "vctrs_slice_range")
}

format.vctrs_slice_range <- function(x, ...) {
  as.character(glue::glue(
    "[{field(x, 'start')}, {field(x, 'stop')}]"
  ))
}

is_slice_range <- function(x) {
  inherits(x, "vctrs_slice_range")
}
