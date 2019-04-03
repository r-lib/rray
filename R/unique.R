rray_unique <- function(x, axis = 1L) {

  # TODO - what here?
  if (is.null(axis)) {
    x <- as.vector(x)
    return(vec_unique(x))
  }

  x_split <- rray_split(x, axes = axis)
  x_unique <- vec_unique(x_split)
  rray_bind(!!!x_unique, axis = axis)
}

rray_unique_loc <- function(x, axis = 1L) {

  # TODO - what here?
  if (is.null(axis)) {
    x <- as.vector(x)
    return(vec_unique_loc(x))
  }

  x_split <- rray_split(x, axes = axis)
  vec_unique_loc(x_split)
}

# TODO - axis = NULL case?
