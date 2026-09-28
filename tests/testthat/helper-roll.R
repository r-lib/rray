expected_roll_each <- function(x, n, axis) {
  dimensions <- dim(x)
  dimensionality <- length(dimensions)
  dimension <- dimensions[[axis]]

  n_dimensions <- dim(as.array(n))
  n_dimensions <- c(
    n_dimensions,
    rep(1L, dimensionality - length(n_dimensions))
  )
  n <- array(n, n_dimensions)

  out <- x
  points <- arrayInd(seq_along(x), dimensions)

  for (i in seq_len(nrow(points))) {
    point <- points[i, ]

    n_point <- ifelse(n_dimensions == 1L, 1L, point)
    shift <- n[matrix(n_point, nrow = 1)]

    source <- point
    source[[axis]] <- (point[[axis]] - 1 - shift) %% dimension + 1

    out[matrix(point, nrow = 1)] <- x[matrix(source, nrow = 1)]
  }

  names <- dimnames(out)

  if (!is.null(names)) {
    names[axis] <- list(NULL)
    dimnames(out) <- names
  }

  out
}
