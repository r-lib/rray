expected_split <- function(x, axis, dimensions) {
  x_dimensions <- dim(x)

  if (length(dimensions) == 1L) {
    dimensions <- rep(dimensions, x_dimensions[[axis]] %/% dimensions)
  }

  starts <- cumsum(dimensions) - dimensions

  lapply(seq_along(dimensions), function(i) {
    indices <- lapply(x_dimensions, seq_len)
    indices[[axis]] <- starts[[i]] + seq_len(dimensions[[i]])
    do.call(`[`, c(list(x), indices, list(drop = FALSE)))
  })
}
