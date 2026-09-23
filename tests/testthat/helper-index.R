index_base <- function(x, ...) {
  indices <- list(...)
  dimensions <- do.call(rray_dimensions_common, indices)
  indices <- lapply(indices, rray_broadcast, dimensions = dimensions)
  size <- prod(dimensions)
  out <- x[rep(NA_integer_, size)]

  for (i in seq_len(size)) {
    point <- vapply(indices, `[[`, integer(1), i)

    if (!anyNA(point)) {
      value <- do.call(`[`, c(list(x), as.list(point), list(drop = FALSE)))
      out[i] <- value
    }
  }

  dim(out) <- dimensions
  dimnames(out) <- NULL
  out
}
