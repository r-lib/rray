expected_split <- function(x, axis, sizes) {
  dimensions <- dim(x)

  if (length(sizes) == 1L) {
    sizes <- rep(sizes, dimensions[[axis]] %/% sizes)
  }

  starts <- cumsum(sizes) - sizes

  lapply(seq_along(sizes), function(i) {
    indices <- lapply(dimensions, seq_len)
    indices[[axis]] <- starts[[i]] + seq_len(sizes[[i]])
    do.call(`[`, c(list(x), indices, list(drop = FALSE)))
  })
}
