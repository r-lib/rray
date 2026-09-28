expected_roll <- function(x, n, axes) {
  n <- rep_len(n, length(axes))
  indices <- lapply(dim(x), seq_len)

  for (i in seq_along(axes)) {
    axis <- axes[[i]]
    dimension <- dim(x)[[axis]]
    indices[[axis]] <- (seq_len(dimension) - 1 - n[[i]]) %% dimension + 1
  }

  do.call(`[`, c(list(x), indices, list(drop = FALSE)))
}
