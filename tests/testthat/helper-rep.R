expected_rep <- function(x, times, axes) {
  times <- rep_len(times, length(axes))
  indices <- lapply(dim(x), seq_len)

  for (i in seq_along(axes)) {
    axis <- axes[[i]]
    indices[[axis]] <- rep.int(indices[[axis]], times[[i]])
  }

  do.call(`[`, c(list(x), indices, list(drop = FALSE)))
}

expected_rep_each <- function(x, times, axis) {
  dimension <- dim(x)[[axis]]

  indices <- lapply(dim(x), seq_len)
  indices[[axis]] <- rep(seq_len(dimension), times = rep_len(times, dimension))

  do.call(`[`, c(list(x), indices, list(drop = FALSE)))
}
