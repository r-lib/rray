stack_slice <- function(x, axis, i) {
  index <- lapply(dim(x), seq_len)
  index[[axis]] <- i
  x <- do.call(`[`, c(list(x), index, list(drop = FALSE)))
  rray_remove_axes(x, axis)
}
