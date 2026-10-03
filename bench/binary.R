devtools::load_all(quiet = TRUE)

iterations <- as.integer(Sys.getenv("RRAY_BENCH_ITERATIONS", "20"))
output <- Sys.getenv("RRAY_BENCH_OUTPUT", "")

operations <- list(
  greater_than = list(fn = rray_greater_than, types = c("integer", "double")),
  equal = list(fn = rray_equal, types = c("integer", "double")),
  pmax = list(fn = rray_pmax, types = c("integer", "double")),
  pmax_na_rm = list(
    fn = \(x, y) rray_pmax(x, y, na_rm = TRUE),
    types = c("integer", "double")
  ),
  and = list(fn = rray_and, types = "logical")
)

shapes <- list(
  "[4000, 4000] [4000, 4000]" = list(c(4000L, 4000L), c(4000L, 4000L)),
  "[4000, 4000] [4000, 1]" = list(c(4000L, 4000L), c(4000L, 1L)),
  "[4000, 4000] [1, 4000]" = list(c(4000L, 4000L), c(1L, 4000L)),
  "[4000, 4000] [1, 1]" = list(c(4000L, 4000L), c(1L, 1L)),
  "[4000, 1] [1, 4000]" = list(c(4000L, 1L), c(1L, 4000L)),
  "[10, 1e6] [10, 1]" = list(c(10L, 1000000L), c(10L, 1L)),
  "[10, 1e6] [1, 1e6]" = list(c(10L, 1000000L), c(1L, 1000000L))
)

make_array <- function(dimensions, type) {
  n <- prod(dimensions)

  values <- switch(
    type,
    logical = sample(c(TRUE, FALSE), n, replace = TRUE),
    integer = sample(100L, n, replace = TRUE),
    double = runif(n)
  )

  array(values, dimensions)
}

set.seed(1)

results <- list()

for (operation in names(operations)) {
  fn <- operations[[operation]]$fn

  for (type in operations[[operation]]$types) {
    for (shape in names(shapes)) {
      x <- make_array(shapes[[shape]][[1L]], type)
      y <- make_array(shapes[[shape]][[2L]], type)

      measurement <- bench::mark(
        fn(x, y),
        iterations = iterations,
        check = FALSE,
        memory = FALSE,
        filter_gc = FALSE
      )

      results[[length(results) + 1L]] <- data.frame(
        operation = operation,
        type = type,
        shape = shape,
        median_ms = as.numeric(measurement$median) * 1000
      )
    }
  }
}

results <- do.call(rbind, results)

print(results, row.names = FALSE)

if (nzchar(output)) {
  saveRDS(
    list(
      results = results,
      iterations = iterations,
      revision = system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
    ),
    output
  )
}
