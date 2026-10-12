devtools::load_all(quiet = TRUE)

iterations <- as.integer(Sys.getenv("RRAY_BENCH_ITERATIONS", "20"))
output <- Sys.getenv("RRAY_BENCH_OUTPUT", "")

operations <- list(
  all = list(fn = rray_cumulative_all, types = "logical"),
  any = list(fn = rray_cumulative_any, types = "logical"),
  max = list(
    fn = rray_cumulative_max,
    types = c("logical", "integer", "double")
  ),
  min = list(
    fn = rray_cumulative_min,
    types = c("logical", "integer", "double")
  ),
  prod = list(
    fn = rray_cumulative_prod,
    types = c("logical", "integer", "double", "complex")
  )
)

shapes <- list(
  "[1.6e7] over 1" = list(16000000L, 1L),
  "[4000, 4000] over 1" = list(c(4000L, 4000L), 1L),
  "[4000, 4000] over 2" = list(c(4000L, 4000L), 2L),
  "[200, 200, 200] over 1" = list(c(200L, 200L, 200L), 1L),
  "[200, 200, 200] over 2" = list(c(200L, 200L, 200L), 2L),
  "[200, 200, 200] over 3" = list(c(200L, 200L, 200L), 3L),
  "[10, 1e6] over 1" = list(c(10L, 1000000L), 1L),
  "[10, 1e6] over 2" = list(c(10L, 1000000L), 2L)
)

make_array <- function(dimensions, type) {
  n <- prod(dimensions)

  values <- switch(
    type,
    logical = sample(c(TRUE, FALSE, NA), n, replace = TRUE),
    integer = sample(100L, n, replace = TRUE),
    double = runif(n),
    complex = complex(real = runif(n), imaginary = runif(n))
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
      axis <- shapes[[shape]][[2L]]

      measurement <- bench::mark(
        fn(x, axis),
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
