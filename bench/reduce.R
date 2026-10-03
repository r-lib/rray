devtools::load_all(quiet = TRUE)

iterations <- as.integer(Sys.getenv("RRAY_BENCH_ITERATIONS", "20"))
output <- Sys.getenv("RRAY_BENCH_OUTPUT", "")

operations <- list(
  sum = list(fn = rray_sum, types = c("integer", "double")),
  prod = list(fn = rray_prod, types = c("integer", "double")),
  max = list(fn = rray_max, types = c("integer", "double")),
  max_na_rm = list(
    fn = \(x, axes) rray_max(x, axes, na_rm = TRUE),
    types = c("integer", "double")
  ),
  min = list(fn = rray_min, types = c("integer", "double")),
  all = list(fn = rray_all, types = "logical"),
  any = list(fn = rray_any, types = "logical"),
  mean = list(fn = rray_mean, types = c("logical", "integer", "double")),
  mean_na_rm = list(
    fn = \(x, axes) rray_mean(x, axes, na_rm = TRUE),
    types = c("logical", "integer", "double")
  )
)

shapes <- list(
  "[4000, 4000] over 1" = list(c(4000L, 4000L), 1L),
  "[4000, 4000] over 2" = list(c(4000L, 4000L), 2L),
  "[4000, 4000] over 1, 2" = list(c(4000L, 4000L), 1:2),
  "[200, 200, 200] over 1" = list(c(200L, 200L, 200L), 1L),
  "[200, 200, 200] over 2" = list(c(200L, 200L, 200L), 2L),
  "[200, 200, 200] over 3" = list(c(200L, 200L, 200L), 3L),
  "[200, 200, 200] over 1, 3" = list(c(200L, 200L, 200L), c(1L, 3L)),
  "[10, 1e6] over 1" = list(c(10L, 1000000L), 1L),
  "[10, 1e6] over 2" = list(c(10L, 1000000L), 2L)
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
      axes <- shapes[[shape]][[2L]]

      measurement <- bench::mark(
        fn(x, axes),
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
