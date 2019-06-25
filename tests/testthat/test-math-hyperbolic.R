context("test-math-hyperbolic")

.fs <- c(
  sinh,
  cosh,
  tanh,
  asinh,
  acosh,
  atanh
)

.f_names <- c(
  "sinh",
  "cosh",
  "tanh",
  "asinh",
  "acosh",
  "atanh"
)

for (i in seq_along(.fs)) {
  .f <- .fs[[i]]
  .f_name <- .f_names[[i]]

  test_that(glue::glue("vctrs dispatch works - {.f_name}"), {
    expect_equal(.f(rray(1)), rray(.f(1)))
  })
}
