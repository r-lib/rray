context("test-math-error-gamma")

.fs <- c(
  gamma,
  lgamma,
  digamma,
  trigamma
)

.f_names <- c(
  "gamma",
  "lgamma",
  "digamma",
  "trigamma"
)

for (i in seq_along(.fs)) {
  .f <- .fs[[i]]
  .f_name <- .f_names[[i]]

  test_that(glue::glue("vctrs dispatch works - {.f_name}"), {
    expect_equal(.f(rray(1)), rray(.f(1)))
    expect_equal(.f(rray(1L)), rray(.f(1L)))
  })
}

