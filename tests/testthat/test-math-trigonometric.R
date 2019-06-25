context("test-math-trigonometric")

.fs <- c(
  sin,
  cos,
  tan,
  asin,
  acos,
  atan,
  sinpi,
  cospi,
  tanpi
)

.f_names <- c(
  "sin",
  "cos",
  "tan",
  "asin",
  "acos",
  "atan",
  "sinpi",
  "cospi",
  "tanpi"
)

for (i in seq_along(.fs)) {
  .f <- .fs[[i]]
  .f_name <- .f_names[[i]]

  test_that(glue::glue("vctrs dispatch works - {.f_name}"), {
    expect_equal(.f(rray(1)), rray(.f(1)))
    expect_equal(.f(rray(1L)), rray(.f(1L)))
  })
}

# ------------------------------------------------------------------------------
# tests for atan2()

test_that(glue::glue("atan2() keeps class of `y` (first argument)"), {
  expect_equal(atan2(rray(1), 2), rray(atan2(1, 2)))
  expect_equal(atan2(1, rray(2)), atan2(1, 2))
})
