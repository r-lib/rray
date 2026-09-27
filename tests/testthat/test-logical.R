test_that("applies logical operations elementwise", {
  x <- array(c(TRUE, TRUE, FALSE, FALSE), c(2L, 2L))
  y <- array(c(TRUE, FALSE, TRUE, FALSE), c(2L, 2L))

  expect_identical(rray_and(x, y), x & y)
  expect_identical(rray_or(x, y), x | y)
  expect_identical(rray_xor(x, y), xor(x, y))
})

test_that("broadcasts both inputs to common dimensions", {
  x <- array(c(TRUE, FALSE, NA), c(3L, 1L))
  y <- array(c(TRUE, FALSE), c(1L, 2L))

  expected_x <- x[, rep(1L, 2L), drop = FALSE]
  expected_y <- y[rep(1L, 3L), , drop = FALSE]

  expect_identical(rray_and(x, y), expected_x & expected_y)
  expect_identical(rray_or(x, y), expected_x | expected_y)
  expect_identical(rray_xor(x, y), xor(expected_x, expected_y))
})

test_that("works with 1D and 3D arrays", {
  expect_identical(
    rray_and(c(TRUE, FALSE, NA), TRUE),
    array(c(TRUE, FALSE, NA), 3L)
  )

  x <- array(c(TRUE, FALSE, NA), c(2L, 3L, 4L))
  y <- array(c(FALSE, TRUE), c(2L, 1L, 1L))
  expected_y <- y[, rep(1L, 3L), rep(1L, 4L), drop = FALSE]

  expect_identical(rray_or(x, y), x | expected_y)
})

test_that("returns logical output for logical input only", {
  expect_snapshot(native_ptype_matrix(rray_and, c("x", "y")))
})

test_that("missing values match base R", {
  values <- c(NA, FALSE, TRUE)
  x <- rep(values, each = length(values))
  y <- rep(values, times = length(values))

  expect_identical(as.vector(rray_and(x, y)), x & y)
  expect_identical(as.vector(rray_or(x, y)), x | y)
  expect_identical(as.vector(rray_xor(x, y)), xor(x, y))
})

test_that("coalesces names across inputs", {
  x <- array(
    TRUE,
    c(3L, 1L),
    dimnames = list(c("r1", "r2", "r3"), "x")
  )
  y <- array(
    FALSE,
    c(1L, 2L),
    dimnames = list("y", c("c1", "c2"))
  )

  expect_identical(
    rray_names(rray_and(x, y)),
    list(c("r1", "r2", "r3"), c("c1", "c2"))
  )

  x <- array(TRUE, 3L, dimnames = list(c("a", "b", "c")))
  y <- array(TRUE, 3L, dimnames = list(c("x", "y", "z")))
  expect_identical(
    rray_names(rray_xor(x, y)),
    list(c("a", "b", "c"))
  )
})

test_that("zero dimensions broadcast against dimensions of 1", {
  x <- array(TRUE, c(1L, 2L))

  expect_identical(rray_and(logical(), x), array(logical(), c(0L, 2L)))
  expect_identical(
    rray_or(x, array(logical(), c(0L, 1L, 2L))),
    array(logical(), c(0L, 2L, 2L))
  )
})

test_that("errors on incompatible dimensions", {
  x <- array(TRUE, c(1L, 2L))
  y <- array(logical(), c(1L, 0L))
  expect_snapshot(rray_and(x, y), error = TRUE)
})

test_that("errors on non-logical input", {
  expect_snapshot(rray_and(1L, TRUE), error = TRUE)
  expect_snapshot(rray_or(TRUE, 1), error = TRUE)
  expect_snapshot(rray_xor(array("a", c(2L, 2L)), TRUE), error = TRUE)
})

test_that("checks `x` before `y`", {
  expect_snapshot(rray_and(1L, 1), error = TRUE)
})

test_that("a type error beats a dimension error", {
  x <- array(1L, c(2L, 2L))
  y <- array(TRUE, c(3L, 3L))
  expect_snapshot(rray_and(x, y), error = TRUE)
})

test_that("errors on scalar and classed input", {
  expect_snapshot(rray_and(NULL, TRUE), error = TRUE)
  expect_snapshot(rray_or(TRUE, NULL), error = TRUE)

  x <- structure(TRUE, class = "foo")
  expect_snapshot(rray_xor(x, TRUE), error = TRUE)
  expect_snapshot(rray_and(TRUE, x), error = TRUE)
})
