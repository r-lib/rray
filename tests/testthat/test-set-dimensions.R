test_that("reshapes a vector into a matrix", {
  out <- rray_set_dimensions(1:6, c(2L, 3L))
  expect_identical(rray_dimensions(out), c(2L, 3L))
  expect_identical(out[, 1], 1:2)
  expect_identical(out[, 2], 3:4)
  expect_identical(out[, 3], 5:6)
})

test_that("reshapes a vector into a 3D array", {
  out <- rray_set_dimensions(1:6, c(3L, 2L, 1L))
  expect_identical(rray_dimensions(out), c(3L, 2L, 1L))
  expect_identical(out[, 1, 1], 1:3)
  expect_identical(out[, 2, 1], 4:6)
})

test_that("reshapes a matrix into a different matrix", {
  x <- array(1:6, c(2L, 3L))
  out <- rray_set_dimensions(x, c(3L, 2L))
  expect_identical(rray_dimensions(out), c(3L, 2L))
  expect_identical(as.integer(out), 1:6)
})

test_that("reshapes a matrix into a vector", {
  x <- array(1:6, c(2L, 3L))
  out <- rray_set_dimensions(x, 6L)
  expect_identical(rray_dimensions(out), 6L)
  expect_identical(as.integer(out), 1:6)
})

test_that("returns input unchanged when dimensions already match", {
  x <- array(1:6, c(2L, 3L))
  expect_identical(rray_set_dimensions(x, c(2L, 3L)), x)
})

test_that("turns vectors into arrays even if implied dimensions stay the same", {
  x <- 1:5
  expect_identical(rray_set_dimensions(x, 5L), array(1:5))
})

test_that("works with all atomic types", {
  expect_identical(
    rray_dimensions(rray_set_dimensions(c(TRUE, FALSE), c(1L, 2L))),
    c(1L, 2L)
  )
  expect_identical(
    rray_dimensions(rray_set_dimensions(c(1.5, 2.5), c(1L, 2L))),
    c(1L, 2L)
  )
  expect_identical(
    rray_dimensions(rray_set_dimensions(c(1 + 2i, 3 + 4i), c(1L, 2L))),
    c(1L, 2L)
  )
  expect_identical(
    rray_dimensions(rray_set_dimensions(c("a", "b"), c(1L, 2L))),
    c(1L, 2L)
  )
  expect_identical(
    rray_dimensions(rray_set_dimensions(as.raw(1:2), c(1L, 2L))),
    c(1L, 2L)
  )
})

test_that("works with list arrays", {
  x <- list("a", 1L)
  out <- rray_set_dimensions(x, c(1L, 2L))
  expect_identical(rray_dimensions(out), c(1L, 2L))
})

test_that("works with zero-size arrays", {
  out <- rray_set_dimensions(integer(), c(0L, 5L))
  expect_identical(rray_dimensions(out), c(0L, 5L))
  expect_identical(rray_size(out), 0)
})

test_that("drops dimension names", {
  # There is no meaningful way to keep them
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  out <- rray_set_dimensions(x, c(3L, 2L))
  expect_null(dimnames(out))
})

test_that("drops names from a named vector", {
  x <- c(a = 1L, b = 2L)
  out <- rray_set_dimensions(x, c(1L, 2L))
  expect_null(dimnames(out))
})

test_that("errors when size would change", {
  expect_snapshot(rray_set_dimensions(1:6, c(6L, 2L)), error = TRUE)
})

test_that("errors on non-array input", {
  expect_snapshot(rray_set_dimensions(NULL, 1L), error = TRUE)
  expect_snapshot(rray_set_dimensions(mean, 1L), error = TRUE)
})

test_that("coerces dimensions to integer", {
  out <- rray_set_dimensions(1:6, c(2, 3))
  expect_identical(rray_dimensions(out), c(2L, 3L))

  expect_snapshot(error = TRUE, {
    rray_set_dimensions(1, 2.5)
  })
})

test_that("errors on non-coercible dimensions", {
  expect_snapshot(rray_set_dimensions(1, "a"), error = TRUE)
})

test_that("errors on empty dimensions", {
  expect_snapshot(rray_set_dimensions(1, integer()), error = TRUE)
})

test_that("errors on missing dimensions", {
  expect_snapshot(rray_set_dimensions(1, NA_integer_), error = TRUE)
})

test_that("errors on negative dimensions", {
  expect_snapshot(rray_set_dimensions(1, -1L), error = TRUE)
})
