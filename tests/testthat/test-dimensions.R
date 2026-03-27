test_that("returns length for non-arrays", {
  expect_identical(rray_dimensions(1), 1L)
  expect_identical(rray_dimensions(1:5), 5L)
  expect_identical(rray_dimensions("a"), 1L)
  expect_identical(rray_dimensions(TRUE), 1L)
})

test_that("returns dim for arrays", {
  expect_identical(rray_dimensions(array(1, 2)), 2L)
  expect_identical(rray_dimensions(array(1, c(2, 3))), c(2L, 3L))
  expect_identical(rray_dimensions(array(1, c(2, 3, 4))), c(2L, 3L, 4L))
})

test_that("returns dim for matrices", {
  expect_identical(rray_dimensions(matrix(1, 2, 3)), c(2L, 3L))
})

test_that("returns length for empty vectors", {
  expect_identical(rray_dimensions(integer()), 0L)
  expect_identical(rray_dimensions(character()), 0L)
})

test_that("errors on NULL", {
  expect_snapshot(rray_dimensions(NULL), error = TRUE)
})

test_that("errors on non-vector types", {
  expect_snapshot(rray_dimensions(mean), error = TRUE)
  expect_snapshot(rray_dimensions(quote(x)), error = TRUE)
  expect_snapshot(rray_dimensions(environment()), error = TRUE)
})

test_that("common dimensions of identical inputs", {
  expect_identical(
    rray_dimensions_common(1:5, 1:5),
    5L
  )
  expect_identical(
    rray_dimensions_common(array(1, c(2, 3)), array(1, c(2, 3))),
    c(2L, 3L)
  )
})

test_that("dimension of 1 is broadcast to the other", {
  expect_identical(
    rray_dimensions_common(array(1, c(1, 3)), array(1, c(2, 1))),
    c(2L, 3L)
  )
  expect_identical(
    rray_dimensions_common(array(1, c(1, 3)), array(1, c(2, 3))),
    c(2L, 3L)
  )
})

test_that("dimensionality is extended", {
  expect_identical(
    rray_dimensions_common(1:5, array(1, c(5, 3))),
    c(5L, 3L)
  )
  expect_identical(
    rray_dimensions_common(array(1, c(2, 3)), array(1, c(2, 3, 4))),
    c(2L, 3L, 4L)
  )
})

test_that("NULL inputs are dropped", {
  expect_identical(
    rray_dimensions_common(NULL, 1:5, NULL),
    5L
  )
})

test_that("single input works", {
  expect_identical(rray_dimensions_common(1:3), 3L)
  expect_identical(
    rray_dimensions_common(array(1, c(2, 3))),
    c(2L, 3L)
  )
})

test_that("more than two inputs work", {
  expect_identical(
    rray_dimensions_common(
      array(1, c(1, 3)),
      array(1, c(2, 1)),
      array(1, c(1, 1, 4))
    ),
    c(2L, 3L, 4L)
  )
})

test_that("`.dimensions` overrides", {
  expect_identical(
    rray_dimensions_common(1:5, .dimensions = c(5L, 3L)),
    c(5L, 3L)
  )
})

test_that("errors on incompatible dimensions", {
  expect_snapshot(
    rray_dimensions_common(array(1, c(2, 3)), array(1, c(4, 3))),
    error = TRUE
  )
})

test_that("errors on zero non-NULL inputs", {
  expect_snapshot(rray_dimensions_common(), error = TRUE)
  expect_snapshot(rray_dimensions_common(NULL, NULL), error = TRUE)
})
