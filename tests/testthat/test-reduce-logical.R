test_that("can reduce along axis 1", {
  x <- array(c(TRUE, TRUE, FALSE, TRUE), c(2L, 2L))
  expect_identical(as.vector(rray_all(x, 1L)), c(TRUE, FALSE))
  expect_identical(as.vector(rray_any(x, 1L)), c(TRUE, TRUE))
})

test_that("can reduce along axis 2", {
  x <- array(c(TRUE, TRUE, FALSE, TRUE), c(2L, 2L))
  expect_identical(as.vector(rray_all(x, 2L)), c(FALSE, TRUE))
  expect_identical(as.vector(rray_any(x, 2L)), c(TRUE, TRUE))
})

test_that("reduced axes become size 1", {
  x <- array(TRUE, c(5L, 2L))
  expect_identical(rray_dimensions(rray_all(x, 1L)), c(1L, 2L))
  expect_identical(rray_dimensions(rray_any(x, 2L)), c(5L, 1L))
})

test_that("can reduce over all axes", {
  x <- array(c(TRUE, FALSE, TRUE, TRUE), c(2L, 2L))

  out <- rray_all(x, c(1L, 2L))
  expect_identical(as.vector(out), FALSE)
  expect_identical(rray_dimensions(out), c(1L, 1L))

  out <- rray_any(x, c(1L, 2L))
  expect_identical(as.vector(out), TRUE)
  expect_identical(rray_dimensions(out), c(1L, 1L))
})

test_that("reducing over no axes returns the input unchanged", {
  x <- array(c(TRUE, FALSE, NA, TRUE), c(2L, 2L))
  expect_identical(rray_all(x, integer()), x)
  expect_identical(rray_any(x, integer()), x)
})

test_that("can reduce axis 3", {
  x <- array(c(TRUE, FALSE), c(2L, 3L, 4L))
  expect_identical(rray_dimensions(rray_all(x, 3L)), c(2L, 3L, 1L))
  expect_identical(as.vector(rray_all(x, 3L)), rep(c(TRUE, FALSE), 3L))
  expect_identical(as.vector(rray_any(x, 3L)), rep(c(TRUE, FALSE), 3L))
})

test_that("coalesces reduction axes", {
  x <- array(rep(c(TRUE, FALSE, NA), 8L), c(1L, 3L, 2L, 4L))

  expected <- function(fn, axes) {
    dimensions <- dim(x)
    dimensions[axes] <- 1L
    kept_axes <- setdiff(seq_along(dimensions), axes)

    values <- if (length(kept_axes)) {
      apply(x, kept_axes, fn)
    } else {
      fn(x)
    }

    array(values, dimensions)
  }

  axes <- list(1L, 2L, 3L, 4L, c(1L, 3L), c(2L, 4L), 1:4)

  for (axis in axes) {
    expect_identical(rray_all(x, axis), expected(all, axis))
    expect_identical(rray_any(x, axis), expected(any, axis))
  }
})

test_that("dimension names are kept for non-reduced axes", {
  x <- array(TRUE, c(5L, 2L), dimnames = list(letters[1:5], c("c1", "c2")))
  expect_identical(dimnames(rray_all(x, 1L)), list(NULL, c("c1", "c2")))
  expect_identical(dimnames(rray_any(x, 2L)), list(letters[1:5], NULL))
})

test_that("dimension names are dropped when all named axes are reduced", {
  x <- array(TRUE, c(5L, 2L), dimnames = list(letters[1:5], NULL))
  expect_null(dimnames(rray_all(x, 1L)))
  expect_null(dimnames(rray_any(x, 1L)))
})

test_that("output type is always logical", {
  expect_identical(storage.mode(rray_all(array(TRUE), 1L)), "logical")
  expect_identical(storage.mode(rray_any(array(TRUE), 1L)), "logical")
})

test_that("missing values follow `all()` and `any()`", {
  expect_identical(as.vector(rray_all(c(TRUE, NA), 1L)), NA)
  expect_identical(as.vector(rray_all(c(NA, TRUE), 1L)), NA)
  expect_identical(as.vector(rray_all(c(FALSE, NA), 1L)), FALSE)
  expect_identical(as.vector(rray_all(c(NA, FALSE), 1L)), FALSE)
  expect_identical(as.vector(rray_all(c(NA, NA), 1L)), NA)

  expect_identical(as.vector(rray_any(c(TRUE, NA), 1L)), TRUE)
  expect_identical(as.vector(rray_any(c(NA, TRUE), 1L)), TRUE)
  expect_identical(as.vector(rray_any(c(FALSE, NA), 1L)), NA)
  expect_identical(as.vector(rray_any(c(NA, FALSE), 1L)), NA)
  expect_identical(as.vector(rray_any(c(NA, NA), 1L)), NA)
})

test_that("na_rm removes missing values", {
  expect_identical(
    as.vector(rray_all(c(TRUE, NA), 1L, na_rm = TRUE)),
    TRUE
  )
  expect_identical(
    as.vector(rray_all(c(FALSE, NA), 1L, na_rm = TRUE)),
    FALSE
  )
  expect_identical(
    as.vector(rray_any(c(TRUE, NA), 1L, na_rm = TRUE)),
    TRUE
  )
  expect_identical(
    as.vector(rray_any(c(FALSE, NA), 1L, na_rm = TRUE)),
    FALSE
  )
})

test_that("na_rm with all missing returns identity", {
  x <- c(NA, NA)
  expect_identical(as.vector(rray_all(x, 1L, na_rm = TRUE)), TRUE)
  expect_identical(as.vector(rray_any(x, 1L, na_rm = TRUE)), FALSE)
})

test_that("na_rm with no missing values matches default", {
  x <- array(c(TRUE, FALSE, TRUE, TRUE), c(2L, 2L))
  expect_identical(rray_all(x, 1L, na_rm = TRUE), rray_all(x, 1L))
  expect_identical(rray_any(x, 1L, na_rm = TRUE), rray_any(x, 1L))
})

test_that("plain vector input works", {
  out <- rray_all(c(TRUE, TRUE), 1L)
  expect_identical(as.vector(out), TRUE)
  expect_identical(rray_dimensions(out), 1L)
})

test_that("axes are coerced to integer", {
  x <- array(c(TRUE, FALSE, TRUE, TRUE), c(2L, 2L))
  expect_identical(rray_all(x, 1), rray_all(x, 1L))
  expect_identical(rray_any(x, 1), rray_any(x, 1L))
})

test_that("reducing a zero-length axis gives the identity value", {
  x <- matrix(logical(), 0L, 2L)

  out <- rray_all(x, 1L)
  expect_identical(rray_dimensions(out), c(1L, 2L))
  expect_identical(as.vector(out), c(TRUE, TRUE))

  out <- rray_any(x, 1L)
  expect_identical(rray_dimensions(out), c(1L, 2L))
  expect_identical(as.vector(out), c(FALSE, FALSE))
})

test_that("reducing over a non-zero-length axis with a zero-length axis", {
  x <- matrix(logical(), 0L, 2L)
  out <- rray_all(x, 2L)
  expect_identical(rray_dimensions(out), c(0L, 1L))
  expect_identical(as.vector(out), logical())
})

test_that("reducing all axes of a zero-length array", {
  x <- matrix(logical(), 0L, 2L)
  expect_identical(as.vector(rray_all(x, c(1L, 2L))), TRUE)
  expect_identical(as.vector(rray_any(x, c(1L, 2L))), FALSE)
})

test_that("errors on axes out of range", {
  x <- array(TRUE, c(2L, 2L))
  expect_snapshot(rray_all(x, 3L), error = TRUE)
  expect_snapshot(rray_any(x, 3L), error = TRUE)
})

test_that("`na_rm` must be `TRUE` or `FALSE`", {
  x <- array(TRUE, c(2L, 2L))
  expect_snapshot(rray_all(x, 1L, na_rm = NA), error = TRUE)
  expect_snapshot(rray_any(x, 1L, na_rm = 1), error = TRUE)
})

test_that("errors on non-logical input", {
  expect_snapshot(rray_all(array(1L, c(2L, 2L)), 1L), error = TRUE)
  expect_snapshot(rray_all(array(1, c(2L, 2L)), 1L), error = TRUE)
  expect_snapshot(rray_all(array(1i, c(2L, 2L)), 1L), error = TRUE)
  expect_snapshot(rray_all(array("a", c(2L, 2L)), 1L), error = TRUE)
  expect_snapshot(
    rray_all(array(as.raw(1:4), c(2L, 2L)), 1L),
    error = TRUE
  )
  expect_snapshot(
    rray_all(array(list(1, 2, 3, 4), c(2L, 2L)), 1L),
    error = TRUE
  )
  expect_snapshot(rray_any(array(1L, c(2L, 2L)), 1L), error = TRUE)
})

test_that("errors on scalar input", {
  expect_snapshot(rray_all(quote(x), 1L), error = TRUE)
  expect_snapshot(rray_any(quote(x), 1L), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(array(TRUE, c(2L, 2L)), class = "foo")
  expect_snapshot(rray_all(x, 1L), error = TRUE)
  expect_snapshot(rray_any(x, 1L), error = TRUE)
})
