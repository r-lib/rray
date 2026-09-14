test_that("can take the mean along axis 1", {
  x <- array(1:10, c(5L, 2L))
  out <- rray_mean_along(x, 1L)
  expect_identical(as.vector(out), c(3, 8))
})

test_that("can take the mean along axis 2", {
  x <- array(1:10, c(5L, 2L))
  out <- rray_mean_along(x, 2L)
  expect_identical(as.vector(out), c(3.5, 4.5, 5.5, 6.5, 7.5))
})

test_that("reduced axes become size 1", {
  x <- array(1:10, c(5L, 2L))
  expect_identical(rray_dimensions(rray_mean_along(x, 1L)), c(1L, 2L))
  expect_identical(rray_dimensions(rray_mean_along(x, 2L)), c(5L, 1L))
})

test_that("can reduce over all axes", {
  x <- array(1:10, c(5L, 2L))
  out <- rray_mean_along(x, c(1L, 2L))
  expect_identical(as.vector(out), 5.5)
  expect_identical(rray_dimensions(out), c(1L, 1L))
})

test_that("reducing over no axes returns the input as a double", {
  x <- array(c(1.5, 2.5, 3.5, 4.5), c(2L, 2L))
  expect_identical(rray_mean_along(x, integer()), x)

  x <- array(1:4, c(2L, 2L))
  expect_identical(
    rray_mean_along(x, integer()),
    array(as.double(1:4), c(2L, 2L))
  )
})

test_that("can reduce over multiple axes of a 3D array", {
  x <- array(1, c(2L, 3L, 4L))
  out <- rray_mean_along(x, c(1L, 2L))
  expect_identical(as.vector(out), rep(1, 4L))
  expect_identical(rray_dimensions(out), c(1L, 1L, 4L))
})

test_that("can reduce axis 3", {
  x <- array(1:24, c(2L, 3L, 4L))
  out <- rray_mean_along(x, 3L)
  expect_identical(rray_dimensions(out), c(2L, 3L, 1L))
  expect_identical(as.vector(out), c(10, 11, 12, 13, 14, 15))
})

test_that("matches `mean()` over every combination of axes", {
  x <- array(c(1e16, 1, -3, 7e15, 1, 1), c(1L, 3L, 2L, 4L))

  expected <- function(axes) {
    dimensions <- dim(x)
    dimensions[axes] <- 1L
    kept_axes <- setdiff(seq_along(dimensions), axes)

    values <- if (length(kept_axes)) {
      apply(x, kept_axes, mean)
    } else {
      mean(x)
    }

    array(values, dimensions)
  }

  axes <- list(1L, 2L, 3L, 4L, c(1L, 3L), c(2L, 4L), 1:4)

  for (axis in axes) {
    expect_identical(rray_mean_along(x, axis), expected(axis))
  }
})

test_that("dimension names are kept for non-reduced axes", {
  x <- array(1:10, c(5L, 2L), dimnames = list(letters[1:5], c("c1", "c2")))
  expect_identical(dimnames(rray_mean_along(x, 1L)), list(NULL, c("c1", "c2")))
  expect_identical(dimnames(rray_mean_along(x, 2L)), list(letters[1:5], NULL))
})

test_that("dimension names are dropped when all named axes are reduced", {
  x <- array(1:10, c(5L, 2L), dimnames = list(letters[1:5], NULL))
  expect_null(dimnames(rray_mean_along(x, 1L)))
})

test_that("output type is always double", {
  expect_identical(storage.mode(rray_mean_along(array(TRUE), 1L)), "double")
  expect_identical(storage.mode(rray_mean_along(array(1L), 1L)), "double")
  expect_identical(storage.mode(rray_mean_along(array(1), 1L)), "double")
})

test_that("logical TRUE is averaged as 1", {
  x <- array(c(TRUE, FALSE, TRUE, TRUE), c(2L, 2L))
  out <- rray_mean_along(x, 1L)
  expect_identical(as.vector(out), c(0.5, 1))
})

test_that("a second pass corrects the rounding error of the first sum", {
  x <- c(1e16, 1, 1, 1)
  expect_identical(as.vector(rray_mean_along(x, 1L)), mean(x))
  expect_identical(as.vector(rray_mean_along(x, 1L)), 2500000000000001)
})

test_that("a sum that overflows to infinity is retried with scaled terms", {
  x <- c(1e308, 1e308, 1e308)
  expect_identical(as.vector(rray_mean_along(x, 1L)), mean(x))
  expect_identical(as.vector(rray_mean_along(x, 1L)), 1e308)
})

test_that("integer NA propagates", {
  x <- array(c(1L, NA_integer_), c(2L, 1L))
  expect_identical(as.vector(rray_mean_along(x, 1L)), NA_real_)

  x <- array(c(NA_integer_, 1L), c(2L, 1L))
  expect_identical(as.vector(rray_mean_along(x, 1L)), NA_real_)
})

test_that("logical NA propagates", {
  x <- array(c(TRUE, NA), c(2L, 1L))
  expect_identical(as.vector(rray_mean_along(x, 1L)), NA_real_)

  x <- array(c(NA, TRUE), c(2L, 1L))
  expect_identical(as.vector(rray_mean_along(x, 1L)), NA_real_)
})

test_that("double NA / NaN propagates, with NA winning over NaN", {
  x <- c(1, NA_real_)
  expect_identical(as.vector(rray_mean_along(x, 1L)), NA_real_)

  x <- c(1, NaN)
  expect_identical(as.vector(rray_mean_along(x, 1L)), NaN)

  x <- c(NaN, 1)
  expect_identical(as.vector(rray_mean_along(x, 1L)), NaN)

  x <- c(NA, NaN)
  expect_identical(as.vector(rray_mean_along(x, 1L)), NA_real_)

  x <- c(NaN, NA)
  expect_identical(as.vector(rray_mean_along(x, 1L)), NA_real_)
})

test_that("Inf matches base R mean", {
  expect_identical(as.vector(rray_mean_along(c(Inf, 1), 1L)), mean(c(Inf, 1)))
  expect_identical(as.vector(rray_mean_along(c(-Inf, 1), 1L)), mean(c(-Inf, 1)))
  expect_identical(
    as.vector(rray_mean_along(c(Inf, -Inf), 1L)),
    mean(c(Inf, -Inf))
  )
  expect_identical(as.vector(rray_mean_along(c(Inf, NaN), 1L)), NaN)
  expect_identical(as.vector(rray_mean_along(c(Inf, NA), 1L)), NA_real_)
})

test_that("na_rm removes integer NA", {
  x <- array(c(1L, NA_integer_, 3L, 5L), c(2L, 2L))
  out <- rray_mean_along(x, 1L, na_rm = TRUE)
  expect_identical(as.vector(out), c(1, 4))
})

test_that("na_rm removes logical NA", {
  x <- array(c(TRUE, NA, FALSE, TRUE), c(2L, 2L))
  out <- rray_mean_along(x, 1L, na_rm = TRUE)
  expect_identical(as.vector(out), c(1, 0.5))
})

test_that("na_rm removes double NA and NaN", {
  x <- c(1, NA_real_, 5)
  expect_identical(as.vector(rray_mean_along(x, 1L, na_rm = TRUE)), 3)

  x <- c(1, NaN, 5)
  expect_identical(as.vector(rray_mean_along(x, 1L, na_rm = TRUE)), 3)
})

test_that("na_rm with all missing values gives `NaN`", {
  x <- c(NA, NA)
  expect_identical(as.vector(rray_mean_along(x, 1L, na_rm = TRUE)), NaN)

  x <- c(NA_integer_, NA_integer_)
  expect_identical(as.vector(rray_mean_along(x, 1L, na_rm = TRUE)), NaN)

  x <- c(NA_real_, NaN)
  expect_identical(as.vector(rray_mean_along(x, 1L, na_rm = TRUE)), NaN)
})

test_that("na_rm does not change the sign of a zero mean", {
  x <- c(-0, NA)
  out <- as.vector(rray_mean_along(x, 1L, na_rm = TRUE))
  expect_identical(out, mean(x, na.rm = TRUE))
  expect_identical(1 / out, Inf)
})

test_that("na_rm keeps infinities", {
  x <- c(Inf, NA, 1)
  expect_identical(as.vector(rray_mean_along(x, 1L, na_rm = TRUE)), Inf)

  x <- c(Inf, -Inf, NA)
  expect_identical(as.vector(rray_mean_along(x, 1L, na_rm = TRUE)), NaN)
})

test_that("na_rm with no missing values matches the default", {
  x <- array(1:10, c(5L, 2L))
  expect_identical(
    rray_mean_along(x, 1L, na_rm = TRUE),
    rray_mean_along(x, 1L)
  )
})

test_that("na_rm matches `mean()` over every combination of axes", {
  x <- array(c(1e16, 1, -3, 7e15, 1, 1), c(1L, 3L, 2L, 4L))
  x[c(1L, 5L, 9L, 20L)] <- NA
  x[c(2L, 6L)] <- NaN

  expected <- function(axes) {
    dimensions <- dim(x)
    dimensions[axes] <- 1L
    kept_axes <- setdiff(seq_along(dimensions), axes)

    values <- if (length(kept_axes)) {
      apply(x, kept_axes, mean, na.rm = TRUE)
    } else {
      mean(x, na.rm = TRUE)
    }

    array(values, dimensions)
  }

  axes <- list(1L, 2L, 3L, 4L, c(1L, 3L), c(2L, 4L), 1:4)

  for (axis in axes) {
    expect_identical(rray_mean_along(x, axis, na_rm = TRUE), expected(axis))
  }
})

test_that("plain vector input works", {
  out <- rray_mean_along(1:3, 1L)
  expect_identical(as.vector(out), 2)
  expect_identical(rray_dimensions(out), 1L)
})

test_that("scalar reduction works", {
  out <- rray_mean_along(5, 1L)
  expect_identical(as.vector(out), 5)
  expect_identical(rray_dimensions(out), 1L)
})

test_that("axes are coerced to integer", {
  x <- array(1:4, c(2L, 2L))
  expect_identical(
    rray_mean_along(x, 1),
    rray_mean_along(x, 1L)
  )
})

test_that("the mean of no values is `NaN`", {
  x <- matrix(numeric(), 0L, 2L)
  out <- rray_mean_along(x, 1L)
  expect_identical(rray_dimensions(out), c(1L, 2L))
  expect_identical(as.vector(out), c(NaN, NaN))
})

test_that("reducing over a non-zero-length axis with a zero-length axis", {
  x <- matrix(numeric(), 0L, 2L)
  out <- rray_mean_along(x, 2L)
  expect_identical(rray_dimensions(out), c(0L, 1L))
  expect_identical(as.vector(out), numeric())
})

test_that("reducing all axes of a zero-length array", {
  x <- matrix(numeric(), 0L, 2L)
  out <- rray_mean_along(x, c(1L, 2L))
  expect_identical(rray_dimensions(out), c(1L, 1L))
  expect_identical(as.vector(out), NaN)
})

test_that("the empty reduction of each input type is `NaN`", {
  zero_size <- function(x) array(x, c(0L, 1L))

  expect_identical(as.vector(rray_mean_along(zero_size(logical()), 1L)), NaN)
  expect_identical(as.vector(rray_mean_along(zero_size(integer()), 1L)), NaN)
  expect_identical(as.vector(rray_mean_along(zero_size(double()), 1L)), NaN)
})

test_that("errors on axes out of range", {
  x <- array(1:4, c(2L, 2L))
  expect_snapshot(rray_mean_along(x, 3L), error = TRUE)
})

test_that("errors on axes not in strictly increasing order", {
  x <- array(1:24, c(2L, 3L, 4L))
  expect_snapshot(rray_mean_along(x, c(1L, 1L)), error = TRUE)
  expect_snapshot(rray_mean_along(x, c(2L, 1L)), error = TRUE)
})

test_that("errors on axes less than 1", {
  x <- array(1:4, c(2L, 2L))
  expect_snapshot(rray_mean_along(x, 0L), error = TRUE)
})

test_that("errors on axes with NA", {
  x <- array(1:4, c(2L, 2L))
  expect_snapshot(rray_mean_along(x, NA_integer_), error = TRUE)
})

test_that("`na_rm` must be `TRUE` or `FALSE`", {
  x <- array(1:4, c(2L, 2L))
  expect_snapshot(rray_mean_along(x, 1L, na_rm = NA), error = TRUE)
  expect_snapshot(rray_mean_along(x, 1L, na_rm = logical()), error = TRUE)
  expect_snapshot(rray_mean_along(x, 1L, na_rm = c(TRUE, FALSE)), error = TRUE)
  expect_snapshot(rray_mean_along(x, 1L, na_rm = 1), error = TRUE)
})

test_that("errors on complex input", {
  x <- array(c(1 + 2i, 3 + 4i, 5 + 6i, 7 + 8i), c(2L, 2L))
  expect_snapshot(rray_mean_along(x, 1L), error = TRUE)
})

test_that("errors on non-numeric input", {
  x <- array(letters[1:4], c(2L, 2L))
  expect_snapshot(rray_mean_along(x, 1L), error = TRUE)

  x <- array(as.raw(1:4), c(2L, 2L))
  expect_snapshot(rray_mean_along(x, 1L), error = TRUE)

  x <- array(list(1, 2, 3, 4), c(2L, 2L))
  expect_snapshot(rray_mean_along(x, 1L), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_mean_along(x, 1L), error = TRUE)
})
