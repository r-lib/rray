test_that("can locate along axis 1", {
  x <- array(c(3L, 1L, 2L, 4L, 6L, 5L), c(3L, 2L))
  expect_identical(rray_locate_max(x, axis = 1L), array(c(1L, 2L), c(1L, 2L)))
  expect_identical(rray_locate_min(x, axis = 1L), array(c(2L, 1L), c(1L, 2L)))
})

test_that("can locate along axis 2", {
  x <- array(c(3L, 1L, 2L, 4L, 0L, 5L), c(3L, 2L))
  expect_identical(
    rray_locate_max(x, axis = 2L),
    array(c(2L, 1L, 2L), c(3L, 1L))
  )
  expect_identical(
    rray_locate_min(x, axis = 2L),
    array(c(1L, 2L, 1L), c(3L, 1L))
  )
})

test_that("can locate along axis 3", {
  x <- array(c(1:6, 12:7, 13:18), c(2L, 3L, 3L))
  expect_identical(rray_locate_max(x, axis = 3L), array(3L, c(2L, 3L, 1L)))
  expect_identical(rray_locate_min(x, axis = 3L), array(1L, c(2L, 3L, 1L)))
})

test_that("matches the oracle along every axis", {
  x <- array((1:72 * 7L) %% 11L, c(2L, 3L, 1L, 4L, 3L))
  x[c(5L, 30L, 31L, 64L)] <- NA

  for (axis in seq_along(dim(x))) {
    for (na_rm in c(FALSE, TRUE)) {
      expect_identical(
        rray_locate_max(x, axis = axis, na_rm = na_rm),
        base_locate(x, axis, which.max, na_rm)
      )
      expect_identical(
        rray_locate_min(x, axis = axis, na_rm = na_rm),
        base_locate(x, axis, which.min, na_rm)
      )
    }
  }
})

test_that("matches the oracle and `rray_max()` across special values", {
  cases <- list(
    c(NA, FALSE, TRUE),
    c(NA, -.Machine$integer.max, 0L, .Machine$integer.max),
    c(NA, NaN, -Inf, -1, 0, 1, Inf)
  )

  for (values in cases) {
    x <- unname(t(as.matrix(expand.grid(rep(list(values), 3L)))))
    lanes <- seq_len(ncol(x))

    for (na_rm in c(FALSE, TRUE)) {
      max_loc <- rray_locate_max(x, axis = 1L, na_rm = na_rm)
      min_loc <- rray_locate_min(x, axis = 1L, na_rm = na_rm)

      expect_identical(max_loc, base_locate(x, 1L, which.max, na_rm))
      expect_identical(min_loc, base_locate(x, 1L, which.min, na_rm))

      expect_identical(
        rray_locate_max(t(x), axis = 2L, na_rm = na_rm),
        t(max_loc)
      )
      expect_identical(
        rray_locate_min(t(x), axis = 2L, na_rm = na_rm),
        t(min_loc)
      )
    }

    max_loc <- as.vector(rray_locate_max(x, axis = 1L))
    min_loc <- as.vector(rray_locate_min(x, axis = 1L))
    max <- as.vector(rray_max(x, axes = 1L))
    min <- as.vector(rray_min(x, axes = 1L))

    expect_identical(is.na(max_loc), is.na(max))
    expect_identical(is.na(min_loc), is.na(min))

    max_found <- !is.na(max_loc)
    min_found <- !is.na(min_loc)

    expect_identical(
      x[cbind(max_loc[max_found], lanes[max_found])],
      max[max_found]
    )
    expect_identical(
      x[cbind(min_loc[min_found], lanes[min_found])],
      min[min_found]
    )
  }
})

test_that("ties return the first position", {
  expect_identical(rray_locate_max(c(1L, 3L, 3L), axis = 1L), array(2L, 1L))
  expect_identical(rray_locate_min(c(3L, 1L, 1L), axis = 1L), array(2L, 1L))
  expect_identical(
    rray_locate_max(c(FALSE, TRUE, TRUE), axis = 1L),
    array(2L, 1L)
  )
  expect_identical(
    rray_locate_min(c(TRUE, FALSE, FALSE), axis = 1L),
    array(2L, 1L)
  )
  expect_identical(rray_locate_max(c(-Inf, -Inf), axis = 1L), array(1L, 1L))
  expect_identical(rray_locate_min(c(Inf, Inf), axis = 1L), array(1L, 1L))
})

test_that("signed zeros tie", {
  expect_identical(rray_locate_max(c(-0, 0), axis = 1L), array(1L, 1L))
  expect_identical(rray_locate_max(c(0, -0), axis = 1L), array(1L, 1L))
  expect_identical(rray_locate_min(c(-0, 0), axis = 1L), array(1L, 1L))
  expect_identical(rray_locate_min(c(0, -0), axis = 1L), array(1L, 1L))
  expect_identical(
    rray_locate_max(t(c(-0, 0)), axis = 2L),
    array(1L, c(1L, 1L))
  )
  expect_identical(
    rray_locate_min(t(c(0, -0)), axis = 2L),
    array(1L, c(1L, 1L))
  )
})

test_that("missing values give `NA`", {
  na <- array(NA_integer_, 1L)
  expect_identical(rray_locate_max(c(TRUE, NA, FALSE), axis = 1L), na)
  expect_identical(rray_locate_min(c(FALSE, NA, TRUE), axis = 1L), na)
  expect_identical(rray_locate_max(c(NA, TRUE), axis = 1L), na)
  expect_identical(rray_locate_min(c(NA, FALSE), axis = 1L), na)
  expect_identical(rray_locate_max(c(1L, NA, 2L), axis = 1L), na)
  expect_identical(rray_locate_min(c(1L, NA, 0L), axis = 1L), na)
  expect_identical(rray_locate_max(c(NA, 1L), axis = 1L), na)
  expect_identical(rray_locate_min(c(NA, 1L), axis = 1L), na)
  expect_identical(rray_locate_max(c(1, NaN, 2), axis = 1L), na)
  expect_identical(rray_locate_min(c(1, NA, 0), axis = 1L), na)
  expect_identical(rray_locate_max(c(NaN, 1), axis = 1L), na)
  expect_identical(rray_locate_min(c(NA, 1), axis = 1L), na)
})

test_that("na_rm skips missing values", {
  x <- c(NA, FALSE, TRUE)
  expect_identical(rray_locate_max(x, axis = 1L, na_rm = TRUE), array(3L, 1L))
  expect_identical(rray_locate_min(x, axis = 1L, na_rm = TRUE), array(2L, 1L))

  x <- c(NA, -.Machine$integer.max, NA)
  expect_identical(rray_locate_max(x, axis = 1L, na_rm = TRUE), array(2L, 1L))
  expect_identical(rray_locate_min(x, axis = 1L, na_rm = TRUE), array(2L, 1L))

  x <- c(NaN, -Inf, NA, Inf)
  expect_identical(rray_locate_max(x, axis = 1L, na_rm = TRUE), array(4L, 1L))
  expect_identical(rray_locate_min(x, axis = 1L, na_rm = TRUE), array(2L, 1L))
})

test_that("na_rm with all missing values gives `NA`", {
  expect_identical(
    rray_locate_max(c(NA, NA), axis = 1L, na_rm = TRUE),
    array(NA_integer_, 1L)
  )
  expect_identical(
    rray_locate_min(c(NA, NA), axis = 1L, na_rm = TRUE),
    array(NA_integer_, 1L)
  )
  expect_identical(
    rray_locate_max(c(NA_integer_, NA_integer_), axis = 1L, na_rm = TRUE),
    array(NA_integer_, 1L)
  )
  expect_identical(
    rray_locate_min(c(NA_integer_, NA_integer_), axis = 1L, na_rm = TRUE),
    array(NA_integer_, 1L)
  )
  expect_identical(
    rray_locate_max(c(NA, NaN), axis = 1L, na_rm = TRUE),
    array(NA_integer_, 1L)
  )
  expect_identical(
    rray_locate_min(c(NaN, NA), axis = 1L, na_rm = TRUE),
    array(NA_integer_, 1L)
  )
})

test_that("locating along a zero-length axis gives `NA`", {
  x <- array(double(), c(0L, 2L))
  expect_identical(rray_locate_max(x, axis = 1L), array(NA_integer_, c(1L, 2L)))
  expect_identical(rray_locate_min(x, axis = 1L), array(NA_integer_, c(1L, 2L)))
  expect_identical(
    rray_locate_max(x, axis = 1L, na_rm = TRUE),
    array(NA_integer_, c(1L, 2L))
  )
})

test_that("locating along a non-zero-length axis with a zero-length axis", {
  x <- array(double(), c(0L, 2L))
  expect_identical(rray_locate_max(x, axis = 2L), array(integer(), c(0L, 1L)))
  expect_identical(rray_locate_min(x, axis = 2L), array(integer(), c(0L, 1L)))
})

test_that("works along an axis with a dimension of `.Machine$integer.max`", {
  skip_if_not_testing_long_vectors()

  x <- array(FALSE, c(.Machine$integer.max, 1))
  x[length(x)] <- TRUE
  expect_identical(
    as.vector(rray_locate_max(x, axis = 1)),
    .Machine$integer.max
  )
  expect_identical(
    as.vector(rray_locate_min(!x, axis = 1)),
    .Machine$integer.max
  )
})

test_that("output type is always integer", {
  expect_identical(rray_locate_max(c(FALSE, TRUE), axis = 1L), array(2L, 1L))
  expect_identical(rray_locate_min(c(FALSE, TRUE), axis = 1L), array(1L, 1L))
  expect_identical(rray_locate_max(c(1, 2), axis = 1L), array(2L, 1L))
  expect_identical(rray_locate_min(c(1, 2), axis = 1L), array(1L, 1L))
})

test_that("dimension names are kept for the other axes", {
  x <- array(1:10, c(5L, 2L), dimnames = list(letters[1:5], c("c1", "c2")))
  expect_identical(
    dimnames(rray_locate_max(x, axis = 1L)),
    list(NULL, c("c1", "c2"))
  )
  expect_identical(
    dimnames(rray_locate_min(x, axis = 2L)),
    list(letters[1:5], NULL)
  )
})

test_that("dimension names are dropped when only `axis` is named", {
  x <- array(1:10, c(5L, 2L), dimnames = list(letters[1:5], NULL))
  expect_null(dimnames(rray_locate_max(x, axis = 1L)))
  expect_null(dimnames(rray_locate_min(x, axis = 1L)))
})

test_that("titles are kept", {
  x <- array(
    1:24,
    c(2L, 3L, 4L),
    dimnames = list(A = c("a", "b"), B = NULL, C = NULL)
  )
  y <- array(1:24, c(2L, 3L, 4L), dimnames = list(A = NULL, B = NULL, C = NULL))

  expect_identical(rray_titles(rray_locate_max(x, axis = 1L)), c("A", "B", "C"))
  expect_identical(rray_titles(rray_locate_min(x, axis = 1L)), c("A", "B", "C"))
  expect_identical(dimnames(rray_locate_max(y, axis = 1L)), dimnames(y))
  expect_identical(dimnames(rray_locate_min(y, axis = 1L)), dimnames(y))
})

test_that("plain vector input works", {
  expect_identical(rray_locate_max(c(1L, 3L, 2L), axis = 1L), array(2L, 1L))
  expect_identical(rray_locate_min(c(1L, 3L, 2L), axis = 1L), array(1L, 1L))
})

test_that("`axis` is coerced to integer", {
  x <- array(1:4, c(2L, 2L))
  expect_identical(rray_locate_max(x, axis = 1), rray_locate_max(x, axis = 1L))
  expect_identical(rray_locate_min(x, axis = 1), rray_locate_min(x, axis = 1L))
})

test_that("`axis` can be a title", {
  x <- array(1:6, c(2L, 3L), dimnames = list(row = NULL, col = NULL))
  expect_identical(
    rray_locate_max(x, axis = "col"),
    rray_locate_max(x, axis = 2L)
  )
  expect_identical(
    rray_locate_min(x, axis = "col"),
    rray_locate_min(x, axis = 2L)
  )
})

test_that("`axis` is validated", {
  x <- array(1:4, c(2L, 2L))
  expect_snapshot(rray_locate_max(x, axis = 0L), error = TRUE)
  expect_snapshot(rray_locate_max(x, axis = 3L), error = TRUE)
  expect_snapshot(rray_locate_max(x, axis = c(1L, 2L)), error = TRUE)
  expect_snapshot(rray_locate_max(x, axis = NA_integer_), error = TRUE)
  expect_snapshot(rray_locate_min(x, axis = 1.5), error = TRUE)
  expect_snapshot(rray_locate_min(x, axis = "row"), error = TRUE)
})

test_that("errors on an unknown `axis` title", {
  x <- array(1:6, c(2L, 3L), dimnames = list(row = NULL, col = NULL))
  expect_snapshot(rray_locate_max(x, axis = "nope"), error = TRUE)
})

test_that("`axis` must be named", {
  expect_snapshot(rray_locate_max(1:3, 1), error = TRUE)
  expect_snapshot(rray_locate_min(1:3, 1), error = TRUE)
})

test_that("`na_rm` must be `TRUE` or `FALSE`", {
  x <- array(1:4, c(2L, 2L))
  expect_snapshot(rray_locate_max(x, axis = 1L, na_rm = NA), error = TRUE)
  expect_snapshot(rray_locate_min(x, axis = 1L, na_rm = 1), error = TRUE)
})

test_that("errors on unsupported types", {
  expect_snapshot(
    rray_locate_max(array(1i, c(2L, 2L)), axis = 1L),
    error = TRUE
  )
  expect_snapshot(
    rray_locate_max(array("a", c(2L, 2L)), axis = 1L),
    error = TRUE
  )
  expect_snapshot(
    rray_locate_max(array(as.raw(1:4), c(2L, 2L)), axis = 1L),
    error = TRUE
  )
  expect_snapshot(
    rray_locate_max(array(list(1, 2, 3, 4), c(2L, 2L)), axis = 1L),
    error = TRUE
  )
  expect_snapshot(
    rray_locate_min(array(1i, c(2L, 2L)), axis = 1L),
    error = TRUE
  )
})

test_that("errors on scalar input", {
  expect_snapshot(rray_locate_max(quote(x), axis = 1L), error = TRUE)
  expect_snapshot(rray_locate_min(quote(x), axis = 1L), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(array(1:4, c(2L, 2L)), class = "foo")
  expect_snapshot(rray_locate_max(x, axis = 1L), error = TRUE)
  expect_snapshot(rray_locate_min(x, axis = 1L), error = TRUE)
})
