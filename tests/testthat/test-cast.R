test_that("which pairs of native types convert", {
  expect_snapshot(native_ptype_matrix(rray_cast, c("from", "to")))
})

test_that("can cast up the numeric tower", {
  expect_identical(rray_cast(TRUE, integer()), array(1L, 1L))
  expect_identical(rray_cast(TRUE, double()), array(1, 1L))
  expect_identical(rray_cast(TRUE, complex()), array(1 + 0i, 1L))
  expect_identical(rray_cast(1L, double()), array(1, 1L))
  expect_identical(rray_cast(1L, complex()), array(1 + 0i, 1L))
  expect_identical(rray_cast(1, complex()), array(1 + 0i, 1L))
})

test_that("can cast down the numeric tower", {
  expect_identical(rray_cast(1L, logical()), array(TRUE, 1L))
  expect_identical(rray_cast(0, logical()), array(FALSE, 1L))
  expect_identical(rray_cast(1, integer()), array(1L, 1L))
})

test_that("missing values are preserved", {
  expect_identical(rray_cast(NA, integer()), array(NA_integer_, 1L))
  expect_identical(rray_cast(NA, double()), array(NA_real_, 1L))
  expect_identical(rray_cast(NA, complex()), array(NA_complex_, 1L))
  expect_identical(rray_cast(NA_integer_, logical()), array(NA, 1L))
  expect_identical(rray_cast(NA_integer_, complex()), array(NA_complex_, 1L))
  expect_identical(rray_cast(NA_real_, complex()), array(NA_complex_, 1L))
  expect_identical(rray_cast(NA_real_, logical()), array(NA, 1L))
  expect_identical(rray_cast(NA_real_, integer()), array(NA_integer_, 1L))
})

test_that("`NaN` matches base R", {
  expect_identical(rray_cast(NaN, complex()), array(complex(real = NaN), 1L))
  expect_identical(rray_cast(NaN, integer()), array(NA_integer_, 1L))
  expect_identical(rray_cast(NaN, logical()), array(NA, 1L))
})

test_that("the integer boundaries cast", {
  max <- .Machine$integer.max
  expect_identical(rray_cast(as.double(max), integer()), array(max, 1L))
  expect_identical(rray_cast(as.double(-max), integer()), array(-max, 1L))
})

test_that("casting to the same type only normalizes the input", {
  x <- array(1:4, c(2L, 2L))
  expect_identical(rray_cast(x, integer()), x)
  expect_identical(
    rray_cast(letters[1:2], character()),
    array(letters[1:2], 2L)
  )
  expect_identical(rray_cast(as.raw(1:2), raw()), array(as.raw(1:2), 2L))
  expect_identical(rray_cast(list(1, 2), list()), array(list(1, 2), 2L))
})

test_that("a bare vector becomes a one dimensional array", {
  expect_identical(
    rray_cast(c(a = 1L, b = 2L), double()),
    array(c(1, 2), 2L, dimnames = list(c("a", "b")))
  )
})

test_that("dimensions and names are kept", {
  x <- array(1:6, c(1L, 2L, 3L), dimnames = list("a", c("b", "c"), NULL))
  out <- rray_cast(x, double())
  expect_identical(rray_dimensions(out), c(1L, 2L, 3L))
  expect_identical(rray_names(out), list("a", c("b", "c"), NULL))
})

test_that("zero size arrays cast", {
  expect_identical(
    rray_cast(array(integer(), c(0L, 2L)), double()),
    array(double(), c(0L, 2L))
  )
  expect_identical(
    rray_cast(array(integer(), c(0L, 0L)), complex()),
    array(complex(), c(0L, 0L))
  )
})

test_that("`rray_cast_common()` casts every input", {
  expect_identical(
    rray_cast_common(1L, TRUE, .to = double()),
    list(array(1, 1L), array(1, 1L))
  )
})

test_that("`rray_cast_common()` keeps the names of `...`", {
  out <- rray_cast_common(x = 1L, y = 2L, .to = double())
  expect_named(out, c("x", "y"))
})

test_that("`rray_cast_common()` works with no inputs", {
  expect_identical(rray_cast_common(.to = double()), list())
})

test_that("errors on a lossy cast, reporting the location", {
  expect_snapshot(rray_cast(c(1, 2.5), integer()), error = TRUE)
  expect_snapshot(rray_cast(c(0, 1, 2), logical()), error = TRUE)
  expect_snapshot(rray_cast(c(1L, 5L), logical()), error = TRUE)
})

test_that("errors when a double is out of integer range", {
  expect_snapshot(rray_cast(2^31, integer()), error = TRUE)
  expect_snapshot(rray_cast(-2^31, integer()), error = TRUE)
})

test_that("errors on types that don't convert", {
  expect_snapshot(rray_cast(letters, integer()), error = TRUE)
  expect_snapshot(rray_cast(as.raw(1), integer()), error = TRUE)
  expect_snapshot(rray_cast(list(1), double()), error = TRUE)
  expect_snapshot(rray_cast(1L, character()), error = TRUE)
})

test_that("complex is a one way trip", {
  expect_snapshot(rray_cast(1i, double()), error = TRUE)
  expect_snapshot(rray_cast(1i, integer()), error = TRUE)
  expect_snapshot(rray_cast(1i, logical()), error = TRUE)
})

test_that("the error names the failing element of `...`", {
  expect_snapshot(
    rray_cast_common(1L, 2.5, .to = integer()),
    error = TRUE
  )
})

test_that("errors on non-array input", {
  expect_snapshot(rray_cast(NULL, integer()), error = TRUE)
  expect_snapshot(rray_cast(1L, NULL), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(1, class = "foo")
  expect_snapshot(rray_cast(x, integer()), error = TRUE)
  expect_snapshot(rray_cast(1, x), error = TRUE)
  expect_snapshot(rray_cast_common(1, .to = x), error = TRUE)
})
