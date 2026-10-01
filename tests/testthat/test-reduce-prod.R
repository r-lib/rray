test_that("can compute product along axis 1", {
  x <- array(1:10, c(5L, 2L))
  out <- rray_prod(x, 1L)
  expect_identical(as.vector(out), c(120, 30240))
})

test_that("can compute product along axis 2", {
  x <- array(1:10, c(5L, 2L))
  out <- rray_prod(x, 2L)
  expect_identical(as.vector(out), c(6, 14, 24, 36, 50))
})

test_that("reduced axes become size 1", {
  x <- array(1:10, c(5L, 2L))
  expect_identical(rray_dimensions(rray_prod(x, 1L)), c(1L, 2L))
  expect_identical(rray_dimensions(rray_prod(x, 2L)), c(5L, 1L))
})

test_that("can reduce over all axes", {
  x <- array(1:10, c(5L, 2L))
  out <- rray_prod(x, c(1L, 2L))
  expect_identical(as.vector(out), 3628800)
  expect_identical(rray_dimensions(out), c(1L, 1L))
})

test_that("reducing over no axes returns the input values unchanged", {
  x <- array(1:10, c(5L, 2L))
  out <- rray_prod(x, integer())
  expect_identical(as.vector(out), as.double(1:10))
  expect_identical(rray_dimensions(out), rray_dimensions(x))
})

test_that("can reduce over multiple axes of a 3D array", {
  x <- array(1, c(2L, 3L, 4L))
  out <- rray_prod(x, c(1L, 2L))
  expect_identical(as.vector(out), rep(1, 4L))
  expect_identical(rray_dimensions(out), c(1L, 1L, 4L))
})

test_that("can reduce axis 3", {
  x <- array(1:24, c(2L, 3L, 4L))
  out <- rray_prod(x, 3L)
  expect_identical(rray_dimensions(out), c(2L, 3L, 1L))
  expect_identical(
    as.vector(out),
    c(1729, 4480, 8505, 14080, 21505, 31104)
  )
})

test_that("dimension names are kept for non-reduced axes", {
  x <- array(1:10, c(5L, 2L), dimnames = list(letters[1:5], c("c1", "c2")))
  expect_identical(
    dimnames(rray_prod(x, 1L)),
    list(NULL, c("c1", "c2"))
  )
  expect_identical(
    dimnames(rray_prod(x, 2L)),
    list(letters[1:5], NULL)
  )
})

test_that("dimension names are dropped when all named axes are reduced", {
  x <- array(1:10, c(5L, 2L), dimnames = list(letters[1:5], NULL))
  expect_null(dimnames(rray_prod(x, 1L)))
})

test_that("output type is double, except complex", {
  expect_identical(storage.mode(rray_prod(array(TRUE), 1L)), "double")
  expect_identical(storage.mode(rray_prod(array(1L), 1L)), "double")
  expect_identical(storage.mode(rray_prod(array(1), 1L)), "double")
  expect_identical(storage.mode(rray_prod(array(1i), 1L)), "complex")
})

test_that("logical values multiply like 0s and 1s", {
  x <- array(c(TRUE, FALSE, TRUE, TRUE), c(2L, 2L))
  out <- rray_prod(x, 1L)
  expect_identical(as.vector(out), c(0, 1))
})

test_that("integer NA propagates", {
  x <- array(c(1L, NA_integer_), c(2L, 1L))
  expect_identical(as.vector(rray_prod(x, 1L)), NA_real_)

  x <- array(c(NA_integer_, 1L), c(2L, 1L))
  expect_identical(as.vector(rray_prod(x, 1L)), NA_real_)
})

test_that("double NA / NaN propagates", {
  x <- c(1, NA_real_)
  expect_identical(as.vector(rray_prod(x, 1L)), NA_real_)

  x <- c(NA_real_, 1)
  expect_identical(as.vector(rray_prod(x, 1L)), NA_real_)

  x <- c(1, NaN)
  expect_identical(as.vector(rray_prod(x, 1L)), NaN)

  x <- c(NaN, 1)
  expect_identical(as.vector(rray_prod(x, 1L)), NaN)

  # Purposefully not comparing directly, as the result is implementation defined
  x <- c(NA, NaN)
  expect_identical(is.na(rray_prod(x, 1L)), array(TRUE, 1L))

  x <- c(NaN, NA)
  expect_identical(is.na(rray_prod(x, 1L)), array(TRUE, 1L))
})

test_that("Inf matches base R prod", {
  expect_identical(
    as.vector(rray_prod(c(Inf, 1), 1L)),
    prod(Inf, 1)
  )
  expect_identical(
    as.vector(rray_prod(c(-Inf, 1), 1L)),
    prod(-Inf, 1)
  )
  expect_identical(
    as.vector(rray_prod(c(Inf, -Inf), 1L)),
    prod(Inf, -Inf)
  )
  expect_identical(
    as.vector(rray_prod(c(Inf, NA), 1L)),
    prod(Inf, NA)
  )
  expect_identical(
    as.vector(rray_prod(c(-Inf, NA), 1L)),
    prod(-Inf, NA)
  )
  expect_identical(
    as.vector(rray_prod(c(Inf, NaN), 1L)),
    prod(Inf, NaN)
  )
  expect_identical(
    as.vector(rray_prod(c(-Inf, NaN), 1L)),
    prod(-Inf, NaN)
  )
  expect_identical(
    as.vector(rray_prod(c(Inf, -Inf, NA), 1L)),
    prod(Inf, -Inf, NA)
  )
  expect_identical(
    as.vector(rray_prod(c(Inf, -Inf, NaN), 1L)),
    prod(Inf, -Inf, NaN)
  )
  expect_identical(
    as.vector(rray_prod(c(0, Inf), 1L)),
    prod(0, Inf)
  )
})

test_that("complex prod works", {
  x <- array(c(1 + 2i, 3 + 4i, 5 + 6i, 7 + 8i), c(2L, 2L))
  expect_identical(as.vector(rray_prod(x, 1L)), c(-5 + 10i, -13 + 82i))
  expect_identical(as.vector(rray_prod(x, 2L)), c(-7 + 16i, -11 + 52i))
})

test_that("complex NA propagates regardless of order", {
  x <- c(1 + 2i, NA_complex_)
  expect_identical(as.vector(rray_prod(x, 1L)), NA_complex_)

  x <- c(NA_complex_, 1 + 2i)
  expect_identical(as.vector(rray_prod(x, 1L)), NA_complex_)
})

test_that("complex NaN in one component spreads to both components", {
  x <- c(complex(real = 1, imaginary = NaN), 1 + 1i)
  out <- as.vector(rray_prod(x, 1L))
  expect_identical(Re(out), NaN)
  expect_identical(Im(out), NaN)

  x <- c(1 + 1i, complex(real = 1, imaginary = NaN))
  out <- as.vector(rray_prod(x, 1L))
  expect_identical(Re(out), NaN)
  expect_identical(Im(out), NaN)
})

test_that("complex zero times Inf gives NaN, matching base R", {
  x <- c(0 + 0i, Inf + 0i)
  expect_identical(as.vector(rray_prod(x, 1L)), prod(x))
})

test_that("complex Inf combined with NA matches base R's prod()", {
  x <- c(Inf + 0i, NA_complex_)
  expect_identical(as.vector(rray_prod(x, 1L)), prod(x))
  expect_identical(as.vector(rray_prod(x, 1L)), NA_complex_)
})

test_that("complex infinities are not recovered, unlike rray_multiply()", {
  x <- c(complex(real = Inf, imaginary = Inf), 1 + 0i)
  out <- as.vector(rray_prod(x, 1L))
  expect_identical(Re(out), NaN)
  expect_identical(Im(out), NaN)
})

test_that("na_rm removes integer NA", {
  x <- array(c(1L, NA_integer_, 3L, 4L), c(2L, 2L))
  out <- rray_prod(x, 1L, na_rm = TRUE)
  expect_identical(as.vector(out), c(1, 12))
})

test_that("na_rm removes double NA and NaN", {
  x <- c(1, NA_real_, 3)
  expect_identical(as.vector(rray_prod(x, 1L, na_rm = TRUE)), 3)

  x <- c(1, NaN, 3)
  expect_identical(as.vector(rray_prod(x, 1L, na_rm = TRUE)), 3)

  x <- c(NA_real_, NaN)
  expect_identical(as.vector(rray_prod(x, 1L, na_rm = TRUE)), 1)
})

test_that("na_rm removes logical NA", {
  x <- array(c(TRUE, NA, FALSE, TRUE), c(2L, 2L))
  out <- rray_prod(x, 1L, na_rm = TRUE)
  expect_identical(as.vector(out), c(1, 0))
})

test_that("na_rm removes an entire complex NA element", {
  x <- c(1 + 2i, NA_complex_)
  out <- as.vector(rray_prod(x, 1L, na_rm = TRUE))
  expect_identical(out, 1 + 2i)

  x <- c(complex(real = 1, imaginary = NaN), 1 + 1i)
  out <- as.vector(rray_prod(x, 1L, na_rm = TRUE))
  expect_identical(out, 1 + 1i)

  x <- c(1 + 1i, complex(real = 1, imaginary = NaN), 2 + 2i)
  out <- as.vector(rray_prod(x, 1L, na_rm = TRUE))
  expect_identical(out, prod(x, na.rm = TRUE))
})

test_that("na_rm with all NA returns identity", {
  x <- c(NA, NA)
  expect_identical(as.vector(rray_prod(x, 1L, na_rm = TRUE)), 1)

  x <- c(NA_integer_, NA_integer_)
  expect_identical(as.vector(rray_prod(x, 1L, na_rm = TRUE)), 1)

  x <- c(NA_real_, NA_real_)
  expect_identical(as.vector(rray_prod(x, 1L, na_rm = TRUE)), 1)

  x <- c(NA_complex_, NA_complex_)
  expect_identical(as.vector(rray_prod(x, 1L, na_rm = TRUE)), 1 + 0i)
})

test_that("na_rm with no NA matches default", {
  x <- array(1:10, c(5L, 2L))
  expect_identical(
    rray_prod(x, 1L, na_rm = TRUE),
    rray_prod(x, 1L)
  )
})

test_that("plain vector input works", {
  out <- rray_prod(1:3, 1L)
  expect_identical(as.vector(out), 6)
  expect_identical(rray_dimensions(out), 1L)
})

test_that("scalar reduction works", {
  out <- rray_prod(5, 1L)
  expect_identical(as.vector(out), 5)
  expect_identical(rray_dimensions(out), 1L)
})

test_that("axes are coerced to integer", {
  x <- array(1:4, c(2L, 2L))
  expect_identical(
    rray_prod(x, 1),
    rray_prod(x, 1L)
  )
})

test_that("reducing a zero-length axis gives identity value", {
  x <- matrix(numeric(), 0L, 2L)
  out <- rray_prod(x, 1L)
  expect_identical(rray_dimensions(out), c(1L, 2L))
  expect_identical(as.vector(out), c(1, 1))
})

test_that("reducing over a non-zero-length axis with a zero-length axis", {
  x <- matrix(numeric(), 0L, 2L)
  out <- rray_prod(x, 2L)
  expect_identical(rray_dimensions(out), c(0L, 1L))
  expect_identical(as.vector(out), numeric())
})

test_that("reducing all axes of a zero-length array", {
  x <- matrix(numeric(), 0L, 2L)
  out <- rray_prod(x, c(1L, 2L))
  expect_identical(rray_dimensions(out), c(1L, 1L))
  expect_identical(as.vector(out), 1)
})

test_that("the identity of an empty reduction has the output type", {
  zero_size <- function(x) array(x, c(0L, 1L))

  expect_identical(as.vector(rray_prod(zero_size(logical()), 1L)), 1)
  expect_identical(as.vector(rray_prod(zero_size(integer()), 1L)), 1)
  expect_identical(as.vector(rray_prod(zero_size(double()), 1L)), 1)
  expect_identical(
    as.vector(rray_prod(zero_size(complex()), 1L)),
    1 + 0i
  )
})

test_that("errors on axes out of range", {
  x <- array(1:4, c(2L, 2L))
  expect_snapshot(rray_prod(x, 3L), error = TRUE)
})

test_that("errors on axes not in strictly increasing order", {
  x <- array(1:24, c(2L, 3L, 4L))
  expect_snapshot(rray_prod(x, c(1L, 1L)), error = TRUE)
  expect_snapshot(rray_prod(x, c(2L, 1L)), error = TRUE)
})

test_that("errors on axes less than 1", {
  x <- array(1:4, c(2L, 2L))
  expect_snapshot(rray_prod(x, 0L), error = TRUE)
})

test_that("errors on axes with NA", {
  x <- array(1:4, c(2L, 2L))
  expect_snapshot(rray_prod(x, NA_integer_), error = TRUE)
})

test_that("`na_rm` must be `TRUE` or `FALSE`", {
  x <- array(1:4, c(2L, 2L))
  expect_snapshot(rray_prod(x, 1L, na_rm = NA), error = TRUE)
  expect_snapshot(rray_prod(x, 1L, na_rm = logical()), error = TRUE)
  expect_snapshot(
    rray_prod(x, 1L, na_rm = c(TRUE, FALSE)),
    error = TRUE
  )
  expect_snapshot(rray_prod(x, 1L, na_rm = 1), error = TRUE)
})

test_that("errors on non-numeric input", {
  x <- array(letters[1:4], c(2L, 2L))
  expect_snapshot(rray_prod(x, 1L), error = TRUE)

  x <- array(as.raw(1:4), c(2L, 2L))
  expect_snapshot(rray_prod(x, 1L), error = TRUE)

  x <- array(list(1, 2, 3, 4), c(2L, 2L))
  expect_snapshot(rray_prod(x, 1L), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_prod(x, 1L), error = TRUE)
})
