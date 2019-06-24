# ------------------------------------------------------------------------------
context("test-arith-add")

test_that("works with scalars", {
  x_base <- new_matrix(1:4)
  x <- as_rray(x_base)

  expect_is(x + 1L, "vctrs_rray_int")

  expect_identical(x + 1L, as_rray(x_base + 1L))
})

test_that("works with broadcasting", {
  x_base <- new_matrix(1:4)
  x <- as_rray(x_base)

  expect_identical(
    x + t(x),
    as_rray(apply(x_base, 1, function(x) x_base + x))
  )
})

test_that("works with 3D+ and broadcasting", {
  x <- rray(1:8, c(2, 2, 2))

  expect_equal(
    x + matrix(1:2),
    x + array(1:2, c(2, 2, 2))
  )
})

test_that("automatic casting occurs", {
  x <- rray(1L)

  expect_is(x + 0, "vctrs_rray_dbl")
  expect_is(x + TRUE, "vctrs_rray_int")
})

test_that("using logicals result in integers", {
  x <- rray(TRUE)
  expect_identical(x + TRUE, rray(2L))
})

# TODO Is this right?
test_that("`NULL` arithmetic is an error", {
  expect_error(rray(1L) + NULL, class = "vctrs_error_incompatible_op")
})

test_that("`NULL` + `NULL` behavior", {
  expect_equal(rray_add(NULL, NULL), new_array(integer()))
})

test_that("length 0 input behavior is defined", {
  expect_equal(rray(integer()) + integer(), rray(integer()))

  # (0, 2) + (1, 1) = (0, 2)
  x <- rray(integer(), c(0, 2))
  y <- rray(1L, c(1, 1))

  expect_equal(x + y, rray(integer(), c(0, 2)))

  # type of length 0 is used
  expect_equal(rray(1L, c(1, 1)) + double(), rray(double(), c(0, 1)))
})

test_that("broadcasting fails gracefully", {
  expect_error(rray(1:2) + 1:3, "\\(2\\) and \\(3\\)")
})

test_that("broadcasting fails correctly with length 0 input", {
  expect_error(rray(1:2) + integer(), "\\(2\\) and \\(0\\)")
})

test_that("dimension names are kept", {
  x <- rray(1:2, c(2, 1), dim_names = list(c("r1", "r2"), c("c1")))

  expect_equal(rray_dim_names(x + 1), rray_dim_names(x))
  expect_equal(rray_dim_names(1 + x), rray_dim_names(x))
  expect_equal(rray_dim_names(x + matrix(1, ncol = 2)), list(c("r1", "r2"), NULL))

  y <- rray(1, c(1, 1), dim_names = list(NULL, c("y_c1")))

  expect_equal(rray_dim_names(x + y), rray_dim_names(x))
  expect_equal(rray_dim_names(y + x), list(rray_row_names(x), rray_col_names(y)))
})

test_that("shortcut operator works", {
  expect_equal(matrix(1L) %b+% matrix(1L), rray_add(matrix(1L), matrix(1L)))
})

test_that("+ integer overflow results in NA", {
  max_int <- 2147483647L
  expect_equal(rray(max_int) + 1L, rray(NA_integer_))
  expect_equal(rray(max_int) + matrix(c(1L, 1L)), rray(NA_integer_, c(2, 1)))
})

# ------------------------------------------------------------------------------
context("test-arith-subtract")

test_that("works with scalars", {
  x_base <- new_matrix(1:4)
  x <- as_rray(x_base)

  expect_is(x - 1L, "vctrs_rray_int")

  expect_identical(x - 1L, as_rray(x_base - 1L))
})

test_that("works with broadcasting", {
  x_base <- new_matrix(1:4)
  x <- as_rray(x_base)

  expect_identical(
    x - t(x),
    as_rray(apply(x_base, 1, function(x) x_base - x))
  )
})

test_that("works with 3D- and broadcasting", {
  x <- rray(1:8, c(2, 2, 2))

  expect_equal(
    x - matrix(1:2),
    x - array(1:2, c(2, 2, 2))
  )
})

test_that("automatic casting occurs", {
  x <- rray(1L)

  expect_is(x - 0, "vctrs_rray_dbl")
  expect_is(x - TRUE, "vctrs_rray_int")
})

test_that("using logicals result in integers", {
  x <- rray(TRUE)
  expect_identical(x - TRUE, rray(0L))
})

# TODO Is this right?
test_that("`NULL` arithmetic is an error", {
  expect_error(rray(1L) - NULL, class = "vctrs_error_incompatible_op")
})

test_that("`NULL` - `NULL` behavior", {
  expect_equal(rray_subtract(NULL, NULL), new_array(integer()))
})

test_that("length 0 input behavior is defined", {
  expect_equal(rray(integer()) - integer(), rray(integer()))

  # (0, 2) - (1, 1) = (0, 2)
  x <- rray(integer(), c(0, 2))
  y <- rray(1L, c(1, 1))

  expect_equal(x - y, rray(integer(), c(0, 2)))

  # type of length 0 is used
  expect_equal(rray(1L, c(1, 1)) - double(), rray(double(), c(0, 1)))
})

test_that("broadcasting fails gracefully", {
  expect_error(rray(1:2) - 1:3, "\\(2\\) and \\(3\\)")
})

test_that("broadcasting fails correctly with length 0 input", {
  expect_error(rray(1:2) - integer(), "\\(2\\) and \\(0\\)")
})

test_that("dimension names are kept", {
  x <- rray(1:2, c(2, 1), dim_names = list(c("r1", "r2"), c("c1")))

  expect_equal(rray_dim_names(x - 1), rray_dim_names(x))
  expect_equal(rray_dim_names(1 - x), rray_dim_names(x))
  expect_equal(rray_dim_names(x - matrix(1, ncol = 2)), list(c("r1", "r2"), NULL))

  y <- rray(1, c(1, 1), dim_names = list(NULL, c("y_c1")))

  expect_equal(rray_dim_names(x - y), rray_dim_names(x))
  expect_equal(rray_dim_names(y - x), list(rray_row_names(x), rray_col_names(y)))
})

test_that("shortcut operator works", {
  expect_equal(matrix(1L) %b-% matrix(1L), rray_subtract(matrix(1L), matrix(1L)))
})

test_that("- integer overflow results in NA", {
  min_int <- -2147483647L
  expect_equal(rray(min_int) - 1L, rray(NA_integer_))
  expect_equal(rray(min_int) - matrix(c(1L, 1L)), rray(NA_integer_, c(2, 1)))
})

# ------------------------------------------------------------------------------
context("test-arith-divide")

test_that("works with scalars", {
  x_base <- new_matrix(1:4)
  x <- as_rray(x_base)

  # always a dbl!
  expect_is(x / 1L, "vctrs_rray_dbl")

  expect_identical(x / 1L, as_rray(x_base / 1L))
})

test_that("works with broadcasting", {
  x_base <- new_matrix(1:4)
  x <- as_rray(x_base)

  expect_identical(
    x / t(x),
    as_rray(apply(x_base, 1, function(x) x_base / x))
  )
})

test_that("works with 3D- and broadcasting", {
  x <- rray(1:8, c(2, 2, 2))

  expect_equal(
    x / matrix(1:2),
    x / array(1:2, c(2, 2, 2))
  )
})

test_that("automatic casting occurs", {
  x <- rray(1L)

  expect_is(x / 1, "vctrs_rray_dbl")
  expect_is(x / TRUE, "vctrs_rray_dbl")
})

test_that("using logicals still results in doubles", {
  x <- rray(TRUE)
  expect_identical(x / TRUE, rray(1))
})

# TODO Is this right?
test_that("`NULL` arithmetic is an error", {
  expect_error(rray(1L) / NULL, class = "vctrs_error_incompatible_op")
})

test_that("`NULL` / `NULL` behavior", {
  expect_equal(rray_divide(NULL, NULL), new_array(numeric()))
})

test_that("length 0 input behavior is defined", {
  expect_equal(rray(integer()) / integer(), rray(double()))

  # (0, 2) / (1, 1) = (0, 2)
  x <- rray(integer(), c(0, 2))
  y <- rray(1L, c(1, 1))

  expect_equal(x / y, rray(double(), c(0, 2)))
})

test_that("broadcasting fails gracefully", {
  expect_error(rray(1:2) / 1:3, "\\(2\\) and \\(3\\)")
})

test_that("broadcasting fails correctly with length 0 input", {
  expect_error(rray(1:2) / integer(), "\\(2\\) and \\(0\\)")
})

test_that("dimension names are kept", {
  x <- rray(1:2, c(2, 1), dim_names = list(c("r1", "r2"), c("c1")))

  expect_equal(rray_dim_names(x / 1), rray_dim_names(x))
  expect_equal(rray_dim_names(1 / x), rray_dim_names(x))
  expect_equal(rray_dim_names(x / matrix(1, ncol = 2)), list(c("r1", "r2"), NULL))

  y <- rray(1, c(1, 1), dim_names = list(NULL, c("y_c1")))

  expect_equal(rray_dim_names(x / y), rray_dim_names(x))
  expect_equal(rray_dim_names(y / x), list(rray_row_names(x), rray_col_names(y)))
})

test_that("shortcut operator works", {
  expect_equal(matrix(1L) %b/% matrix(1L), rray_divide(matrix(1L), matrix(1L)))
})

test_that("division coerces to double", {
  expect_equal(rray(1L) / 2L, rray(0.5))
})

test_that("division by 0 is Inf / -Inf", {
  expect_equal(rray(1L) / 0, rray(Inf))
  expect_equal(rray(1L) / -0, rray(-Inf))
  expect_equal(rray(-1L) / 0, rray(-Inf))
  expect_equal(rray(-1L) / -0, rray(Inf))
})

# ------------------------------------------------------------------------------

context("test-arith-multiply")

test_that("works with scalars", {
  x_base <- new_matrix(1:4)
  x <- as_rray(x_base)

  expect_is(x * 1L, "vctrs_rray_int")

  expect_identical(x * 1L, as_rray(x_base * 1L))
})

test_that("works with broadcasting", {
  x_base <- new_matrix(1:4)
  x <- as_rray(x_base)

  expect_identical(
    x * t(x),
    as_rray(apply(x_base, 1, function(x) x_base * x))
  )
})

test_that("works with 3D- and broadcasting", {
  x <- rray(1:8, c(2, 2, 2))

  expect_equal(
    x * matrix(1:2),
    x * array(1:2, c(2, 2, 2))
  )
})

test_that("automatic casting occurs", {
  x <- rray(1L)

  expect_is(x * 1, "vctrs_rray_dbl")
  expect_is(x * TRUE, "vctrs_rray_int")
})

test_that("using logicals result in integers", {
  x <- rray(TRUE)
  expect_identical(x * TRUE, rray(1L))
})

# TODO Is this right?
test_that("`NULL` arithmetic is an error", {
  expect_error(rray(1L) * NULL, class = "vctrs_error_incompatible_op")
})

test_that("`NULL` * `NULL` behavior", {
  expect_equal(rray_multiply(NULL, NULL), new_array(integer()))
})

test_that("length 0 input behavior is defined", {
  expect_equal(rray(integer()) * integer(), rray(integer()))

  # (0, 2) * (1, 1) = (0, 2)
  x <- rray(integer(), c(0, 2))
  y <- rray(1L, c(1, 1))

  expect_equal(x * y, rray(integer(), c(0, 2)))
})

test_that("broadcasting fails gracefully", {
  expect_error(rray(1:2) * 1:3, "\\(2\\) and \\(3\\)")
})

test_that("broadcasting fails correctly with length 0 input", {
  expect_error(rray(1:2) * integer(), "\\(2\\) and \\(0\\)")
})

test_that("dimension names are kept", {
  x <- rray(1:2, c(2, 1), dim_names = list(c("r1", "r2"), c("c1")))

  expect_equal(rray_dim_names(x * 1), rray_dim_names(x))
  expect_equal(rray_dim_names(1 * x), rray_dim_names(x))
  expect_equal(rray_dim_names(x * matrix(1, ncol = 2)), list(c("r1", "r2"), NULL))

  y <- rray(1, c(1, 1), dim_names = list(NULL, c("y_c1")))

  expect_equal(rray_dim_names(x * y), rray_dim_names(x))
  expect_equal(rray_dim_names(y * x), list(rray_row_names(x), rray_col_names(y)))
})

test_that("shortcut operator works", {
  expect_equal(matrix(1L) %b*% matrix(1L), rray_multiply(matrix(1L), matrix(1L)))
})

# TODO this seems wrong, should be overflow
# test_that("* integer overflow", {
#   min_int <- -2147483647L
#   expect_equal(rray(min_int) * 2L, rray(NA_integer_))
#   expect_equal(rray(min_int) * matrix(c(2L, 2L)), rray(NA_integer_, c(2, 1)))
# })

# ------------------------------------------------------------------------------
context("test-arith-pow")

test_that("works with scalars", {
  x_base <- new_matrix(1:4)
  x <- as_rray(x_base)

  expect_is(x ^ 1L, "vctrs_rray_dbl")

  expect_identical(x ^ 1L, as_rray(x_base ^ 1L))
})

test_that("works with broadcasting", {
  x_base <- new_matrix(1:4)
  x <- as_rray(x_base)

  expect_identical(
    x ^ t(x),
    as_rray(apply(x_base, 1, function(x) x_base ^ x))
  )
})

test_that("works with 3D- and broadcasting", {
  x <- rray(1:8, c(2, 2, 2))

  expect_equal(
    x ^ matrix(1:2),
    x ^ array(1:2, c(2, 2, 2))
  )
})

test_that("automatic casting occurs", {
  x <- rray(1L)

  expect_is(x ^ 1, "vctrs_rray_dbl")
  expect_is(x ^ TRUE, "vctrs_rray_dbl")
})

# TODO Is this right?
test_that("`NULL` arithmetic is an error", {
  expect_error(rray(1L) ^ NULL, "not permitted", class = "vctrs_error_incompatible_op")
})

test_that("`NULL` ^ `NULL` behavior", {
  expect_equal(rray_pow(NULL, NULL), new_array(numeric()))
})

test_that("length 0 input behavior is defined", {
  expect_equal(rray(integer()) ^ integer(), rray(double()))

  # (0, 2) ^ (1, 1) = (0, 2)
  x <- rray(integer(), c(0, 2))
  y <- rray(1L, c(1, 1))

  expect_equal(x ^ y, rray(double(), c(0, 2)))
})

test_that("broadcasting fails gracefully", {
  expect_error(rray(1:2) ^ (1:3), "\\(2\\) and \\(3\\)")
})

test_that("broadcasting fails correctly with length 0 input", {
  expect_error(rray(1:2) ^ integer(), "\\(2\\) and \\(0\\)")
})

test_that("dimension names are kept", {
  x <- rray(1:2, c(2, 1), dim_names = list(c("r1", "r2"), c("c1")))

  expect_equal(rray_dim_names(x ^ 1), rray_dim_names(x))
  expect_equal(rray_dim_names(1 ^ x), rray_dim_names(x))
  expect_equal(rray_dim_names(x ^ matrix(1, ncol = 2)), list(c("r1", "r2"), NULL))

  y <- rray(1, c(1, 1), dim_names = list(NULL, c("y_c1")))

  expect_equal(rray_dim_names(x ^ y), rray_dim_names(x))
  expect_equal(rray_dim_names(y ^ x), list(rray_row_names(x), rray_col_names(y)))
})

test_that("shortcut operator works", {
  expect_equal(matrix(1L) %b^% matrix(1L), rray_pow(matrix(1L), matrix(1L)))
})

# ------------------------------------------------------------------------------
context("test-arith-modulus")

test_that("vctrs dispatch works", {
  expect_equal(rray(1) %% 2, rray(1 %% 2))
})

# ------------------------------------------------------------------------------
context("test-arith-integer-division")

test_that("can use integer division", {
  expect_identical(rray(1) %/% 2, rray(1 %/% 2))
  expect_identical(rray(2) %/% 2, rray(2 %/% 2))

  expect_identical(rray(2L) %/% 2L, rray(2L %/% 2L))
})

test_that("broadcasting applies", {
  expect_identical(
    rray(1:5) %/% matrix(1:2, nrow = 1),
    rray(
      matrix(rep(1:5, 2), nrow = 5) %/% matrix(rep(1:2, 5), nrow = 5, byrow = TRUE),
      c(5, 2)
    )
  )
})

test_that("dim names are kept", {
  expect_equal(
    rray(1:2) %/% rray(2L, c(1, 1), dim_names = list(NULL, "c1")),
    rray(0:1, c(2, 1), dim_names = list(NULL, "c1"))
  )
})

test_that("can use integer division with logicals", {
  expect_identical(rray(TRUE) %/% TRUE, rray(TRUE %/% TRUE))
})

test_that("can divide by 0 without crashing", {
  expect_identical(rray(1) %/% 0, rray(1 %/% 0))
  expect_identical(rray(1L) %/% 0L, rray(1L %/% 0L))
  expect_identical(rray(TRUE) %/% FALSE, rray(TRUE %/% FALSE))
})

# ------------------------------------------------------------------------------
context("test-arith-identity")

test_that("can use identity", {
  x <- rray(1L)
  expect_equal(rray_identity(x), x)
  expect_equal(+x, x)

  # rray_identity() on numerics / integers returns
  # x unchanged, which means dim names don't become rray compliant
  y <- matrix(1L)
  expect_equal(rray_identity(y), matrix(1L))
})

test_that("can identity a logical", {
  expect_equal(rray_identity(TRUE), new_array(1L))
  expect_equal(rray_identity(FALSE), new_array(0L))
})

test_that("cannot identity NULL", {
  expect_error(rray_identity(NULL))
})

# ------------------------------------------------------------------------------
context("test-arith-opposite")

test_that("can use opposite", {
  x <- rray(1L)
  expect_equal(rray_opposite(x), rray(-1L))
  expect_equal(-x, rray(-1L))

  y <- matrix(1L)
  expect_equal(rray_opposite(y), new_matrix(-1L))
})

test_that("can opposite a logical", {
  expect_equal(rray_opposite(TRUE), new_array(-1L))
  expect_equal(rray_opposite(FALSE), new_array(0L))
})

test_that("cannot opposite NULL", {
  expect_error(rray_opposite(NULL))
})

# ------------------------------------------------------------------------------
context("test-arith-extra")

test_that("Fallthrough operation throws unsupported operation error", {
  expect_error(rray(1) + "a", class = "vctrs_error_incompatible_op")
})

# ------------------------------------------------------------------------------

test_that("meta dim names are kept", {
  x <- rray(1:2, c(2, 1))
  y <- rray(1:2, c(2, 1))
  rray_dim_names(x) <- list(xR = NULL, xC = NULL)
  rray_dim_names(y) <- list(yR = NULL, yC = NULL)

  expect_equal(
    names(rray_dim_names(x + y)),
    names(rray_dim_names(x))
  )

  expect_equal(
    names(rray_dim_names(y + x)),
    names(rray_dim_names(y))
  )

  names(rray_dim_names(x)) <- NULL

  expect_equal(
    names(rray_dim_names(x + y)),
    names(rray_dim_names(y))
  )

  expect_equal(
    names(rray_dim_names(y + x)),
    names(rray_dim_names(y))
  )
})

test_that("can assign meta names to `NULL` without affecting dim names", {
  x <- rray(1:2, c(2, 1))
  rray_dim_names(x) <- list(R = c("r1", "r2"), C = "c1")

  names(rray_dim_names(x)) <- NULL

  expect_equal(names(rray_dim_names(x)), NULL)
  expect_equal(rray_dim_names(x), list(c("r1", "r2"), "c1"))
})
