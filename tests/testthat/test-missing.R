# ------------------------------------------------------------------------------
context("test-is-na")

test_that("can check for missing values", {
  x <- rray(c(1, NA, 2), c(3, 1))
  expect_equal(is.na(x), rray(c(FALSE, TRUE, FALSE), c(3, 1)))
})

test_that("dim names are retained", {
  x <- rray(c(1, NA, 2), c(3, 1), dim_names = list(NULL, "c1"))
  expect_equal(
    rray_dim_names(is.na(x)),
    rray_dim_names(x)
  )
})

# ------------------------------------------------------------------------------
context("test-is-na-assignment")

test_that("assigning NA works with entire rows", {
  x <- rray(c(1, 3, 2), c(3, 1, 2))
  is.na(x) <- c(TRUE, FALSE, TRUE)

  expect_equal(
    x,
    rray(c(NA, 3, NA), c(3, 1, 2))
  )
})

test_that("can use a logical of length 1", {
  x <- rray(c(1, 3, 2), c(3, 1, 2))
  is.na(x) <- TRUE

  expect_equal(x, rray(NA_real_, c(3, 1, 2)))
})

test_that("logical must recycle", {
  x <- rray(c(1, 3, 2), c(3, 1, 2))
  expect_error(is.na(x) <- c(TRUE, FALSE))
})

test_that("cannot use positional NA assignment", {
  x <- rray(c(1, 3, 2), c(3, 1, 2))
  expect_error(is.na(x) <- c(1, 2))
})

test_that("dim names are retained when assigning NAs", {
  x <- rray(c(1, 3, 2), c(3, 1), dim_names = list(NULL, "c1"))
  is.na(x) <- c(TRUE, FALSE, TRUE)
  expect_equal(
    rray_dim_names(x),
    list(NULL, "c1")
  )
})
