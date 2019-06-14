# ------------------------------------------------------------------------------
# yank assign

test_that("can use a yank assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_yank(x, 1) <- NA
  expect_is(x, "vctrs_rray")
  expect_equal(as.vector(x), c(NA, 2:8))
})

test_that("value is broadcast in integer yank assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_yank(x, 1:7) <- NA
  expect_equal(x, rray(c(rep(NA_integer_, 7), 8L), c(2, 2, 2)))
})

test_that("assigning to 0 does nothing", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_yank(x, 0) <- 1
  expect_equal(x, rray(1:8, dim = c(2, 2, 2)))
})

test_that("assigning to NULL does nothing", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_yank(x, NULL) <- 1
  expect_equal(x, rray(1:8, dim = c(2, 2, 2)))
})

test_that("broadcast can fail gracefully in yank assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_error(rray_yank(x, 1) <- c(1, 2), "due to dimension 1")
})

test_that("can yank assign with base R objects", {
  x <- matrix(1:8, nrow = 2)
  rray_yank(x, 1:4) <- 4:1
  expect_equal(as.vector(x)[1:4], 4:1)
})

test_that("yank assigning a non-vector is an error", {
  x <- array(1:5)
  expect_error(rray_yank(x, 1) <- NULL, class = "vctrs_error_scalar_type")
  expect_error(rray_yank(x, 1) <- environment(), class = "vctrs_error_scalar_type")
})

test_that("can yank assign with a logical array", {
  x <- rray(1:8, dim = c(2, 2, 2))
  idx <- rray(rep(c(TRUE, FALSE), 4), c(2, 2, 2))
  rray_yank(x, idx) <- c(10L, 11L, 12L, 13L)

  expect <- rray(c(10L, 2L, 11L, 4L, 12L, 6L, 13L, 8L), c(2, 2, 2))
  expect_equal(x, expect)
})

# ------------------------------------------------------------------------------
# `[[<-`

test_that("can use a [[ position assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  x[[1]] <- NA
  expect_is(x, "vctrs_rray")
  expect_equal(as.vector(x), c(NA, 2:8))
})

test_that("can assign to non-contiguous positions", {
  x <- rray(1:8, dim = c(2, 2, 2))
  x[[c(1, 3)]] <- NA
  expect_is(x, "vctrs_rray")
  expect_equal(as.vector(x), c(NA, 2, NA, 4:8))
})

test_that("can assign with a logical matrix", {
  x <- rray(1:8, dim = c(2, 2, 2))
  idx <- rray(c(FALSE, TRUE, rep(FALSE, 6)), c(2, 2, 2))
  x[[idx]] <- NA
  expect_is(x, "vctrs_rray")
  expect_equal(as.vector(x), c(1, NA, 3:8))
})

test_that("can assign with a logical vector", {
  x <- rray(1:8, dim = c(2, 2, 2))
  idx <- c(FALSE, TRUE, rep(FALSE, 6))
  x[[idx]] <- NA
  expect_is(x, "vctrs_rray")
  expect_equal(as.vector(x), c(1, NA, 3:8))
})

test_that("cannot use a [[ index assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_error(x[[1, 1, 1]] <- NA, "but 3 indexers")
  expect_error(x[[1, 1, 1]] <- NA, "value")
})

test_that("trailing dots are not ignored in `[[<-`", {
  x <- rray(1:8, c(2, 2, 2))
  expect_error(x[[1,]] <- 1, "2 indexers")

  x <- rray(1:4, c(2, 2))
  expect_error(x[[1,]] <- 1, "2 indexers")
})

test_that("assigning NULL in `[[<-` is an error", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_error(x[[1]] <- NULL, class = "vctrs_error_scalar_type")
})
