context("test-exp")

test_that("basic", {
  expect_equal(rray_exp(rray(1)), rray(exp(1)))
  expect_equal(rray_exp(rray(TRUE)), rray(exp(TRUE)))
  expect_equal(rray_exp(rray(matrix(1:5))), rray(exp(matrix(1:5))))
})

test_that("dim names are kept", {
  nms <- list("r1", "c1")
  x <- rray(1, c(1, 1), dim_names = nms)
  expect_equal(rray_dim_names(rray_exp(x)), rray_dim_names(x))
})

test_that("corner cases", {
  expect_equal(rray_exp(Inf), new_array(Inf))
  expect_equal(rray_exp(-Inf), new_array(0))

  # exp(0) == 1
  expect_equal(rray_exp(0), new_array(1))

  expect_equal(rray_exp(NaN), new_array(NaN))
})

test_that("vctrs dispatch works", {
  expect_equal(exp(rray(1)), rray_exp(rray(1)))
  expect_equal(exp(rray(1L)), rray_exp(rray(1L)))
})

# ------------------------------------------------------------------------------
context("test-exp2")

test_that("basic", {
  expect_equal(rray_exp2(rray(1)), rray(2 ^ 1))
  expect_equal(rray_exp2(rray(TRUE)), rray(2 ^ 1))
  expect_equal(rray_exp2(rray(matrix(1:5))), rray(2 ^ matrix(1:5)))
})

test_that("dim names are kept", {
  nms <- list("r1", "c1")
  x <- rray(1, c(1, 1), dim_names = nms)
  expect_equal(rray_dim_names(rray_exp2(x)), rray_dim_names(x))
})

test_that("corner cases", {
  expect_equal(rray_exp2(Inf), new_array(Inf))
  expect_equal(rray_exp2(-Inf), new_array(0))

  # 2 ^ 0 == 1
  expect_equal(rray_exp2(0), new_array(1))

  expect_equal(rray_exp2(NaN), new_array(NaN))
})

# ------------------------------------------------------------------------------
context("test-expm1")

test_that("basic", {
  expect_equal(rray_expm1(rray(1)), rray(expm1(1)))
  expect_equal(rray_expm1(rray(TRUE)), rray(expm1(1)))
  expect_equal(rray_expm1(rray(matrix(1:5))), rray(expm1(matrix(1:5))))
})

test_that("dim names are kept", {
  nms <- list("r1", "c1")
  x <- rray(1, c(1, 1), dim_names = nms)
  expect_equal(rray_dim_names(rray_expm1(x)), rray_dim_names(x))
})

test_that("corner cases", {
  expect_equal(rray_expm1(Inf), new_array(expm1(Inf)))
  expect_equal(rray_expm1(-Inf), new_array(expm1(-Inf)))

  expect_equal(rray_expm1(0), new_array(expm1(0)))

  expect_equal(rray_expm1(NaN), new_array(expm1(NaN)))
})

test_that("vctrs dispatch works", {
  expect_equal(expm1(rray(1)), rray_expm1(rray(1)))
  expect_equal(expm1(rray(1L)), rray_expm1(rray(1L)))
})

# ------------------------------------------------------------------------------
context("test-log")

test_that("basic", {
  expect_equal(rray_log(rray(1)), rray(log(1)))
  expect_equal(rray_log(rray(TRUE)), rray(log(1)))
  expect_equal(rray_log(rray(matrix(1:5))), rray(log(matrix(1:5))))
})

test_that("dim names are kept", {
  nms <- list("r1", "c1")
  x <- rray(1, c(1, 1), dim_names = nms)
  expect_equal(rray_dim_names(rray_log(x)), rray_dim_names(x))
})

test_that("corner cases", {
  expect_equal(rray_log(Inf), new_array(log(Inf)))
  expect_equal(rray_log(-Inf), new_array(suppressWarnings(log(-Inf))))

  expect_equal(rray_log(0), new_array(log(0)))

  expect_equal(rray_log(NaN), new_array(log(NaN)))
})

test_that("different bases", {
  expect_equal(rray_log(rray(100), base = 1), rray(log(100, base = 1)))
  expect_equal(rray_log(rray(5), base = .5), rray(log(5, base = .5)))
  expect_equal(rray_log(rray(5), base = Inf), rray(log(5, base = Inf)))
})

test_that("vctrs dispatch works", {
  expect_equal(log(rray(1)), rray_log(rray(1)))
})

# ------------------------------------------------------------------------------
context("test-log2")

test_that("basic", {
  expect_equal(rray_log2(rray(1)), rray(log2(1)))
  expect_equal(rray_log2(rray(TRUE)), rray(log2(1)))
  expect_equal(rray_log2(rray(matrix(1:5))), rray(log2(matrix(1:5))))
})

test_that("dim names are kept", {
  nms <- list("r1", "c1")
  x <- rray(1, c(1, 1), dim_names = nms)
  expect_equal(rray_dim_names(rray_log2(x)), rray_dim_names(x))
})

test_that("corner cases", {
  expect_equal(rray_log2(Inf), new_array(log2(Inf)))
  expect_equal(rray_log2(-Inf), new_array(suppressWarnings(log2(-Inf))))

  expect_equal(rray_log2(0), new_array(log2(0)))

  expect_equal(rray_log2(NaN), new_array(log2(NaN)))
})

test_that("vctrs dispatch works", {
  expect_equal(log2(rray(1)), rray_log2(rray(1)))
})

# ------------------------------------------------------------------------------
context("test-log10")

test_that("basic", {
  expect_equal(rray_log10(rray(1)), rray(log10(1)))
  expect_equal(rray_log10(rray(TRUE)), rray(log10(1)))
  expect_equal(rray_log10(rray(matrix(1:5))), rray(log10(matrix(1:5))))
})

test_that("dim names are kept", {
  nms <- list("r1", "c1")
  x <- rray(1, c(1, 1), dim_names = nms)
  expect_equal(rray_dim_names(rray_log10(x)), rray_dim_names(x))
})

test_that("corner cases", {
  expect_equal(rray_log10(Inf), new_array(log10(Inf)))
  expect_equal(rray_log10(-Inf), new_array(suppressWarnings(log10(-Inf))))

  expect_equal(rray_log10(0), new_array(log10(0)))

  expect_equal(rray_log10(NaN), new_array(log10(NaN)))
})

test_that("vctrs dispatch works", {
  expect_equal(log10(rray(1)), rray_log10(rray(1)))
})

# ------------------------------------------------------------------------------
context("test-log1p")

test_that("basic", {
  expect_equal(rray_log1p(rray(1)), rray(log1p(1)))
  expect_equal(rray_log1p(rray(TRUE)), rray(log1p(1)))
  expect_equal(rray_log1p(rray(matrix(1:5))), rray(log1p(matrix(1:5))))
})

test_that("dim names are kept", {
  nms <- list("r1", "c1")
  x <- rray(1, c(1, 1), dim_names = nms)
  expect_equal(rray_dim_names(rray_log1p(x)), rray_dim_names(x))
})

test_that("corner cases", {
  expect_equal(rray_log1p(Inf), new_array(log1p(Inf)))
  expect_equal(rray_log1p(-Inf), new_array(suppressWarnings(log1p(-Inf))))

  expect_equal(rray_log1p(0), new_array(log1p(0)))

  expect_equal(rray_log1p(NaN), new_array(log1p(NaN)))
})

test_that("vctrs dispatch works", {
  expect_equal(log1p(rray(1)), rray_log1p(rray(1)))
})

# ------------------------------------------------------------------------------
