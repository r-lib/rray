# ------------------------------------------------------------------------------
context("test-any")

test_that("returns a single value with shaped arrays", {
  expect_equal(any(rray(c(TRUE, FALSE), c(2, 2))), rray(TRUE))
  expect_equal(any(rray(c(FALSE, FALSE), c(2, 2))), rray(FALSE))
})

test_that("`na.rm` propagates", {
  expect_equal(any(rray(c(NA, 0L)), na.rm = FALSE), rray(NA))
  expect_equal(any(rray(c(NA, 0L)), na.rm = TRUE), rray(FALSE))
})

# ------------------------------------------------------------------------------
context("test-all")

test_that("returns a single value with shaped arrays", {
  expect_equal(all(rray(c(TRUE, FALSE), c(2, 2))), rray(FALSE))
  expect_equal(all(rray(c(TRUE, TRUE), c(2, 2))), rray(TRUE))
})

test_that("`na.rm` propagates", {
  expect_equal(all(rray(c(NA, 1L)), na.rm = FALSE), rray(NA))
  expect_equal(all(rray(c(NA, 1L)), na.rm = TRUE), rray(TRUE))
})

# ------------------------------------------------------------------------------
context("test-prod")

test_that("returns same values as base R", {
  x <- rray(c(5, 6), c(2, 2))
  expect_equal(prod(x), rray(prod(vec_data(x))))
  expect_equal(prod(x, x), rray(prod(vec_data(x), vec_data(x))))
})

test_that("broadcasts input using vctrs", {
  x <- rray(c(5, 6), c(2, 2))
  expect_equal(prod(x, 5), prod(x, matrix(5, c(1, 2))))
})

test_that("`na.rm` propagates", {
  expect_equal(prod(rray(c(NA, 1L)), na.rm = FALSE), rray(NA_real_))
  expect_equal(prod(rray(c(NA, 1L)), na.rm = TRUE), rray(1))
})

# ------------------------------------------------------------------------------
context("test-sum")

test_that("returns same values as base R", {
  x <- rray(c(5, 6), c(2, 2))
  expect_equal(sum(x), rray(sum(vec_data(x))))
  expect_equal(sum(x, x), rray(sum(vec_data(x), vec_data(x))))
})

test_that("broadcasts input using vctrs", {
  x <- rray(c(5, 6), c(2, 2))
  expect_equal(sum(x, 5), rray(sum(x, matrix(5, c(1, 2)))))
})

test_that("`na.rm` propagates", {
  expect_equal(sum(rray(c(NA, 1L)), na.rm = FALSE), rray(NA_integer_))
  expect_equal(sum(rray(c(NA, 1L)), na.rm = TRUE), rray(1L))
})

# ------------------------------------------------------------------------------
context("test-cummax")

test_that("vctrs dispatch works", {
  x <- rray(5:1)
  expect_equal(cummax(x), rray(cummax(vec_data(x))))
})

test_that("flattens 2D+ arrays", {
  x <- rray(5:1, c(5, 1))
  expect_equal(cummax(x), rray(rep(5L, 5)))
})

test_that("keeps names if x is 1D", {
  x <- rray(5:1, dim_names = list(letters[1:5]))
  expect_equal(rray_dim_names(cummax(x)), rray_dim_names(x))
})

# ------------------------------------------------------------------------------
context("test-cummin")

test_that("vctrs dispatch works", {
  x <- rray(1:5)
  expect_equal(cummin(x), rray(cummin(vec_data(x))))
})

test_that("flattens 2D+ arrays", {
  x <- rray(1:5, c(5, 1))
  expect_equal(cummin(x), rray(rep(1L, 5)))
})

test_that("keeps names if x is 1D", {
  x <- rray(1:5, dim_names = list(letters[1:5]))
  expect_equal(rray_dim_names(cummin(x)), rray_dim_names(x))
})

# ------------------------------------------------------------------------------
context("test-cumsum")

test_that("vctrs dispatch works", {
  x <- rray(1:5)
  expect_equal(cumsum(x), rray(cumsum(vec_data(x))))
})

test_that("flattens 2D+ arrays", {
  x <- rray(1:5, c(5, 1))
  expect_equal(cumsum(x), rray(cumsum(1:5)))
})

test_that("keeps names if x is 1D", {
  x <- rray(1:5, dim_names = list(letters[1:5]))
  expect_equal(rray_dim_names(cumsum(x)), rray_dim_names(x))
})

test_that("integer overflow throws a warning", {
  x <- rray(c(2147483647L, 1L))
  expect_warning(cumsum(x), "integer overflow")
})

# ------------------------------------------------------------------------------
context("test-cumprod")

test_that("vctrs dispatch works", {
  x <- rray(1:5)
  expect_equal(cumprod(x), rray(cumprod(vec_data(x))))
})

test_that("flattens 2D+ arrays", {
  x <- rray(1:5, c(5, 1))
  expect_equal(cumprod(x), rray(cumprod(1:5)))
})

test_that("keeps names if x is 1D", {
  x <- rray(1:5, dim_names = list(letters[1:5]))
  expect_equal(rray_dim_names(cumprod(x)), rray_dim_names(x))
})

test_that("a double is returned so no integer overflow occurs", {
  x <- rray(c(2147483647L, 2L))
  expect_equal(storage.mode(cumprod(x)), "double")
})

# ------------------------------------------------------------------------------
context("test-mean")

test_that("vctrs dispatch works", {
  x <- rray(1:5)
  expect_equal(mean(x), rray(mean(vec_data(x))))
})

test_that("flattens 2D+ arrays", {
  x <- rray(1:5, c(5, 1))
  expect_equal(mean(x), rray(mean(1:5)))
})

test_that("logicals become numerics", {
  x <- rray(TRUE, c(5, 1))
  expect_equal(mean(x), rray(1))
})

# ------------------------------------------------------------------------------
context("test-is-nan")

test_that("vctrs dispatch works", {
  nms <- list(NULL, "c1")
  x <- rray(c(1, NaN, 2), c(3, 1), dim_names = nms)
  expect <- rray(c(FALSE, TRUE, FALSE), c(3, 1), dim_names = nms)
  expect_equal(is.nan(x), expect)
})

# ------------------------------------------------------------------------------
context("test-is-finite")

test_that("vctrs dispatch works", {
  nms <- list(NULL, "c1")
  x <- rray(c(1, NaN, 2), c(3, 1), dim_names = nms)
  expect <- rray(c(TRUE, FALSE, TRUE), c(3, 1), dim_names = nms)
  expect_equal(is.finite(x), expect)

  y <- rray(c(1, Inf, 2), c(3, 1), dim_names = nms)
  expect_equal(is.finite(y), expect)
})

# ------------------------------------------------------------------------------
context("test-is-infinite")

test_that("vctrs dispatch works", {
  nms <- list(NULL, "c1")
  x <- rray(c(1, NaN, 2), c(3, 1), dim_names = nms)
  expect <- rray(c(FALSE, FALSE, FALSE), c(3, 1), dim_names = nms)
  expect_equal(is.infinite(x), expect)

  y <- rray(c(1, Inf, 2), c(3, 1), dim_names = nms)
  expect <- rray(c(FALSE, TRUE, FALSE), c(3, 1), dim_names = nms)
  expect_equal(is.infinite(y), expect)
})

# ------------------------------------------------------------------------------
context("test-math-unary")

.fs <- c(
  abs,
  sign,

  ceiling,
  floor,
  trunc,
  round,
  signif,

  exp,
  expm1,
  log,
  log2,
  log10,
  log1p,

  sin,
  cos,
  tan,
  asin,
  acos,
  atan,
  sinpi,
  cospi,
  tanpi,

  sinh,
  cosh,
  tanh,
  asinh,
  acosh,
  atanh,

  gamma,
  lgamma,
  digamma,
  trigamma
)

.f_names <- c(
  "abs",
  "sign",

  "ceiling",
  "floor",
  "trunc",
  "round",
  "signif",

  "exp",
  "expm1",
  "log",
  "log2",
  "log10",
  "log1p",

  "sin",
  "cos",
  "tan",
  "asin",
  "acos",
  "atan",
  "sinpi",
  "cospi",
  "tanpi",

  "sinh",
  "cosh",
  "tanh",
  "asinh",
  "acosh",
  "atanh",

  "gamma",
  "lgamma",
  "digamma",
  "trigamma"
)

for (i in seq_along(.fs)) {
  .f <- .fs[[i]]
  .f_name <- .f_names[[i]]

  test_that(glue::glue("vctrs dispatch works - {.f_name}"), {
    expect_equal(.f(rray(1)), rray(.f(1)))
    expect_equal(.f(rray(1L)), rray(.f(1L)))
  })
}

# ------------------------------------------------------------------------------
context("test-unary-math-extra")

test_that("vctrs dispatch passes `base` through", {
  expect_equal(log(rray(2), base = 2), rray(log(2, base = 2)))
})

test_that("vctrs dispatch passes `digits` through", {
  expect_equal(round(rray(1)), rray(round(1)))
  expect_equal(round(rray(1.5), digits = 0), rray(round(1.5, digits = 0)))
})

test_that("vctrs dispatch passes `digits` through", {
  expect_equal(signif(rray(1.5), digits = 1), rray(signif(1.5, digits = 1)))
})

# ------------------------------------------------------------------------------
context("test-binary-math")

test_that(glue::glue("atan2() keeps class of `y` (first argument)"), {
  expect_equal(atan2(rray(1), 2), rray(atan2(1, 2)))
  expect_equal(atan2(1, rray(2)), atan2(1, 2))
})
