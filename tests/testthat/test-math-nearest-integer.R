context("test-ceiling")

test_that("basic", {
  expect_equal(rray_ceiling(2.5), new_array(ceiling(2.5)))
  expect_equal(rray_ceiling(3.5), new_array(ceiling(3.5)))
})

test_that("edge cases", {
  expect_equal(rray_ceiling(Inf), new_array(ceiling(Inf)))
  expect_equal(rray_ceiling(-Inf), new_array(ceiling(-Inf)))

  expect_equal(rray_ceiling(0), new_array(ceiling(0)))
  expect_equal(rray_ceiling(-0), new_array(ceiling(-0)))

  expect_equal(rray_ceiling(NaN), new_array(ceiling(NaN)))
})

test_that("dim names are kept", {
  nms <- list("r1", "c1")
  x <- rray(1, c(1, 1), dim_names = nms)
  expect_equal(dim_names(rray_ceiling(x)), dim_names(x))
})

test_that("vctrs dispatch works", {
  expect_equal(ceiling(rray(1)), rray_ceiling(rray(1)))
})

# ------------------------------------------------------------------------------
context("test-floor")

test_that("basic", {
  expect_equal(rray_floor(2.5), new_array(floor(2.5)))
  expect_equal(rray_floor(3.5), new_array(floor(3.5)))
})

test_that("edge cases", {
  expect_equal(rray_floor(Inf), new_array(floor(Inf)))
  expect_equal(rray_floor(-Inf), new_array(floor(-Inf)))

  expect_equal(rray_floor(0), new_array(floor(0)))
  expect_equal(rray_floor(-0), new_array(floor(-0)))

  expect_equal(rray_floor(NaN), new_array(floor(NaN)))
})

test_that("dim names are kept", {
  nms <- list("r1", "c1")
  x <- rray(1, c(1, 1), dim_names = nms)
  expect_equal(dim_names(rray_floor(x)), dim_names(x))
})

test_that("vctrs dispatch works", {
  expect_equal(floor(rray(1)), rray_floor(rray(1)))
})

# ------------------------------------------------------------------------------
context("test-trunc")

test_that("basic", {
  expect_equal(rray_trunc(2.5), new_array(trunc(2.5)))
  expect_equal(rray_trunc(3.5), new_array(trunc(3.5)))
})

test_that("edge cases", {
  expect_equal(rray_trunc(Inf), new_array(trunc(Inf)))
  expect_equal(rray_trunc(-Inf), new_array(trunc(-Inf)))

  expect_equal(rray_trunc(0), new_array(trunc(0)))
  expect_equal(rray_trunc(-0), new_array(trunc(-0)))

  expect_equal(rray_trunc(NaN), new_array(trunc(NaN)))
})

test_that("dim names are kept", {
  nms <- list("r1", "c1")
  x <- rray(1, c(1, 1), dim_names = nms)
  expect_equal(dim_names(rray_trunc(x)), dim_names(x))
})

test_that("vctrs dispatch works", {
  expect_equal(trunc(rray(1)), rray_trunc(rray(1)))
})

test_that("extra arguments are caught", {
  expect_error(trunc(rray(1), 2), "does not support")
})

# ------------------------------------------------------------------------------
context("test-round")

test_that("basic", {
  expect_equal(rray_round(2.5), new_array(round(2.5)))
  expect_equal(rray_round(3.5), new_array(round(3.5)))
})

test_that("edge cases", {
  expect_equal(rray_round(Inf), new_array(round(Inf)))
  expect_equal(rray_round(-Inf), new_array(round(-Inf)))

  expect_equal(rray_round(0), new_array(round(0)))
  expect_equal(rray_round(-0), new_array(round(-0)))

  expect_equal(rray_round(NaN), new_array(round(NaN)))
})

test_that("dim names are kept", {
  nms <- list("r1", "c1")
  x <- rray(1, c(1, 1), dim_names = nms)
  expect_equal(dim_names(rray_round(x)), dim_names(x))
})

test_that("vctrs dispatch works", {
  expect_equal(round(rray(1)), rray_round(rray(1)))
})

test_that("digits", {
  expect_equal(round(rray(1.222), 1), rray_round(rray(1.222), 1))
  expect_equal(round(rray(1.222), 2), rray_round(rray(1.222), 2))
  expect_equal(round(rray(1.222), 3), rray_round(rray(1.222), 3))

  # always towards even number
  expect_equal(round(rray(2.5), 0), rray_round(rray(2.5), 0))
  expect_equal(round(rray(2.25), 1), rray_round(rray(2.25), 1))
})

# ------------------------------------------------------------------------------
context("test-signif")

test_that("basic", {
  expect_equal(rray_signif(2.5), new_array(signif(2.5)))
  expect_equal(rray_signif(3.5), new_array(signif(3.5)))
})

test_that("edge cases", {
  expect_equal(rray_signif(Inf), new_array(signif(Inf)))
  expect_equal(rray_signif(-Inf), new_array(signif(-Inf)))

  expect_equal(rray_signif(0), new_array(signif(0)))
  expect_equal(rray_signif(-0), new_array(signif(-0)))

  expect_equal(rray_signif(NaN), new_array(signif(NaN)))
})

test_that("dim names are kept", {
  nms <- list("r1", "c1")
  x <- rray(1, c(1, 1), dim_names = nms)
  expect_equal(dim_names(rray_signif(x)), dim_names(x))
})

test_that("vctrs dispatch works", {
  expect_equal(signif(rray(1)), rray_signif(rray(1)))
})

test_that("digits", {
  expect_equal(signif(rray(1.222), 1), rray_signif(rray(1.222), 1))
  expect_equal(signif(rray(1.222), 2), rray_signif(rray(1.222), 2))
  expect_equal(signif(rray(1.222), 3), rray_signif(rray(1.222), 3))

  expect_equal(signif(rray(2.5), 1), rray_signif(rray(2.5), 1))
  expect_equal(signif(rray(2.25), 2), rray_signif(rray(2.25), 2))
})

# ------------------------------------------------------------------------------
