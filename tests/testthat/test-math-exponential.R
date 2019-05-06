context("test-exp")

test_that("basic", {
  expect_equal(rray_exp(rray(1)), rray(exp(1)))
  expect_equal(rray_exp(rray(TRUE)), rray(exp(TRUE)))
  expect_equal(rray_exp(rray(matrix(1:5))), rray(exp(matrix(1:5))))
})

test_that("dim names are kept", {
  nms <- list("r1", "c1")
  x <- rray(1, c(1, 1), dim_names = nms)
  expect_equal(dim_names(rray_exp(x)), dim_names(x))
})

test_that("corner cases", {
  expect_equal(rray_exp(Inf), new_array(Inf))
  expect_equal(rray_exp(-Inf), new_array(0))

  # exp(0) == 1
  expect_equal(rray_exp(0), new_array(1))

  expect_equal(rray_exp(NaN), new_array(NaN))
})
