test_that("basics", {
  expect_identical(rray_clip(1:3, 1, 2), new_array(c(1L, 2L, 2L)))
  expect_identical(
    rray_clip(rray(1:6, c(2, 3, 1)), 1, 3),
    rray(c(1L, 2L, rep(3L, 4)), c(2, 3, 1))
  )
})

test_that("dimension names are kept", {
  x <- rray(c(0, 0), c(2, 1), list(c("r1", "r2"), "c1"))
  expect_equal(
    rray_clip(x, 1, 2),
    rray(1, c(2, 1), list(c("r1", "r2"), "c1"))
  )
})

test_that("cannot clip if `low` is greater than `high`", {
  expect_error(rray_clip(1, 2, 1), "less than or equal to")
})

test_that("`low` and `high` must be size 1", {
  expect_error(rray_clip(1, c(1, 1), 1), "1, not size 2", class = "vctrs_error_assert_size")
  expect_error(rray_clip(1, 1, c(1, 1)), "1, not size 2", class = "vctrs_error_assert_size")
})

test_that("cannot clip with NULL bounds", {
  expect_error(rray_clip(1, NULL, 1), "not NULL", class = "vctrs_error_scalar_type")
  expect_error(rray_clip(1, 1, NULL), "not NULL", class = "vctrs_error_scalar_type")
})

test_that("clip NULL", {
  expect_equal(rray_clip(NULL, 1, 1), NULL)
})

test_that("clip with length 0 x", {
  expect_equal(rray_clip(logical(), 1, 1), new_array(logical()))
  expect_equal(rray_clip(numeric(), 1, 1), new_array(numeric()))
})
