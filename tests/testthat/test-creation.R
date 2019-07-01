context("test-rray-creation")

# ------------------------------------------------------------------------------
# new_rray() constructor

test_that("Can create rrays", {

  # [3, 1] no names
  expect_equal(
    new_rray(c(1, 2, 3), 3L, 1L),
    structure(
      c(1, 2, 3),
      dim = c(3L, 1L),
      dimnames = list(NULL, NULL),
      class = c("vctrs_rray_dbl", "vctrs_rray", "vctrs_vctr")
    )
  )

  # [3, 1] with names
  expect_equal(
    new_rray(c(1, 2, 3), 3L, 1L, list(c("r1", "r2", "r3"), "c1")),
    structure(
      c(1, 2, 3),
      dim = c(3L, 1L),
      dimnames = list(c("r1", "r2", "r3"), "c1"),
      class = c("vctrs_rray_dbl", "vctrs_rray", "vctrs_vctr")
    )
  )

  # [3, 1, 1]
  expect_equal(
    new_rray(c(1, 2, 3), 3L, c(1L, 1L)),
    structure(
      c(1, 2, 3),
      dim = c(3L, 1L, 1L),
      dimnames = list(NULL, NULL, NULL),
      class = c("vctrs_rray_dbl", "vctrs_rray", "vctrs_vctr")
    )
  )

  # [3, 1, 1] with null row names
  expect_equal(
    new_rray(c(1, 2, 3), 3L, c(1L, 1L), list(character(0), "c1", "depth1")),
    structure(
      c(1, 2, 3),
      dim = c(3L, 1L, 1L),
      dimnames = list(NULL, "c1", "depth1"),
      class = c("vctrs_rray_dbl", "vctrs_rray", "vctrs_vctr")
    )
  )

})

test_that("Error if no dim", {
  expect_error(new_rray(c(1, 2, 3), dim_names = list(c("r1", "r2", "r3"), "c1")))
})

test_that("Error if bad dim", {
  expect_error(new_rray(c(1, 2, 3), "apple"))
  expect_error(new_rray(c(1, 2, 3), 3)) # not an integer
})

test_that("Error if bad names", {
  expect_error(new_rray(c(1, 2, 3), c(3L, 1L), list("only_one")))
  expect_error(new_rray(c(1, 2, 3), c(3L, 1L), list(1)))
})

# ------------------------------------------------------------------------------
# rray() helper constructor testing

test_that("Can reshape on the way in", {
  x <- 1:6
  expect_equal(rray_dim(rray(x, c(1, 6))), c(1, 6))
  expect_equal(rray_dim(rray(x, c(6, 1))), c(6, 1))
  expect_equal(rray_dim(rray(x, c(2, 3))), c(2, 3))

  x_6x1 <- as.matrix(x)
  expect_equal(rray_dim(rray(x_6x1, c(1, 6))), c(1, 6))

  x_1D <- as.array(x)
  expect_equal(rray_dim(rray(x_1D, c(1, 6))), c(1, 6))
  expect_equal(rray_dim(rray(x_1D, c(6, 1))), c(6, 1))
  expect_equal(rray_dim(rray(x_1D, c(2, 3))), c(2, 3))
})

test_that("Can broadcast on the way in", {

  x <- 1:6
  expect_equal(rray_dim(rray(x, c(6, 2))), c(6, 2))
  expect_equal(rray_dim(rray(x, c(6, 2, 1))), c(6, 2, 1))

  # can't do this, not valid broadcasting b/x x is treated
  # as having 6 rows (1 col matrix idea).
  expect_error(rray(x, c(2, 6)), "\\(6\\) to \\(2, 6\\)")

  x_6x1 <- as.matrix(x)
  expect_equal(rray_dim(rray(x_6x1, c(6, 2))), c(6, 2))
  expect_equal(rray_dim(rray(x_6x1, c(6, 2, 1))), c(6, 2, 1))
})

test_that("can test if objects are rrays", {
  expect_true(is_rray(rray(1:5)))
  expect_false(is_rray(1:5))
})
