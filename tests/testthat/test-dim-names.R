context("test-rray-dim-names")

test_that("can get dim names of rrays", {
  x <- rray(1, c(1, 1), dim_names = list("x", "y"))
  expect_equal(rray_dim_names(x), list("x", "y"))
})

test_that("can get dim names of base R", {
  x <- array(1, c(1, 1), dimnames = list("x", "y"))
  expect_equal(rray_dim_names(x), list("x", "y"))
})

test_that("works with non-array vectors", {
  x <- 1:2
  names(x) <- c("x", "y")
  expect_equal(rray_dim_names(x), list(c("x", "y")))
})

# ------------------------------------------------------------------------------

context("test-rray-set-dim-names")

test_that("can set dim names of rrays", {
  x <- rray(1, c(1, 1))
  x <- rray_set_dim_names(x, list("x", "y"))
  expect_equal(rray_dim_names(x), list("x", "y"))
})

test_that("can set dim names of arrays", {
  x <- array(1, c(1, 1))
  x <- rray_set_dim_names(x, list("x", "y"))
  expect_equal(rray_dim_names(x), list("x", "y"))
})

test_that("can set dim names to `NULL` to reset them", {
  x <- rray(1, c(1, 1), dim_names = list("x", "y"))
  x <- rray_set_dim_names(x, NULL)
  expect_equal(rray_dim_names(x), list(NULL, NULL))
})

test_that("errors with wrong length axis names", {
  x <- array(1, c(1, 1))
  expect_error(rray_set_dim_names(x, list(c("x", "x2"), "y")), "size of each dimension's")
})

test_that("errors with wrong length dim names", {
  x <- array(1, c(1, 1))
  expect_error(rray_set_dim_names(x, list("x", "y", "z")), "`dim_names` \\(3\\)")
})

test_that("errors with non-character/null axis names", {
  x <- array(1, c(1, 1))
  expect_error(rray_set_dim_names(x, list(1, "y")), "character vectors, or `NULL`")
})

test_that("assignment form works", {
  x <- rray(1, c(1, 1))
  rray_dim_names(x) <- list("x", "y")
  expect_equal(rray_dim_names(x), list("x", "y"))
})

# ------------------------------------------------------------------------------

context("test-rray-axis-names")

test_that("can pull axis names", {
  x <- rray(1, c(1, 1), list("x", "y"))
  expect_equal(rray_axis_names(x, 1), "x")
})

test_that("can pull names from non-array vectors", {
  x <- 1:2
  names(x) <- c("x", "y")
  expect_equal(rray_axis_names(x, 1), names(x))
})

test_that("errors with bad axis", {
  x <- rray(1, c(1, 1))
  expect_error(rray_axis_names(x, 3), "is 2")
})

test_that("rray_row_names is a wrapper around rray_axis_names", {
  x <- rray(1, c(1, 1), list("x", "y"))
  expect_equal(rray_row_names(x), rray_axis_names(x, 1))
})

test_that("rray_col_names is a wrapper around rray_axis_names", {
  x <- rray(1, c(1, 1), list("x", "y"))
  expect_equal(rray_col_names(x), rray_axis_names(x, 2))
})

# ------------------------------------------------------------------------------

context("test-rray-set-axis-names")

test_that("can set axis names with base R objects", {
  x <- array(1, c(1, 1), dimnames = list("x", "y"))
  x <- rray_set_axis_names(x, 1, "xx")
  expect_equal(rray_dim_names(x), list("xx", "y"))
})

test_that("can set axis names on rrays", {
  x <- rray(1, c(1, 1), dim_names = list("x", "y"))
  x <- rray_set_axis_names(x, 1, "xx")
  expect_equal(rray_dim_names(x), list("xx", "y"))
})

test_that("can set axis names on non-array vectors", {
  x <- 1:2
  x <- rray_set_axis_names(x, 1, c("x", "y"))
  expect_equal(rray_axis_names(x, 1), c("x", "y"))
})

test_that("can unset axis names with `NULL`", {
  x <- rray(1, c(1, 1), dim_names = list("x", "y"))
  x <- rray_set_axis_names(x, 1, NULL)
  expect_equal(rray_dim_names(x), list(NULL, "y"))
})

test_that("can set meta name", {
  x <- rray(1, c(1, 1), dim_names = list("x", "y"))
  x <- rray_set_axis_names(x, 1, NULL, meta = "x_meta")
  expect_equal(rray_dim_names(x), list(x_meta = NULL, "y"))
})

test_that("existing meta name is kept if `meta = NULL`", {
  x <- rray(1, c(1, 1), dim_names = list(x_meta = "x", NULL))
  x <- rray_set_axis_names(x, 1, "x2")
  expect_equal(rray_dim_names(x), list(x_meta = "x2", NULL))
})

test_that("assignment form works", {
  x <- rray(1, c(1, 1), dim_names = list(x_meta = "x", NULL))
  rray_axis_names(x, 1) <- "x2"
  expect_equal(rray_dim_names(x), list(x_meta = "x2", NULL))
})

test_that("row names assignment form works", {
  x <- rray(1, c(1, 1), dim_names = list(x_meta = "x", NULL))
  rray_row_names(x) <- "x2"
  expect_equal(rray_dim_names(x), list(x_meta = "x2", NULL))
})

test_that("col names assignment form works", {
  x <- rray(1, c(1, 1), dim_names = list(x_meta = "x", NULL))
  rray_col_names(x) <- "y"
  expect_equal(rray_dim_names(x), list(x_meta = "x", "y"))
})

test_that("can set meta names with row/col helpers", {
  x <- rray(1, c(1, 1))
  expect_equal(
    rray_dim_names(rray_set_row_names(x, NULL, meta = "hi")),
    list(hi = NULL, NULL)
  )

  expect_equal(
    rray_dim_names(rray_set_col_names(x, NULL, meta = "hi")),
    list(NULL, hi = NULL)
  )
})

# ------------------------------------------------------------------------------

context("test-base-name-generics")

test_that("names() works consistently to base R with rrays", {
  # 2D+ don't have "names"
  x <- rray(1, c(1, 1), list("x", "y"))
  expect_equal(names(x), NULL)

  # 1D have names
  y <- rray(1:2, dim_names = list(c("x", "y")))
  expect_equal(names(y), c("x", "y"))
})

test_that("dimnames()", {
  x <- rray(1, c(1, 1), list("x", "y"))
  expect_equal(dimnames(x), rray_dim_names(x))
})

test_that("names<-()", {
  x <- rray(1, 1, list("x"))
  names(x) <- "x2"
  expect_equal(rray_dim_names(x), list("x2"))
})

test_that("names<-() errors with 2D+ objects", {
  x <- rray(1, c(1, 1), list("x", "y"))
  expect_error(names(x) <- "hi", "Cannot set `names`")
})

test_that("dimnames<-()", {
  x <- rray(1, c(1, 1), list("x", "y"))
  dimnames(x) <- list("x2", "y2")
  expect_equal(rray_dim_names(x), list("x2", "y2"))
})
