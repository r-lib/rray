context("test-split")

test_that("can split 1D `x`", {
  x <- array(1:5, dimnames = list(NULL))
  expect <- map(1:5, function(x) array(x, dimnames = list(NULL)))
  expect_equal(rray_split(x, axes = 1), expect)
})

test_that("can split 2D `x`", {
  x <- array(1:6, dim = c(2, 3), dimnames = list(NULL))

  expect <- list(rray_subset(x, 1L), rray_subset(x, 2L))
  expect_equal(rray_split(x, axes = 1), expect)

  expect <- list(rray_subset(x, , 1L), rray_subset(x, , 2L), rray_subset(x, , 3L))
  expect_equal(rray_split(x, axes = 2), expect)
})

test_that("can set `n`", {
  x <- array(1:8, dim = c(2, 4), dimnames = list(NULL))
  expect <- list(rray_subset(x, , 1:2), rray_subset(x, , 3:4))
  expect_equal(rray_split(x, axes = 2, n = 2), expect)
})

test_that("can split along multiple axes", {
  x <- array(1:8, dim = c(2, 4), dimnames = list(NULL))

  # iteration order matters!
  expect <- rlang::flatten(map(1:4, function(.x) map(1:2, function(.y) {rray_subset(x, .y, .x)})))
  expect_equal(rray_split(x, axes = c(2, 1)), expect)

  expect <- rlang::flatten(map(1:2, function(.x) map(1:4, function(.y) {rray_subset(x, .x, .y)})))
  expect_equal(rray_split(x, axes = c(1, 2)), expect)
})

test_that("`n` can be used with a multisplit", {
  x <- array(1:8, dim = c(2, 4), dimnames = list(NULL))

  expect <- list(
    rray_subset(x, 1, 1:2),
    rray_subset(x, 2, 1:2),
    rray_subset(x, 1, 3:4),
    rray_subset(x, 2, 3:4)
  )

  expect_equal(rray_split(x, axes = c(2, 1), n = c(2, 2)), expect)
})

test_that("dimension names are kept", {

  x <- rray(
    x = 1:16,
    dim = c(2, 2, 2, 2),
    dim_names = list(
      c("r1", "r2"),
      c("c1", "c2"),
      c("d1", "d2"),
      c("e1", "e2")
    )
  )

  expect_equal(
    map(rray_split(x, 1L), row_names),
    list("r1", "r2")
  )

  expect_equal(
    map(map(rray_split(x, 1L), dim_names), function(x) x[-1]),
    list(dim_names(x)[-1], dim_names(x)[-1])
  )

  all_nms <- map(rray_split(x, c(4L, 3L)), dim_names)

  expect_equal(
    all_nms[[1]],
    list(
      c("r1", "r2"),
      c("c1", "c2"),
      c("d1"),
      c("e1")
    )
  )

  expect_equal(
    all_nms[[4]],
    list(
      c("r1", "r2"),
      c("c1", "c2"),
      c("d2"),
      c("e2")
    )
  )

})

test_that("cannot split along an axis higher than dimensionality", {
  x <- array(1:8, dim = c(2, 4), dimnames = list(NULL))
  expect_error(rray_split(x, c(2, 3)), "is 2")
})

test_that("axis is validated", {
  x <- array(1:8, dim = c(2, 4), dimnames = list(NULL))
  expect_error(rray_split(x, "hi"))
})

test_that("`n` must divide equally", {
  x <- array(1:8, dim = c(2, 4), dimnames = list(NULL))
  expect_error(rray_split(x, 1, n = 3), "does not result in equal division")
})
