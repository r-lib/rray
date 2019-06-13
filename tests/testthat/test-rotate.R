context("test-rotate")

test_that("can rotate a matrix (counterclockwise-ish)", {

  vals <- 1:6
  vals_rot <- vals[c(4, 1, 5, 2, 6, 3)]
  x <- rray(vals, c(3, 2))
  x_rot <- rray(vals_rot, c(2, 3))

  expect_equal(
    rray_rotate(x),
    x_rot
  )
})

test_that("can rotate a matrix (clockwise-ish)", {

  vals <- 1:6
  vals_rot <- vals[rev(c(4, 1, 5, 2, 6, 3))]
  x <- rray(vals, c(3, 2))
  x_rot <- rray(vals_rot, c(2, 3))

  expect_equal(
    rray_rotate(x, from = 2, to = 1),
    x_rot
  )
})

test_that("can rotate 180 degress", {

  vals <- 1:6
  vals_rot <- vals[c(6, 5, 4, 3, 2, 1)]
  x <- rray(vals, c(3, 2))
  x_rot <- rray(vals_rot, c(3, 2))

  expect_equal(
    rray_rotate(x, times = 2),
    x_rot
  )
})

test_that("can rotate 270 degress", {

  vals <- 1:6
  vals_rot <- vals[c(3, 6, 2, 5, 1, 4)]
  x <- rray(vals, c(3, 2))
  x_rot <- rray(vals_rot, c(2, 3))

  expect_equal(
    rray_rotate(x, times = 3),
    x_rot
  )
})

test_that("matrix names are rotated correctly", {

  nms <- list(letters[1:3], letters[4:5])
  vals <- 1:6
  x <- rray(vals, c(3, 2), dim_names = nms)

  # from = 1, to = 2, times = 1
  nms_rot_1 <- nms[c(2, 1)]
  nms_rot_1[[1]] <- rev(nms_rot_1[[1]])
  vals_rot_1 <- vals[c(4, 1, 5, 2, 6, 3)]
  x_rot_1 <- rray(vals_rot_1, c(2, 3), nms_rot_1)

  expect_equal(
    rray_rotate(x),
    x_rot_1
  )

  # from = 2, to = 1, times = 1
  nms_rot_2 <- nms[c(2, 1)]
  nms_rot_2[[2]] <- rev(nms_rot_2[[2]])
  vals_rot_2 <- vals[rev(c(4, 1, 5, 2, 6, 3))]
  x_rot_2 <- rray(vals_rot_2, c(2, 3), nms_rot_2)

  expect_equal(
    rray_rotate(x, from = 2, to = 1),
    x_rot_2
  )

  # from = 2, to = 1, times = 2
  nms_rot_3 <- nms[c(1, 2)]
  nms_rot_3[[1]] <- rev(nms_rot_3[[1]])
  nms_rot_3[[2]] <- rev(nms_rot_3[[2]])
  vals_rot_3 <- vals[6:1]
  x_rot_3 <- rray(vals_rot_3, c(3, 2), nms_rot_3)

  expect_equal(
    rray_rotate(x, from = 2, to = 1, times = 2),
    x_rot_3
  )
})

test_that("rotating names works if some are `NULL`", {

  nms <- list(letters[1:3], NULL)
  vals <- 1:6
  x <- rray(vals, c(3, 2), dim_names = nms)

  # from = 1, to = 2, times = 1
  nms_rot_1 <- nms[c(2, 1)]
  vals_rot_1 <- vals[c(4, 1, 5, 2, 6, 3)]
  x_rot_1 <- rray(vals_rot_1, c(2, 3), nms_rot_1)

  expect_equal(
    rray_rotate(x),
    x_rot_1
  )

})

test_that("rotations work on higher dimensions", {

  nms <- list(letters[1:3], letters[4:5])
  vals <- 1:6
  x <- rray(vals, c(3, 2), dim_names = nms)
  x <- rray_expand(x, 3)

  # from = 1, to = 2, times = 1
  nms_rot_1 <- nms[c(2, 1)]
  nms_rot_1[[1]] <- rev(nms_rot_1[[1]])
  vals_rot_1 <- vals[c(4, 1, 5, 2, 6, 3)]
  x_rot_1 <- rray(vals_rot_1, c(2, 3), nms_rot_1)
  x_rot_1 <- rray_expand(x_rot_1, 3)

  expect_equal(rray_rotate(x), x_rot_1)

  # from = 1, to = 3, times = 1
  nms_rot_2 <- rlang::list2(NULL, !!!nms[c(2, 1)])
  x_rot_2 <- rray(NA_integer_, c(1, 2, 3), dim_names = nms_rot_2)
  x_rot_2[1,,1] <- matrix(c(1L, 4L), nrow = 1)
  x_rot_2[1,,2] <- matrix(c(2L, 5L), nrow = 1)
  x_rot_2[1,,3] <- matrix(c(3L, 6L), nrow = 1)

  expect_equal(
    rray_rotate(x, from = 1, to = 3),
    x_rot_2
  )

  # from = 3, to = 1, times = 1
  nms_rot_3 <- rlang::list2(NULL, !!!nms[c(2, 1)])
  nms_rot_3[[3]] <- rev(nms_rot_3[[3]])
  x_rot_3 <- rray(NA_integer_, c(1, 2, 3), dim_names = nms_rot_3)
  x_rot_3[1,,1] <- matrix(c(3L, 6L), nrow = 1)
  x_rot_3[1,,2] <- matrix(c(2L, 5L), nrow = 1)
  x_rot_3[1,,3] <- matrix(c(1L, 4L), nrow = 1)

  expect_equal(
    rray_rotate(x, from = 3, to = 1),
    x_rot_3
  )

})

test_that("from/to must be different", {
  expect_error(
    rray_rotate(matrix(1), from = 1, to = 1),
    "`from` and `to` must be different"
  )
})

test_that("rotating a vector is an error", {
  expect_error(rray_rotate(1), "`x` must have at least 2 dimensions")
})

test_that("can call rray_rotate() with `NULL` input", {
  expect_equal(rray_rotate(NULL), NULL)
})
