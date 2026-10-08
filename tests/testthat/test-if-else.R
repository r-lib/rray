test_that("condition selects true, false, and missing values", {
  condition <- c(TRUE, FALSE, NA)

  expect_identical(
    rray_if_else(condition, 1L, 2L),
    array(c(1L, 2L, NA_integer_), 3L)
  )
  expect_identical(
    rray_if_else(condition, 1L, 2L, missing = 3L),
    array(c(1L, 2L, 3L), 3L)
  )
})

test_that("condition determines the dimensions and names are dropped", {
  condition <- array(
    c(TRUE, FALSE, NA, TRUE),
    c(2L, 2L),
    dimnames = list(c("a", "b"), c("x", "y"))
  )
  true <- array(1:2, c(2L, 1L), dimnames = list(c("c", "d"), "z"))
  false <- c(e = 10L, f = 20L)

  expect_identical(
    rray_if_else(condition, true, false, missing = 99L),
    array(c(1L, 20L, 99L, 2L), c(2L, 2L))
  )
  expect_identical(
    rray_if_else(c(a = TRUE, b = FALSE), 1L, 2L),
    array(c(1L, 2L), 2L)
  )
})

test_that("branches broadcast independently over three axes", {
  condition <- array(
    c(TRUE, FALSE, FALSE, NA, NA, TRUE, FALSE, NA),
    c(2L, 2L, 2L)
  )
  true <- array(1:2, c(2L, 1L, 1L))
  false <- array(c(10L, 20L), c(1L, 2L, 1L))
  missing <- array(c(30L, 40L), c(1L, 1L, 2L))

  expect_identical(
    rray_if_else(condition, true, false, missing = missing),
    array(c(1L, 10L, 20L, 30L, 40L, 2L, 20L, 40L), c(2L, 2L, 2L))
  )
})

test_that("all supplied branches determine the common type", {
  expect_identical(
    rray_if_else(c(TRUE, FALSE), 1L, 2L, missing = 3),
    array(c(1, 2), 2L)
  )
  expect_identical(
    rray_if_else(c(FALSE, FALSE), 1L, 2, missing = 3L),
    array(c(2, 2), 2L)
  )
})

test_that("missing conditions use a value of the result type", {
  expect_identical(rray_if_else(NA, TRUE, FALSE), array(NA, 1L))
  expect_identical(rray_if_else(NA, 1L, 2L), array(NA_integer_, 1L))
  expect_identical(rray_if_else(NA, 1, 2), array(NA_real_, 1L))
  expect_identical(rray_if_else(NA, 1i, 2i), array(NA_complex_, 1L))
  expect_identical(rray_if_else(NA, "a", "b"), array(NA_character_, 1L))
  expect_identical(
    rray_if_else(NA, as.raw(1), as.raw(2)),
    array(as.raw(0), 1L)
  )
  expect_identical(
    rray_if_else(NA, list(1L), list(2L)),
    array(list(NULL), 1L)
  )
})

test_that("character and list branches retain the selected values", {
  expect_identical(
    rray_if_else(c(TRUE, FALSE, NA), "a", "b", missing = "c"),
    array(c("a", "b", "c"), 3L)
  )
  expect_identical(
    rray_if_else(c(TRUE, FALSE, NA), list(1L), list("b"), missing = list(3)),
    array(list(1L, "b", 3), 3L)
  )
})

test_that("zero-size conditions retain their dimensions", {
  condition <- array(logical(), c(0L, 2L))
  true <- array(integer(), c(0L, 1L))

  expect_identical(
    rray_if_else(condition, true, 2L, missing = 3L),
    array(integer(), c(0L, 2L))
  )
})

test_that("condition must be an unclassed logical array", {
  expect_snapshot(rray_if_else(1:2, 1L, 2L), error = TRUE)
  expect_snapshot(
    rray_if_else(structure(TRUE, class = "foo"), 1L, 2L),
    error = TRUE
  )
})

test_that("each branch must be an array", {
  expect_snapshot(rray_if_else(TRUE, NULL, 1L), error = TRUE)
  expect_snapshot(rray_if_else(TRUE, 1L, NULL), error = TRUE)
  expect_snapshot(rray_if_else(TRUE, 1L, 2L, missing = mean), error = TRUE)
})

test_that("all branches must have compatible types", {
  expect_snapshot(rray_if_else(TRUE, 1L, "a"), error = TRUE)
  expect_snapshot(
    rray_if_else(TRUE, 1L, 2L, missing = "a"),
    error = TRUE
  )
})

test_that("all branches must broadcast even when unselected", {
  condition <- array(TRUE, c(2L, 2L))
  bad <- array(1L, c(3L, 1L))

  expect_snapshot(rray_if_else(condition, bad, 1L), error = TRUE)
  expect_snapshot(rray_if_else(condition, 1L, bad), error = TRUE)
  expect_snapshot(
    rray_if_else(condition, 1L, 2L, missing = bad),
    error = TRUE
  )
  expect_snapshot(
    rray_if_else(condition, 1L, 2L, missing = array(3L, c(1L, 1L, 1L))),
    error = TRUE
  )
})

test_that("dots must be empty", {
  expect_snapshot(rray_if_else(TRUE, 1L, 2L, 3L), error = TRUE)
})
