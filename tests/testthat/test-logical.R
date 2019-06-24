context("test-and")

test_that("can perform element-wise &", {
  x <- rray(c(TRUE, FALSE), c(2, 2))

  expect_equal(x & TRUE, x)
  expect_equal(x & FALSE, rray(FALSE, c(2, 2)))

  expect_equal(x & matrix(TRUE), x)

  expect_equal(x & matrix(c(FALSE, FALSE)), rray(FALSE, c(2, 2)))
})

test_that("can perform & with 3D", {
  x <- rray(c(TRUE, FALSE), c(2, 2, 2))
  expect_equal(x & c(TRUE, FALSE), x)
})

test_that("& works with base R", {
  x <- new_matrix(TRUE, c(2, 2))
  expect_equal(rray_logical_and(x, FALSE), new_matrix(FALSE, c(2, 2)))
})

test_that("& works with non-logicals", {
  x <- rray(c(1L, 0L))

  expect_equal(x & 1L, rray(c(TRUE, FALSE)))
  expect_equal(x & 0L, rray(FALSE, 2))

  expect_equal(x & 1, rray(c(TRUE, FALSE)))
  expect_equal(x & 0, rray(FALSE, 2))
})

# TODO Is this right?
test_that("`NULL` logical op is an error", {
  expect_error(rray(TRUE) & NULL, class = "vctrs_error_incompatible_op")
})

test_that("& works with 0-length input", {
  expect_equal(rray_logical_and(logical(), logical()), new_array(logical()))

  expect_equal(rray_logical_and(1L, integer()), new_array(logical()))

  # common dim is kept
  x <- matrix(logical(), nrow = 0, ncol = 2)
  y <- matrix(TRUE, nrow = 1, ncol = 1)
  expect_equal(rray_logical_and(x, y), new_array(logical(), c(0, 2)))

  # common dim is kept
  x <- array(logical(), c(2, 1, 0))
  y <- array(logical(), c(1, 0, 1))
  expect_equal(rray_logical_and(x, y), new_array(logical(), c(2, 0, 0)))
})

test_that("& errors gracefully on bad broadcast", {
  x <- rray(c(TRUE, FALSE))
  y <- rray(c(TRUE, FALSE, TRUE))
  expect_error(x & y, "\\(2\\) and \\(3\\)")
})

test_that("& fails when input that can't be cast to logical", {
  x <- rray(1:2)
  expect_error(x & 1, "`x` <integer> to `to` <logical>", class = "vctrs_error_cast_lossy")
})

test_that("& fails with non-broadcastable 0-length input", {
  x <- rray(c(TRUE, FALSE))
  expect_error(x & logical(), "\\(2\\) and \\(0\\)")
})

test_that("Correct error is thrown with base R types", {
  expect_error(
    rray_logical_and(matrix(logical(), 0, 2), matrix(logical(), 0, 3)),
    "\\(0, 2\\) and \\(0, 3\\)"
  )
})

# ------------------------------------------------------------------------------

context("test-or")

test_that("can perform element-wise |", {
  x <- rray(c(TRUE, FALSE), c(2, 2))

  expect_equal(x | TRUE, rray(TRUE, c(2, 2)))
  expect_equal(x | FALSE, x)

  expect_equal(x | matrix(TRUE), rray(TRUE, c(2, 2)))

  expect_equal(x | matrix(c(FALSE, FALSE)), x)
})

test_that("can perform | with 3D", {
  x <- rray(c(TRUE, FALSE), c(2, 2, 2))
  expect_equal(x | c(FALSE, TRUE), rray(TRUE, c(2, 2, 2)))
})

test_that("| works with base R", {
  x <- new_matrix(TRUE, c(2, 2))
  expect_equal(rray_logical_or(x, FALSE), x)
})

test_that("| works with non-logicals", {
  x <- rray(c(1L, 0L))

  expect_equal(x | 1L, rray(c(TRUE, TRUE)))
  expect_equal(x | 0L, rray(c(TRUE, FALSE)))

  expect_equal(x | 1, rray(c(TRUE, TRUE)))
  expect_equal(x | 0, rray(c(TRUE, FALSE)))
})

# TODO Is this right?
test_that("`NULL` logical op is an error", {
  expect_error(rray(TRUE) | NULL, class = "vctrs_error_incompatible_op")
})

test_that("| works with 0-length input", {
  expect_equal(rray_logical_or(logical(), logical()), new_array(logical()))

  expect_equal(rray_logical_or(1L, integer()), new_array(logical()))

  # common dim is kept
  x <- matrix(logical(), nrow = 0, ncol = 2)
  y <- matrix(TRUE, nrow = 1, ncol = 1)
  expect_equal(rray_logical_or(x, y), new_array(logical(), c(0, 2)))

  # common dim is kept
  x <- array(logical(), c(2, 1, 0))
  y <- array(logical(), c(1, 0, 1))
  expect_equal(rray_logical_or(x, y), new_array(logical(), c(2, 0, 0)))
})

test_that("| errors gracefully on bad broadcast", {
  x <- rray(c(TRUE, FALSE))
  y <- rray(c(TRUE, FALSE, TRUE))
  expect_error(x | y, "\\(2\\) and \\(3\\)")
})

test_that("| fails when input that can't be cast to logical", {
  x <- rray(1:2)
  expect_error(x | 1, "`x` <integer> to `to` <logical>", class = "vctrs_error_cast_lossy")
})

test_that("| fails with non-broadcastable 0-length input", {
  x <- rray(c(TRUE, FALSE))
  expect_error(x | logical(), "\\(2\\) and \\(0\\)")
})

# ------------------------------------------------------------------------------

context("test-not")

test_that("can perform element-wise !", {
  x <- rray(c(TRUE, FALSE), c(2, 2))
  expect_equal(!x, rray(c(FALSE, TRUE), c(2, 2)))
})

test_that("can perform ! with 3D", {
  x <- rray(c(TRUE, FALSE), c(2, 2, 2))
  expect_equal(!x, rray(c(FALSE, TRUE), c(2, 2, 2)))
})

test_that("! works with base R", {
  x <- new_matrix(TRUE, c(2, 2))
  expect_equal(rray_logical_not(x), new_matrix(FALSE, c(2, 2)))
})

test_that("! works with non-logicals", {
  x <- rray(c(1L, 0L))
  expect_equal(!x, rray(c(FALSE, TRUE)))
})

# treated as logical(0)
test_that("! works with NULL input", {
  expect_equal(rray_logical_not(NULL), logical())
})

test_that("dim names are kept with !", {
  x <- rray(c(TRUE, FALSE), c(2, 2), dim_names = list(c("r1", "r2"), c("c1", "c2")))
  expect_equal(rray_dim_names(!x), rray_dim_names(x))
})

test_that("! works with 0-length input", {
  expect_equal(rray_logical_not(logical()), new_array(logical()))
  expect_equal(rray_logical_not(integer()), new_array(logical()))
})

test_that("! fails when input that can't be cast to logical", {
  x <- rray(1:2)
  expect_error(!x, "`x` <integer> to `to` <logical>", class = "vctrs_error_cast_lossy")
})

# ------------------------------------------------------------------------------

context("test-any")

test_that("works with rray objects", {

  x <- rray(c(TRUE, FALSE), c(2, 2, 2))

  expect_equal(rray_any(x), rray(TRUE, c(1, 1, 1)))

  expect_equal(rray_any(x, 1), rray(TRUE, c(1, 2, 2)))

  expect_equal(rray_any(x, 2), rray(c(TRUE, FALSE, TRUE, FALSE), c(2, 1, 2)))

  expect_equal(rray_any(x, 3), rray(c(TRUE, FALSE, TRUE, FALSE), c(2, 2, 1)))
})

test_that("works over multiple axes", {
  x <- rray(c(TRUE, FALSE), c(2, 2, 2))

  expect_equal(rray_any(x, c(1, 2)), rray(TRUE, c(1, 1, 2)))

  expect_equal(rray_any(x, c(2, 3)), rray(c(TRUE, FALSE), c(2, 1, 1)))

  expect_equal(rray_any(x, c(1, 3)), rray(c(TRUE, TRUE), c(1, 2, 1)))

  expect_error(rray_any(x, c(2, 1)), "Reducing axes should be sorted", class = "std::runtime_error")
})

test_that("works with base R", {
  expect_equal(rray_any(rep(TRUE, 5)), new_array(TRUE))
  expect_equal(rray_any(rep(TRUE, 5)), rray_any(rep(TRUE, 5), axes = 1))
})

test_that("works with NULL input", {
  expect_equal(rray_any(NULL), new_array(FALSE))
})

test_that("works with 0-length input", {
  expect_equal(rray_any(logical()), new_array(FALSE))
  expect_equal(rray_any(logical(), axes = 1), new_array(FALSE))

  # matches numpy
  # essentially, reducing along the 0 axis makes it a 1 in the result so it has a FALSE value
  x <- array(logical(), c(0, 2))
  expect_equal(rray_any(x), new_array(FALSE, c(1, 1)))
  expect_equal(rray_any(x, 1), new_array(FALSE, c(1, 2)))
  expect_equal(rray_any(x, 2), new_array(logical(), c(0, 1)))
})

# TODO ensure this doesnt crash R
test_that("reducing over multiple axes where at least one is size 0", {
  x <- array(logical(), c(0, 0, 2))

  expect_equal(rray_any(x), new_array(FALSE, c(1, 1, 1)))

  #rray_any(x, c(1, 2))
  #rray_any(x, c(1, 3))
})

test_that("reducing over multiple axes works consistently", {
  expect_equal(rray_any(matrix(FALSE, 1, 1), c(1, 2)), new_matrix(FALSE, c(1, 1)))
  expect_equal(rray_any(matrix(FALSE, 2, 2), c(1, 2)), new_matrix(FALSE, c(1, 1)))
  expect_equal(rray_any(matrix(FALSE, 3, 3), c(1, 2)), new_matrix(FALSE, c(1, 1)))
})

test_that("fails when can't cast to logical", {
  expect_error(rray_any(1:5), "`x` <integer> to `to` <logical>", class = "vctrs_error_cast_lossy")
})

# ------------------------------------------------------------------------------

context("test-all")

test_that("works with rray objects", {

  x <- rray(c(TRUE, FALSE), c(2, 2, 2))

  expect_equal(rray_all(x), rray(FALSE, c(1, 1, 1)))

  expect_equal(rray_all(x, 1), rray(FALSE, c(1, 2, 2)))

  expect_equal(rray_all(x, 2), rray(c(TRUE, FALSE, TRUE, FALSE), c(2, 1, 2)))

  expect_equal(rray_all(x, 3), rray(c(TRUE, FALSE, TRUE, FALSE), c(2, 2, 1)))
})

test_that("works over multiple axes", {
  x <- rray(c(TRUE, FALSE), c(2, 2, 2))

  expect_equal(rray_all(x, c(1, 2)), rray(FALSE, c(1, 1, 2)))

  expect_equal(rray_all(x, c(2, 3)), rray(c(TRUE, FALSE), c(2, 1, 1)))

  expect_equal(rray_all(x, c(1, 3)), rray(c(FALSE, FALSE), c(1, 2, 1)))

  expect_error(rray_all(x, c(2, 1)), "Reducing axes should be sorted", class = "std::runtime_error")
})

test_that("works with base R", {
  expect_equal(rray_all(rep(TRUE, 5)), new_array(TRUE))
  expect_equal(rray_all(rep(TRUE, 5)), rray_all(rep(TRUE, 5), axes = 1))
})

test_that("works with NULL input", {
  expect_equal(rray_all(NULL), new_array(TRUE))
})

test_that("works with 0-length input", {
  expect_equal(rray_all(logical()), new_array(TRUE))
  expect_equal(rray_all(logical(), axes = 1), new_array(TRUE))

  # matches numpy
  # essentially, reducing along the 0 axis makes it a 1 in the result so it has a TRUE value
  x <- array(logical(), c(0, 2))
  expect_equal(rray_all(x), new_array(TRUE, c(1, 1)))
  expect_equal(rray_all(x, 1), new_array(TRUE, c(1, 2)))
  expect_equal(rray_all(x, 2), new_array(logical(), c(0, 1)))
})

# TODO ensure this doesnt crash R
test_that("reducing over multiple axes where at least one is size 0", {
  x <- array(logical(), c(0, 0, 2))

  expect_equal(rray_all(x), new_array(TRUE, c(1, 1, 1)))

  #rray_all(x, c(1, 2))
  #rray_all(x, c(1, 3))
})

test_that("reducing over multiple axes works consistently", {
  expect_equal(rray_all(matrix(TRUE, 1, 1), c(1, 2)), new_matrix(TRUE, c(1, 1)))
  expect_equal(rray_all(matrix(TRUE, 2, 2), c(1, 2)), new_matrix(TRUE, c(1, 1)))
  expect_equal(rray_all(matrix(TRUE, 3, 3), c(1, 2)), new_matrix(TRUE, c(1, 1)))
})

test_that("fails when can't cast to logical", {
  expect_error(rray_all(1:5), "`x` <integer> to `to` <logical>", class = "vctrs_error_cast_lossy")
})

# ------------------------------------------------------------------------------
context("test-if_else")

test_that("broadcasting works among the three inputs", {

  expect_identical(
    rray_if_else(
      c(TRUE, FALSE),
      5,
      rray(1:4, c(2, 2))
    ),
    rray(c(5, 2, 5, 4), c(2, 2))
  )

  # 3D
  expect_identical(
    rray_if_else(
      rray(c(TRUE, FALSE), c(2, 2, 2)),
      5,
      rray(1:4, c(2, 2))
    ),
    rray_broadcast(rray(c(5, 2, 5, 4), c(2, 2)), c(2, 2, 2))
  )

})

test_that("length 0 inputs give correct results", {

  expect_equal(rray_if_else(logical(), 1, 0), new_array(numeric()))
  expect_equal(rray_if_else(logical(), 1L, 0L), new_array(integer()))

  expect_identical(
    rray_if_else(
      matrix(logical(), 5, 0),
      matrix(1, 5, 1),
      1
    ),
    new_matrix(numeric(), c(5, 0))
  )
})

test_that("length 0 broadcasting can fail gracefully", {
  expect_error(
    rray_if_else(
      matrix(logical(), 1, 0),
      matrix(1, 1, 5),
      1
    ),
    "\\(1, 0\\) and \\(1, 5\\)"
  )
})

test_that("broadcasting can fail gracefully", {
  expect_error(rray_if_else(TRUE, 1:5, 1:2), "\\(5\\) and \\(2\\)")

  # this isn't the best error because the failure comes after the first
  # set of common dimensions are found, but not much we can do and it isn't
  # _that_ bad
  expect_error(
    rray_if_else(c(TRUE, FALSE), matrix(1, ncol = 2), 1:3),
    "\\(2, 2\\) and \\(3\\)"
  )
})

test_that("result is common type of `true` and `false` even if one isn't used", {
  expect_identical(rray_if_else(TRUE, 1L, 2), new_array(1))
})

test_that("`true` and `false` must have a common type", {
  expect_error(rray_if_else(1, factor(1), 2.5), "unknown inner type", class = "rlang_error")
})

test_that("`condition` must be castable to a logical", {
  expect_error(rray_if_else(1.5, 1, 2), "`x` <double> to `to` <logical>", class = "vctrs_error_cast_lossy")
})
