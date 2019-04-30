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

# treated as logical(0)
test_that("& works with NULL input", {
  x <- rray(c(TRUE, FALSE), c(2, 2))
  expect_equal(x & NULL, x[0,])

  expect_equal(rray_logical_and(NULL, NULL), new_array(logical()))

  expect_equal(rray_logical_and(logical(), NULL), new_array(logical()))
})

test_that("dim names are kept with NULL input", {
  x <- rray(c(TRUE, FALSE), c(2, 2), dim_names = list(c("r1", "r2"), c("c1", "c2")))
  expect_equal(dim_names(x & NULL), dim_names(x[0,]))
})

test_that("& works with 0-length input", {
  expect_equal(rray_logical_and(logical(), logical()), new_array(logical()))

  expect_equal(rray_logical_and(1L, integer()), new_array(logical()))

  # common dim is kept
  x <- matrix(logical(), nrow = 0, ncol = 2)
  y <- matrix(TRUE, nrow = 5, ncol = 1)
  expect_equal(rray_logical_and(x, y), new_array(logical(), c(0, 2)))

  # common dim is kept
  x <- array(logical(), c(2, 1, 0))
  y <- array(logical(), c(1, 0, 2))
  expect_equal(rray_logical_and(x, y), new_array(logical(), c(2, 0, 0)))
})

test_that("& errors gracefully on bad broadcast", {
  x <- rray(c(TRUE, FALSE))
  y <- rray(c(TRUE, FALSE, TRUE))
  expect_error(x & y, "\\(2\\) and \\(3\\)")
})

test_that("& fails when input that can't be cast to logical", {
  x <- rray(1:2)
  expect_error(x & 1, "<integer> to <logical>")
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

# treated as logical(0)
test_that("| works with NULL input", {
  x <- rray(c(TRUE, FALSE), c(2, 2))
  expect_equal(x | NULL, x[0,])

  expect_equal(rray_logical_or(NULL, NULL), new_array(logical()))

  expect_equal(rray_logical_or(logical(), NULL), new_array(logical()))
})

test_that("dim names are kept with NULL input", {
  x <- rray(c(TRUE, FALSE), c(2, 2), dim_names = list(c("r1", "r2"), c("c1", "c2")))
  expect_equal(dim_names(x | NULL), dim_names(x[0,]))
})

test_that("| works with 0-length input", {
  expect_equal(rray_logical_or(logical(), logical()), new_array(logical()))

  expect_equal(rray_logical_or(1L, integer()), new_array(logical()))

  # common dim is kept
  x <- matrix(logical(), nrow = 0, ncol = 2)
  y <- matrix(TRUE, nrow = 5, ncol = 1)
  expect_equal(rray_logical_or(x, y), new_array(logical(), c(0, 2)))

  # common dim is kept
  x <- array(logical(), c(2, 1, 0))
  y <- array(logical(), c(1, 0, 2))
  expect_equal(rray_logical_or(x, y), new_array(logical(), c(2, 0, 0)))
})

test_that("| errors gracefully on bad broadcast", {
  x <- rray(c(TRUE, FALSE))
  y <- rray(c(TRUE, FALSE, TRUE))
  expect_error(x | y, "\\(2\\) and \\(3\\)")
})

test_that("| fails when input that can't be cast to logical", {
  x <- rray(1:2)
  expect_error(x | 1, "<integer> to <logical>")
})
