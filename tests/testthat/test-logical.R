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
  x <- rray(c(TRUE, FALSE), c(1, 2))
  expect_equal(x & NULL, x[0,])

  expect_equal(rray_logical_and(NULL, NULL), new_array(logical()))

  expect_equal(rray_logical_and(logical(), NULL), new_array(logical()))
})

test_that("dim names are kept with NULL input", {
  x <- rray(c(TRUE, FALSE), c(1, 2), dim_names = list(c("r1"), c("c1", "c2")))
  expect_equal(dim_names(x & NULL), dim_names(x[0,]))
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
  expect_error(x & 1, "<integer> to <logical>")
})

test_that("& fails with non-broadcastable 0-length input", {
  x <- rray(c(TRUE, FALSE))
  expect_error(x & logical(), "\\(2\\) and \\(0\\)")
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
  x <- rray(c(TRUE, FALSE), c(1, 2))
  expect_equal(x | NULL, x[0,])

  expect_equal(rray_logical_or(NULL, NULL), new_array(logical()))

  expect_equal(rray_logical_or(logical(), NULL), new_array(logical()))
})

test_that("dim names are kept with NULL input", {
  x <- rray(c(TRUE, FALSE), c(1, 2), dim_names = list(c("r1"), c("c1", "c2")))
  expect_equal(dim_names(x | NULL), dim_names(x[0,]))
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
  expect_error(x | 1, "<integer> to <logical>")
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
  expect_equal(rray_logical_not(NULL), new_array(logical()))
})

test_that("dim names are kept with !", {
  x <- rray(c(TRUE, FALSE), c(2, 2), dim_names = list(c("r1", "r2"), c("c1", "c2")))
  expect_equal(dim_names(!x), dim_names(x))
})

test_that("! works with 0-length input", {
  expect_equal(rray_logical_not(logical()), new_array(logical()))
  expect_equal(rray_logical_not(integer()), new_array(logical()))
})

test_that("! fails when input that can't be cast to logical", {
  x <- rray(1:2)
  expect_error(!x, "<integer> to <logical>")
})

# ------------------------------------------------------------------------------

context("test-any")

test_that("works with rray objects", {

  x <- rray(c(TRUE, FALSE), c(2, 2, 2))

  expect_equal(rray_any(x), rray(TRUE, c(1, 1, 1)))

  expect_equal(rray_any(x, 1), rray(TRUE, c(1, 2, 2)))

  expect_equal(rray_any(x, 2), rray(c(TRUE, TRUE, FALSE, FALSE), c(2, 1, 2)))

  expect_equal(rray_any(x, 3), rray(c(TRUE, TRUE, FALSE, FALSE), c(2, 2, 1)))
})

test_that("works over multiple axes", {
  x <- rray(c(TRUE, FALSE), c(2, 2, 2))

  expect_equal(rray_any(x, c(1, 2)), rray(TRUE, c(1, 1, 2)))

  expect_equal(rray_any(x, c(2, 3)), rray(c(TRUE, FALSE), c(2, 1, 1)))

  expect_equal(rray_any(x, c(1, 3)), rray(c(TRUE, TRUE), c(1, 2, 1)))

  expect_error(rray_any(x, c(2, 1)), "Reducing axes should be sorted")
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

test_that("fails when can't cast to logical", {
  expect_error(rray_any(1:5), "<integer> to <logical>")
})
