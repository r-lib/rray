# ------------------------------------------------------------------------------
# rray_ptype()

test_that("every native type maps to its own empty vector", {
  for (ptype in native_ptypes) {
    expect_identical(rray_ptype(ptype), ptype)
  }
})

test_that("the ptype comes from the type, never the data or the dimensions", {
  expect_identical(rray_ptype(array(1:6, c(2L, 3L))), integer())
  expect_identical(rray_ptype(c(a = 1.5, b = 2.5)), double())
  expect_identical(rray_ptype(array(character(), c(0L, 2L))), character())
})

test_that("ptypes are shared, so R copies before modifying them", {
  x <- rray_ptype(1L)
  attr(x, "foo") <- 1L
  expect_identical(rray_ptype(1L), integer())
})

test_that("errors on a scalar", {
  expect_snapshot(rray_ptype(sum), error = TRUE)

  x <- NULL
  expect_snapshot(rray_ptype(x), error = TRUE)
})

test_that("errors on a classed object", {
  x <- structure(1, class = "foo")
  expect_snapshot(rray_ptype(x), error = TRUE)
})

test_that("`arg` defaults to the caller's expression", {
  f <- function(myinput) rray_ptype(myinput)
  expect_snapshot(f(sum), error = TRUE)
})

test_that("`arg` can be overridden or emptied", {
  expect_snapshot(rray_ptype(sum, arg = "vals"), error = TRUE)
  expect_snapshot(rray_ptype(sum, arg = ""), error = TRUE)
})

test_that("`call` blames the caller", {
  f <- function(x) rray_ptype(x)
  expect_snapshot(f(sum), error = TRUE)
})

test_that("dots must be empty", {
  expect_snapshot(rray_ptype(1L, 2L), error = TRUE)
})

# ------------------------------------------------------------------------------
# rray_ptype2()

test_that("the common type of every pair of native types", {
  expect_snapshot(native_ptype_matrix(rray_ptype2, c("x", "y")))
})

test_that("the type is read off arrays of any dimensionality or size", {
  expect_identical(rray_ptype2(1:2, array(1, c(2L, 3L))), double())
  expect_identical(rray_ptype2(array(TRUE, c(1L, 1L, 1L)), 1L), integer())
  expect_identical(rray_ptype2(array(integer(), c(0L, 2L)), 1), double())
})

test_that("a ptype is a bare empty vector, never an array", {
  out <- rray_ptype2(array(1L, c(2L, 3L)), array(1, c(2L, 3L)))
  expect_identical(out, double())
  expect_null(dim(out))
})

test_that("errors on types that don't combine", {
  expect_snapshot(rray_ptype2(character(), integer()), error = TRUE)
  expect_snapshot(rray_ptype2(raw(), integer()), error = TRUE)
  expect_snapshot(rray_ptype2(list(), double()), error = TRUE)
  expect_snapshot(rray_ptype2(character(), list()), error = TRUE)
})

test_that("`x_arg` and `y_arg` default to the caller's expression", {
  expect_snapshot(rray_ptype2(1L, "a"), error = TRUE)
  f <- function(lhs, rhs) rray_ptype2(lhs, rhs)
  expect_snapshot(f(1L, "a"), error = TRUE)
})

test_that("`x_arg` and `y_arg` can be overridden", {
  expect_snapshot(
    rray_ptype2(1L, "a", x_arg = "lhs", y_arg = "rhs"),
    error = TRUE
  )
})

test_that("an empty arg is left out of the message", {
  expect_snapshot(rray_ptype2(1L, "a", x_arg = "", y_arg = ""), error = TRUE)
})

test_that("`call` blames the caller, and can be overridden", {
  f <- function(a, b) rray_ptype2(a, b)
  expect_snapshot(f(1L, "a"), error = TRUE)

  g <- function(a, b) rray_ptype2(a, b, call = caller_env())
  outer <- function() g(1L, "a")
  expect_snapshot(outer(), error = TRUE)
})

test_that("`...` must be empty", {
  expect_snapshot(rray_ptype2(1L, 2L, 5), error = TRUE)
})

test_that("errors on non-array input", {
  expect_snapshot(rray_ptype2(NULL, integer()), error = TRUE)
  expect_snapshot(rray_ptype2(integer(), sum), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(1L, class = "foo")
  expect_snapshot(rray_ptype2(x, integer()), error = TRUE)
  expect_snapshot(rray_ptype2(integer(), x), error = TRUE)
})
