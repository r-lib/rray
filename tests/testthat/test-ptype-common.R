test_that("works with one input", {
  expect_identical(rray_ptype_common(1L), integer())
})

test_that("climbs to the widest type", {
  expect_identical(rray_ptype_common(TRUE, 1L, 2.5), double())
  expect_identical(rray_ptype_common(2.5, 1L, TRUE), double())
  expect_identical(rray_ptype_common(TRUE, 1i, 1L), complex())
})

test_that("`.ptype` wins over the common type of `...`", {
  expect_identical(rray_ptype_common(1L, "a", .ptype = double()), double())
  expect_identical(rray_ptype_common(.ptype = double()), double())
})

test_that("`.ptype` is reduced to a bare empty vector", {
  expect_identical(
    rray_ptype_common(1L, .ptype = array(1.5, c(2L, 3L))),
    double()
  )
})

test_that("the error names the input that set the common type", {
  expect_snapshot(rray_ptype_common(1L, "a"), error = TRUE)
  expect_snapshot(rray_ptype_common(1L, 2.5, "a"), error = TRUE)
  expect_snapshot(rray_ptype_common(x = 1L, y = 2.5, z = "a"), error = TRUE)
})

test_that("errors on no inputs", {
  expect_snapshot(rray_ptype_common(), error = TRUE)
})

test_that("errors on non-array input", {
  expect_snapshot(rray_ptype_common(1L, NULL), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(1L, class = "foo")
  expect_snapshot(rray_ptype_common(1L, x), error = TRUE)
})

test_that("`.arg` names `...` as a whole", {
  expect_snapshot(rray_ptype_common(1L, "a", .arg = "foo"), error = TRUE)
  expect_snapshot(
    rray_ptype_common(x = 1L, y = "a", .arg = "foo"),
    error = TRUE
  )
})

test_that("`.call` blames the caller", {
  f <- function(...) rray_ptype_common(...)
  expect_snapshot(f(1L, "a"), error = TRUE)
})

test_that("errors on a bad `.ptype`", {
  expect_snapshot(rray_ptype_common(1L, .ptype = sum), error = TRUE)
  expect_snapshot(
    rray_ptype_common(1L, .ptype = structure(1, class = "foo")),
    error = TRUE
  )
})

test_that("`.ptype_arg` renames `.ptype` in the error", {
  expect_snapshot(
    rray_ptype_common(1L, .ptype = sum, .ptype_arg = "pt"),
    error = TRUE
  )
  expect_snapshot(
    rray_ptype_common(1L, .ptype = sum, .ptype_arg = ""),
    error = TRUE
  )
})
