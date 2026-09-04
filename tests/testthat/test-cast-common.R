test_that("`.to` casts every input", {
  expect_identical(
    rray_cast_common(1L, TRUE, .to = double()),
    list(array(1, 1L), array(1, 1L))
  )
})

test_that("`.to = NULL` casts to the common type of `...`", {
  expect_identical(
    rray_cast_common(1L, 2.5),
    list(array(1, 1L), array(2.5, 1L))
  )
  expect_identical(
    rray_cast_common(TRUE, 1L),
    list(array(1L, 1L), array(1L, 1L))
  )
})

test_that("keeps the names of `...`", {
  out <- rray_cast_common(x = 1L, y = 2L, .to = double())
  expect_named(out, c("x", "y"))
})

test_that("works with no inputs when `.to` is supplied", {
  expect_identical(rray_cast_common(.to = double()), list())
})

test_that("errors on no inputs when `.to` is not supplied", {
  expect_snapshot(rray_cast_common(), error = TRUE)
})

test_that("errors when `...` has no common type", {
  expect_snapshot(rray_cast_common(1L, "a"), error = TRUE)
})

test_that("the error names the failing element of `...`", {
  expect_snapshot(rray_cast_common(1L, 2.5, .to = integer()), error = TRUE)
})

test_that("`.arg` names `...` as a whole", {
  expect_snapshot(rray_cast_common(1L, "a", .arg = "foo"), error = TRUE)
  expect_snapshot(
    rray_cast_common(1L, 2.5, .to = integer(), .arg = "foo"),
    error = TRUE
  )
})

test_that("`.to_arg` renames `.to` in the error", {
  expect_snapshot(
    rray_cast_common(1, .to = sum, .to_arg = "target"),
    error = TRUE
  )
})

test_that("errors on a bad `.to`", {
  expect_snapshot(rray_cast_common(1, .to = sum), error = TRUE)
  expect_snapshot(
    rray_cast_common(1, .to = structure(1, class = "foo")),
    error = TRUE
  )
})
