test_that("title getters read the names of the dimension names list", {
  x <- matrix(
    1:6,
    2,
    3,
    dimnames = list(Row = c("a", "b"), Column = NULL)
  )

  expect_identical(rray_titles(x), c("Row", "Column"))
  expect_identical(rray_axis_title(x, 1), "Row")
  expect_identical(rray_axis_title(x, 2), "Column")

  names(dimnames(x)) <- c("Row", "")
  expect_identical(rray_titles(x), c("Row", ""))
  expect_identical(rray_axis_title(x, 2), "")

  names(dimnames(x)) <- c("Row", NA_character_)
  expect_identical(rray_axis_title(x, 2), NA_character_)
})

test_that("title getters return NULL without a titles vector", {
  expect_null(rray_titles(matrix(1:6, 2, 3)))
  expect_null(rray_axis_title(matrix(1:6, 2, 3), 1))
  expect_null(rray_titles(c(a = 1, b = 2)))
  expect_null(rray_axis_title(c(a = 1, b = 2), 1))

  x <- matrix(1:6, 2, 3, dimnames = list(c("a", "b"), NULL))
  expect_null(rray_titles(x))
  expect_null(rray_axis_title(x, 2))
})

test_that("title setters create a dimension names list when needed", {
  x <- matrix(1:6, 2, 3)

  out <- rray_set_titles(x, c("Row", "Column"))
  expect_identical(dimnames(out), list(Row = NULL, Column = NULL))
  expect_null(dimnames(x))

  printed <- paste(capture.output(print(out)), collapse = "\n")
  expect_match(printed, "Row", fixed = TRUE)
  expect_match(printed, "Column", fixed = TRUE)

  out <- rray_set_axis_title(x, 2, "Column")
  expect_identical(
    dimnames(out),
    structure(list(NULL, NULL), names = c("", "Column"))
  )
  expect_identical(rray_axis_title(out, 1), "")
})

test_that("title setters keep element names and do not change the input", {
  x <- matrix(
    1:6,
    2,
    3,
    dimnames = list(Row = c("a", "b"), Column = c("x", "y", "z"))
  )
  original <- dimnames(x)

  out <- rray_set_titles(x, c("First", "Second"))
  expect_identical(
    dimnames(out),
    setNames(unname(original), c("First", "Second"))
  )
  expect_identical(dimnames(x), original)

  out <- rray_set_axis_title(x, 1, "First")
  expect_identical(
    dimnames(out),
    setNames(unname(original), c("First", "Column"))
  )
  expect_identical(dimnames(x), original)
})

test_that("title setters handle plain vectors and one-dimensional arrays", {
  x <- c(a = 1, b = 2)
  out <- rray_set_axis_title(x, 1, "Entry")

  expect_identical(dim(out), 2L)
  expect_identical(dimnames(out), list(Entry = c("a", "b")))
  expect_identical(names(x), c("a", "b"))

  y <- array(1:3, 3)
  out <- rray_set_titles(y, "Entry")
  expect_identical(dimnames(out), list(Entry = NULL))
})

test_that("title setters remove one or all titles", {
  x <- matrix(
    1:6,
    2,
    3,
    dimnames = list(Row = c("a", "b"), Column = c("x", "y", "z"))
  )

  out <- rray_set_axis_title(x, 1, NULL)
  expect_identical(rray_titles(out), c("", "Column"))
  expect_identical(rray_axis_title(out, 1), "")

  out <- rray_set_axis_title(out, 2, "")
  expect_null(rray_titles(out))
  expect_identical(dimnames(out), unname(dimnames(x)))

  out <- rray_set_titles(x, NULL)
  expect_null(rray_titles(out))
  expect_identical(dimnames(out), unname(dimnames(x)))

  out <- rray_set_titles(x, c("", ""))
  expect_null(rray_titles(out))
  expect_identical(dimnames(out), unname(dimnames(x)))
})

test_that("removing absent titles does not make a dimension names list", {
  x <- matrix(1:6, 2, 3)

  expect_null(dimnames(rray_set_titles(x, NULL)))
  expect_null(dimnames(rray_set_titles(x, c("", ""))))
  expect_null(dimnames(rray_set_axis_title(x, 1, NULL)))
  expect_null(dimnames(rray_set_axis_title(x, 1, "")))
})

test_that("title getters reject invalid input and axes", {
  x <- matrix(1:6, 2, 3)
  classed <- structure(x, class = "foo")

  expect_snapshot(rray_titles(NULL), error = TRUE)
  expect_snapshot(rray_titles(classed), error = TRUE)
  expect_snapshot(rray_axis_title(x, 0), error = TRUE)
  expect_snapshot(rray_axis_title(x, 3), error = TRUE)
  expect_snapshot(rray_axis_title(x, NA_integer_), error = TRUE)
  expect_snapshot(rray_axis_title(x, c(1, 2)), error = TRUE)
})

test_that("title setters reject invalid input and values", {
  x <- matrix(1:6, 2, 3)
  classed <- structure(x, class = "foo")

  expect_snapshot(rray_set_titles(classed, NULL), error = TRUE)
  expect_snapshot(rray_set_titles(x, 1:2), error = TRUE)
  expect_snapshot(rray_set_titles(x, "Row"), error = TRUE)
  expect_snapshot(rray_set_axis_title(x, 0, "Row"), error = TRUE)
  expect_snapshot(rray_set_axis_title(x, 1, 1L), error = TRUE)
  expect_snapshot(rray_set_axis_title(x, 1, character()), error = TRUE)
  expect_snapshot(rray_set_axis_title(x, 1, c("Row", "Column")), error = TRUE)
})
