# ------------------------------------------------------------------------------
# view() basics

test_that("view() produces an ALTREP view", {
  expect_true(is_view(view(1L)))
  expect_true(is_view(view(1)))
  expect_true(is_view(view("a")))
  expect_true(is_view(view(TRUE)))
  expect_true(is_view(view(1i)))
  expect_true(is_view(view(as.raw(1))))
  expect_true(is_view(view(list(1))))
})

test_that("non-views are detected", {
  expect_false(is_view(1L))
  expect_false(is_view(1))
  expect_false(is_view("a"))
  expect_false(is_view(TRUE))
  expect_false(is_view(NULL))
})

test_that("view preserves length", {
  expect_length(view(1:5), 5L)
  expect_length(view(c(1.5, 2.5, 3.5)), 3L)
  expect_length(view(c("a", "b")), 2L)
  expect_length(view(c(TRUE, FALSE, TRUE)), 3L)
  expect_length(view(complex(3)), 3L)
  expect_length(view(raw(4)), 4L)
  expect_length(view(list(1, 2)), 2L)
})

test_that("view preserves type", {
  expect_type(view(1L), "integer")
  expect_type(view(1), "double")
  expect_type(view("a"), "character")
  expect_type(view(TRUE), "logical")
  expect_type(view(1i), "complex")
  expect_type(view(as.raw(1)), "raw")
  expect_type(view(list(1)), "list")
})

# ------------------------------------------------------------------------------
# Attributes

test_that("view starts with no attributes", {
  x <- array(1:4, c(2L, 2L), dimnames = list(c("a", "b"), NULL))
  attr(x, "foo") <- 1
  v <- view(x)
  expect_null(attributes(v))
  expect_view_of(v, x)
})

test_that("modifying dim on a view does not affect the original", {
  x <- array(1:6, c(2L, 3L))
  v <- view(x)
  dim(v) <- c(3L, 2L)
  expect_identical(dim(x), c(2L, 3L))
  expect_identical(dim(v), c(3L, 2L))
})

# ------------------------------------------------------------------------------
# Read access

test_that("view provides read access to integer data", {
  x <- 1:5
  v <- view(x)
  expect_identical(v[1], 1L)
  expect_identical(v[2:4], 2:4)
})

test_that("view provides read access to double data", {
  x <- c(1.5, 2.5, 3.5)
  v <- view(x)
  expect_identical(v[1], 1.5)
  expect_identical(v[3], 3.5)
})

test_that("view provides read access to logical data", {
  x <- c(TRUE, FALSE, NA)
  v <- view(x)
  expect_identical(v[1], TRUE)
  expect_identical(v[2:3], c(FALSE, NA))
})

test_that("view provides read access to complex data", {
  x <- c(1 + 2i, 3 + 4i)
  v <- view(x)
  expect_identical(v[1], 1 + 2i)
  expect_identical(v[2], 3 + 4i)
})

test_that("view provides read access to raw data", {
  x <- as.raw(1:3)
  v <- view(x)
  expect_identical(v[1], as.raw(1))
  expect_identical(v[2:3], as.raw(c(2, 3)))
})

test_that("view provides read access to character data", {
  x <- c("a", "b", "c")
  v <- view(x)
  expect_identical(v[1], "a")
  expect_identical(v[3], "c")
})

test_that("view provides read access to list data", {
  x <- list("a", 1L, TRUE)
  v <- view(x)
  expect_identical(v[[1]], "a")
  expect_identical(v[2:3], list(1L, TRUE))
})

# ------------------------------------------------------------------------------
# Ownership

test_that("view starts out sharing data", {
  x <- 1:3
  v <- view(x)
  expect_view_of(v, x)
})

test_that("read-only access to a dimensioned view shares data", {
  xs <- list(
    c(TRUE, FALSE),
    c(1L, 2L),
    c(1.5, 2.5),
    c(1 + 2i, 3 + 4i)
  )

  for (x in xs) {
    v <- rray_set_dimensions(x, c(1L, 2L))

    expect_identical(view_read_access(v), TRUE)
    expect_view_of(v, x)
  }
})

test_that("a read-only pointer stays valid after a writable request", {
  xs <- list(
    c(TRUE, FALSE),
    c(1L, 2L),
    c(1.5, 2.5),
    c(1 + 2i, 3 + 4i),
    as.raw(c(1, 2)),
    c("a", "b"),
    list(1, "a")
  )

  for (x in xs) {
    x_before <- unserialize(serialize(x, NULL))
    v <- view(x)

    expect_identical(view_pointer_is_stable(v), TRUE)
    expect_view_of(v, x)
    expect_identical(x, x_before)
  }
})

test_that("changing a view gives a plain vector", {
  x <- 1:3
  v <- view(x)
  v[1] <- 99L
  expect_identical(is_view(v), FALSE)
  expect_identical(v[1], 99L)
  expect_identical(x, 1:3)

  x <- c(1, 2, 3)
  v <- view(x)
  v[1] <- 99
  expect_identical(is_view(v), FALSE)
  expect_identical(v[1], 99)
  expect_identical(x, c(1, 2, 3))

  x <- c(TRUE, FALSE, TRUE)
  v <- view(x)
  v[1] <- FALSE
  expect_identical(is_view(v), FALSE)
  expect_identical(v[1], FALSE)
  expect_identical(x, c(TRUE, FALSE, TRUE))

  x <- c(1 + 0i, 2 + 0i)
  v <- view(x)
  v[1] <- 99 + 0i
  expect_identical(is_view(v), FALSE)
  expect_identical(v[1], 99 + 0i)
  expect_identical(x, c(1 + 0i, 2 + 0i))

  x <- as.raw(1:3)
  v <- view(x)
  v[1] <- as.raw(99)
  expect_identical(is_view(v), FALSE)
  expect_identical(v[1], as.raw(99))
  expect_identical(x, as.raw(1:3))

  x <- c("a", "b", "c")
  v <- view(x)
  v[1] <- "z"
  expect_identical(is_view(v), FALSE)
  expect_identical(v[1], "z")
  expect_identical(x, c("a", "b", "c"))

  x <- list("a", "b")
  v <- view(x)
  v[[1]] <- "z"
  expect_identical(is_view(v), FALSE)
  expect_identical(v[[1]], "z")
  expect_identical(x, list("a", "b"))
})

test_that("a second change happens in place", {
  x <- 1:3
  v <- view(x)

  v[1] <- 10L
  address <- obj_address(v)

  v[2] <- 20L
  expect_identical(obj_address(v), address)
  expect_identical(v, c(10L, 20L, 3L))
  expect_identical(x, 1:3)

  x <- as.list(1:3)
  v <- view(x)

  v[1] <- list(10L)
  address <- obj_address(v)

  v[2] <- list(20L)
  expect_identical(obj_address(v), address)
  expect_identical(v, list(10L, 20L, 3L))
  expect_identical(x, as.list(1:3))
})

test_that("a temporary view is never reused as an output", {
  x <- c(1, 2, 3, 4, 5, 6)
  out <- rray_set_dimensions(x, c(2L, 3L)) + 1
  expect_identical(x, c(1, 2, 3, 4, 5, 6))
  expect_identical(out, array(c(2, 3, 4, 5, 6, 7), c(2L, 3L)))

  x <- 1:6
  out <- -rray_set_dimensions(x, c(2L, 3L))
  expect_identical(x, 1:6)
  expect_identical(out, array(-(1:6), c(2L, 3L)))
})

test_that("a view of an ALTREP vector shares it", {
  x <- 1:100
  v <- view(x)
  expect_identical(is_altrep(view_data(v)), TRUE)
  expect_view_of(v, x)
  expect_identical(v, 1:100)
})

test_that("setting an element of a character or list view errors", {
  expect_snapshot(error = TRUE, {
    view_set_elt(view(c("a", "b")))
    view_set_elt(view(list(1, 2)))
  })
})

# ------------------------------------------------------------------------------
# A view of a view

test_that("a view of a view does not nest", {
  x <- 1:3
  v1 <- view(x)
  v2 <- view(v1)

  expect_identical(is_view(view_data(v2)), FALSE)

  expect_view_of(v1, x)
  expect_view_of(v2, x)
})

test_that("a view of a view drops its attributes", {
  x <- 1:4
  v1 <- rray_set_dimensions(x, c(2L, 2L))
  v2 <- view(v1)
  expect_null(attributes(v2))
  expect_identical(dim(v1), c(2L, 2L))
  expect_view_of(v2, x)
})

# ------------------------------------------------------------------------------
# Duplication

test_that("shallow duplicate of a view is a plain vector", {
  x <- 1:3
  v <- view(x)
  v2 <- v
  dim(v2) <- c(1L, 3L)
  expect_identical(is_view(v2), FALSE)
  expect_identical(v2, array(1:3, c(1L, 3L)))
  expect_null(dim(v))
  expect_view_of(v, x)
})

test_that("duplicating a view keeps the view's attributes", {
  x <- structure(
    1:6,
    dim = c(2L, 3L),
    dimnames = list(c("a", "b"), c("c", "d", "e"))
  )
  v <- rray_set_dimensions(x, c(3L, 2L))
  expect_view_of(v, x)

  expected <- array(1:6, c(3L, 2L))

  shallow <- rlang::duplicate(v, shallow = TRUE)
  expect_identical(is_view(shallow), FALSE)
  expect_identical(shallow, expected)

  deep <- rlang::duplicate(v, shallow = FALSE)
  expect_identical(is_view(deep), FALSE)
  expect_identical(deep, expected)

  expect_identical(attributes(v), attributes(expected))
  expect_identical(attr(x, "dimnames"), list(c("a", "b"), c("c", "d", "e")))
})

test_that("deep copy via c() produces a plain vector", {
  v <- view(1:3)
  d <- c(v)
  expect_false(is_view(d))
  expect_identical(d, 1:3)
})

# ------------------------------------------------------------------------------
# Serialization

test_that("serialization round-trips a view to a plain vector", {
  v <- rray_set_dimensions(1:5, 5L)
  expect_identical(is_view(v), TRUE)
  rt <- unserialize(serialize(v, NULL))
  expect_identical(is_view(rt), FALSE)
  expect_identical(as.integer(rt), 1:5)
  expect_identical(dim(rt), 5L)
})

test_that("serialization round-trips all types", {
  test_roundtrip <- function(x) {
    v <- view(x)
    rt <- unserialize(serialize(v, NULL))
    expect_false(is_view(rt))
    rt
  }
  expect_identical(test_roundtrip(c(TRUE, FALSE)), c(TRUE, FALSE))
  expect_identical(test_roundtrip(1:3), 1:3)
  expect_identical(test_roundtrip(c(1.5, 2.5)), c(1.5, 2.5))
  expect_identical(test_roundtrip(c(1 + 2i)), c(1 + 2i))
  expect_identical(test_roundtrip(as.raw(1:2)), as.raw(1:2))
  expect_identical(test_roundtrip(c("a", "b")), c("a", "b"))
  expect_identical(test_roundtrip(list(1, "a")), list(1, "a"))
})

# ------------------------------------------------------------------------------
# Zero-length

test_that("view works with zero-length vectors", {
  expect_true(is_view(view(integer())))
  expect_true(is_view(view(double())))
  expect_true(is_view(view(character())))
  expect_true(is_view(view(logical())))
  expect_true(is_view(view(complex())))
  expect_true(is_view(view(raw())))
  expect_true(is_view(view(list())))

  expect_length(view(integer()), 0L)
  expect_length(view(double()), 0L)
})

# ------------------------------------------------------------------------------
# Integration with rray_set_dimensions

test_that("rray_set_dimensions returns a view", {
  expect_true(is_view(rray_set_dimensions(1:6, c(2L, 3L))))
  expect_true(is_view(rray_set_dimensions(as.double(1:6), c(2L, 3L))))
  expect_true(is_view(rray_set_dimensions(letters[1:6], c(2L, 3L))))
  expect_true(is_view(rray_set_dimensions(c(TRUE, FALSE), c(1L, 2L))))
  expect_true(is_view(rray_set_dimensions(as.raw(1:2), c(1L, 2L))))
  expect_true(is_view(rray_set_dimensions(c(1i, 2i), c(1L, 2L))))
  expect_true(is_view(rray_set_dimensions(list("a", "b"), c(1L, 2L))))
})

test_that("rray_set_dimensions view shares the underlying data", {
  x <- 1:6
  y <- rray_set_dimensions(x, c(2L, 3L))
  expect_view_of(y, x)
})

test_that("modifying reshaped array does not affect original", {
  x <- 1:6
  out <- rray_set_dimensions(x, c(2L, 3L))
  out[1, 1] <- 99L
  expect_identical(x, 1:6)
})

# ------------------------------------------------------------------------------
# NA handling

test_that("view preserves NAs", {
  expect_identical(
    as.integer(view(c(1L, NA_integer_, 3L))),
    c(1L, NA_integer_, 3L)
  )
  expect_identical(as.double(view(c(1, NA_real_, 3))), c(1, NA_real_, 3))
  expect_identical(
    as.character(view(c("a", NA_character_))),
    c("a", NA_character_)
  )
  expect_identical(as.logical(view(c(TRUE, NA))), c(TRUE, NA))
  expect_identical(as.complex(view(c(1i, NA_complex_))), c(1i, NA_complex_))
})

# ------------------------------------------------------------------------------
# Subsetting

test_that("subsetting a view does not produce a view", {
  x <- view(1:10)
  expect_false(is_view(x[1:5]))
})

test_that("subsetting a view works", {
  v <- view(1:10)
  expect_identical(v[c(1, 5, 10)], c(1L, 5L, 10L))

  v <- view(1:10 + 0)
  expect_identical(v[c(1, 5, 10)], c(1, 5, 10))

  v <- view(as.raw(1:10))
  expect_identical(v[c(1, 5, 10)], as.raw(c(1, 5, 10)))

  v <- view(letters)
  expect_identical(v[1:3], c("a", "b", "c"))

  v <- view(list(1, "a", TRUE))
  expect_identical(v[1:2], list(1, "a"))
})
