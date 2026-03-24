# ------------------------------------------------------------------------------
# wrap() basics

test_that("wrapping produces an ALTREP wrapper", {
  expect_true(is_wrapper(wrap(1L)))
  expect_true(is_wrapper(wrap(1)))
  expect_true(is_wrapper(wrap("a")))
  expect_true(is_wrapper(wrap(TRUE)))
  expect_true(is_wrapper(wrap(1i)))
  expect_true(is_wrapper(wrap(as.raw(1))))
  expect_true(is_wrapper(wrap(list(1))))
})

test_that("non-wrappers are detected", {
  expect_false(is_wrapper(1L))
  expect_false(is_wrapper(1))
  expect_false(is_wrapper("a"))
  expect_false(is_wrapper(TRUE))
  expect_false(is_wrapper(NULL))
})

test_that("wrapper preserves length", {
  expect_length(wrap(1:5), 5L)
  expect_length(wrap(c(1.5, 2.5, 3.5)), 3L)
  expect_length(wrap(c("a", "b")), 2L)
  expect_length(wrap(c(TRUE, FALSE, TRUE)), 3L)
  expect_length(wrap(complex(3)), 3L)
  expect_length(wrap(raw(4)), 4L)
  expect_length(wrap(list(1, 2)), 2L)
})

test_that("wrapper preserves type", {
  expect_type(wrap(1L), "integer")
  expect_type(wrap(1), "double")
  expect_type(wrap("a"), "character")
  expect_type(wrap(TRUE), "logical")
  expect_type(wrap(1i), "complex")
  expect_type(wrap(as.raw(1)), "raw")
  expect_type(wrap(list(1)), "list")
})

# ------------------------------------------------------------------------------
# Attributes

test_that("wrapper clones attributes from the original", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  w <- wrap(x)
  expect_identical(dim(w), dim(x))
  expect_identical(dimnames(w), dimnames(x))
})

test_that("wrapper gets its own attribute pairlist", {
  x <- c(a = 1L, b = 2L)
  w <- wrap(x)
  names(w) <- c("x", "y")
  expect_identical(names(x), c("a", "b"))
  expect_identical(names(w), c("x", "y"))
})

test_that("modifying dim on wrapper does not affect original", {
  x <- array(1:6, c(2L, 3L))
  w <- wrap(x)
  dim(w) <- c(3L, 2L)
  expect_identical(dim(x), c(2L, 3L))
  expect_identical(dim(w), c(3L, 2L))
})

# ------------------------------------------------------------------------------
# Read access

test_that("wrapper provides read access to integer data", {
  x <- 1:5
  w <- wrap(x)
  expect_identical(w[1], 1L)
  expect_identical(w[2:4], 2:4)
})

test_that("wrapper provides read access to double data", {
  x <- c(1.5, 2.5, 3.5)
  w <- wrap(x)
  expect_identical(w[1], 1.5)
  expect_identical(w[3], 3.5)
})

test_that("wrapper provides read access to logical data", {
  x <- c(TRUE, FALSE, NA)
  w <- wrap(x)
  expect_identical(w[1], TRUE)
  expect_identical(w[2:3], c(FALSE, NA))
})

test_that("wrapper provides read access to complex data", {
  x <- c(1 + 2i, 3 + 4i)
  w <- wrap(x)
  expect_identical(w[1], 1 + 2i)
  expect_identical(w[2], 3 + 4i)
})

test_that("wrapper provides read access to raw data", {
  x <- as.raw(1:3)
  w <- wrap(x)
  expect_identical(w[1], as.raw(1))
  expect_identical(w[2:3], as.raw(c(2, 3)))
})

test_that("wrapper provides read access to character data", {
  x <- c("a", "b", "c")
  w <- wrap(x)
  expect_identical(w[1], "a")
  expect_identical(w[3], "c")
})

test_that("wrapper provides read access to list data", {
  x <- list("a", 1L, TRUE)
  w <- wrap(x)
  expect_identical(w[[1]], "a")
  expect_identical(w[2:3], list(1L, TRUE))
})

# ------------------------------------------------------------------------------
# Ownership

test_that("wrapper starts out sharing data", {
  x <- 1:3
  w <- wrap(x)
  expect_identical_addresses(wrapper_readonly(w), x)
})

test_that("write access triggers ownership via writable dataptr", {
  x <- 1:3
  w <- wrap(x)
  w[1] <- 99L
  expect_different_addresses(wrapper_readonly(w), x)
  expect_identical(w[1], 99L)
  expect_identical(x, 1:3)

  x <- c(1, 2, 3)
  w <- wrap(x)
  w[1] <- 99
  expect_different_addresses(wrapper_readonly(w), x)
  expect_identical(w[1], 99)
  expect_identical(x, c(1, 2, 3))

  x <- c(TRUE, FALSE, TRUE)
  w <- wrap(x)
  w[1] <- FALSE
  expect_different_addresses(wrapper_readonly(w), x)
  expect_identical(w[1], FALSE)
  expect_identical(x, c(TRUE, FALSE, TRUE))

  x <- c(1 + 0i, 2 + 0i)
  w <- wrap(x)
  w[1] <- 99 + 0i
  expect_different_addresses(wrapper_readonly(w), x)
  expect_identical(w[1], 99 + 0i)
  expect_identical(x, c(1 + 0i, 2 + 0i))

  x <- as.raw(1:3)
  w <- wrap(x)
  w[1] <- as.raw(99)
  expect_different_addresses(wrapper_readonly(w), x)
  expect_identical(w[1], as.raw(99))
  expect_identical(x, as.raw(1:3))
})

test_that("write access triggers ownership via set_elt", {
  x <- c("a", "b", "c")
  w <- wrap(x)
  w[1] <- "z"
  expect_different_addresses(wrapper_readonly(w), x)
  expect_identical(w[1], "z")
  expect_identical(x, c("a", "b", "c"))

  x <- list("a", "b")
  w <- wrap(x)
  w[[1]] <- "z"
  expect_different_addresses(wrapper_readonly(w), x)
  expect_identical(w[[1]], "z")
  expect_identical(x, list("a", "b"))
})

test_that("second write does not re-clone via writable dataptr", {
  x <- 1:3
  w <- wrap(x)

  w[1] <- 10L
  x_clone_address <- obj_address(wrapper_readonly(w))
  expect_false(x_clone_address == obj_address(x))

  w[2] <- 20L
  x_clone_address2 <- obj_address(wrapper_readonly(w))
  expect_true(x_clone_address == x_clone_address2)
})

test_that("second write does not re-clone via set_elt", {
  x <- as.list(1:3)
  w <- wrap(x)

  w[1] <- list(10L)
  x_clone_address <- obj_address(wrapper_readonly(w))
  expect_false(x_clone_address == obj_address(x))

  w[2] <- list(20L)
  x_clone_address2 <- obj_address(wrapper_readonly(w))
  expect_true(x_clone_address == x_clone_address2)
})

# ------------------------------------------------------------------------------
# Wrapping a wrapper

test_that("wrapping a wrapper does not create nested wrappers", {
  x <- 1:3
  w1 <- wrap(x)
  w2 <- wrap(w1)

  expect_false(is_wrapper(wrapper_readonly(w2)))

  expect_identical(
    obj_address(x),
    obj_address(wrapper_readonly(w1))
  )
  expect_identical(
    obj_address(wrapper_readonly(w1)),
    obj_address(wrapper_readonly(w2))
  )
})

test_that("wrapping a wrapper uses the wrapper's attributes", {
  # (even though it actually wraps `x`)

  x <- structure(1:3, foo = "bar")

  w1 <- structure(wrap(x), a = "b")
  expect_mapequal(attributes(w1), list(foo = "bar", a = "b"))

  w2 <- wrap(w1)
  expect_mapequal(attributes(w2), list(foo = "bar", a = "b"))
})

test_that("wrapping a wrapper gives independent attributes", {
  x <- c(a = 1L, b = 2L, c = 3L)
  w1 <- wrap(x)
  w2 <- wrap(w1)
  names(w1) <- c("x", "y", "z")
  expect_identical(names(w2), c("a", "b", "c"))
})

# ------------------------------------------------------------------------------
# Duplication

test_that("shallow duplicate of wrapper produces another wrapper", {
  w <- wrap(1:3)
  w2 <- w
  dim(w2) <- c(1L, 3L)
  expect_true(is_wrapper(w2))
  expect_null(dim(w))
})

test_that("deep copy via c() produces a regular non-wrapper vector", {
  w <- wrap(1:3)
  d <- c(w)
  expect_false(is_wrapper(d))
  expect_identical(d, 1:3)
})

# ------------------------------------------------------------------------------
# Serialization

test_that("serialization round-trips wrapper to plain vector", {
  w <- wrap(1:5)
  attr(w, "custom") <- "hello"
  rt <- unserialize(serialize(w, NULL))
  expect_false(is_wrapper(rt))
  expect_identical(as.integer(rt), 1:5)
  expect_identical(attr(rt, "custom"), "hello")
})

test_that("serialization round-trips all types", {
  test_roundtrip <- function(x) {
    w <- wrap(x)
    rt <- unserialize(serialize(w, NULL))
    expect_false(is_wrapper(rt))
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

test_that("wrapper works with zero-length vectors", {
  expect_true(is_wrapper(wrap(integer())))
  expect_true(is_wrapper(wrap(double())))
  expect_true(is_wrapper(wrap(character())))
  expect_true(is_wrapper(wrap(logical())))
  expect_true(is_wrapper(wrap(complex())))
  expect_true(is_wrapper(wrap(raw())))
  expect_true(is_wrapper(wrap(list())))

  expect_length(wrap(integer()), 0L)
  expect_length(wrap(double()), 0L)
})

# ------------------------------------------------------------------------------
# Integration with rray_reshape

test_that("rray_reshape returns a wrapper", {
  expect_true(is_wrapper(rray_reshape(1:6, c(2L, 3L))))
  expect_true(is_wrapper(rray_reshape(as.double(1:6), c(2L, 3L))))
  expect_true(is_wrapper(rray_reshape(letters[1:6], c(2L, 3L))))
  expect_true(is_wrapper(rray_reshape(c(TRUE, FALSE), c(1L, 2L))))
  expect_true(is_wrapper(rray_reshape(as.raw(1:2), c(1L, 2L))))
  expect_true(is_wrapper(rray_reshape(c(1i, 2i), c(1L, 2L))))
  expect_true(is_wrapper(rray_reshape(list("a", "b"), c(1L, 2L))))
})

test_that("rray_reshape wrapper shares the underlying data", {
  x <- 1:6
  y <- rray_reshape(x, c(2L, 3L))
  expect_identical(obj_address(x), obj_address(wrapper_readonly(y)))
})

test_that("modifying reshaped array does not affect original", {
  x <- 1:6
  out <- rray_reshape(x, c(2L, 3L))
  out[1, 1] <- 99L
  expect_identical(x, 1:6)
})

# ------------------------------------------------------------------------------
# NA handling

test_that("wrapper preserves NAs", {
  expect_identical(
    as.integer(wrap(c(1L, NA_integer_, 3L))),
    c(1L, NA_integer_, 3L)
  )
  expect_identical(as.double(wrap(c(1, NA_real_, 3))), c(1, NA_real_, 3))
  expect_identical(
    as.character(wrap(c("a", NA_character_))),
    c("a", NA_character_)
  )
  expect_identical(as.logical(wrap(c(TRUE, NA))), c(TRUE, NA))
  expect_identical(as.complex(wrap(c(1i, NA_complex_))), c(1i, NA_complex_))
})

# ------------------------------------------------------------------------------
# Subsetting

test_that("subsetting a wrapper does not produce a wrapper", {
  x <- wrap(1:10)
  expect_false(is_wrapper(x[1:5]))
})

test_that("subsetting a wrapper works", {
  w <- wrap(1:10)
  expect_identical(w[c(1, 5, 10)], c(1L, 5L, 10L))

  w <- wrap(1:10 + 0)
  expect_identical(w[c(1, 5, 10)], c(1, 5, 10))

  w <- wrap(as.raw(1:10))
  expect_identical(w[c(1, 5, 10)], as.raw(c(1, 5, 10)))

  w <- wrap(letters)
  expect_identical(w[1:3], c("a", "b", "c"))

  w <- wrap(list(1, "a", TRUE))
  expect_identical(w[1:2], list(1, "a"))
})
