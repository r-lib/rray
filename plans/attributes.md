# `r_wrap()` starts with no attributes

Today `r_wrap(x)` copies every attribute of `x` onto the new wrapper. This
plan makes the wrapper start with no attributes, so each caller sets exactly
the ones it wants.

The work is one pull request. It lands before `plans/view.md`, so this plan
still uses the current `wrapper` names.

---

# Why

## Stray attributes leak through

Functions that go through `r_wrap()` keep any attribute the input happens to
carry. Functions that allocate a new result drop them. So two reshapes of the
same input disagree:

```r
x <- array(1:4, c(2, 2))
attr(x, "foo") <- 1
names(x) <- letters[1:4]

attributes(rray_set_dimensions(x, c(4L, 1L)))
#> $dim: 4 1
#> $foo: 1
#> $names: "a" "b" "c" "d"

attributes(rray_permute_axes(x, 2:1))
#> $dim: 2 2
```

`rray_insert_axes()` keeps `foo` and `names` the same way. `class` can't leak,
because `check_unclassed()` rejects classed input first.

After this plan, every function above returns only `dim` and, when there are
any, `dimnames`.

## Every caller only wants `dim` and `dimnames`

All 9 callers of `r_wrap()` set `dim`, `dimnames`, or both, and nothing else.
Two of them spend extra work undoing the copy: `rray_set_dimensions()` clears
`dimnames`, and `vec_as_array()` clears `names`.

## No pairlist copy

`r_attrib_clone_from()` allocates a copy of the attribute pairlist on every
call. A wrapper with no attributes skips that.

## Nothing relies on the copy

No test outside `test-wrapper.R` checks that a stray attribute survives, and
no C code reads an attribute other than `dim` or `dimnames` off the result of
`arg_as_array()`.

---

# Out of scope

Some paths return `x` itself, so stray attributes still pass through them.
This plan leaves them alone. A later plan can decide whether they should also
return a view with only `dim` and `dimnames`:

- `vec_as_array()` when `x` already has `dim`.

- `rray_clear_titles()` and `rray_clear_axis_title()` when there are no
  titles to clear.

- `rray_cast()` when `x` and `to` have the same type, through
  `vec_as_array()`.

---

# A note on order

`Rf_setAttrib()` refuses to set `dimnames` on an object with no `dim`. Every
caller that sets `dimnames` must set `dim` first. The names and titles setters
used to get `dim` for free from the copy, so they now set it themselves from
`r_dim(x)`.

---

# Checklist

Work through it top to bottom. Each item is one change plus the test that
pins it down.

The test fixture used below is an array with a stray attribute and `names`:

```r
x <- array(1:4, c(2L, 2L))
attr(x, "foo") <- 1
names(x) <- letters[1:4]
```

Write it out in each test, since AGENTS.md wants tests to stand on their own.

## 1. `r_wrap()` in `src/wrapper.c`

- [ ] Delete the `r_attrib_clone_from(out, x)` line, and the comment attached
      to it ("Creates a fresh attribute pairlist! Note that we always use `x`
      here, not `data`."). The comment goes because its code goes.

- [ ] Leave every other comment in `r_wrap()` as it is. `plans/view.md`
      deals with them.

## 2. `rray_set_dimensions()` in `src/dimensions.c`

- [ ] Delete `r_attrib_poke_dim_names(out, r_null)`. The wrapper has no
      `dimnames` to clear. Keep `r_attrib_poke_dim(out, dimensions)`.

- [ ] Test in `tests/testthat/test-dimensions.R`, next to "drops names from a
      named vector":

      ```r
      test_that("drops other attributes", {
        x <- array(1:4, c(2L, 2L))
        attr(x, "foo") <- 1
        names(x) <- letters[1:4]
        expect_identical(rray_set_dimensions(x, c(4L, 1L)), array(1:4, c(4L, 1L)))
      })
      ```

## 3. `rray_expand_dimensionality()` in `src/dimensionality.c`

- [ ] No code change. It already sets `dim`, then `dimnames` when `x` has
      any.

- [ ] No new test. It is internal, and its only caller is `rray_stack()`,
      which combines the pieces into a new array. Stray attributes already
      never reach the result of `rray_stack()`, so there is nothing new to
      observe.

## 4. `rray_insert_axes()` in `src/insert-axes.c`

- [ ] No code change. It already sets `dim`, then `dimnames` when there are
      any.

- [ ] Test in `tests/testthat/test-insert-axes.R`, next to "does not modify
      the input":

      ```r
      test_that("drops other attributes", {
        x <- array(1:4, c(2L, 2L))
        attr(x, "foo") <- 1
        names(x) <- letters[1:4]
        expect_identical(rray_insert_axes(x, 1L), array(1:4, c(1L, 2L, 2L)))
      })
      ```

## 5. `rray_remove_axes()` in `src/remove-axes.c`

- [ ] No code change, for the same reason as `rray_insert_axes()`.

- [ ] Test in `tests/testthat/test-remove-axes.R`, next to "does not modify
      the input". Start from a `c(1L, 2L, 2L)` array, so there is an axis to
      remove:

      ```r
      test_that("drops other attributes", {
        x <- array(1:4, c(1L, 2L, 2L))
        attr(x, "foo") <- 1
        names(x) <- letters[1:4]
        expect_identical(rray_remove_axes(x, 1L), array(1:4, c(2L, 2L)))
      })
      ```

## 6. `rray_set_names()` in `src/names.c`

- [ ] Add `r_attrib_poke_dim(out, r_dim(x))` right after `r_wrap(x)`, before
      `r_attrib_poke_dim_names(out, names)`. `x` has `dim`, since it went
      through `arg_as_array()`.

- [ ] Test in `tests/testthat/test-names.R`, next to "rray_set_names()
      doesn't touch the data". Cover both a list of names and `NULL`:

      ```r
      test_that("rray_set_names() drops other attributes", {
        x <- array(1:4, c(2L, 2L))
        attr(x, "foo") <- 1
        names(x) <- letters[1:4]

        names <- list(c("a", "b"), c("c", "d"))
        expect_identical(
          rray_set_names(x, names),
          array(1:4, c(2L, 2L), dimnames = names)
        )
        expect_identical(rray_set_names(x, NULL), array(1:4, c(2L, 2L)))
      })
      ```

## 7. `rray_set_axis_names()` in `src/names.c`

This one also backs `rray_set_row_names()` and `rray_set_column_names()`.

- [ ] Add `r_attrib_poke_dim(out, r_dim(x))` right after `r_wrap(x)`, before
      `r_attrib_poke_dim_names(out, new_names)`.

- [ ] Test in `tests/testthat/test-names.R`, next to "rray_set_axis_names()
      leaves other axes untouched":

      ```r
      test_that("rray_set_axis_names() drops other attributes", {
        x <- array(1:4, c(2L, 2L))
        attr(x, "foo") <- 1
        names(x) <- letters[1:4]
        expect_identical(
          rray_set_axis_names(x, 1L, c("a", "b")),
          array(1:4, c(2L, 2L), dimnames = list(c("a", "b"), NULL))
        )
      })
      ```

## 8. `rray_set_titles()` in `src/titles.c`

This one also backs `rray_set_axis_title()`, which calls it.

- [ ] Add `r_attrib_poke_dim(out, r_dim(x))` right after `r_wrap(x)`, before
      `r_attrib_poke_dim_names(out, new_names)`.

- [ ] Test in `tests/testthat/test-titles.R`, next to "title setters keep
      element names and do not change the input". Cover both setters:

      ```r
      test_that("title setters drop other attributes", {
        x <- array(1:4, c(2L, 2L))
        attr(x, "foo") <- 1
        names(x) <- letters[1:4]

        expect_identical(
          rray_set_titles(x, c("a", "b")),
          array(1:4, c(2L, 2L), dimnames = list(a = NULL, b = NULL))
        )
        expect_identical(
          rray_set_axis_title(x, 1L, "a"),
          array(1:4, c(2L, 2L), dimnames = list(a = NULL, NULL))
        )
      })
      ```

## 9. `rray_clear_titles()` in `src/titles.c`

This is the `titles = NULL` path of `rray_set_titles()`, when there are
titles to clear.

- [ ] Add `r_attrib_poke_dim(out, r_dim(x))` right after `r_wrap(x)`, before
      `r_attrib_poke_dim_names(out, new_names)`.

- [ ] Add a case to the test from step 8. Start from `x` with titles, so the
      clear actually happens:

      ```r
      x <- array(1:4, c(2L, 2L), dimnames = list(a = c("r1", "r2"), b = NULL))
      attr(x, "foo") <- 1
      expect_identical(
        rray_set_titles(x, NULL),
        array(1:4, c(2L, 2L), dimnames = list(c("r1", "r2"), NULL))
      )
      ```

## 10. `vec_as_array()` in `src/utils.c`

- [ ] Delete `r_attrib_zap(out, r_syms.names)`. The wrapper never had
      `names`. Keep the rest: `dim` from the length, then `dimnames` from
      `r_names(x)`.

- [ ] Leave the comment above `vec_as_array()` as it is. "Clears `names`"
      still describes the result.

- [ ] Test in `tests/testthat/test-cast.R`, next to "a bare vector becomes a
      one dimensional array". This is the path where `vec_as_array()` hands
      its result straight back to the user:

      ```r
      test_that("a bare vector drops other attributes", {
        x <- structure(c(a = 1L, b = 2L), foo = 1)
        expect_identical(
          rray_cast(x, integer()),
          array(1:2, 2L, dimnames = list(c("a", "b")))
        )
      })
      ```

## 11. `tests/testthat/test-wrapper.R`

- [ ] Replace "wrapper clones attributes from the original" with:

      ```r
      test_that("wrapper starts with no attributes", {
        x <- array(1:4, c(2L, 2L), dimnames = list(c("a", "b"), NULL))
        attr(x, "foo") <- 1
        w <- wrap(x)
        expect_null(attributes(w))
        expect_wraps(w, x)
      })
      ```

- [ ] Delete "wrapper gets its own attribute pairlist". A wrapper has no
      pairlist to share, and "modifying dim on wrapper does not affect
      original" already checks that changing `w` leaves `x` alone.

- [ ] Keep "modifying dim on wrapper does not affect original" as it is.

- [ ] "read-only access to a dimensioned wrapper shares data" sets `dim` on
      `x` before wrapping, so the wrapper is no longer dimensioned. Build the
      wrapper with `rray_set_dimensions(x, c(1L, 2L))` instead, and drop the
      `dim(x) <-` line.

- [ ] Replace "wrapping a wrapper uses the wrapper's attributes" and
      "wrapping a wrapper gives independent attributes" with one test. The
      old test built `w1` with `structure()`, which copies the wrapper into a
      plain vector, so it never wrapped a wrapper at all:

      ```r
      test_that("wrapping a wrapper drops its attributes", {
        x <- 1:4
        w1 <- rray_set_dimensions(x, c(2L, 2L))
        w2 <- wrap(w1)
        expect_null(attributes(w2))
        expect_identical(dim(w1), c(2L, 2L))
        expect_wraps(w2, x)
      })
      ```

- [ ] "serialization round-trips wrapper to plain vector" sets an attribute
      with `attr(w, "custom") <-`, which also copies the wrapper first. Build
      the wrapper with `rray_set_dimensions(1:5, 5L)` instead, and check that
      `dim` survives the round trip in place of `custom`. Add
      `expect_identical(is_wrapper(w), TRUE)` before serializing, so the test
      fails if `w` stops being a wrapper.

- [ ] Every other test wraps a vector with no attributes, or goes through
      `rray_set_dimensions()`. Leave them as they are.

## 12. `tests/testthat/test-wrapper-mutations.R`

- [ ] No change. The battery builds its wrapper with `rray_set_dimensions()`,
      which still sets `dim`.

---

# Checks

1. `clang-format -i src/*.c src/*.h`, over all files.

2. `air format .`

3. `Rscript -e "devtools::test()"`, all green. If any test outside the ones
   listed above fails, it relied on a stray attribute leaking through. Stop
   and report it rather than changing the expectation.

4. The protection pass from AGENTS.md, as a separate step. The new
   `r_attrib_poke_dim(out, r_dim(x))` lines read `dim` off `x`, which is
   protected, and `Rf_setAttrib()` protects its arguments.

5. No new comments anywhere in `src/` or `tests/`.

6. `Rscript -e "devtools::check()"`.
