# `rray_if_else()`

## Status

This is an implementation plan. The function is not yet implemented.

## Interface and behavior

```r
rray_if_else(condition, true, false, ..., missing = NULL)
```

`...` must be empty. Use the package's existing `check_dots_empty0(...)`
pattern. There is no R-side `ptype`, argument label, or error call parameter.
The C entry point takes `condition_arg`, `true_arg`, `false_arg`,
`missing_arg`, and `error_call` so other C code can supply its own labels and
call later.

`condition` must be an unclassed logical array or a plain logical vector.
Do not cast another type to logical. Normalize it with `arg_as_array()`, so a
plain vector becomes a one-dimensional array. Its dimensions determine the
result dimensions. It is never broadcast.

`true` and `false` must be unclassed arrays or plain vectors. `missing = NULL`
means that a missing condition produces a missing value of the result type.
When `missing` is supplied, it must also be an unclassed array or plain vector.
All supplied branches must broadcast to the dimensions of `condition`, even if
the condition does not select a branch. Each supplied branch also contributes
to the common result type, even if it is never selected. Use rray4's existing
common type and cast rules. Do not add a type override.

The result is a new array with the exact dimensions of `condition` and no
dimension names. Drop names from `condition` and every branch. For example:

```r
condition <- array(c(TRUE, FALSE, NA), c(3L, 1L))
rray_if_else(condition, 1L, 2, missing = 3L)
```

This returns a double array with dimensions `c(3L, 1L)` and values
`c(1, 2, 3)`.

## Implementation

1. Add `R/if-else.R` with the exported wrapper and roxygen documentation. The
   wrapper checks empty `...` and calls `ffi_rray_if_else` with `condition`,
   `true`, `false`, `missing`, and `environment()`. Add `rray_if_else` under
   Elementwise in `_pkgdown.yml`.

2. Add `src/if-else.c`, `src/if-else.h`, and `src/decl/if-else-decl.h`. Put
   `ffi_rray_if_else()` first, followed by `rray_if_else()` and its helpers in
   use order. Keep FFI declarations in `src/init.c`, register the five-argument
   `.Call`, and declare only the internal function in `src/if-else.h`. The
   internal signature accepts the four `struct rray_arg*` arguments and a
   `struct r_lazy error_call`. Prefix every FFI parameter with `ffi_`. The
   FFI uses `new_wrapper_arg()` for the four default input labels and the
   wrapper frame as the error call.

3. In the internal entry point, check that `condition` is unclassed and has
   logical storage. Normalize it with `arg_as_array()` and protect the result.
   Normalize and protect `true`, `false`, and any supplied `missing` with the
   same array rules. Reject `NULL` for `true` and `false`; treat only
   `missing = NULL` as absent.

4. Determine the common type with `rray_ptype2(true, false, ...)`, then fold
   in `missing` when supplied. Track which of `true` or `false` supplied the
   current type, and use that label in a type error against `missing`, as
   vctrs' `ptype_finalize()` does. Pass `error_call` at each step. Cast each
   supplied branch with `rray_cast()` to that type, protecting every cast
   result. Use rray4's supported types: logical, integer, double, complex,
   character, raw, and list. Existing common type rules decide which mixes
   are allowed.

5. Read the dimensions of `condition`. Check each supplied branch with
   `check_broadcastable()` against those exact dimensions, then compute its
   broadcast strides with `rray_fill_broadcast_strides_from_dimensions()`.
   Check all branches before allocating or filling the result. Do not call
   `rray_broadcast()` to create full-size copies.

6. Dispatch once on the common type. Allocate one result vector of the size
   of `condition` and attach the condition dimensions after filling. Keep
   `condition` as the flat, contiguous selector: output position `i` reads
   `condition[i]`. Use `rray_run_iterator_init2()` and `next2()` for `true` and
   `false` when `missing` is absent. When it is supplied, pass three stride
   arrays to `rray_run_iterator_init()` and advance with `next3()`. Output
   writes directly at `i`, so it needs no output stride. Zero stride handling
   applies to the branch inputs.

7. Within each iterator run, read the selected branch at its mapped location
   and advance that branch's location by its run stride. Hoist a branch value
   outside the inner loop when its stride is zero. Choose each run's zero
   stride path once, following `src/binary.h` and `src/broadcast.c`. Handle
   `TRUE`, `FALSE`, and `NA` explicitly. For `NA` with no `missing` branch,
   write the type's missing value, matching vctrs: logical and integer `NA`,
   double `NA_real_`, complex `NA_complex_`, character `NA_character_`, raw
   `00`, and list `NULL`. Use rlang's data access and element setters,
   including write barriers for character and list output. Do not copy names
   from a selected branch; array axis names cannot vary by element.

8. Protect every allocated `r_obj*` through the last call that can allocate.
   Perform the required separate protection review over each new or touched
   pointer. Run `clang-format -i src/*.c src/*.h`, `air format .`, and
   `devtools::document()` after implementation.

## Tests and checks

Add `tests/testthat/test-if-else.R`. Cover all three condition states with and
without `missing`, a plain logical vector yielding a one-dimensional array,
and a matrix or higher-dimensional condition preserving its shape. Verify that
condition and branch names are dropped. Test type promotion across `true`,
`false`, and `missing`, including a branch that is never selected, plus
incompatible types and a non-logical condition.

Test scalar branches, equal shapes, first-axis and later-axis broadcasting,
and three differently broadcast branches in one call. Verify errors for each
branch's incompatible dimensions and excess dimensionality, including an
unselected branch. Include zero-length conditions and branches, `NA` without
`missing` for every supported output type, and list and character values that
need write barriers. Snapshot only errors raised by rray4. Check that `...`
rejects supplied values.

Run `devtools::test(filter = '^if-else')`, then `devtools::test()` and
`pkgdown::check_pkgdown()`. The behavior and names must follow this plan even
where `vec_if_else()` uses vector names or one-dimensional recycling instead.
