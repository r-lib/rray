# rray4 implementation plan

## What this is

rray4 is a reimagining of rray, written in pure C with no xtensor. It is a
toolkit of `rray_*()` functions that work on base R arrays, plus an extension
system that lets other packages plug their own array classes in.

There is no rray class. rray4 ships functions, not a type.

This document is the guide for agents working on rray4 across many sessions.
Part 4 is the ordered list of pull requests. Part 5 is the catalogue of
functions to work through after the foundations land.

## How to use this plan

Read Part 1 and Part 2 before starting any pull request. They are short on
purpose.

Then find your pull request in Part 4, or your function in Part 5. Each entry
says what to build, which files to touch, and what "done" means.

One pull request per entry. Keep them small. Use the `gh-stack` skill when
several depend on each other.

---

# Part 1: Conventions

Terminology (size, axis, dimension, dimensions, dimensionality) is defined in
`CLAUDE.md`. Use it exactly.

## Files

Each feature is a `src/{name}.c` and `src/{name}.h` pair, with an R file at
`R/{name}.R` and tests at `tests/testthat/test-{name}.R`.

Functions that need a per type implementation get a `src/{name}-template.h`
included once per type from `src/{name}.c`. Copy the shape of
`src/broadcast-template.h`.

Private declarations go in `src/decl/{name}-decl.h`.

## Naming

- Internal C: `rray_{name}()`, returns C types.

- FFI wrappers: `ffi_rray_{name}()`, thin SEXP bridges, placed above the
  internal functions in the `.c` file.

- Headers declare internal functions only, never FFI wrappers.

- `src/init.c` uses `extern` declarations, it does not include feature headers.

## C style

Follow vctrs and rlang closely. Prefer an rlang wrapper over the raw R API
every time (`r_globals.na_int`, `r_length()`, `r_attrib_get()`).

Prefer `r_ssize` over `int`. Mark variables `const` where you can. Grab a data
pointer before a loop rather than indexing the `r_obj*`.

Protect anything that allocates with `KEEP()` and `FREE()`.

Never touch `src/rlang/`.

Run `clang-format -i src/*.c src/*.h` over all files after any C change.

## Comments

No comments in C or R code, other than roxygen2 on exported R functions. This
overrides the usual defaults.

## R style

Run `air format .` after any R change. Use `|>`, not `%>%`. Use `\() ...` for
one line anonymous functions and `function() {...}` otherwise. The package must
work on R 4.1, so no `_$x`.

## Prose

Plain words, short sentences. Show a small example instead of writing a
paragraph. No em dashes.

---

# Part 2: The core ideas

## 2.1 Proxy and restore

The proxy system is the boundary between an R object and the C implementation.

```
x    <- rray_proxy(x)      # a native, unclassed array, or an error
out  <- <C implementation>
out  <- rray_restore(out, x)
```

A **native** type is one of the seven R vector types: logical, integer, double,
complex, raw, character, list.

### The proxy contract

`rray_proxy(x)` returns an unclassed array of a native type that has the **same
dimensions and the same names** as `x`.

Because of that, `rray_dimensions()`, `rray_names()`, `rray_size()` and
`rray_dimensionality()` are all "take the proxy, then read an attribute".

A bare vector proxies to a one dimensional array. `rray_proxy(1:5)` is
`array(1:5, 5L)`, and any `names` move to `dimnames`.

That means **arrays go in and arrays come out**, everywhere. `rray_sum(1:5, 1)`
returns `array(15L, 1L)`, not `15L`. We lean into this rather than trying to
hide it.

### The restore contract

`rray_restore(x, to)` takes the **class and type defining attributes from
`to`**, and the **dimensions and names from `x`**.

Restore must never assume anything about `to`'s dimensions. It is routinely
called with a `to` that has a dimension of 0 while `x` has real data.

### Dispatch

Both are generics that dispatch on `class(x)[[1]]` only, with **no
inheritance**, exactly like vctrs. Methods are ordinary S3 methods registered
with `S3method()`, so a user writes:

```r
rray_proxy.my_array <- function(x) { ... }
rray_restore.my_array <- function(x, to) { ... }
```

Dispatch happens in C, with a fast path that skips it entirely when `x` has no
class attribute.

### What we do not ship

No built in methods for factor, Date, POSIXct or difftime. Those are not
arrays.

## 2.2 The type system

Modelled on vctrs, with manual double dispatch and no inheritance.

### Ptypes

A ptype describes type and nothing else. It has no relationship to dimensions.

```r
rray_ptype(array(1, c(2, 3)))
#> <double array, dim 0>

rray_ptype(array(1, c(4, 5, 6)))
#> the same object
```

Concretely, the ptype of any double array is `structure(double(), dim = 0L)`.
Dimensionality 1, dimension 0, no names. The native ptypes are shared static
objects.

`rray_ptype()` proxies, computes the native ptype, and restores, so a ptype
**carries the class**. That is what gives `rray_ptype2()` something to dispatch
on.

### The invariant that matters

> A ptype must fully determine storage. `rray_proxy(rray_ptype(x))` must give a
> zero length native array of exactly the storage type that `x` uses.

For a class with fixed storage, like a `dollars_array` that is always integer
backed, this is free.

For a class that is generic over storage, it means there is **no single ptype**
for that class. There is an integer backed one and a double backed one, and they
are different types. That is the honest answer, and once it holds everything
downstream works.

This is documented, not checked.

### Ptype2 and cast

```r
rray_ptype2(x, y)        # methods: rray_ptype2.<x_class>.<y_class>
rray_cast(x, to)         # methods: rray_cast.<to_class>.<x_class>
rray_ptype_common(..., .ptype = NULL)
rray_cast_common(..., .ptype = NULL)
```

`rray_cast(x, to)` means "cast `x` to the type of `to`". It changes type only.
It **never** changes dimensions or names. Broadcasting is always a separate
step.

`.ptype` is run through `rray_ptype()` first, so `.ptype = double()` works and
means "a double array".

There is **no fallback method of any kind**. Not even for identical classes. If
`rray_ptype2.foo.foo` is missing, that is an error, and the message says which
method is missing. This is stricter than vctrs on purpose, because a class that
is generic over storage must not have integer backed and double backed variants
silently collapsed.

There is no unspecified type. `rray_add(x, NA)` does not work today. Document
that gap. We may need it later.

Native rules, implemented in C, with a fast path when neither side has a class:

- Numeric tower: `lgl` to `int` to `dbl` to `cpl`.

- `chr`, `list` and `raw` each stand alone. They coerce only with themselves.

Lossy casts error, and the error says what was lost.

### Writing methods for a class

Two recipes, both worth putting in the documentation.

Fixed storage, where nothing may be inferred:

```r
rray_ptype2.dollars_array.dollars_array <- function(x, y) x
```

Generic over storage, deferring to the native rule by proxying and recursing:

```r
rray_ptype2.wrapper_array.wrapper_array <- function(x, y) {
  ptype <- rray_ptype2(rray_proxy(x), rray_proxy(y))
  rray_restore(ptype, x)
}
```

The proxies are bare native arrays, so the recursion lands on the native method
and terminates. This "proxy and recurse" shape is the one pattern to learn, and
it shows up again in 2.3.

## 2.3 Arithmetic and reduction types

The type system alone cannot answer "what type does this op work in", because
`lgl + lgl` is `int` and `int / int` is `dbl`. So there are three more hooks.

| family | hook | dispatch | ops |
|---|---|---|---|
| binary elementwise | `rray_arithmetic_ptype2(op, x, y)` | double | `+ - * / ^ %% %/%`, `maximum`, `minimum`, `hypot` |
| unary elementwise | `rray_arithmetic_ptype(op, x)` | single | unary `-` |
| reduction | `rray_reduction_ptype(op, x)` | single | `sum`, `prod`, `mean`, `max`, `min` |

Each returns **one ptype**, used both to cast the inputs and to restore the
output. That works because we always promote before computing, so the input type
and the output type are the same.

`op` is a single string from a closed vocabulary that rray4 ships. Users cannot
invent new ops.

### The pipeline

```
p    <- rray_arithmetic_ptype2(op, x, y)
x    <- rray_cast(x, p)
y    <- rray_cast(y, p)
px   <- rray_proxy(x)
py   <- rray_proxy(y)
dims <- rray_dimensions_common(px, py)
out  <- <C loop over two broadcast iterators>
out  <- rray_restore(out, p)
```

Unary and reduction ops use the same pipeline with one input.

### Native promotion tables

For binary ops, read these as "apply `rray_ptype2()` first, then promote".

Binary elementwise:

| op | lgl | int | dbl | cpl |
|---|---|---|---|---|
| `+` `-` `*` | int | int | dbl | cpl |
| `/` `^` | dbl | dbl | dbl | cpl |
| `%%` `%/%` | int | int | dbl | error |
| `maximum` `minimum` | int | int | dbl | error |
| `hypot` | dbl | dbl | dbl | error |

Unary elementwise:

| op | lgl | int | dbl | cpl |
|---|---|---|---|---|
| `-` | int | int | dbl | cpl |

Reduction:

| op | lgl | int | dbl | cpl |
|---|---|---|---|---|
| `sum` | int | int | dbl | cpl |
| `prod` | dbl | dbl | dbl | cpl |
| `mean` | dbl | dbl | dbl | cpl |
| `max` `min` | int | int | dbl | error |

`sum` on an integer array stays integer and errors on overflow. `prod` promotes
to double, matching base R's `prod()`, because integer products overflow almost
immediately.

### Ops that are not covered

An op whose output type is fixed regardless of its input does not use these
hooks at all. It casts its inputs with plain `rray_ptype2()`, computes, and
returns a **bare** array with no class restored.

- Comparison (`rray_equal()` and friends) returns a bare logical array.

- `rray_all()` and `rray_any()` take logical and return a bare logical array.

- `rray_max_pos()` and `rray_min_pos()` return a bare integer array.

A comparison of two `foo_array`s is a logical array, not a `foo_array`. That is
the rule, stated once: **these hooks are only for ops whose output type equals
their input type.**

### Writing methods

Same two recipes as 2.2. Fixed storage enumerates ops:

```r
rray_arithmetic_ptype2.dollars_array.dollars_array <- function(op, x, y) {
  switch(
    op,
    "+" = ,
    "-" = ,
    "%%" = ,
    "%/%" = x,
    "/" = double(),
    stop_unsupported_op(op)
  )
}
```

Note `"/"` returning a bare `double()`. Both sides get cast to a plain double
array and the result has no class, because a ratio of dollars is a plain number.
A method **may** return a bare ptype, dropping its own class. That is
intentional.

Generic over storage proxies and recurses, exactly as in 2.2.

Because there is no fallback, a class that has not thought about arithmetic
simply errors. That is what protects `dollars_array` from silent promotion.

The cost, same as vctrs: interoperating with bare arrays needs methods against
them too, so `rray_add(dollars, 1L)` needs
`rray_arithmetic_ptype2.dollars_array.integer`.

## 2.4 Broadcasting

Broadcasting is independent of type, the same way recycling is.

```r
rray_dimensions(x)
rray_dimensions_common(..., .dimensions = NULL)
rray_broadcast(x, dimensions)
rray_broadcast_common(..., .dimensions = NULL)
```

Rules, per axis:

- Equal dimensions are compatible.

- A dimension of 1 broadcasts to anything, including 0.

- Anything else is an error, with a message naming the axis and both dimensions.

- Missing trailing axes are treated as a dimension of 1, so dimensionality can
  grow.

Dimensionality can never shrink. `rray_broadcast()` errors on any decrease, even
when the dimensions being dropped are all 1.

`RRAY_MAX_DIMENSIONALITY` is 64.

Long arrays are supported where it is not painful. R allows an array's total
length to exceed 2^31 while every individual dimension stays under it, because
`dim` is always integer. That is why `rray_size()` returns a double while
`rray_dimensions()` returns integer. Use `r_ssize` throughout. If long array
support makes something genuinely hard, drop it and say so in the pull request.

## 2.5 Names

`rray_names(x)` returns the `dimnames` attribute as it is. A one dimensional
array with names returns a one element list. No normalization on read.

Setters take a list of length exactly the dimensionality, where each element is
either `NULL` or a character vector of exactly that axis' dimension. `NULL`
clears everything. Short lists are not padded, they are an error.

There are no `<-` replacement forms for now.

### The three names rules

**Broadcast.** An axis keeps its names if its dimension is unchanged, and loses
them if its dimension changed. New trailing axes have no names. An axis of
dimension 1 broadcast to 1 counts as unchanged and keeps its length 1 names.

**Reduce.** Reduced axes lose their names. Every other axis keeps them.

**Coalesce**, used by binary ops. For each axis of the common dimensions:

1. Use `x`'s names if `x`'s dimension there equals the common dimension and `x`
   has names there.

2. Otherwise apply the same test to `y`.

3. Otherwise `NULL`.

This lives in an internal `rray_names_common()`. It is not exported for now.

The reasoning for the split: a function that manipulates an existing array is
still functionally the same array, so names travel with the axes that did not
move. A binary op builds a genuinely new array, but dropping every name would
make `rray_add(named_matrix, 1)` lose its names, which reads as a bug.

## 2.6 Iterators

`struct rray_iterator` walks a multidimensional space one step at a time and
reports a 1D location in a possibly different space. It is what lets us step
over broadcast arrays without ever materialising them.

Two initialisers today:

- `rray_broadcast_iterator_init()`. Walks the view space, reports a location in
  the input space. Size 1 dimensions contribute no stride, which is what makes
  broadcasting free.

- `rray_reduction_iterator_init()`. Walks the input space, reports a location in
  the output space, where reduced axes have a dimension of 1.

Accessors are `rray_iterator_location()` and, once PR 4 lands,
`rray_iterator_point()`.

Binary ops use two plain iterators stepped side by side. There is no binary
iterator type.

Invent a new iterator only when a function genuinely cannot be expressed with
these. Say so explicitly in the pull request when you do.

---

# Part 3: Testing

vctrs is the standard to aim for: minimally exhaustive. Few tests, each pulling
its weight, covering the corners that actually break.

Every function pull request covers:

- **Every native type it supports.** If a function claims to work on all seven,
  test all seven.

- **Zero size arrays.** A dimension of 0 on some axis, and on every axis.

- **Dimensionality 1, 2, and 3 or more.** One dimensional arrays are the case
  people forget.

- **Names.** Kept where the rule says kept, dropped where the rule says dropped.
  Include the case where only some axes have names.

- **Errors.** Every error path, with `expect_snapshot(error = TRUE)`, so the
  full message is reviewable.

- **The proxy path**, once it exists. A small test class defined in a
  `helper-*.R` file, exercised through the function.

Mechanics:

- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`, helpers in
  `tests/testthat/helper-{name}.R`.

- Never put code outside a `test_that()` block.

- No section header comments.

- Prefer a specific expectation over `expect_true()`.

- Use `expect_snapshot(error = TRUE)` for errors and `expect_snapshot()` for
  warnings, never `expect_error()` or `expect_warning()`.

- Place new tests next to similar existing ones.

---

# Part 4: The pull requests

PRs 1 to 15 are ordered and each depends on the one before. After that, work
through Part 5 in any order that respects the dependencies noted there.

## PR 1: Package housekeeping

No behavior change.

- Fill in `DESCRIPTION`. Title, description, author.

- Add `_pkgdown.yml` with a reference index covering the existing exports.

- Add `plans/` to `.Rbuildignore` so `R CMD check` stays clean.

**Done when** `devtools::check()` and `pkgdown::check_pkgdown()` are clean.

## PR 2: Argument names in errors

`arg_as_array()` takes an `arg` string but hardcodes `"x"` in its message
(`src/utils.c`). Thread the argument name through properly.

Style is a plain `const char*`. No vctrs style arg struct.

- `src/utils.c`, `src/utils.h`: fix `arg_as_array()`.

- `src/dimensions.c`: `arg_as_dimensions()` takes and uses `arg`.

- `src/axes.c`: same for `arg_as_axes()`.

**Done when** snapshot tests show the right argument name for each call site.

## PR 3: Names API

- `rray_axis_names(x, axis)`. Single axis only, returns a character vector or
  `NULL`.

- `rray_row_names(x)`, `rray_col_names(x)`. Shortcuts for axes 1 and 2.

- `rray_set_names(x, names)`, `rray_set_axis_names(x, axis, names)`,
  `rray_set_row_names(x, names)`, `rray_set_col_names(x, names)`.

Validation as described in 2.5. Exact lengths, `NULL` clears.

Files: `R/names.R`, `src/names.c`, `src/names.h`, `tests/testthat/test-names.R`.

## PR 4: Names builder and the point iterator

This is a refactor with no behavior change, and it cleans up the ugliest code in
the package.

`rray_broadcast_names()` and `rray_reduce_names()` both scan once to see whether
anything survives, then allocate and fill while repeating the same conditions.
`rray_split_names()` does manual stride arithmetic to spread names across output
elements.

Add `struct rray_names_builder`:

- The caller allocates a one slot list, `KEEP`s it, and passes it to
  `rray_names_builder_init()`. This is the shelter.

- `rray_names_builder_poke(&b, axis, names)` allocates the real names list into
  slot 0 on the first non `NULL` poke.

- `rray_names_builder_result(&b)` returns `r_null` if nothing was ever poked.

Nothing is allocated when nothing survives, and protection is already in place
when it is.

Add `rray_iterator_point()` to `src/iterator.h`, alongside
`rray_iterator_location()`. Nothing reads `v_point` today, so this is a new
capability.

Then rewrite all three helpers:

- `rray_broadcast_names()` and `rray_reduce_names()` collapse to one loop each
  using the builder.

- `rray_split_names()` drops the stride arithmetic entirely. Walk the output
  space with an `rray_iterator` initialised with the out dimensions as both the
  point and location dimensions, and for each output element read
  `rray_iterator_point()`. The names on split axis `i` are
  `x_names[[i]][point[i]]`, and every other axis copies straight across.

Files: `src/names.c`, `src/names.h`, `src/iterator.h`,
`src/broadcast-template.h`, `src/reduce.c`, `src/split-template.h`.

**Done when** the existing tests pass unchanged and the three helpers are
noticeably shorter.

## PR 5: `rray_broadcast_common()`

```r
rray_broadcast_common(..., .dimensions = NULL)
```

Returns a list of arrays, all broadcast to the common dimensions. Mirrors
`rray_dimensions_common()`. `NULL` inputs are dropped.

Files: `R/broadcast.R`, `src/broadcast.c`, `src/broadcast.h`.

## PR 6: `rray_names_common()`

The coalesce rule from 2.5. Internal C plus an unexported R wrapper so it can be
tested directly before it has a real caller.

Signature is `rray_names_common(..., .dimensions = NULL)`, where `.dimensions`
defaults to `rray_dimensions_common(...)`. It needs the common dimensions to
apply the rule at all, so that argument is load bearing.

Files: `src/names.c`, `src/names.h`, `R/names.R`.

## PR 7: `rray_proxy()` and `rray_restore()`

The dispatch machinery from 2.1.

- C dispatch on `class(x)[[1]]` with no inheritance, following how vctrs looks
  methods up in the S3 table.

- Fast path that skips dispatch entirely when `x` has no class attribute.

- The default for a bare native vector with no `dim` produces a one dimensional
  array, moving `names` to `dimnames`. This is what `vec_as_array()` in
  `src/utils.c` already does, so move it here.

- Validate that a method returned a native type, and error clearly if not.

Files: `R/proxy.R`, `src/proxy.c`, `src/proxy.h`, `src/decl/proxy-decl.h`.

Tests need a small classed array in `tests/testthat/helper-proxy.R`. Keep it
around, later pull requests reuse it.

## PR 8: Retrofit the proxy

Route every existing function through the proxy and restore. `arg_as_array()`
becomes proxy aware.

Covers `rray_dimensions()`, `rray_dimensionality()`, `rray_size()`,
`rray_names()`, `rray_broadcast()`, `rray_set_dimensions()`, `rray_split()`,
`rray_sum()`, and everything from PRs 3, 5 and 6.

All of these are structural, so they restore to `x`'s own class.

**Done when** the helper class from PR 7 round trips through every one of them.

## PR 9: `rray_ptype()`

Native ptypes as shared static objects. Proxy, compute, restore.

Document the "a ptype must fully determine storage" invariant here.

Files: `R/ptype.R`, `src/ptype.c`, `src/ptype.h`.

## PR 10: `rray_ptype2()` and `rray_ptype_common()`

Double dispatch, no inheritance, no fallback. Native rules in C with a fast path
when neither side has a class.

`.ptype` runs through `rray_ptype()` inside `rray_ptype_common()`, not at the
call sites.

The "method is missing" error names the exact method that would fix it.

Files: `R/ptype2.R`, `src/ptype2.c`, `src/ptype2.h`.

## PR 11: `rray_cast()` and `rray_cast_common()`

Double dispatch on `to` then `x`. Type only, dimensions and names untouched.
Lossy casts error and say what was lost.

Files: `R/cast.R`, `src/cast.c`, `src/cast.h`.

## PR 12: `rray_arithmetic_ptype2()` and `rray_add()`

The hook, the native promotion table, the full binary pipeline from 2.3, and one
function using it end to end.

The C loop uses two broadcast iterators stepped side by side. Names come from
`rray_names_common()`.

Files: `R/arithmetic-ptype.R`, `src/arithmetic-ptype.c`,
`src/arithmetic-ptype.h`, `R/add.R`, `src/add.c`, `src/add.h`,
`src/add-template.h`.

Tests must cover a class with fixed storage and a class that is generic over
storage, using both recipes from 2.3.

## PR 13: The rest of the binary arithmetic

`rray_subtract()`, `rray_multiply()`, `rray_divide()`, `rray_power()`,
`rray_modulo()`, `rray_integer_divide()`.

All the same shape as `rray_add()`. Share the template.

## PR 14: `rray_reduction_ptype()` and `rray_sum()`

The reduction hook and its native table. Retrofit `rray_sum()` to use it, which
gives it class support and the `lgl` to `int` promotion.

Files: `R/reduction-ptype.R`, `src/reduction-ptype.c`,
`src/reduction-ptype.h`, `R/sum.R`, `src/sum.c`.

Fix the comment in `src/sum-template.h` claiming a logical array can never
overflow an integer sum. That is false once long arrays are supported.

## PR 15: `rray_arithmetic_ptype()` and `rray_opposite()`

The unary elementwise hook. It has exactly one caller, so it lands here rather
than up front.

---

# Part 5: Function reference

Each entry gives what the function does, what the original rray did, the
proposed signature, and which family it belongs to.

Families, from 2.3 and 2.5:

- **Structural.** Proxy, operate, restore to `x`'s own class. Names follow the
  per axis rules.

- **Computational.** Cast to a common type, broadcast, allocate fresh, restore
  to the computed ptype. Names coalesce.

- **Fixed output.** Cast inputs with plain `rray_ptype2()`, return a bare array.

## 5.1 Shape

### `rray_reshape()`

Reinterpret the same elements under new dimensions. Size cannot change.

```r
x <- matrix(1:6, ncol = 1)
rray_reshape(x, c(2, 3))
rray_reshape(x, c(3, 2, 1))
try(rray_reshape(x, c(6, 2)))
```

Drops all names, because no axis survives in a recognisable form.

`rray_set_dimensions()` already does exactly this. Decide in this pull request
whether `rray_reshape()` is a second name for it or whether one of them goes.

Signature: `rray_reshape(x, dimensions)`. Structural. Attributes only, so use
`r_wrap()`.

Files: already exist as `R/dimensions.R` and `src/dimensions.c`.

### `rray_flatten()`

Collapse to one dimension.

```r
rray_flatten(rray(1:10, c(5, 2)))     # (5, 2) -> (10)
```

Names are kept when the first axis' dimension is unchanged, which is the general
broadcast rule applied to a collapse.

```r
y <- array(1:2, 2, dimnames = list(c("a", "b")))
rray_flatten(y)                       # (2) -> (2), names kept
rray_flatten(t(rray_reshape(y, c(2, 1))))  # (1, 2) -> (2), names dropped
```

Signature: `rray_flatten(x)`. Structural. Attributes only.

Files: `R/flatten.R`, `src/flatten.c`, `src/flatten.h`.

### `rray_squeeze()`

Drop axes whose dimension is 1.

```r
x <- rray(1:10, c(10, 1))
rray_squeeze(x)                 # (10, 1) -> (10)

y <- rray_reshape(x, c(10, 1, 1))
rray_squeeze(y)                 # (10, 1, 1) -> (10)
rray_squeeze(y, axes = 2)       # (10, 1, 1) -> (10, 1)
```

Names on surviving axes are kept and move with them.

Signature: `rray_squeeze(x, axes)`. Axes required, following the reduction
convention. Structural. Attributes only.

Files: `R/squeeze.R`, `src/squeeze.c`, `src/squeeze.h`.

### `rray_expand()`

Insert an axis of dimension 1.

```r
x <- rray(1:10, c(5, 2))     # row names a-e, col names c1, c2
rray_expand(x, 1)            # (1, 5, 2)
rray_expand(x, 2)            # (5, 1, 2)
rray_expand(x, 3)            # (5, 2, 1)
```

Names follow their original axis to its new position. In `rray_expand(x, 1)` the
5 row names become the names of the new second axis, and the new first axis has
none. This is the difference from a plain reshape, which drops everything.

Signature: `rray_expand(x, axis)`. Single axis. Structural. Attributes only.

Files: `R/expand.R`, `src/expand.c`, `src/expand.h`.

### `rray_transpose()`

Permute axes.

```r
x <- rray(1:6, c(3, 2))
rray_transpose(x)                    # (3, 2) -> (2, 3)
rray_transpose(x, rev(1:2))          # identical

x_3d <- rray_broadcast(x, c(3, 2, 2))
rray_transpose(x_3d)                 # (3, 2, 2) -> (2, 2, 3), reverses all axes
rray_transpose(x_3d, c(2, 1, 3))     # flips the first two, leaves the third
```

Names travel with their axis.

Signature: `rray_transpose(x, permutation = NULL)`, where `NULL` reverses all
axes. Structural, but it moves data, so it needs a real C loop.

Needs a new iterator, or an existing one initialised with permuted strides.
Work that out in the pull request and say which you chose.

Files: `R/transpose.R`, `src/transpose.c`, `src/transpose.h`,
`src/transpose-template.h`.

### `rray_tile()`

Repeat an array along axes.

```r
x <- matrix(1:5)
rray_tile(x, 2)             # repeat rows twice
rray_tile(x, c(2, 3))       # rows twice, columns three times
rray_tile(x, c(1, 2, 2))    # tile into a third dimension
```

Different from broadcasting: broadcasting only repeats an axis whose dimension
is 1, tiling repeats any axis.

All names on tiled axes are dropped. Untitled axes keep theirs.

Signature: `rray_tile(x, times)`. Structural.

Files: `R/tile.R`, `src/tile.c`, `src/tile.h`, `src/tile-template.h`.

### `rray_flip()`

Reverse the order along an axis.

```r
x <- rray(1:10, c(5, 2))
rray_flip(x, 1)      # reverse the rows
rray_flip(x, 2)      # reverse the columns
```

Names on the flipped axis are reversed with it. Other axes are untouched.

Signature: `rray_flip(x, axis)`. Single axis. Structural.

Files: `R/flip.R`, `src/flip.c`, `src/flip.h`, `src/flip-template.h`.

## 5.2 Elementwise arithmetic

All computational, all using `rray_arithmetic_ptype2()` and the pipeline from
2.3, all sharing one template.

| function | op |
|---|---|
| `rray_add(x, y)` | `+` |
| `rray_subtract(x, y)` | `-` |
| `rray_multiply(x, y)` | `*` |
| `rray_divide(x, y)` | `/` |
| `rray_power(x, y)` | `^` |
| `rray_modulo(x, y)` | `%%` |
| `rray_integer_divide(x, y)` | `%/%` |
| `rray_opposite(x)` | unary `-` |

Match R's own semantics for missing values, `NaN`, and division by zero. Check
`/Users/davis/files/r/r-svn` when a case is unclear rather than guessing.

Files: `R/arithmetic.R`, `src/arithmetic.c`, `src/arithmetic.h`,
`src/arithmetic-template.h`. `rray_add()` lands first in PR 12 and may live in
its own file until PR 13 generalises it.

## 5.3 Other elementwise numeric

### `rray_maximum()` and `rray_minimum()`

Elementwise maximum and minimum of two arrays, with broadcasting. Not to be
confused with `rray_max()` and `rray_min()`, which reduce.

Computational, ops `"maximum"` and `"minimum"`.

Signature: `rray_maximum(x, y, ..., na_rm = FALSE)`.

Files: `R/extremum.R`, `src/extremum.c`, `src/extremum.h`.

### `rray_hypot()`

Elementwise `sqrt(x^2 + y^2)`, computed without intermediate overflow.

Computational, op `"hypot"`, always double.

Signature: `rray_hypot(x, y)`.

Files: `R/hypot.R`, `src/hypot.c`, `src/hypot.h`.

### `rray_multiply_add()`

`x * y + z`, fused. Three way broadcasting.

Computational. Three way names coalescing, which is a straight extension of the
two way rule.

Signature: `rray_multiply_add(x, y, z)`.

Files: `R/multiply-add.R`, `src/multiply-add.c`, `src/multiply-add.h`.

### `rray_clip()`

Bound values between a low and a high.

```r
x <- matrix(1:10, ncol = 2)
rray_clip(x, 1, 5)
```

**A human should design review this before implementation.** Three way
broadcasting raises questions the original rray did not answer well: whether
`low` and `high` broadcast against `x` or must be scalars, and what happens when
`low > high`.

Signature: `rray_clip(x, low, high)`. Computational.

Files: `R/clip.R`, `src/clip.c`, `src/clip.h`.

### `rray_if_else()`

Elementwise choice between two arrays based on a logical array.

**A human should design review this before implementation.** Three way
broadcasting again, plus the question of how `condition` relates to the type
system when `true` and `false` have different types.

Signature: `rray_if_else(condition, true, false)`. Computational on `true` and
`false`, with `condition` cast to logical.

Files: `R/if-else.R`, `src/if-else.c`, `src/if-else.h`.

### `rray_full_like()`, `rray_ones_like()`, `rray_zeros_like()`

An array with the dimensions and type of `x`, filled with a single value.

```r
rray_full_like(x, 5)
rray_ones_like(x)
rray_zeros_like(x)
```

Structural in the sense that they keep `x`'s class, but they build a fresh
array. Names are kept, since every axis keeps its dimension.

Signature: `rray_full_like(x, value)`, `rray_ones_like(x)`,
`rray_zeros_like(x)`.

Files: `R/full-like.R`, `src/full-like.c`, `src/full-like.h`.

## 5.4 Comparison and logical

All fixed output. Inputs cast with plain `rray_ptype2()`, result is a **bare
logical array** with no class restored. Names coalesce as usual.

| function | meaning |
|---|---|
| `rray_equal(x, y)` | `==` |
| `rray_not_equal(x, y)` | `!=` |
| `rray_greater(x, y)` | `>` |
| `rray_greater_equal(x, y)` | `>=` |
| `rray_lesser(x, y)` | `<` |
| `rray_lesser_equal(x, y)` | `<=` |

Files: `R/compare.R`, `src/compare.c`, `src/compare.h`,
`src/compare-template.h`.

Logical ops take logical input and return a bare logical array.

| function | meaning |
|---|---|
| `rray_logical_and(x, y)` | `&` |
| `rray_logical_or(x, y)` | `\|` |
| `rray_logical_not(x)` | `!` |

Edge cases worth testing, from the original's documentation:

```r
x <- array(TRUE, c(1, 2))
logical() & x                        # common dimensions are (0, 2)
x & array(logical(), c(0, 1, 2))     # common dimensions are (0, 2, 2)
try(x & array(logical(), c(1, 0)))   # 2 and 0 do not broadcast
```

Files: `R/logical.R`, `src/logical.c`, `src/logical.h`.

Optional, decide when you get there: `rray_all_equal(x, y)` and
`rray_any_not_equal(x, y)`, which reduce a comparison to a single logical.

## 5.5 Reductions

All use `rray_reduction_ptype()` and the reduction iterator. All keep
dimensionality, with reduced axes collapsed to a dimension of 1. There is no
`keep_dimensions` argument.

`axes` is **required** on every one of them. The original defaulted it to `NULL`
meaning "all axes". We do not.

Names follow the reduce rule: reduced axes lose their names, everything else
keeps them.

| function | op | notes |
|---|---|---|
| `rray_sum(x, axes, ..., na_rm = FALSE)` | `sum` | exists, retrofit in PR 14 |
| `rray_prod(x, axes, ..., na_rm = FALSE)` | `prod` | promotes int to dbl |
| `rray_mean(x, axes, ..., na_rm = FALSE)` | `mean` | promotes lgl and int to dbl |
| `rray_max(x, axes, ..., na_rm = FALSE)` | `max` | |
| `rray_min(x, axes, ..., na_rm = FALSE)` | `min` | |

`rray_all(x, axes)` and `rray_any(x, axes)` are fixed output. Logical input,
bare logical array out.

`rray_max_pos(x, axis)` and `rray_min_pos(x, axis)` are fixed output. They give
the position of the maximum or minimum along a single axis as a bare integer
array.

```r
x <- rray(c(1:10, 20:11), dim = c(5, 2, 2))
rray_max_pos(x, 1)     # position of the max along the rows
rray_max_pos(x, 2)     # along the columns
```

Files: `R/prod.R`, `R/mean.R`, `R/extremum-reduce.R`, `R/logical-reduce.R`,
`R/max-pos.R`, each with its C pair. `rray_sum()` already exists.

## 5.6 Indexing

**A human should design review this whole section before any of it is
implemented.** It was the most confusing part of the original rray, and it
deserves a fresh look rather than a port. What follows documents what rray did,
so the review has something concrete to react to.

Three ways to pull data out, distinguished by what happens to dimensionality.

### `rray_subset()`, by index, keeps dimensionality

Never drops dimensions. Ignores trailing commas, so `x[1]` and `x[1, ]` agree.
Missing arguments select a whole axis.

```r
x <- rray(1:8, c(2, 2, 2))

rray_subset(x, 1)        # first row, still (1, 2, 2)
rray_subset(x, , 1)      # all rows, first column, still (2, 1, 2)
```

Base R cannot do the second one without fully specifying every axis and passing
`drop = FALSE`.

Index types: integer-ish selects elements, logical must be length 1 or the
dimension, character requires names on that axis, `NULL` means 0.

### `rray_extract()`, by index, always drops

Always returns a one dimensional result, and never keeps names.

```r
x <- rray(1:16, c(2, 4, 2), dim_names = list(c("r1", "r2"), NULL, NULL))

rray_extract(x, 1)          # first row, flattened
rray_extract(x, 1, 1:2)     # first row, first two columns, flattened
```

Like `x[[i, j, ...]]` but each subscript may have length greater than 1.

### `rray_yank()`, by position, always flattens

Pulls elements out by their position in the flat array, ignoring dimensions
entirely. Always one dimensional, never keeps names.

```r
x <- rray(10:17, c(2, 2, 2))

rray_yank(x, 1:3)
rray_yank(x, FALSE)
```

`i` is an integer vector of positions, a logical of length 1 or `rray_size(x)`,
or a logical with exactly `x`'s dimensions.

### `rray_slice()`

Subset a single axis by index, keeping dimensionality.

```r
rray_slice(x, i, axis)
```

### The assignment forms

`rray_subset_assign()`, `rray_extract_assign()`, `rray_yank_assign()`,
`rray_slice_assign()`.

The original cast `value` to `x` rather than the other way round, and broadcast
`value` to the shape of the selection. Both decisions are worth re-examining in
the design review.

No `<-` replacement forms for now, consistent with the names API.

Files: one pair per function, plus a shared `src/index.c` and `src/index.h` for
turning user supplied subscripts into locations.

## 5.7 Binding

**A human should design review this before implementation.** The axis semantics
in the original were subtle, particularly how binding "up" into a new axis
interacts with broadcasting and with names.

```r
a <- matrix(1:4, ncol = 2)
b <- matrix(5:6, ncol = 1)

rray_bind(a, b, .axis = 2)    # bind along columns
rray_bind(a, b, .axis = 1)    # bind along rows, broadcasting automatically
rray_bind(a, b, .axis = 3)    # bind up into a new third axis
```

The second one is not possible with `rbind()`, because `a` and `b` have
different column counts and `b` has to broadcast.

Signatures: `rray_bind(..., .axis)`, `rray_rbind(...)`, `rray_cbind(...)`.

Computational, since a common type is needed across all inputs.

Files: `R/bind.R`, `src/bind.c`, `src/bind.h`, `src/bind-template.h`.

## 5.8 Order and duplicates

**A human should design review this before implementation.** It also depends on
`rray_proxy_compare()` and `rray_proxy_equal()`, which we deferred and which
need designing first.

Everything here works along an axis, comparing whole slices rather than
individual elements.

### `rray_sort()`

```r
x <- rray(c(20:11, 1:10), dim = c(5, 2, 2))

rray_sort(x, 1)      # sort looking along the rows
rray_sort(x, 2)      # along the columns
rray_sort(x, 3)      # along the third axis
```

Names on the sorted axis are dropped, because they no longer line up. Other axes
keep theirs.

Signature: `rray_sort(x, axis)`. Structural.

### `rray_unique()`, `rray_unique_loc()`, `rray_unique_count()`

Deduplicate slices along an axis.

```r
x <- rray(c(1, 1, 3, 3, 2, 2, 4, 4), c(2, 2, 2))

rray_unique(x, 1)          # unique rows
rray_unique_loc(x, 2)      # positions of the unique columns
rray_unique_count(x, 2)    # how many unique columns
```

`rray_unique_loc()` returns a bare integer array. `rray_unique_count()` returns
a single integer.

### `rray_duplicate_any()`, `rray_duplicate_detect()`, `rray_duplicate_id()`

```r
x <- rray(c(1, 1, 2, 2), c(2, 2))

rray_duplicate_any(x, 1)       # are any rows duplicated
rray_duplicate_detect(x, 1)    # TRUE wherever a duplicate exists, first included
rray_duplicate_id(x, 1)        # position of the first occurrence of each slice
```

All three take a single axis. `_any` returns a single logical, the other two
return bare arrays.

Files: `R/sort.R`, `R/unique.R`, `R/duplicate.R`, each with a C pair, plus
whatever the compare and equal proxies need.

---

# Part 6: Out of scope

Deliberately not ported from the original rray.

- **The rray class.** `new_rray()`, `as_rray()`, `is_rray()`, `rray()`,
  `as_array()`, `as_matrix()`, the print and format methods, `[` and `[[`
  methods, and every `vctrs_rray` S3 method. rray4 ships functions, not a type.

- **Operators.** `%b+%`, `%b-%`, `%b*%`, `%b/%`, `%b^%`, and the `vec_arith()`
  methods.

- **`rray_dot()`.** Matrix multiplication, which needed BLAS.

- **`rray_diag()`.**

- **`rray_rotate()`.** Expressible as a transpose plus a flip.

- **`pad()`** and the padding indexer.

- **`rray_shape()`, `rray_shape2()`, `rray_shapecast()`.**

- **The container and inner type split.** rray4 has one type system, not two.

- **The purrr compatibility shims** in `compat-purrr.R`.

- **`rray_identity()`, `rray_elems()`.**

---

# Part 7: Possible improvements for later

Not now. Revisit once the foundations are in place and there is a working
version to measure against.

## Iterator runs

Every template today steps the iterator once per element, even when no
broadcasting is happening and the mapping is the identity.

Instead of a bespoke fast path in each function, the iterator could report a
**run length**: the next N output elements map to N consecutive input locations,
or to the same location N times. Every template then shares one shape:

```c
while (i < size) {
  const r_ssize n = rray_iterator_run(&it);
  // copy or fill n elements
  i += n;
}
```

That covers "no broadcasting at all" and "broadcasting along a later axis" with
one mechanism, and it is one thing to test rather than a fast path per function.

It is more machinery in `iterator.h`, which is why it waits. Do it as its own
pull request, with benchmarks against the version that came before it.

Functions most likely to benefit: `rray_broadcast()`, the elementwise
arithmetic family, and `rray_tile()`.

## An unspecified type

`rray_add(x, NA)` does not work, because there is no unspecified type. vctrs has
one and it has been painful. See whether we can live without it first.

## `rray_proxy_compare()` and `rray_proxy_equal()`

Needed by everything in 5.8. Design them when the first sorting or deduplication
function is written, not before.
