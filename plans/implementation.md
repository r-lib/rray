# rray4 implementation plan

## What this is

rray4 is a reimagining of rray, written in pure C with no xtensor. It is a
toolkit of `rray_*()` functions that broadcast, reshape, reduce and index base R
arrays.

It works on bare arrays only. There is no rray class, and classed input is an
error. The extension system that would let other packages plug their own array
classes in is deliberately deferred, and is written up separately in
`plans/extensions.md`.

This document is the guide for agents working on rray4 across many sessions.
Part 4 is the ordered list of pull requests. Part 5 is the catalogue of
functions to work through after the foundations land.

## How to use this plan

Read Part 1 and Part 2 before starting any pull request. They are short on
purpose.

Then find your pull request in Part 4, or your function in Part 5. Each entry
says what to build, which files to touch, and what "done" means.

One pull request per entry. Keep them small.

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

**This one is under review.** PR 5 trials a shell and core split instead. If it
lands, this section changes and there will be no `-template.h` files. Check
whether PR 5 has been decided before writing a new template.

Private declarations go in `src/decl/{name}-decl.h`.

## Naming

- Internal C: `rray_{name}()`, returns C types.

- FFI wrappers: `ffi_rray_{name}()`, thin SEXP bridges, placed above the
  internal functions in the `.c` file.

- Headers declare internal functions only, never FFI wrappers.

- `src/init.c` uses `extern` declarations, it does not include feature headers.

## C style

Follow vctrs and rlang closely. Prefer an rlang wrapper over the raw R API every
time (`r_globals.na_int`, `r_length()`, `r_attrib_get()`).

Prefer `r_ssize` over `int`. Mark variables `const` where you can. Grab a data
pointer before a loop rather than indexing the `r_obj*`.

Protect anything that allocates with `KEEP()` and `FREE()`.

Never touch `src/rlang/`.

Run `clang-format -i src/*.c src/*.h` over all files after any C change.

## Comments

No comments in C or R code, other than roxygen2 on exported R functions. This
overrides the usual defaults.

## R style

Run `air format .` after any R change. Use `|>`, not `%>%`. Use `\() ...` for one
line anonymous functions and `function() {...}` otherwise. The package must work
on R 4.1, so no `_$x`.

## Prose

Plain words, short sentences. Show a small example instead of writing a
paragraph. No em dashes.

Never use the term "load bearing". Say what the thing actually does.

---

# Part 2: The core ideas

## 2.1 Arrays in, arrays out

Every rray4 function takes arrays and returns arrays.

A **native** type is one of the seven R vector types: logical, integer, double,
complex, raw, character, list. Every function works on native types, and the
per type templates cover all seven unless a function says otherwise.

Three rules at the boundary:

**Bare input only.** A small `check_unclassed()` helper tests `r_is_object()` and
errors if it is true. Bare matrices and arrays pass, because `matrix` and `array`
are implicit classes with no attribute set. Anything else is refused rather than
silently unclassed or silently corrupted.

**A bare vector becomes a one dimensional array.** `1:5` is treated as
`array(1:5, 5L)`, and any `names` move to `dimnames`. This is what
`arg_as_array()` does, once `check_unclassed()` has passed.

**Arrays always come back.** `rray_sum(1:5, 1)` returns `array(15L, 1L)`, not
`15L`. We lean into this rather than trying to hide it.

Because a vector is normalized on the way in, `rray_dimensions()`,
`rray_names()`, `rray_size()` and `rray_dimensionality()` are all "normalize,
then read an attribute".

## 2.2 Broadcasting

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

## 2.3 Names

There are five rules. Every function in Part 5 declares which one it follows.

**Follow the axis.** An axis keeps its names if its dimension is unchanged, and
loses them if its dimension changed. Whatever it keeps travels with it to
wherever the axis ends up. New axes have no names.

This is the rule for almost everything that reshapes an array. An axis of
dimension 1 broadcast to 1 counts as unchanged and keeps its length 1 names.

**Reduce.** Reduced axes lose their names. Every other axis keeps them.

Nearly the same rule, but a reduced axis loses its names even when its dimension
was already 1, so it needs stating separately.

**Coalesce.** Used by anything with two or more array inputs. For each axis of
the common dimensions:

1. Use `x`'s names if `x`'s dimension there equals the common dimension and `x`
   has names there.

2. Otherwise apply the same test to `y`.

3. Otherwise `NULL`.

This lives in an internal `rray_names_common()`.

**Subset.** Names are subset alongside the data, so an axis keeps the names of
the elements that survived.

**Dropped.** All names are discarded. Used where no axis survives in a
recognisable form, such as a reshape.

The reasoning behind the split: a function that manipulates an existing array is
still functionally the same array, so names travel with the axes that did not
move. A function with two inputs builds a genuinely new array, but dropping
every name would make `rray_add(named_matrix, 1)` lose its names, which reads as
a bug.

### The names API

`rray_names(x)` returns the `dimnames` attribute as it is. A one dimensional
array with names returns a one element list. No normalization on read.

Setters take a list of length exactly the dimensionality, where each element is
either `NULL` or a character vector of exactly that axis' dimension. `NULL`
clears everything. Short lists are not padded, they are an error.

There are no `<-` replacement forms.

## 2.4 Types

There is no user facing type system. A **type** here is just an `enum r_type`,
because without classes there is nothing else for it to carry.

The internal type rules exist because three kinds of function need them:

- `rray_bind()` needs a common type across many inputs.

- `rray_add()` needs a common type across two inputs, plus a promotion that
  depends on the operator.

- `rray_sum()` needs a promotion that depends on the operator.

The C interface is small:

```c
enum r_type rray_type2(enum r_type x, enum r_type y);
enum r_type rray_type_common(r_obj* xs, struct r_lazy error_call);
r_obj*      rray_cast(r_obj* x, enum r_type to, struct r_lazy error_call);
r_obj*      rray_cast_common(r_obj* xs, enum r_type to, struct r_lazy error_call);
```

`rray_cast()` changes type only. It never touches dimensions or names.
Broadcasting is always a separate step. Lossy casts are an error, and the error
says what was lost.

### The common type rules

- Numeric tower: `lgl` to `int` to `dbl` to `cpl`.

- `chr`, `list` and `raw` each stand alone. They combine only with themselves.

There is no fallback and no coercion across families. `rray_type2(chr, int)` is
an error.

A user facing function that needs a type override takes a `.ptype` argument,
matching `.dimensions` elsewhere. It takes a prototype object, so
`.ptype = double()` means "a double array", and we read its `r_typeof()`.

Note that `NA` is logical, so it sits at the bottom of the numeric tower and
`rray_add(x, NA)` works with no special handling. vctrs needs an unspecified
type for this. We do not, at least until `rray_bind()` wants
`rray_bind(chr_array, NA)` to work. See Part 7.

### Operator promotion

Some operators need a type the common type rules cannot give, because
`lgl + lgl` is `int` and `int / int` is `dbl`.

Each family gets its own operator enum and its own type function.

```c
enum r_type rray_binary_type(enum rray_binary_op op, enum r_type x, enum r_type y);
enum r_type rray_reduction_type(enum rray_reduction_op op, enum r_type x);
```

Separate enums rather than one shared vocabulary, so each function can only be
handed an operator its family actually has.

Each returns **one type**, used both to cast the inputs and to allocate the
output. That works because we always promote before computing, so the input type
and the output type are the same.

The tables below cover four types. `chr`, `raw` and `list` are an error for
every operator, so the arithmetic and reduction families are the one place where
a template does not cover all seven native types.

For binary operators, read the tables as "apply `rray_type2()` first, then
promote".

Binary elementwise:

| op | lgl | int | dbl | cpl |
|---|---|---|---|---|
| `+` `-` `*` | int | int | dbl | cpl |
| `/` `^` | dbl | dbl | dbl | cpl |
| `%%` `%/%` | int | int | dbl | error |

Reduction:

| op | lgl | int | dbl | cpl |
|---|---|---|---|---|
| `sum` | int | int | dbl | cpl |
| `prod` | dbl | dbl | dbl | cpl |
| `mean` | dbl | dbl | dbl | cpl |

`sum` on an integer array stays integer and errors on overflow. `prod` promotes
to double, matching base R's `prod()`, because integer products overflow almost
immediately.

### Operators that promote nothing

`maximum`, `minimum`, `max` and `min` are in the tables' families but not in the
tables, because picking the largest of some values cannot change their type. The
maximum of two logicals is a logical.

They still go through `rray_binary_type()` and `rray_reduction_type()`, which for
them return the type unchanged and error on `cpl`, since complex numbers have no
ordering. So the type function is doing validation rather than promotion.

### Operators with a fixed output type

An operator whose output type is fixed regardless of its input does not use the
promotion tables. It finds the common type of its inputs with `rray_type2()`,
casts, computes, and allocates the output at its own fixed type.

- Comparison (`rray_equal()` and friends) returns a logical array.

- `rray_all()` and `rray_any()` take logical and return a logical array.

- `rray_max_pos()` and `rray_min_pos()` return an integer array.

Stated once: **the promotion tables are only for operators whose output type
equals their input type.**

## 2.5 Iterators

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

Functions with two array inputs use two plain iterators stepped side by side.
There is no binary iterator type.

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
  full message is reviewable. That includes classed input being refused.

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

PRs 1 to 11 build the foundations. Work through them in order, since each
assumes the ones before it have landed. After that, work through Part 5 in any
order that respects the dependencies noted there.

## PR 1: Package housekeeping — done

No behavior change.

- Fill in `DESCRIPTION`. Title, description, author.

- Add `_pkgdown.yml` with a reference index covering the existing exports.

- Add `plans/` to `.Rbuildignore` so `R CMD check` stays clean.

**Done when** `devtools::check()` and `pkgdown::check_pkgdown()` are clean.

## PR 2: Argument checking — done

**Add `check_unclassed()`.** A small helper that tests `r_is_object()` and errors
if it is true. Every entry point calls it first, before `arg_as_array()`.

This fixes a real bug. Today a classed array is accepted, and the class then
survives or is dropped depending on which function you called, because
`vec_as_array()` clones attributes through `r_wrap()` while `rray_broadcast()`
pokes only `dim` onto a fresh allocation.

Keeping the check separate leaves `arg_as_array()` doing one thing: turning a
bare vector into a one dimensional array.

**Use the argument name.** `arg_as_array()` takes an `arg` string and hardcodes
`"x"` in its message. Thread the name through properly. Style is a plain
`const char*`, no vctrs style arg struct.

- `src/utils.c`, `src/utils.h`: `check_unclassed()`, and the `arg` fix in
  `arg_as_array()`.

- `src/dimensions.c`: `arg_as_dimensions()` takes and uses `arg`.

- `src/axes.c`: same for `arg_as_axes()`.

**Done when** snapshot tests show the right argument name for each call site, and
a classed array is refused by every existing function.

## PR 3: Names API

- `rray_axis_names(x, axis)`. Single axis only, returns a character vector or
  `NULL`.

- `rray_row_names(x)`, `rray_col_names(x)`. Shortcuts for axes 1 and 2.

- `rray_set_names(x, names)`, `rray_set_axis_names(x, axis, names)`,
  `rray_set_row_names(x, names)`, `rray_set_col_names(x, names)`.

Validation as described in 2.3. Exact lengths, `NULL` clears.

Files: `R/names.R`, `src/names.c`, `src/names.h`, `tests/testthat/test-names.R`.

## PR 4: Lazy list and the point iterator

A refactor with no behavior change, and it cleans up the ugliest code in the
package.

`rray_broadcast_names()` and `rray_reduce_names()` both scan once to see whether
anything survives, then allocate and fill while repeating the same conditions.
`rray_split_names()` does manual stride arithmetic to spread names across output
elements.

Add `struct rray_lazy_list`, following `struct lazy_raw` in vctrs' `src/lazy.h`:

- `new_rray_lazy_list(size)` stores the struct in a raw vector and leaves the
  list itself unallocated.

- The struct holds a `PROTECT_INDEX`, so it reprotects itself on allocation
  rather than asking the caller to arrange a shelter.

- `init_rray_lazy_list()` allocates on first use and is a no-op after that.

Names are the first user. A names list is only allocated once an axis actually
survives, so the common case where nothing survives allocates nothing. Keep it
general rather than calling it a names builder, since it is a plain lazy
`VECSXP` and other functions will want one.

Add `rray_iterator_point()` to `src/iterator.h`, alongside
`rray_iterator_location()`. Nothing reads `v_point` today, so this is a new
capability.

Then rewrite all three helpers:

- `rray_broadcast_names()` and `rray_reduce_names()` collapse to one loop each,
  poking into a lazy list.

- `rray_split_names()` drops the stride arithmetic entirely. Walk the output
  space with an `rray_iterator` initialised with the out dimensions as both the
  point and location dimensions, and for each output element read
  `rray_iterator_point()`. The names on split axis `i` are
  `x_names[[i]][point[i]]`, and every other axis copies straight across.

Files: `src/lazy.h`, `src/names.c`, `src/names.h`, `src/iterator.h`,
`src/broadcast-template.h`, `src/reduce.c`, `src/split-template.h`.

**Done when** the existing tests pass unchanged and the three helpers are
noticeably shorter.

## PR 5: Shell and core spike

A trial, not a commitment. Convert `rray_broadcast()` to a different shape for
per type code, measure it, and decide whether the rest of the package follows.

### Why

The `src/{name}-template.h` pattern templates the whole function, but almost
none of the function depends on the type:

| template | templated body | lines touching a type |
|---|---|---|
| `broadcast` | ~80 | 6 |
| `split` | ~99 | 9 |
| `sum` | ~72 | 8 |

Roughly 92% of each body is dimension checks, allocation, iterator setup and
names handling, none of which is typed, and all of which is compiled seven
times. `RRAY_ONCE` exists to carve out the parts that are not really templates,
which is the same problem showing through.

### The shape to try

Split each function into a type-independent shell and a small typed core. The
shell is ordinary C, written once:

```c
r_obj* rray_broadcast(r_obj* x, r_obj* dimensions, struct r_lazy error_call) {
  // dimensions, checks, allocation, iterator setup: all untyped

  switch (r_typeof(x)) {
  case R_TYPE_logical: broadcast_lgl(x, out, size, &it); break;
  case R_TYPE_integer: broadcast_int(x, out, size, &it); break;
  // ...
  }

  // names
}
```

The core is a short macro with one row per type, following the shape of `SLICE`
in vctrs' `src/slice.c`:

```c
#define RRAY_BROADCAST_LOOP(CTYPE, CONST_DEREF, DEREF)   \
  const CTYPE* v_x = CONST_DEREF(x);                     \
  CTYPE* v_out = DEREF(out);                             \
  for (r_ssize i = 0; i < size; ++i) {                   \
    v_out[i] = v_x[rray_iterator_location(p_it)];        \
    rray_iterator_next(p_it);                            \
  }

static void broadcast_lgl(BROADCAST_ARGS) { RRAY_BROADCAST_LOOP(int, r_lgl_cbegin, r_lgl_begin); }
static void broadcast_int(BROADCAST_ARGS) { RRAY_BROADCAST_LOOP(int, r_int_cbegin, r_int_begin); }
static void broadcast_dbl(BROADCAST_ARGS) { RRAY_BROADCAST_LOOP(double, r_dbl_cbegin, r_dbl_begin); }
```

Write `chr` and `list` out by hand. They need the write barrier, so they are
genuinely different, and `RRAY_OUT_ASSIGN` currently hides that by making all
seven look alike.

### What this is trying to buy

- The untyped 92% written once instead of seven times.

- No `#if/#elif` config block, no `#undef` list to keep in sync, no `RRAY_ONCE`.

- A call site table you can read at a glance and diff between types.

- Working clangd. A `-template.h` opened on its own has no `RRAY_TYPE` defined,
  so the file you edit most is full of false errors.

- A better answer for the combinatorial case. Seven arithmetic operators across
  four types is 28 loops, which is 28 one line rows if the macro takes the
  scalar operation, against 28 template inclusions.

### The risk to measure

The loop now sits behind a function call that the compiler may not inline.
Benchmark `rray_broadcast()` before and after, across small and large arrays and
across a few types. The call happens once per array rather than once per
element, so it should not matter, but check rather than assume.

### The decision

**If it works:** update the Files section in Part 1 to describe the shell and
core pattern instead of `-template.h`, and open follow up pull requests to
convert `split` and `sum`. Everything in Part 5 then uses the new shape, so no
function entry needs a `-template.h` file listed.

**If it does not:** keep the templates, and record the benchmark here so nobody
re-opens this.

Either way the outcome gets written into the plan. Do not leave both patterns in
the package.

Files: `src/broadcast.c`, `src/broadcast.h`, deleting
`src/broadcast-template.h` if it goes ahead.

## PR 6: `rray_broadcast_common()`

```r
rray_broadcast_common(..., .dimensions = NULL)
```

Returns a list of arrays, all broadcast to the common dimensions. Mirrors
`rray_dimensions_common()`. `NULL` inputs are dropped.

Files: `R/broadcast.R`, `src/broadcast.c`, `src/broadcast.h`.

## PR 7: `rray_names_common()`

The coalesce rule from 2.3. Internal C plus an unexported R wrapper so it can be
tested directly before it has a real caller.

Signature is `rray_names_common(..., .dimensions = NULL)`, where `.dimensions`
defaults to `rray_dimensions_common(...)`. The rule cannot be applied without the
common dimensions, so that argument is not optional decoration.

Files: `src/names.c`, `src/names.h`, `R/names.R`.

## PR 8: Native types

The internal type interface from 2.4. All C, no exports, with unexported R
wrappers so it can be tested directly.

- `rray_type2()` and `rray_type_common()`. The numeric tower, with `chr`, `list`
  and `raw` standing alone.

- `rray_cast()` and `rray_cast_common()`. Type only, dimensions and names
  untouched, lossy casts error and say what was lost.

Files: `src/type.c`, `src/type.h`, `src/cast.c`, `src/cast.h`,
`src/cast-template.h`.

## PR 9: Binary promotion and `rray_add()`

`enum rray_binary_op`, `rray_binary_type()` and its table, then one function
using it end to end.

The C loop uses two broadcast iterators stepped side by side. Names come from
`rray_names_common()`.

Files: `src/op.h` for the enums, `src/type.c` for the table, `R/arithmetic.R`,
`src/arithmetic.c`, `src/arithmetic.h`, `src/arithmetic-template.h`.

## PR 10: The rest of the binary arithmetic

`rray_subtract()`, `rray_multiply()`, `rray_divide()`, `rray_power()`,
`rray_modulo()`, `rray_integer_divide()`.

All the same shape as `rray_add()`. Share the template.

## PR 11: Reduction promotion and `rray_sum()`

`rray_reduction_type()` and its table. Retrofit `rray_sum()` to use it, which
gives it the `lgl` to `int` promotion.

Fix the comment in `src/sum-template.h` claiming a logical array can never
overflow an integer sum. That is false once long arrays are supported.

Files: `src/type.c`, `R/sum.R`, `src/sum.c`, `src/sum-template.h`.

---

# Part 5: Function reference

Each entry gives what the function does, what the original rray did, the
proposed signature, the files it touches, and two rules.

**Names rule**, from 2.3: follow the axis, reduce, coalesce, subset, or
dropped.

**Type rule**, from 2.4:

- *Preserved.* The output has `x`'s type. Any other array argument is cast to
  it, rather than being given a say in the result.

- *Common.* Every input has a say. They are cast to a common type with
  `rray_type2()`.

- *Promoted.* The common type, then pushed through the operator's promotion
  table.

- *Fixed.* Output type is fixed regardless of input.

Entries below list a `src/{name}-template.h` where a function needs per type
code. If PR 5 lands, those files do not exist and the per type code lives in
`src/{name}.c` instead. Nothing else about an entry changes.

## 5.1 Shape

### `rray_set_dimensions()`

Reinterpret the same elements under new dimensions. Size cannot change.

```r
x <- matrix(1:6, ncol = 1)
rray_set_dimensions(x, c(2, 3))
rray_set_dimensions(x, c(3, 2, 1))
try(rray_set_dimensions(x, c(6, 2)))
```

Names: dropped. Type: preserved.

Signature: `rray_set_dimensions(x, dimensions)`. It only changes attributes, so
it uses `r_wrap()` rather than allocating.

There is no `rray_reshape()`. It would be a second name for this.

Files: already exist as `R/dimensions.R` and `src/dimensions.c`.

### `rray_flatten()`

Collapse to one dimension.

```r
rray_flatten(array(1:10, c(5, 2)))     # (5, 2) -> (10)
```

Names: follow the axis. Type: preserved. So names survive only when the first
axis' dimension is unchanged.

```r
y <- array(1:2, 2, dimnames = list(c("a", "b")))
rray_flatten(y)                                   # (2) -> (2), names kept
rray_flatten(array(1:2, c(2, 1), dimnames = list(c("a", "b"), NULL)))
                                                  # (2, 1) -> (2), names kept
rray_flatten(array(1:2, c(1, 2), dimnames = list(NULL, c("a", "b"))))
                                                  # (1, 2) -> (2), names dropped
```

Signature: `rray_flatten(x)`. Attributes only.

Files: `R/flatten.R`, `src/flatten.c`, `src/flatten.h`.

### `rray_squeeze()`

Drop axes whose dimension is 1.

```r
x <- array(1:10, c(10, 1))
rray_squeeze(x, 2)                       # (10, 1) -> (10)

y <- array(1:10, c(10, 1, 1))
rray_squeeze(y, c(2, 3))                 # (10, 1, 1) -> (10)
rray_squeeze(y, 2)                       # (10, 1, 1) -> (10, 1)
```

Names: follow the axis. Type: preserved. Surviving axes keep their names and
carry them to their new position.

Signature: `rray_squeeze(x, axes)`. Axes required, following the reduction
convention. Attributes only.

Files: `R/squeeze.R`, `src/squeeze.c`, `src/squeeze.h`.

### `rray_expand()`

Insert an axis of dimension 1.

```r
x <- array(1:10, c(5, 2))     # row names a-e, col names c1, c2
rray_expand(x, 1)             # (1, 5, 2)
rray_expand(x, 2)             # (5, 1, 2)
rray_expand(x, 3)             # (5, 2, 1)
```

Names: follow the axis. Type: preserved. In `rray_expand(x, 1)` the 5 row names
become the names of the new second axis, and the inserted first axis has none.
This is the difference from `rray_set_dimensions()`, which drops everything.

Signature: `rray_expand(x, axis)`. Single axis. Attributes only.

Files: `R/expand.R`, `src/expand.c`, `src/expand.h`.

### `rray_transpose()`

Permute axes.

```r
x <- array(1:6, c(3, 2))
rray_transpose(x, c(2, 1))           # (3, 2) -> (2, 3)

x_3d <- rray_broadcast(x, c(3, 2, 2))
rray_transpose(x_3d, c(3, 2, 1))     # (3, 2, 2) -> (2, 2, 3), reverses all axes
rray_transpose(x_3d, c(2, 1, 3))     # flips the first two, leaves the third
```

Names: follow the axis. Type: preserved. No dimension changes, so every axis
keeps its names and carries them to its new position.

Signature: `rray_transpose(x, permutation)`. Required, with no default, matching
`axes` on the reductions and on `rray_squeeze()`. A full reversal is easy enough
to write out:

```r
rray_transpose(x, rev(seq_len(rray_dimensionality(x))))
```

It moves data, so it needs a real C loop.

Needs a new iterator, or an existing one initialised with permuted strides. Work
that out in the pull request and say which you chose.

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

Names: follow the axis. Type: preserved. Tiled axes change dimension so they
lose their names, untiled axes keep theirs.

Signature: `rray_tile(x, times)`.

Files: `R/tile.R`, `src/tile.c`, `src/tile.h`, `src/tile-template.h`.

### `rray_flip()`

Reverse the order along an axis.

```r
x <- array(1:10, c(5, 2))
rray_flip(x, 1)      # reverse the rows
rray_flip(x, 2)      # reverse the columns
```

Names: follow the axis, with one exception. The flipped axis keeps its dimension,
so it keeps its names, but they must be **reversed** alongside the data rather
than copied across.

Type: preserved.

Signature: `rray_flip(x, axis)`. Single axis.

Files: `R/flip.R`, `src/flip.c`, `src/flip.h`, `src/flip-template.h`.

## 5.2 Elementwise arithmetic

Names: coalesce. Type: promoted.

All binary, all sharing one template and the pipeline from 2.4: promote, cast
both, find common dimensions, loop with two broadcast iterators.

| function | op |
|---|---|
| `rray_add(x, y)` | `+` |
| `rray_subtract(x, y)` | `-` |
| `rray_multiply(x, y)` | `*` |
| `rray_divide(x, y)` | `/` |
| `rray_power(x, y)` | `^` |
| `rray_modulo(x, y)` | `%%` |
| `rray_integer_divide(x, y)` | `%/%` |

There is no unary negation. `-x` already works on a bare array, so a function
for it would add nothing.

Match R's own semantics for missing values, `NaN`, and division by zero. Check
`/Users/davis/files/r/r-svn` when a case is unclear rather than guessing.

Files: `R/arithmetic.R`, `src/arithmetic.c`, `src/arithmetic.h`,
`src/arithmetic-template.h`.

## 5.3 Other elementwise numeric

### `rray_maximum()` and `rray_minimum()`

Elementwise maximum and minimum of two arrays, with broadcasting. Not to be
confused with `rray_max()` and `rray_min()`, which reduce.

Names: coalesce. Type: common, errors on `cpl`. Ops `maximum` and `minimum`,
which promote nothing, as 2.4 explains.

Signature: `rray_maximum(x, y, ..., na_rm = FALSE)`.

Files: `R/extremum.R`, `src/extremum.c`, `src/extremum.h`.

### `rray_clip()`

Bound values between a low and a high, elementwise.

```r
x <- matrix(1:10, ncol = 2)
rray_clip(x, 1, 5)
```

`low` and `high` are broadcast to `x`'s dimensions. They do not have to be
scalars, so each element can have its own bounds. `x` itself is never broadcast,
so the output always has `x`'s dimensions.

It is an error for any `low` to be greater than its matching `high`.

Names: follow the axis. Type: preserved, with `low` and `high` cast to `x`'s
type. Since `x` is never broadcast, every axis keeps its dimension, so in
practice all of `x`'s names survive and none of `low`'s or `high`'s are
consulted.

`x` is the subject here and the bounds are parameters, which is why neither rule
looks at `low` or `high`. This is the one function with several array inputs that
does not coalesce.

Signature: `rray_clip(x, low, high)`.

Files: `R/clip.R`, `src/clip.c`, `src/clip.h`.

### `rray_if_else()`

Elementwise choice between two arrays based on a logical array.

Follow vctrs' `vec_if_else()` closely. Read `R/if-else.R` and `src/if-else.c` in
vctrs before starting. The three things worth taking from it:

- **A `missing` argument.** If not `NULL`, it supplies the value wherever
  `condition` is `NA`, rather than forcing a missing value into the output. This
  is the main thing `ifelse()` gets wrong.

- **`missing` participates in the type.** The output type is the common type of
  `true`, `false` **and** `missing`, not just the first two.

- **A `ptype` override**, which wins over that common type.

The array specific part: `condition` drives the shape. `true`, `false` and
`missing` are broadcast to `condition`'s dimensions, and `condition` itself is
never broadcast.

Names: dropped. Type: common across `true`, `false` and `missing`, overridden by
`.ptype`. `condition` is cast to logical and takes no part in either rule.

**Names is the one place not to follow `vec_if_else()`.** It assigns names
elementwise, giving each output element the name from whichever branch supplied
it:

```r
vec_if_else(c(TRUE, FALSE, TRUE), c(a = 1, b = 2, c = 3), c(x = 9, y = 8, z = 7))
#> a y c
#> 1 8 3
```

That works because a vector's names are per element. An array's names live on
axes, so it does not translate: two elements in the same row can come from
different branches, and the row can only have one name. We drop all names
instead.

Signature: `rray_if_else(condition, true, false, ..., missing = NULL, .ptype =
NULL)`.

Files: `R/if-else.R`, `src/if-else.c`, `src/if-else.h`.

### `rray_full_like()`, `rray_ones_like()`, `rray_zeros_like()`

An array with the dimensions and type of `x`, filled with a single value.

```r
rray_full_like(x, 5)
rray_ones_like(x)
rray_zeros_like(x)
```

Names: dropped. Type: preserved, with `value` cast to `x`'s type.

These borrow `x`'s dimensions and type but none of its data, so the result is a
new array rather than a manipulated `x`. Names would no longer describe anything
that is actually there.

Signature: `rray_full_like(x, value)`, `rray_ones_like(x)`, `rray_zeros_like(x)`.

Files: `R/full-like.R`, `src/full-like.c`, `src/full-like.h`.

## 5.4 Comparison and logical

Names: coalesce. Type: fixed, logical output.

| function | meaning |
|---|---|
| `rray_equal(x, y)` | `==` |
| `rray_not_equal(x, y)` | `!=` |
| `rray_greater_than(x, y)` | `>` |
| `rray_greater_than_or_equal(x, y)` | `>=` |
| `rray_less_than(x, y)` | `<` |
| `rray_less_than_or_equal(x, y)` | `<=` |

Files: `R/compare.R`, `src/compare.c`, `src/compare.h`,
`src/compare-template.h`.

Logical operators take logical input and return a logical array.

| function | meaning |
|---|---|
| `rray_and(x, y)` | `&` |
| `rray_or(x, y)` | `\|` |

There is no negation. `!x` already works on a bare array, so a function for it
would add nothing.

Edge cases worth testing, from the original's documentation:

```r
x <- array(TRUE, c(1, 2))
rray_and(logical(), x)                        # common dimensions (0, 2)
rray_and(x, array(logical(), c(0, 1, 2)))     # common dimensions (0, 2, 2)
try(rray_and(x, array(logical(), c(1, 0))))   # 2 and 0 do not broadcast
```

Files: `R/logical.R`, `src/logical.c`, `src/logical.h`.

## 5.5 Reductions

All use the reduction iterator. All keep dimensionality, with reduced axes
collapsed to a dimension of 1. There is no `keep_dimensions` argument.

`axes` is **required** on every one of them. The original defaulted it to `NULL`
meaning "all axes". We do not.

Names: reduce.

| function | op | type rule |
|---|---|---|
| `rray_sum(x, axes, ..., na_rm = FALSE)` | `sum` | promoted, exists, retrofit in PR 11 |
| `rray_prod(x, axes, ..., na_rm = FALSE)` | `prod` | promoted, int to dbl |
| `rray_mean(x, axes, ..., na_rm = FALSE)` | `mean` | promoted, lgl and int to dbl |
| `rray_max(x, axes, ..., na_rm = FALSE)` | `max` | preserved, errors on cpl |
| `rray_min(x, axes, ..., na_rm = FALSE)` | `min` | preserved, errors on cpl |
| `rray_all(x, axes)` | | fixed, logical in, logical out |
| `rray_any(x, axes)` | | fixed, logical in, logical out |

`rray_max_pos(x, axis)` and `rray_min_pos(x, axis)` give the position of the
maximum or minimum along a single axis. Type: fixed, integer output.

```r
x <- array(c(1:10, 20:11), c(5, 2, 2))
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

Type: preserved throughout.

### `rray_subset()`, by index, keeps dimensionality

Never drops dimensions. Ignores trailing commas, so `x[1]` and `x[1, ]` agree.
Missing arguments select a whole axis.

```r
x <- array(1:8, c(2, 2, 2))

rray_subset(x, 1)        # first row, still (1, 2, 2)
rray_subset(x, , 1)      # all rows, first column, still (2, 1, 2)
```

Base R cannot do the second one without fully specifying every axis and passing
`drop = FALSE`.

Index types: integer-ish selects elements, logical must be length 1 or the
dimension, character requires names on that axis, `NULL` means 0.

Names: subset.

### `rray_extract()`, by index, always drops

Always returns a one dimensional result, and never keeps names.

```r
x <- array(1:16, c(2, 4, 2), dimnames = list(c("r1", "r2"), NULL, NULL))

rray_extract(x, 1)          # first row, flattened
rray_extract(x, 1, 1:2)     # first row, first two columns, flattened
```

Like `x[[i, j, ...]]` but each subscript may have length greater than 1.

Names: dropped.

### `rray_yank()`, by position, always flattens

Pulls elements out by their position in the flat array, ignoring dimensions
entirely. Always one dimensional.

```r
x <- array(10:17, c(2, 2, 2))

rray_yank(x, 1:3)
rray_yank(x, FALSE)
```

`i` is an integer vector of positions, a logical of length 1 or `rray_size(x)`,
or a logical with exactly `x`'s dimensions.

Names: dropped.

### `rray_slice()`

Subset a single axis by index, keeping dimensionality.

```r
rray_slice(x, i, axis)
```

Names: subset.

### The assignment forms

`rray_subset_assign()`, `rray_extract_assign()`, `rray_yank_assign()`,
`rray_slice_assign()`.

The original cast `value` to `x` rather than the other way round, and broadcast
`value` to the shape of the selection. Both decisions are worth re-examining in
the design review.

No `<-` replacement forms, consistent with the names API.

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

Names: coalesce on the axes that are not bound. The bound axis concatenates its
names, which needs its own rule worked out in the design review.

Type: common. This is the main consumer of `rray_type_common()`, so it takes a
`.ptype` argument for an override, matching `.dimensions` elsewhere.

Signatures: `rray_bind(..., .axis, .ptype = NULL)`, `rray_rbind(..., .ptype =
NULL)`, `rray_cbind(..., .ptype = NULL)`.

Files: `R/bind.R`, `src/bind.c`, `src/bind.h`, `src/bind-template.h`.

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

- **The container and inner type split.** rray4 has one set of type rules, and
  they are internal.

- **The purrr compatibility shims** in `compat-purrr.R`.

- **`rray_identity()`, `rray_elems()`.**

- **`rray_all_equal()`, `rray_any_not_equal()`.**

- **Unary wrappers around base operators.** No `rray_opposite()` and no
  negation, because `-x` and `!x` already work on a bare array.

Deferred rather than dropped:

- **Support for classed arrays**, through a proxy and restore system and a
  generic type system. Written up in full in `plans/extensions.md`, including
  why it is deferred and the cases that must shape its design.

- **Order and duplicates.** `rray_sort()`, `rray_unique()`,
  `rray_unique_loc()`, `rray_unique_count()`, `rray_duplicate_any()`,
  `rray_duplicate_detect()`, `rray_duplicate_id()`. Comparing whole slices
  rather than individual elements is a different shape of problem from
  everything else here, and it wants a comparison and equality story that does
  not exist yet. Design it properly when the rest of the package is working.

---

# Part 7: Possible improvements for later

Not now. Revisit once the foundations are in place and there is a working
version to measure against.

## Iterator runs

Every per type loop today steps the iterator once per element, even when no
broadcasting is happening and the mapping is the identity.

Instead of a bespoke fast path in each function, the iterator could report a
**run length**: the next N output elements map to N consecutive input locations,
or to the same location N times. Every loop then shares one shape:

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

Functions most likely to benefit: `rray_broadcast()`, the elementwise arithmetic
family, and `rray_tile()`.

## Unary elementwise math

There is no unary elementwise family today. `-x` works on a bare array already,
and `abs()`, `sqrt()` and friends are out of scope.

If one is ever wanted, it follows the shape of the other two families: an
`enum rray_unary_op` and an `rray_unary_type()` beside `rray_binary_type()` and
`rray_reduction_type()`.

## An unspecified type

`NA` is logical, so it sits at the bottom of the numeric tower and needs no
special handling for arithmetic. But `rray_bind(chr_array, NA)` fails, because
`rray_type2(chr, lgl)` is an error.

vctrs solves this with an unspecified type, and it has been painful. See whether
`rray_bind()` can live without it first.

## Classed arrays

See `plans/extensions.md`.
