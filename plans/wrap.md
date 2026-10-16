# An immutable `r_wrap()`

`r_wrap()` (`src/wrapper.c`) has a memory safety bug. This plan fixes it by
making the wrapper immutable: its data never changes after it is created.
rray keeps its zero-copy reshapes (`rray_set_dimensions()`,
`rray_insert_axes()`, `rray_remove_axes()`, the names and titles setters, and
the internal reshapes in `rray_split()`, `rray_stack()` and
`rray_unstack()`).

The work is one pull request. Part 1 explains the bug, Part 2 the design and
why it is safe, Part 3 the code changes, and Part 4 the tests.

Earlier versions of this plan (commits `9172a64` and `4565791`) took other
routes: locking the wrapper like base R, with `r_unwrap()` everywhere, and
then removing the wrapper entirely. Both are dropped. Nothing from them is
implemented yet.

---

# Part 1: The bug

When something asks the wrapper for a writable pointer, and the wrapped data
might be shared, the wrapper copies the data and puts the copy in `data1`.
Any read-only pointer handed out before that points into the old `data1`:

```c
const double* p = REAL_RO(w);
double* q = REAL(w);
q[0] = 99;
// p[0] == 1, q[0] == 99, w[1] == 99
```

That pointer is stale at best. It can also dangle: refcounts in R only go
down when a slot is overwritten, not when the object holding the reference is
garbage collected. So the old `data1` can look shared while nothing reachable
holds it, and the next GC frees it.

Base R's own wrapper had the same bug, fixed in R-devel by Luke Tierney
(`dbc99e13e8`, `c40bda6593`, `3580627fa3`). His fix locks the wrapper and
copies on the first pointer request of any kind.

There is a second, smaller problem. Today a pointer request on the wrapper
can allocate (the copy), and so can longjmp on an out of memory error. Code
calling `REAL_RO()` does not expect that, and IDEs like RStudio and Positron
(ark) are not always ready for a jump from such a point.

---

# Part 2: Design

## The rules

1. **`data1` never changes.** It is set once in `r_wrap()`. Every pointer
   into it stays valid for as long as the wrapper is reachable, because the
   wrapper keeps `data1` alive.

2. **`data1` is the input itself**, or the `data1` of a wrapper input. An
   ALTREP input from another class (a compact sequence like `1:n`, a
   deferred string, an arrow or vroom vector) is wrapped as is. We rely on
   that class keeping its data pointer stable, which is the basic contract of
   an ALTREP `Dataptr` method. Base R's own wrapper breaks that contract in
   released R versions, but that is base R's bug, every `dim<-` user is
   already exposed to it, and it is fixed in R-devel.

3. **Every wrapper is marked not mutable**, with `MARK_NOT_MUTABLE()`. R then
   treats the wrapper as shared, so it duplicates the wrapper before changing
   it in place.

4. **`Duplicate` always returns a plain vector**, never a wrapper. So every
   change R makes lands on a fresh vector R owns.

5. **A data pointer request never copies.** A read-only request returns a
   pointer into `data1`. A writable request on an atomic wrapper also returns
   a pointer into `data1`. A writable request on a character or list wrapper
   aborts, as it does today.

6. **`Set_elt` aborts.** Only code that writes into an object it does not
   own can reach it.

## Why rule 5 returns a pointer for writable requests

A lot of C code asks for a writable pointer only to read. `REAL()`,
`INTEGER()` and friends are writable requests, and in R 4.6 even `RAW_RO()`
calls `RAW()` internally (see the note in `src/rlang/vec.h`). In testing,
about 600 base R operations made 30,303 writable requests on wrappers. All
of them were reads. Aborting there would break base R.

## Why this is safe

`MARK_NOT_MUTABLE()` sets the refcount to its maximum, and
`DECREMENT_REFCNT()` never lowers it from there (`src/include/Defn.h`). Every
place where R reuses an object's memory, or writes into an argument instead
of allocating, first checks `NO_REFERENCES()`, `MAYBE_REFERENCED()` or
`MAYBE_SHARED()`. That is 72 checks across `arithmetic.c`, `subassign.c`,
`coerce.c`, `attrib.c`, `eval.c` and others. All of them see the wrapper as
shared, and copy first.

The case that matters most is reuse of a temporary. `w + 1` writes the result
into `w`'s memory when `w` has no references (`R_allocOrReuseVector()` in
`src/main/arithmetic.h`). That is how a temporary rray result gets a writable
request today. A not-mutable wrapper never passes that check.

A prototype of this design was tested against R 4.6:

- About 600 base R operations on wrappers of all seven types, sharing data
  with a vector still bound in R: 831 wrappers, 30,303 writable requests, no
  change to any wrapper's data or to any original vector. Character and list
  wrappers got no writable requests and no `Set_elt` calls.

- rray's own test suite: 4,310 wrappers, no change to any wrapper's data.
  The only writable requests from rray itself were raw reads, through the
  `RAW_RO()` issue above.

Part 4 turns this into a permanent test file, run on every CI platform.

## Allocation

The wrapper's own methods never allocate, apart from `Duplicate`, which R
expects to allocate. Dataptr, Dataptr_or_null, Elt, Get_region and Length only
read `data1`.

When `data1` is a plain vector, `REAL_RO()` and friends on a wrapper never
allocate and never longjmp. When `data1` is an ALTREP vector, its own
`Dataptr` may allocate on the first request, the way a compact sequence
expands itself. That is exactly what reading the input directly would do, so
the wrapper adds no new allocation point.

## Costs

- **The first change to a wrapper copies**, even when nothing else shares its
  data. `y <- rray_set_dimensions(x, d); y[1] <- 0` copies `y` once, and
  later changes to `y` happen in place. Base R's `dim<-` would usually have
  copied at the reshape instead, so overall this is no worse than base R.

- **Code that breaks R's rules writes through to the original.** C code that
  writes into an object through `REAL()` without checking whether it is
  shared would write into the user's original vector. That is wrong values,
  not unsafe memory, and the same code already does it to any ordinary shared
  vector.

---

# Part 3: Changes to `src/wrapper.c`

Keep the file ordered top down per AGENTS.md: `r_wrap()`, then the class,
data, generic, dataptr, element, and test helper sections, as today.

## `r_wrap()`

```c
r_obj* r_wrap(r_obj* x) {
  R_altrep_class_t cls = wrapper_class(r_typeof(x));

  r_obj* data = is_wrapper(x) ? wrapper_data(x) : x;

  r_obj* out = KEEP(R_new_altrep(cls, data, r_null));

  r_attrib_clone_from(out, x);

  MARK_NOT_MUTABLE(out);

  FREE(1);
  return out;
}
```

## Data section

Replace `wrapper_readonly()` and `wrapper_writable()` with:

```c
static inline r_obj* wrapper_data(r_obj* x) {
  return R_altrep_data1(x);
}
```

Wrapping a wrapper shares its `data1`, as today. Since `data1` never changes,
sharing it between wrappers is safe.

Update the declarations in `src/decl/wrapper-decl.h`.

## Generic section

- `wrapper_duplicate()`: deep returns `Rf_duplicate(wrapper_data(x))`, as
  today. Shallow returns `r_clone_data(wrapper_data(x))` instead of
  `r_wrap(x)`. R's default `DuplicateEX` copies the wrapper's attributes onto
  either result.

- `wrapper_inspect()` and `wrapper_length()`: `wrapper_data(x)`.

## Dataptr section

For each atomic type, both branches read `data1`:

```c
static void* wrapper_double_dataptr(r_obj* x, Rboolean writeable) {
  return (void*) r_dbl_cbegin(wrapper_data(x));
}
```

The `writeable` parameter stays in the signature, since R requires it, and
goes unused.

Character and list keep aborting on a writable request, and read
`wrapper_data(x)` otherwise.

## Dataptr_or_null, Elt and Get_region sections

`wrapper_readonly(x)` becomes `wrapper_data(x)`. No other change.

## Set_elt section

`wrapper_character_set_elt()` and `wrapper_list_set_elt()` abort, with the
same kind of message as the writable dataptr methods:

```c
static void wrapper_character_set_elt(r_obj* x, r_ssize i, r_obj* v) {
  r_abort("Can't set an element of a character wrapper.");
}
```

Keep them registered, so the message is ours rather than R's default.

## Test helpers

- Remove `ffi_test_wrapper_writable()`.

- Rename `ffi_test_wrapper_readonly()` to `ffi_test_wrapper_data()`. It
  aborts if `ffi_x` is not a wrapper, since reading `data1` from anything else
  crashes R.

- Add `ffi_test_is_altrep()`, returning `r_lgl(ALTREP(ffi_x))`.

- Add `ffi_test_wrapper_pointer_is_stable()`. For each type, take a
  read-only pointer, then a writable one (atomic types) or a second read-only
  one (character, list), in two separate statements, since the evaluation
  order of the operands of `==` is unspecified in C. Return whether they are
  equal, and equal to the pointer into `wrapper_data(ffi_x)`.

Update `src/init.c` to match.

## Existing comments

AGENTS.md says existing comments stay exactly as they are. Comments attached
to code this plan deletes go with that code: the comment inside
`wrapper_writable()`, and the one on the shallow branch of
`wrapper_duplicate()`. Comments whose code stays but now describe the old
behavior stay as they are, and get quoted in the pull request description so
Davis can update them:

- The header comment of `r_wrap()`: "Writable access forces a shallow
  duplication of the wrapped object if it EVER looks shared, ...".

- In `r_wrap()`: "We manage writable access to the underlying data by always
  cloning it before giving out a writable handle if it is ever
  `MAYBE_SHARED()` ...".

- In `r_wrap()`: "Reference count on `data` is bumped when we wrap it here.
  It now looks at least "referenced", but not necessarily "shared"." The
  refcount of `data` no longer matters.

- In `wrapper_duplicate()`: "Wrapper only guarantees shallow ownership."

---

# Part 4: Tests

## `tests/testthat/helper-wrapper.R`

Remove `wrapper_readonly()`, `wrapper_writable()`,
`expect_identical_addresses()` and `expect_different_addresses()`. Add:

```r
wrapper_data <- function(x) {
  .Call(ffi_test_wrapper_data, x)
}
is_altrep <- function(x) {
  .Call(ffi_test_is_altrep, x)
}
wrapper_pointer_is_stable <- function(x) {
  .Call(ffi_test_wrapper_pointer_is_stable, x)
}

expect_wraps <- function(w, x) {
  expect_identical(obj_address(wrapper_data(w)), obj_address(x))
}
```

## `tests/testthat/test-wrapper.R`

Rewrite the "Ownership" tests for the new behavior. Keep the basics,
attributes, wrapping-a-wrapper, serialization, zero-length, NA and subsetting
tests, with `wrapper_readonly()` replaced by `wrapper_data()`.

- "wrapper starts out sharing data": `expect_wraps(w, x)`.

- "a read-only pointer stays valid after a writable request": for every
  type, `expect_identical(wrapper_pointer_is_stable(w), TRUE)`, then
  `expect_wraps(w, x)` and the original `x` unchanged.

- "changing a wrapper gives a plain vector": for each atomic type,
  `w <- wrap(x); w[1] <- <value>`, then `is_wrapper(w)` is `FALSE`, `w[1]` is
  the new value, and `x` is unchanged. The same for `[[<-` on a list wrapper
  and `[<-` on a character wrapper. This replaces the four "write access
  triggers ownership" and "second write does not re-clone" tests.

- "a second change happens in place": after `w[1] <- <value>`, record
  `obj_address(w)`, do `w[2] <- <value>`, and expect the same address.

- "a temporary wrapper is never reused as an output":
  `rray_set_dimensions(x, d) + 1` leaves `x` unchanged. This is the
  `R_allocOrReuseVector()` case.

- "shallow duplicate of a wrapper is a plain vector": replaces "shallow
  duplicate of wrapper produces another wrapper". `y <- w; dim(y) <- d`, then
  `is_wrapper(y)` is `FALSE` and `x` is unchanged.

- "wrapping an ALTREP vector shares it": `x <- 1:100; w <- wrap(x)`, then
  `is_altrep(wrapper_data(w))` is `TRUE`, `expect_wraps(w, x)`, and `w`
  equals `1:100`.

- "wrapping a wrapper shares its data": `expect_wraps(wrap(wrap(x)), x)`.

- "setting an element of a character or list wrapper errors": call the
  `Set_elt` method from C through a small test helper, and snapshot the error.
  R never reaches it on its own, so only a direct call can test it.

## `tests/testthat/test-wrapper-mutations.R`

A battery that checks base R never writes into a wrapper. It uses the real
wrapper, through `rray_set_dimensions()`, with no instrumentation, so it runs
as is on every R version and OS in the CI matrix (release on macOS, Windows
and Linux, devel, and oldrel-1 to oldrel-3 on Linux).

### What each case checks

For one type and one operation `f`:

```r
x <- mutation_vector(type)
x_before <- unserialize(serialize(x, NULL))

w <- rray_set_dimensions(x, c(10L, 10L))
plain <- unserialize(serialize(w, NULL))

expect_identical(f(w), f(plain), info = name)
expect_identical(x, x_before, info = name)
expect_identical(w, plain, info = name)
expect_wraps(w, x)
```

- `f(w)` equals `f(plain)`: the wrapper behaves exactly like a plain array.
  `serialize()` writes a wrapper as an ordinary vector, so `plain` is a plain
  array with the same values and attributes.

- `x` is unchanged: nothing wrote into the shared data.

- `w` is unchanged, and still wraps `x`: nothing changed the wrapper.

A write through a writable pointer into `data1` would change `x`, because
they are the same memory. A character or list operation that asked for a
writable pointer or called `Set_elt` would error, and fail the case.

### Layout

`tests/testthat/helper-wrapper-mutations.R` holds:

- `mutation_vector(type)`: a length 100 vector for each of the seven types,
  as plain vectors (for example `as.double(1:100) + 0.5` and `1:100 + 0L`).
  Two more types cover wrapped ALTREP data: `altrep_integer` (`1:100`, a
  compact sequence) and `altrep_character` (`as.character(1:100)`, a
  deferred string). They reuse the integer and character cases.

- `mutation_cases(type)`: a named list of one-argument functions for that
  type. It combines the cases for the type with the cases that apply to
  every type.

- `expect_no_mutation(type)`: runs the check above for every case of the
  type.

`test-wrapper-mutations.R` then has one `test_that()` per type, each calling
`expect_no_mutation()`. AGENTS.md forbids code outside `test_that()` in a test
file, which is why the case lists live in the helper.

### Cases

Every type:

- printing and formatting: `print()` (through `capture.output()`),
  `format()`, `str()`, `summary()`, `head()`, `dput()`.

- comparison: `identical()`, `all.equal()`.

- serialization: `serialize()` round trip, `saveRDS()` and `readRDS()`.

- subsetting: `w[1:5, 2]`, `w[[3]]`, `split()`, `match()`, `%in%`, `rev()`,
  `as.list()`, `lapply()` over elements, `do.call(c, list(w, w))`.

- changes on a copy: `y <- w` followed by each of `y[1] <- y[2]`,
  `y[[2]] <- y[[3]]`, `y[] <- y[1]`, `dim(y) <- c(20L, 5L)`,
  `dim(y) <- NULL`, `dimnames(y) <- ...`, `attr(y, "foo") <- 1`,
  `class(y) <- "foo"`, `diag(y) <- y[1]`, and `is.na(y) <- 3` (not raw).

- a single binding: `local({ y <- rray_set_dimensions(w, 100L); y[1] <- y[2];
  y })`.

- rray on a wrapper: `rray_slice(rray_reverse(w, axes = 1L), 1:3, 1:2)`.

Double and integer:

- arithmetic: `w + 1`, `1 + w`, `w * w`, `-w`, `w^2`, `w %% 3`, `w %/% 3`,
  `w == 1`, `(w + 0) + 1`.

- math: `abs()`, `sqrt(abs())`, `exp(w / 1000)`, `round()`, `floor()`.

- reductions and cumulatives: `sum()`, `mean()`, `range()`, `var()`,
  `median()`, `quantile()`, `cumsum()`, `cumprod(w / 100)`, `cummax()`.

- sorting and ranking: `sort()`, `sort(method = "radix")`,
  `sort(partial = 5)`, `order()`, `rank()`, `unique()`, `duplicated()`,
  `table()`, `which.max()`, `findInterval()`, `cut()`, `rle()`.

- missing values: `is.na()`, `anyNA()`.

- matrices: `t()`, `aperm()`, `%*%`, `crossprod()`,
  `solve(w + diag(1000, 10))`, `qr()`, `svd()`, `det()`,
  `chol(crossprod(w) + diag(10))`, `apply(w, 1, sum)`, `sweep()`, `scale()`,
  `rowSums()`, `colMeans()`, `diag()`, `outer()`, `kronecker()`.

- conversion and combining: `matrix()`, `array()`, `as.vector()`,
  `as.integer()`, `as.character()`, `c()`, `rbind()`, `cbind()`, `unlist()`,
  `vapply()`, `ifelse()`, `pmin()`, `replace()`, `diff()`, `fft()`,
  `stats::filter()`.

Logical: `!`, `&`, `xor()`, `which()`, `any()`, `sum()`, `ifelse()`,
`!(!w)`.

Complex: `Re()`, `Mod()`, `Conj()`, `w + 1i`, `sqrt()`, `fft()`, `%*%`,
`solve()`, `cumsum()`.

Raw: `as.integer()`, `xor()`, `rawToChar()`, `rawShift()`.

Character: `toupper()`, `nchar()`, `paste0()`, `gsub()`, `substr<-` on a
copy, `sprintf()`, `strsplit()`, `sort()`, `unique()`, `factor()`,
`enc2utf8()`, `trimws()`, `startsWith()`, `chartr()`, `Encoding<-` on a copy.

List: `lapply()`, `unlist()`, `rapply()`, `sapply()`, `vapply()`, `mapply()`,
`do.call(sum, ...)`, `y[[1]] <- "z"` and `y[2] <- list(NULL)` on a copy.

---

# Part 5: Checks

1. `clang-format -i src/*.c src/*.h`, over all files.

2. `air format .`

3. `Rscript -e "devtools::test()"`, all green, including both wrapper test
   files.

4. The protection pass from AGENTS.md, as a separate step. In particular,
   every method other than `Duplicate` and the aborting ones does no
   allocation of its own.

5. No new comments anywhere in `src/` or `tests/`. The stale comments from
   Part 3 are quoted in the pull request description.

6. `Rscript -e "devtools::check()"`.

7. CI green on the whole matrix, which is the point of the mutation battery.
