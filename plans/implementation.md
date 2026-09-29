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
Part 4 is the catalogue of functions still to build.

## How to use this plan

Read Part 1 and Part 2 before starting any pull request. They are short on
purpose.

Then find your function in Part 4. Each entry says what to build, which files
to touch, and what "done" means.

One pull request per entry. Keep them small.

---

# Part 1: Conventions

Terminology (size, axis, dimension, dimensions, dimensionality) is defined in
`CLAUDE.md`. Use it exactly.

## Files

Each feature is a `src/{name}.c` and `src/{name}.h` pair, with an R file at
`R/{name}.R` and tests at `tests/testthat/test-{name}.R`.

Functions that need per type code split into a shell and a core, both living in
`src/{name}.c`. Copy the shape of `src/broadcast.c`.

The shell is ordinary C, written once. It holds everything that does not depend
on the type: argument checks, dimension arithmetic, iterator setup and names. It
ends in a `switch` on `r_typeof(x)` that hands off to the core.

The core is one small function per type. It takes `x`, the output size and the
iterator, allocates the output at its own type, fills it, and returns it.
Allocation belongs to the core because the type is the one thing the shell does
not know.

Putting the core behind a function call costs nothing. It was benchmarked across
five types and four shapes, and every case landed within 1%, which is the run to
run noise, because the call happens once per array rather than once per element.
Do not re-open this.

Two families vary from this.

`rray_split()` returns a list of arrays rather than one array, and `r_typeof(x)`
is enough to allocate every element at the right type, so the shell builds the
list and the core fills what it is handed.

The binary arithmetic operators share one shell across many files, so the shell
lives in `src/arithmetic.c` and each operator gets its own
`src/arithmetic-{op}.c`. They switch on the type pair rather than on one type,
and the switch returns the core instead of calling it. 2.4 covers why.

The reducers that take `(x, axes, ..., na_rm = FALSE)` share the same shape of
shell for the same reason. `rray_reduce()` and the `RRAY_REDUCE` macro live in
`src/reduce.c`/`src/reduce.h`, and each reducer gets its own
`src/reduce-{name}.c`, switching on `rray_typeof(x)` and returning the core.
`rray_sum_along()` is the first of these, in `src/reduce-sum.c`.

Each core's whole body is a call into a macro, following `SLICE` in vctrs'
`src/slice.c`. There are usually two. `RRAY_{NAME}_ATOMIC` covers `lgl`, `int`,
`dbl`, `cpl` and `raw`, which write straight to a data pointer.
`RRAY_{NAME}_BARRIER` covers `chr` and `list`, which write through the barrier.
Undefine both once the cores are written.

The arithmetic family needs only one macro, since it has no `chr` or `list`
cores, but several files share it. So `RRAY_ARITHMETIC` lives in
`src/arithmetic.h` and nothing undefines it. Only undefine a macro the file
defined itself.

Write each core's parameter list out in full. Do not hide it behind a macro.

A flag that swaps the scalar operation, like `na_rm`, is folded into the same
switch that dispatches on type, returning the core instead of calling it:

```c
case RRAY_TYPE_double:
  return na_rm ? rray_sum_along_dbl_na_rm : rray_sum_along_dbl;
```

So there is one core per type per variant, the flag stays off the core's
parameter list, and the loop is written once. `rray_max_along()` and
`rray_min_along()` want the same shape when they land, sharing `rray_reduce()`'s
shell. `rray_max_pos()` and `rray_min_pos()` take a single `axis` rather than
`axes` and return positions rather than reduced values, so whether they fit this
shell at all is still an open question for whoever picks them up.

A `.c` file reads top down: the main entry point first, its helpers below, in
the order they are used. For `src/broadcast.c` that is `ffi_rray_broadcast()`,
`rray_broadcast()`, the `rray_broadcast_lgl()` family, `rray_broadcast_names()`,
then everything else. You should meet the shell before the cores it dispatches
to.

`src/decl/{name}-decl.h` is what makes that ordering work. Declare every helper
there, so the `.c` file never needs a forward declaration of its own.

A decl header has no includes and no include guard. It is included last, from
exactly one `.c` file, so everything it names is already in scope and there is
nothing to guard against. It could not reach a sibling header in `src/` anyway,
since only `src/rlang` is on the include path.

## Naming

- Internal C: `rray_{name}()`, returns C types.

- FFI wrappers: `ffi_rray_{name}()`, thin SEXP bridges, placed above the
  internal functions in the `.c` file.

- Typed cores are file static, and keep the prefix with a type suffix, e.g.
  `rray_broadcast_dbl()`. A binary core carries both input types, `x` first, so
  `rray_add_int_dbl()` reads integers from `x` and doubles from `y`.

- Headers declare internal functions only, never FFI wrappers.

- `src/init.c` uses `extern` declarations, it does not include feature headers.

- An `arg` is a `struct rray_arg*`, the argument tag ported from vctrs. Tags
  nest, and nothing is materialised until an error is actually raised, so
  carrying one through a loop costs nothing. `src/arg.c` is the whole of it.

  `rray_args` holds the fixed ones, so `check_unclassed(x, rray_args.x, ...)`.
  A function taking `...` builds one tag with `new_subscript_arg()`, which reads
  a pointer to the caller's loop index and gives an input's name when it has one
  and `..2` otherwise. `rray_arg_format()` turns a tag into a string with the
  backticks already on, so messages write `%s` and not `` `%s` ``.

## C style

Follow vctrs and rlang closely. Prefer an rlang wrapper over the raw R API every
time (`r_globals.na_int`, `r_length()`, `r_attrib_get()`).

Prefer `r_ssize` over `int`. Mark variables `const` where you can. Grab a data
pointer before a loop rather than indexing the `r_obj*`.

Protect anything that allocates with `KEEP()` and `FREE()`.

**Ruthlessly avoid protection issues.** A value that allocates is unprotected
the instant it exists, including one built inline as a function argument, e.g.
`f(x, r_chr(arg))`. If the callee allocates anything before it uses that
argument, a GC in that window can collect it out from under you. Trace every
allocation forward to its last use and make sure something on the protection
stack covers it the whole way, not just at the call site that looks risky. This
class of bug compiles fine, passes tests that don't happen to trigger a GC, and
only shows up as a rare crash or corrupted value. Check for it explicitly in
every pull request that touches C code, don't wait for it to be caught in
review.

Allocate a names list only once an axis actually survives, so the common case
where nothing survives allocates nothing:

```c
r_obj* out = r_null;
r_keep_loc out_loc;
KEEP_HERE(out, &out_loc);
```

Allocate inside the loop, right before the first assignment, and reprotect with
`KEEP_AT()`. `rray_broadcast_names()` is the example.

`#include "decl/{name}-decl.h"` always goes last, after every other include,
separated from them by one blank line.

Never touch `src/rlang/`.

Run `clang-format -i src/*.c src/*.h` over all files after any C change.

## Comments

**Do not write comments.** Not in C, not in R. The only exception is roxygen2 on
exported R functions. This overrides the usual defaults.

This is not "write fewer comments", it is "write none". Do not explain what the
code does, do not justify a choice, do not label a section, do not flag a tricky
line. Not even one short line. If you think your comment is the exception
because it explains something genuinely non-obvious, it is not.

Comments already in the code stay. Leave them exactly as they are, and do not
add new ones next to them. Anything that needs explaining goes in the pull
request, not the source.

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
per type cores cover all seven unless a function says otherwise.

Three rules at the boundary:

**Bare input only.** A small `check_unclassed()` helper tests `r_is_object()` and
errors if it is true. Bare matrices and arrays pass, because `matrix` and `array`
are implicit classes with no attribute set. Anything else is refused rather than
silently unclassed or silently corrupted.

**A bare vector becomes a one dimensional array.** `1:5` is treated as
`array(1:5, 5L)`, and any `names` move to `dimnames`. This is what
`arg_as_array()` does, once `check_unclassed()` has passed.

**Arrays always come back.** `rray_sum_along(1:5, 1)` returns `array(15L, 1L)`,
not
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

- Anything else is an error, with a message naming the axis, both dimensions and
  both inputs. Which input owns the existing dimension is tracked per axis,
  since the input that set axis 1 can differ from the one that set axis 2. That
  is why vctrs' single `arg-counter.c` counter does not fit here. Only
  `rray_dimensions_common()` needs that tracking. `rray_dimensions2()` is the
  two input form, and with two inputs a conflict always names `x` and `y`.

- Missing trailing axes are treated as a dimension of 1, so dimensionality can
  grow.

Dimensionality can never shrink. `rray_broadcast()` errors on any decrease, even
when the dimensions being dropped are all 1.

A `NULL` is not an array, so anything taking arrays refuses it. That holds for
one input and for `...` alike, so `rray_dimensions_common(NULL, 1:5)` is an
error, not a way to skip an argument. A caller holding an array that might be
absent drops it before the call.

The one exception is `.dimensions`. It returns before `...` is looked at, so
nothing in `...` is checked when it is supplied. That matches `.size` in
`vec_size_common()`.

`rray_broadcast_common()` keeps the names of `...` on the list it returns, the
same way `vec_recycle_common()` keeps them.

`RRAY_MAX_DIMENSIONALITY` is 64.

Long arrays are supported where it is not painful. R allows an array's total
length to exceed 2^31 while every individual dimension stays under it, because
`dim` is always integer. That is why `rray_size()` returns a double while
`rray_dimensions()` returns integer. Use `r_ssize` throughout. If long array
support makes something genuinely hard, drop it and say so in the pull request.

## 2.3 Names

There are five rules. Every function in Part 4 declares which one it follows.

**Follow the axis.** An axis keeps its names if its dimension is unchanged, and
loses them if its dimension changed. Whatever it keeps travels with it to
wherever the axis ends up. New axes have no names.

This is the rule for almost everything that reshapes an array. An axis of
dimension 1 broadcast to 1 counts as unchanged and keeps its length 1 names.

**Reduce.** Reduced axes lose their names. Every other axis keeps them.

Nearly the same rule, but a reduced axis loses its names even when its dimension
was already 1, so it needs stating separately.

This lives in `rray_reduce_names(x, axes)`. Reductions take one array, so unlike
the broadcasting family below there is no `2` or `_common` form, and no fill
helper to share between them.

**Coalesce.** Used by anything with two or more array inputs. For each axis of
the common dimensions:

1. Use `x`'s names if `x`'s dimension there equals the common dimension and `x`
   has names there.

2. Otherwise apply the same test to `y`.

3. Otherwise `NULL`.

This lives in three internal functions, one per arity:

```c
r_obj* rray_broadcast_names(r_obj* x, r_obj* dimensions);
r_obj* rray_broadcast_names2(r_obj* x, r_obj* y, r_obj* dimensions);
r_obj* rray_broadcast_names_common(r_obj* xs, r_obj* dimensions);
```

All three fill one output list from a shared helper, never overwriting a slot
that an earlier input already filled. Coalescing is just that "don't overwrite"
rule, so the one-input case is the follow the axis rule and needs no separate
implementation.

They take arrays rather than extracted names, because `arg_as_array()` gives
every input a real `dim`, which makes `r_dim()` and `r_dim_names()` free reads
off an object the caller already protects. They assume validated, broadcastable
inputs, so they take no `arg` and no `error_call` and cannot fail. Validation
belongs in the caller, which has always done it already.

**Subset.** Names are subset alongside the data, so an axis keeps the names of
the elements that survived.

`rray_split()` is the one case producing many arrays at once, so it owns its
own name handling rather than calling into this family. Each output slices the
names on the split axis over the same range as its data, and carries the names
on every other axis over whole.

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

There is no user facing type system.

A **type** is an `enum rray_type`, our own enum holding the seven native types
plus `RRAY_TYPE_scalar`. Restricting it this way means a `switch` over a type
can be exhaustive with no `default`, so the compiler catches a missing case.
`rray_typeof()` reads one off an object and `rray_type_as_c_string()` names one
in an error message. Both live in `src/type.c` and both are written out arm by
arm, rather than translating to an `enum r_type` and borrowing rlang's answer,
so `RRAY_TYPE_scalar` gets a real answer like every other type.

`RRAY_TYPE_scalar` is the fall through for anything that is not a native type,
following vctrs' `VCTRS_TYPE_scalar`. It means `rray_typeof()` is total and
never errors, so there is no separate validating step. An invalid input already
arrives as a type, and rejecting it is one more arm of a switch that was being
written anyway.

A **ptype** is the empty vector standing for a type, so `double()` for
`RRAY_TYPE_double`. There is one of each in rlang's `r_globals`, already built
at load and marked shared, so anything returning a ptype hands one of those back
rather than allocating. A ptype is a bare vector, not an array: it is a type
token, not data, so it carries no `dim`.

`rray_ptype(x, arg, error_call)` takes an object to its ptype. It is the whole
of validating an input and reducing it to a type, so the scalar case is just
another arm of its switch:

```c
case RRAY_TYPE_list:
  return r_globals.empty_list;
case RRAY_TYPE_scalar:
  stop_scalar_input(x, arg, error_call);
```

`stop_scalar_input()` raises the "must be an array" error, and takes the object
so it can name the offending type. `rray_ptype2()` and `rray_cast()` reject
scalars the same way, from the bottom of their own switches, so neither opens
with a run of `if` checks before it gets to work.

The internal type rules exist because three kinds of function need them:

- `rray_combine()` needs a common type across many inputs.

- `rray_add()` needs a common type across two inputs, plus a promotion that
  depends on the operator.

- `rray_sum_along()` needs a promotion that depends on the operator.

The C interface is small, and takes and returns `r_obj*` ptypes the way vctrs
does:

```c
r_obj* rray_ptype2(
  r_obj* x,
  r_obj* y,
  enum rray_side* side,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);
r_obj* rray_ptype_common(
  r_obj* xs,
  r_obj* ptype,
  struct rray_arg* arg,
  struct rray_arg* ptype_arg,
  struct r_lazy error_call
);

r_obj* rray_cast(
  r_obj* x,
  r_obj* to,
  struct rray_arg* x_arg,
  struct rray_arg* to_arg,
  struct r_lazy error_call
);
r_obj* rray_cast_common(
  r_obj* xs,
  r_obj* to,
  struct rray_arg* arg,
  struct rray_arg* to_arg,
  struct r_lazy error_call
);
```

`ptype` and `to` may be `NULL` on the `_common` pair, in which case the common
type of `xs` is computed. That is the same shape as `.dimensions` in
`rray_dimensions_common()`: when it is supplied, `...` is never looked at.

Working out a common type has to start somewhere, and `NULL` is not a type here,
so it starts at the first input. That means `rray_ptype_common()` needs at least
one input or a `ptype`, and errors on neither. `plans/null.md` writes up what it
would take to lift that.

### Argument tags and the error call

Every one of these takes a tag per input, and the R wrappers expose them so a
caller can make an error blame its own argument and its own call. The FFI builds
each one with `new_lazy_arg()`, reading the tag out of the frame only once an
error is actually raised, so a default of `caller_arg(x)` costs nothing when
nothing goes wrong. The error call is read out of the frame the same way.

```r
rray_ptype(x, ..., arg = caller_arg(x), call = caller_env())
rray_ptype2(
  x,
  y,
  ...,
  x_arg = caller_arg(x),
  y_arg = caller_arg(y),
  call = caller_env()
)
rray_ptype_common(
  ...,
  .ptype = NULL,
  .arg = "",
  .ptype_arg = ".ptype",
  .call = caller_env()
)
rray_cast(x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env())
rray_cast_common(
  ...,
  .to = NULL,
  .arg = "",
  .to_arg = ".to",
  .call = caller_env()
)
```

`call = caller_env()` means the wrapper is blamed rather than the rray4
function, which is the point of taking it:

```r
f <- function(a, b) rray_ptype2(a, b)
f(1L, "a")
#> Error in `f()`:
#> ! Can't combine `a` <integer> and `b` <character>.
```

Called straight from the top level there is no wrapper to blame, so the message
has no `Error in` at all. That is rlang's behaviour for the global environment,
and it is what vctrs does too.

The defaults follow vctrs. An input the caller names gets `caller_arg()`, so the
error quotes what they actually wrote. A prototype does not, because `to` is a
positional argument holding an anonymous type and naming it says nothing. `.arg`
is the tag for `...` as a whole, so `.arg = "foo"` turns `..2` into `foo[[2]]`.

`.to_arg` and `.ptype_arg` are the exception, and name their argument by
default. Those are arguments the caller typed, so a bad one is worth pointing
at. vctrs has no equivalent and hardcodes `.ptype` even inside
`vec_cast_common()`, where the argument is really called `.to`.

An empty tag drops out of the message rather than printing empty backticks:

```r
rray_ptype2(1L, "a", x_arg = "", y_arg = "")
#> Error: Can't combine <integer> and <character>.
```

`rray_arg_type_format()` writes the `` `x` <integer> `` half of those messages
and handles the empty case. `rray_arg_format_input()` does the same for a
message that opens with the tag, falling back to the word "Input".

`rray_ptype2()` dispatches through `rray_typeof2()`, which maps a pair of types
onto a symmetric `enum rray_type2` with one entry per unordered pair. Both
switches are written out in full, following vctrs' `vec_typeof2()` and
`vec_ptype2_switch_native()`. Do not collapse either into a rank function or any
other arithmetic shortcut.

Because the pair is symmetric, `RRAY_TYPE2_logical_integer` cannot say which of
the two inputs the common type came from. `rray_typeof2()` reports that
separately through an `enum rray_side` out parameter, set to `RRAY_SIDE_left`,
`RRAY_SIDE_right` or `RRAY_SIDE_both` on every arm. It is vctrs' `int* left`
under a clearer name.

`rray_ptype_common()` is why it exists. It combines `xs` left to right, and has
to keep a tag pointing at whichever input set the running type so that its error
names the right one:

```r
rray_ptype_common(1L, 2.5, "a")
#> Error: Can't combine `..2` <double> and `..3` <character>.
```

It moves that tag when the side comes back `RRAY_SIDE_right`, and leaves it
where it is otherwise.

`rray_cast()` changes type only. It never touches dimensions or names.
Broadcasting is always a separate step. Lossy casts are an error, and the error
says what was lost and where.

It does not use the pair enum, because a cast has a direction and the pair does
not. It is a switch on `x`'s type wrapping a switch on the target type, with the
pairs that do not convert falling to a `default` arm. Those are the only
switches over an `enum rray_type` that are not written out in full, and they are
that way because most of the table is an error.

Casting up the tower always works. Casting down works only when nothing is lost:

```r
rray_cast(c(0, 1), logical())       # fine
try(rray_cast(c(0, 2), logical()))  # 2 is not a logical value
```

Complex is one way. Anything can cast into it, nothing casts out of it, matching
vctrs.

Casting into complex zeroes the imaginary part, so a missing value lands in the
real part alone and never becomes `NA_complex_`:

```r
Im(rray_cast(NA_real_, complex()))  # 0, not NA
```

That is what R itself does. `ComplexFromReal()` in `src/main/coerce.c` guards
the `NA_complex_` branch behind `NA_TO_COMPLEX_NA`, which is never defined, so
every build takes the `z.r = x; z.i = 0;` path. vctrs agrees for integer and
double but returns a full `NA_complex_` for logical, so do not copy it here.

Files: `src/type.c` for `enum rray_type`, `src/typeof2.c` for the pair enum and
`enum rray_side`, then `src/ptype.c`, `src/ptype-common.c`, `src/cast.c` and
`src/cast-common.c`. `src/cast.h` holds the scalar casts as `static inline`
functions under a `Lossless` and a `Lossy` section, so `src/cast.c` and the
arithmetic cores share one definition of each.

### The common type rules

- Numeric tower: `lgl` to `int` to `dbl` to `cpl`.

- `chr`, `list` and `raw` each stand alone. They combine only with themselves.

There is no fallback and no coercion across families. `rray_ptype2(chr, int)` is
an error.

A user facing function that needs a type override takes a `.ptype` argument,
matching `.dimensions` elsewhere. It takes a prototype object, so
`.ptype = double()` means "a double array", and we reduce it with
`rray_ptype()`.

Note that `NA` is logical, so it sits at the bottom of the numeric tower and
`rray_add(x, NA)` works with no special handling. vctrs needs an unspecified
type for this. We do not, at least until `rray_combine()` wants
`rray_combine(chr_array, NA)` to work. See Part 6.

### Operator promotion

Some operators need a type the common type rules cannot give, because
`lgl + lgl` is `int` and `int / int` is `dbl`.

There is no promotion function and no operator enum. Each operator writes its
row of the tables below out as a `switch` on the types of its inputs, and each
arm names the core that handles that case. A binary operator switches over
`enum rray_type2`, one arm per unordered pair. A reduction takes one array, so
it switches over `enum rray_type`.

A binary core is specialised on all three types at once: what `x` holds, what
`y` holds, and what comes out. So `rray_add_int_dbl()` reads integers from `x`,
doubles from `y`, and writes doubles.

That means nothing is ever cast up front. `rray_add(int_array, 1)` allocates its
double output and nothing else, even though a bare `1` is a double, because the
integers convert one element at a time inside the loop.

Because the pair enum is symmetric, `RRAY_TYPE2_integer_double` cannot say which
input was which. The `enum rray_side` out parameter of `rray_typeof2()` does, so
the arm picks between two cores:

```c
case RRAY_TYPE2_integer_double:
  return (side == RRAY_SIDE_right) ? rray_add_int_dbl : rray_add_dbl_int;
```

Four supported types in either position is 16 cores per operator. That is a lot
of function definitions, but each body is a single `RRAY_ARITHMETIC` call and
the only real logic is the scalar operation, of which there are three per
operator, one per output type.

The switch picks the core rather than running it, and the shell calls it before
any dimension work. So a type error always beats a dimension error:

```r
rray_add(array("a", c(2, 2)), array("b", c(3, 3)))
#> Error in `rray_add()`:
#> ! Can't apply `+` to `x` <character> and `y` <character>.
```

The per element conversions live in `src/cast.h` as `static inline` functions,
shared with `src/cast.c` so the two can't drift. That matters most for complex,
where the rule that a missing value lands in the real part alone is easy to get
wrong twice.

The tables below cover four types. `chr`, `raw` and `list` are an error for
every operator, so the arithmetic and reduction families are the one place where
the per type cores do not cover all seven native types.

Binary elementwise:

| op | lgl | int | dbl | cpl |
|---|---|---|---|---|
| `+` `-` `*` | int | int | dbl | cpl |
| `/` `^` | dbl | dbl | dbl | cpl |

Reduction:

| op | lgl | int | dbl | cpl |
|---|---|---|---|---|
| `sum` | int | int | dbl | cpl |
| `prod` | dbl | dbl | dbl | cpl |
| `mean` | dbl | dbl | dbl | error |

`sum` on an integer array stays integer and errors on overflow. `prod` promotes
to double, matching base R's `prod()`, because integer products overflow almost
immediately. `mean` does not support complex.

### Operators that promote nothing

`pmax`, `pmin`, `max` and `min` are in the tables' families but not in the
tables, because picking the largest of some values cannot change their type. The
maximum of two logicals is a logical.

Their switches still list every pair, but the arms for `cpl` error, since
complex numbers have no ordering, and every other arm picks a core whose output
type equals its input type.

### Operators with a fixed output type

An operator whose output type is fixed regardless of its input does not use the
promotion tables. Its pair switch still names a core per pair, but every core
allocates the output at the one fixed type.

- Comparison (`rray_equal()` and friends) returns a logical array.

- `rray_all_along()` and `rray_any_along()` take logical and return a logical
  array.

- `rray_max_pos()` and `rray_min_pos()` return an integer array.

Stated once: **the promotion tables are only for operators whose output type
equals their input type.**

## 2.5 Iterators

An iterator walks a multidimensional **point** space one step at a time and
reports a 1D **location** in a possibly different space. That is what lets us
step over broadcast arrays without ever materialising them. A dimension of 1 in
a location space contributes no stride, which is what makes broadcasting free.

All three live in `src/iterator.h`, which has no `.c` file because every one of
them is `static inline`.

- `struct rray_point_iterator` reports only the point it is on, through
  `rray_point_iterator_point()`.

- `struct rray_iterator` adds one location space, read with
  `rray_iterator_location()`.

- `struct rray_iterator2` adds a second, read with `rray_iterator2_location1()`
  and `rray_iterator2_location2()`. It walks the point space once rather than
  twice.

There is one initialiser per struct, and it takes the point dimensions followed
by the dimensions of each location space. Which space is which is the caller's
choice, and the two directions both come up:

- Broadcasting walks the output and reads back into the input, so the broadcast
  dimensions are the point space. `rray_broadcast()`.

- Reducing walks the input and accumulates into the output, so the input
  dimensions are the point space and the reduced axes have a dimension of 1.
  `rray_sum_along()`.

- Two input functions walk the common dimensions and read back into each input.
  `rray_add()`.

Invent a new iterator only when a function genuinely cannot be expressed with
these. Say so explicitly in the pull request when you do.

---

# Part 3: Testing

vctrs is the standard to aim for: minimally exhaustive. Few tests, each pulling
its weight, covering the corners that actually break.

Every function pull request covers:

- **Every native type it supports.** If a function claims to work on all seven,
  test all seven. A function of two arrays covers the pairs instead, by
  snapshotting `native_ptype_matrix()`, which reports the output type of every
  pair and `NA` where the pair errors. That confirms in one place that a core
  exists everywhere one should.

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

- The arithmetic family splits one operator per file in `src/` and in the tests,
  so `rray_add()` is `src/arithmetic-add.c` and `test-arithmetic-add.R`.
  Snapshot names are file wide, so this lets every operator say "errors on
  integer overflow" without colliding. No `# ----` header, since each file
  covers one function.

- The R side does not split. Every operator's binding lives in `R/arithmetic.R`,
  and they share one documentation page, because the page has one thing to say
  and repeating it per operator would be seven copies of it.

- Never put code outside a `test_that()` block.

- No section header comments.

- Prefer a specific expectation over `expect_true()`.

- Use `expect_snapshot(error = TRUE)` for errors and `expect_snapshot()` for
  warnings, never `expect_error()` or `expect_warning()`.

- Place new tests next to similar existing ones.

---

# Part 4: Function reference

Each entry gives what the function does, what the original rray did, the
proposed signature, the files it touches, and two rules.

**Names rule**, from 2.3: follow the axis, reduce, coalesce, subset, or
dropped.

**Type rule**, from 2.4:

- *Preserved.* The output has `x`'s type. Any other array argument is cast to
  it, rather than being given a say in the result.

- *Common.* Every input has a say. They are cast to a common type with
  `rray_ptype2()`.

- *Promoted.* The common type, then pushed through the operator's promotion
  table.

- *Fixed.* Output type is fixed regardless of input.

## 4.1 Shape

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

Files: `R/flip.R`, `src/flip.c`, `src/flip.h`.

## 4.2 Other elementwise numeric

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

## 4.3 Reductions

All use the reduction iterator. All keep dimensionality, with reduced axes
collapsed to a dimension of 1. There is no `keep_dimensions` argument.

`axes` is **required** on every one of them. The original defaulted it to `NULL`
meaning "all axes". We do not.

Names: reduce.

| function | op | type rule |
|---|---|---|
| `rray_max_along(x, axes, ..., na_rm = FALSE)` | `max` | preserved, errors on cpl |
| `rray_min_along(x, axes, ..., na_rm = FALSE)` | `min` | preserved, errors on cpl |

`rray_max_pos(x, axis)` and `rray_min_pos(x, axis)` give the position of the
maximum or minimum along a single axis. Type: fixed, integer output.

```r
x <- array(c(1:10, 20:11), c(5, 2, 2))
rray_max_pos(x, 1)     # position of the max along the rows
rray_max_pos(x, 2)     # along the columns
```

`rray_max_along()` and `rray_min_along()` share `rray_sum_along()`'s
`(x, axes, ..., na_rm = FALSE)` shape, so they add `@rdname reduce` entries to
`R/reduce.R` and their own `src/reduce-{name}.c` beside it, on top of the
`rray_reduce()` shell in `src/reduce.c`/`src/reduce.h`.

`rray_max_pos()` and `rray_min_pos()` take `axis` rather than `axes` and return
positions rather than reduced values, so they do not match `rray_reduce()`'s
shape. They need their own file and topic, `R/max-pos.R`, with its own C pair.
Whether any part of `rray_reduce()` can be shared with them is a design question
for whoever picks them up.

---

# Part 5: Out of scope

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

- **`rray_modulo()` and `rray_integer_divide()`** (`%%` and `%/%`). See
  `plans/mod-and-idiv.md`.

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

# Part 6: Possible improvements for later

Not now. Revisit once the foundations are in place and there is a working
version to measure against.

## `x_arg` and `call` on the exported functions

vctrs gives its functions these so another package's wrapper can make an error
blame its own argument and its own call. The type functions already take both,
so `rray_ptype2(x, y, x_arg = "lhs", call = my_call)` works. Follow the pattern
in `src/ptype.c` when spreading them further, and 2.4 for the defaults.

The exported functions still do not take them. Adding an argument to every
exported function, and documenting it, is its own pull request. Do it when a
real caller wants it, not before.

## Unary elementwise math

There is no unary elementwise family today. `-x` works on a bare array already,
and `abs()`, `sqrt()` and friends are out of scope.

If one is ever wanted, it follows the shape of the binary family: a
`src/arithmetic-{op}.c` per operator with a switch over `enum rray_type`, which
is the single input version of what the binary operators do with
`enum rray_type2`.

## A null type

`NULL` is a scalar here, so it is an error everywhere. vctrs makes it a real
type that combines with anything, which is how `vec_ptype_common()` answers an
empty call instead of erroring.

See `plans/null.md`.

## An unspecified type

`NA` is logical, so it sits at the bottom of the numeric tower and needs no
special handling for arithmetic. But `rray_combine(chr_array, NA)` fails, because
`rray_ptype2(chr, lgl)` is an error.

vctrs solves this with an unspecified type, and it has been painful. See whether
`rray_combine()` can live without it first.

## Classed arrays

See `plans/extensions.md`.
