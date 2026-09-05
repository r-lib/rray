# Adding a null type

Future work. This document describes what it would take to make `NULL` a real
type in rray4's type system, the way it is in vctrs.

Nothing here is built yet. Read the whole document before starting, then work
through Part 6 in order.

---

# Part 1: Where things stand

rray4 has eight types today, listed in `enum rray_type` in `src/type.h`:

```
logical, integer, double, complex, character, raw, list, scalar
```

The first seven are the vector types we support. The eighth, `scalar`, is the
catch all for everything we do not support. `rray_typeof()` in `src/type.c`
returns `RRAY_TYPE_scalar` for anything that is not one of the seven.

`NULL` falls into that catch all. So today:

```r
rray_ptype(NULL)              # Error: `NULL` must be an array, not `NULL`.
rray_ptype2(NULL, integer())  # Error
rray_cast(NULL, integer())    # Error
rray_ptype_common(1L, NULL)   # Error: `..2` must be an array, not `NULL`.
```

And because the running type has to start somewhere, `rray_ptype_common()` has
to reject an empty call:

```r
rray_ptype_common()  # Error: Must supply at least one array to `...`.
```

That last error is the reason this document exists.

## The problem it causes

`src/ptype-common.c` starts its answer from the first input:

```c
r_obj* out = rray_ptype(v_xs[0], x_arg, error_call);

for (x_i = 1; x_i < n; ++x_i) {
  out = rray_ptype2(out, v_xs[x_i], &side, out_arg, x_arg, error_call);
}
```

With no inputs there is no `v_xs[0]`, so there is nothing to start from, so we
error. That error then leaks into `rray_cast_common()`, which asks
`rray_ptype_common()` for its target type first:

```r
rray_cast_common(.to = double())  # list()
rray_cast_common()                # Error
```

Two calls that both cast nothing, one of which works and one of which does not.

---

# Part 2: What vctrs does

vctrs does not have this problem because `NULL` is the first entry of its
`enum vctrs_type`, and it behaves as a starting value that any other type wins
against:

```r
vec_ptype2(NULL, 1L)    # integer()
vec_ptype2(1L, NULL)    # integer()
vec_ptype2(NULL, NULL)  # NULL
```

Because combining `NULL` with anything gives back that thing, `vec_ptype_common()`
can start its answer at `NULL` instead of at the first input. Look at
`reduce(r_null, ...)` in `vctrs/src/ptype-common.c`. With no inputs there is
nothing to combine, so the starting value falls straight out:

```r
vec_ptype_common()      # NULL
vec_cast_common()       # list()
```

No special case for the empty call anywhere in the code.

The rest of the vctrs behaviour follows from that:

```r
vec_ptype(NULL)             # NULL
vec_cast(NULL, integer())   # NULL
vec_cast(1L, NULL)          # 1L
vec_cast_common(NULL, 1L)   # list(NULL, 1)
vec_size(NULL)              # 0
```

Note the last two. Casting `NULL` to a real type gives back `NULL`, it does not
give back a zero length vector. So `vec_cast_common()` leaves the `NULL` alone.

vctrs handles `NULL` by checking for it at the top of `vec_ptype2()` and
`vec_cast()`, before it reaches the big switch. We should not copy that. rray4
prefers switches that list every case, so `NULL` goes in the switch with
everything else. See `feedback_exhaustive_type_switch` in the project memory.

---

# Part 3: What rray4 would look like afterwards

```r
rray_ptype(NULL)               # NULL
rray_ptype2(NULL, 1L)          # integer()
rray_ptype2(1L, NULL)          # integer()
rray_ptype2(NULL, NULL)        # NULL
rray_ptype_common()            # NULL
rray_ptype_common(NULL, NULL)  # NULL
rray_ptype_common(1L, NULL)    # integer()
rray_cast(NULL, integer())     # NULL
rray_cast(1L, NULL)            # array(1L, 1L)
rray_cast_common()             # list()
rray_cast_common(NULL, 1L)     # list(NULL, array(1, 1L))
```

Everything else stays as it is. In particular `rray_size(NULL)`,
`rray_dimensions(NULL)`, `rray_broadcast(NULL, ...)` and friends keep erroring.
See Part 5, which is the part most likely to bite.

---

# Part 4: Decisions to make before writing code

Four of these have a clear answer and one does not. Settle all five with Davis
before starting.

## 4a. Where `RRAY_TYPE_null` goes in the enum

It must go first:

```c
enum rray_type {
  RRAY_TYPE_null,
  RRAY_TYPE_logical,
  RRAY_TYPE_integer,
  RRAY_TYPE_double,
  RRAY_TYPE_complex,
  RRAY_TYPE_character,
  RRAY_TYPE_raw,
  RRAY_TYPE_list,
  RRAY_TYPE_scalar
};
```

The reason is `enum rray_side`, added in the type support pull request.
`rray_typeof2()` reports which of its two arguments the common type came from,
and for every pair in the enum today the winner is whichever type is later in
the list. Putting null first keeps that true, so the side values for the new
pairs follow the same pattern as all the others: `null` on the left means the
right side wins, `null` on the right means the left side wins, and two nulls
mean both.

Nothing in the code depends on the numeric value of an `enum rray_type`, so
shifting the existing seven up by one is safe.

## 4b. What `rray_cast(x, NULL)` returns

vctrs returns `x` untouched. We should return `vec_as_array(x)`, so
`rray_cast(1L, NULL)` gives `array(1L, 1L)`.

Reason: every other path through `rray_cast()` returns an array, and callers
lean on that. Returning a plain vector from exactly one branch is a trap.

This case is only reachable from a direct `rray_cast()` call. It cannot come out
of `rray_cast_common()`, because if any input is not `NULL` then the common type
is not `NULL` either.

## 4c. Whether `rray_ptype_common()` should still reject an empty call

Adding a null type does not force you to allow zero inputs. You could add the
type and keep the error.

But removing that error is the whole point of the exercise, so remove it. If
Davis wants to keep it, this document is not worth acting on.

## 4d. What `rray_type_as_c_string()` returns for null

`"NULL"`, matching what R itself calls it. It is only used to build error
messages like ``Can't combine `x` <integer> and `y` <character>.``, and a
message about `<NULL>` never actually fires, because combining with `NULL`
always succeeds. Fill it in anyway so the switch stays complete.

## 4e. Whether the test helper grows

`tests/testthat/helper-ptype.R` has `native_ptypes`, a list of the seven vector
types, and `native_ptype_matrix()`, which builds a table of every pair. The
`rray_ptype2()` table is checked by a snapshot in `tests/testthat/_snaps/ptype.md`.

Recommendation: add `null = NULL` as the first entry of `native_ptypes`. It is a
native type after the change, so it belongs. The pair table grows from seven by
seven to eight by eight and the snapshot needs accepting.

Check the loops that already use `native_ptypes` still hold with `NULL` in the
list. They should. `rray_ptype(NULL)` returns `NULL`, and
`rray_ptype_common(NULL, NULL)` returns `NULL`, so the existing
`expect_identical(..., ptype)` checks pass unchanged.

The one thing to watch is that `list(null = NULL, lgl = logical(), ...)` really
does have eight elements. It does. `list()` keeps a `NULL` element, unlike
assigning `NULL` into an existing list.

---

# Part 5: The hazard

Read this part twice.

`arg_as_array()` in `src/utils.c` is how most of the package rejects bad input:

```c
r_obj* arg_as_array(r_obj* x, struct rray_arg* arg, struct r_lazy error_call) {
  if (rray_typeof(x) == RRAY_TYPE_scalar) {
    stop_scalar_input(x, arg, error_call);
  }

  return vec_as_array(x);
}
```

`NULL` is caught by that check today only because `rray_typeof(NULL)` returns
`RRAY_TYPE_scalar`. The moment it returns `RRAY_TYPE_null` instead, `NULL` walks
straight past the check and into `vec_as_array()`, which calls `r_wrap()`, which
has no wrapper class for `NULL` and aborts with `Can't wrap a NULL.`

That is a bad error message reaching users from ten call sites:

- `src/broadcast.c`

- `src/dimensions.c` (two)

- `src/names.c` (four)

- `src/size.c`

- `src/split.c`

- `src/sum.c`

So `arg_as_array()` must reject null explicitly:

```c
const enum rray_type type = rray_typeof(x);

if (type == RRAY_TYPE_null || type == RRAY_TYPE_scalar) {
  stop_scalar_input(x, arg, error_call);
}

return vec_as_array(x);
```

`stop_scalar_input()` builds its message from `r_obj_type_friendly(x)`, which
gives ``NULL`` for `NULL`. So the message stays exactly
``must be an array, not `NULL`.`` and none of the snapshots for those ten
functions move. Confirm that by running the full test suite, not by reading.

One naming point. `stop_scalar_input()` is now also used for null, which is not a
scalar. Either rename it to something like `stop_not_array()` across the package,
or leave it and accept the mismatch. Ask Davis. Renaming touches `src/utils.c`,
`src/utils.h`, `src/ptype.c` and `src/cast.c`, and changes no messages.

---

# Part 6: The changes, file by file

## 6a. `src/type.h`

Add `RRAY_TYPE_null` as the first entry of `enum rray_type`. See 4a.

## 6b. `src/type.c`

In `rray_typeof()`, add a case before the catch all:

```c
case R_TYPE_null:
  return RRAY_TYPE_null;
```

In `rray_type_as_c_string()`, add:

```c
case RRAY_TYPE_null:
  return "NULL";
```

## 6c. `src/typeof2.h`

`enum rray_type2` lists every pair once, in a fixed order. It has 36 entries
today. Add a null block at the top, giving 45:

```c
RRAY_TYPE2_null_null,
RRAY_TYPE2_null_logical,
RRAY_TYPE2_null_integer,
RRAY_TYPE2_null_double,
RRAY_TYPE2_null_complex,
RRAY_TYPE2_null_character,
RRAY_TYPE2_null_raw,
RRAY_TYPE2_null_list,
RRAY_TYPE2_null_scalar,
```

## 6d. `src/typeof2.c`

`rray_typeof2()` is a switch on `x` wrapping a switch on `y`, so it has one arm
per ordered pair. It has 64 arms today and needs 81.

Add a new outer `case RRAY_TYPE_null` with nine inner arms, and add one
`case RRAY_TYPE_null` inner arm to each of the eight existing outer cases. That
is 17 new arms.

The side values follow the same rule as everything else in the file. For the new
outer null block:

```c
case RRAY_TYPE_null:
  switch (y) {
  case RRAY_TYPE_null:
    *side = RRAY_SIDE_both;
    return RRAY_TYPE2_null_null;
  case RRAY_TYPE_logical:
    *side = RRAY_SIDE_right;
    return RRAY_TYPE2_null_logical;
  ...
  }
```

and in each existing outer block, null on the right means the left side wins:

```c
case RRAY_TYPE_null:
  *side = RRAY_SIDE_left;
  return RRAY_TYPE2_null_logical;
```

Do not shortcut this with a comparison on the enum values. Write every arm out.

## 6e. `src/ptype.c`

In `rray_ptype()`, add:

```c
case RRAY_TYPE_null:
  return r_null;
```

In `rray_ptype2()`, add nine cases to the switch:

```c
case RRAY_TYPE2_null_null:
  return r_null;
```

`RRAY_TYPE2_null_logical` joins the group returning `r_globals.empty_lgl`,
`RRAY_TYPE2_null_integer` joins the integer group, and so on for double,
complex, character, raw and list.

`RRAY_TYPE2_null_scalar` joins the group of scalar cases at the bottom of the
switch. Nothing in that branch changes. It blames `x` when `x` is the scalar and
`y` otherwise, which is right whichever way round the null and the scalar are.

## 6f. `src/ptype-common.c`

This is the payoff. Delete the empty check:

```c
if (n == 0) {
  r_abort_lazy_call(error_call, "Must supply at least one array to `...`.");
}
```

Start the answer at `r_null` and run the loop from zero instead of one:

```c
r_keep_loc out_pi;
r_obj* out = r_null;
KEEP_HERE(out, &out_pi);

for (x_i = 0; x_i < n; ++x_i) {
  enum rray_side side;
  out = rray_ptype2(out, v_xs[x_i], &side, out_arg, x_arg, error_call);
  KEEP_AT(out, out_pi);

  if (side == RRAY_SIDE_right) {
    out_i = x_i;
  }
}
```

`rray_ptype()` is no longer called from this function except on the `.ptype`
path. That is fine, it is still called from `ffi_rray_ptype()`.

Two things to check while you are here, both of which should be fine:

- The argument names in error messages must not move. With `out_i` starting at
  zero and the first pass reporting `RRAY_SIDE_right`, the first input is still
  blamed as `..1`. Every snapshot in `tests/testthat/_snaps/ptype-common.md`
  other than the deleted empty case should be unchanged.

- `new_subscript_arg()` is now built even when `n` is zero. It is only read when
  an error is raised, and with zero inputs the loop never runs, so nothing reads
  it. Confirm it does not object to `n` of zero.

There is one thing this does not give you. The `.ptype` argument uses `NULL` to
mean "not supplied", so there is no way to ask `rray_ptype_common()` for the null
type on purpose. vctrs has the same limitation with `.ptype`.

## 6g. `src/cast.c`

`rray_cast_switch()` is a switch on `x_type` wrapping a switch on `to_type`.

Add a new outer case for null. Every real target gives back `NULL`, and a scalar
target still errors:

```c
case RRAY_TYPE_null:
  switch (to_type) {
  case RRAY_TYPE_scalar:
    stop_scalar_input(to, to_arg, error_call);
  default:
    return r_null;
  }
```

Then add `case RRAY_TYPE_null: return vec_as_array(x);` to each of the seven
existing inner switches, per decision 4b. It has to be written out, because the
`default:` in those switches calls `stop_incompatible_cast()`.

Nothing else in `src/cast.c` changes. `NULL` never reaches the `RRAY_CAST` macro
or any of the per type cast functions, so `vec_as_array()` is never asked to wrap
it.

## 6h. `src/cast-common.c`

No change. Read it and convince yourself, because it looks like it should need
one.

`rray_cast_common()` asks `rray_ptype_common()` for `to`, then casts each input
to it. `to` can now come back as `r_null`, but that only happens when `...` is
empty or every element is `NULL`. If `...` is empty the loop does not run and you
get `list()`. If every element is `NULL` then every `rray_cast(NULL, NULL)`
returns `NULL` and you get a list of nulls. Both are right.

## 6i. `src/utils.c`

Reject null in `arg_as_array()`. See Part 5. This one is not optional.

## 6j. Headers and decl files

No signatures change, so `src/ptype.h`, `src/cast.h`, `src/decl/ptype-decl.h`
and `src/decl/cast-decl.h` are untouched unless you take the
`stop_scalar_input()` rename from Part 5.

## 6k. R files

No change. `rray_ptype()`, `rray_ptype2()`, `rray_ptype_common()`, `rray_cast()`
and `rray_cast_common()` are all internal, none are exported, and none have
roxygen documentation. There is no `NAMESPACE` or `_pkgdown.yml` work.

---

# Part 7: Tests

Every one of these is a test that exists today and asserts the old behaviour.
They will fail, and each failure is expected.

## `tests/testthat/test-ptype.R`

`errors on a scalar` uses `x <- NULL` for its second case. `rray_ptype(NULL)`
now returns `NULL`. Drop that half and keep the `sum` case, then add a test that
`rray_ptype(NULL)` returns `NULL`.

`errors on non-array input` starts with `rray_ptype2(NULL, integer())`, which now
returns `integer()`. Move it to a new test covering the null pairs, and leave the
`sum` case where it is.

`the common type of every pair of native types` is the snapshot of the pair
table. It grows to eight by eight if you take decision 4e.

## `tests/testthat/test-ptype-common.R`

`errors on no inputs` becomes a test that `rray_ptype_common()` returns `NULL`.
Delete its snapshot from `tests/testthat/_snaps/ptype-common.md`.

`errors on non-array input` uses `rray_ptype_common(1L, NULL)`, which now returns
`integer()`. Replace the input with a real scalar such as `sum`.

`one and two inputs of the same type agree` already loops over `native_ptypes`
and picks up null for free if you take decision 4e.

## `tests/testthat/test-cast.R`

`errors on non-array input` covers both `rray_cast(NULL, integer())` and
`rray_cast(1L, NULL)`. Both now succeed. Move them into a new test for the null
behaviour and replace them here with scalars.

`` `x` is checked before `to` `` sets `x <- NULL` and `to <- NULL` and checks that
`x` is blamed. Both are now valid, so the test no longer tests anything. Use two
scalars instead, for example `x <- sum` and `to <- sum`.

## `tests/testthat/test-cast-common.R`

`errors on no inputs when .to is not supplied` becomes a test that
`rray_cast_common()` returns `list()`. Delete its snapshot.

Add a test that `rray_cast_common(NULL, 1L)` returns
`list(NULL, array(1, 1L))`, which is the case that shows nulls survive a cast
rather than turning into empty vectors.

## New tests to add

- `rray_ptype(NULL)` returns `NULL`.

- `rray_ptype2()` with null on either side gives the other type, for all seven
  vector types. A loop over `native_ptypes` is enough.

- `rray_ptype2(NULL, NULL)` returns `NULL`.

- `rray_ptype2(NULL, sum)` and `rray_ptype2(sum, NULL)` both blame the scalar.

- `rray_cast(NULL, to)` returns `NULL` for all seven vector types, and for `NULL`.

- `rray_cast(NULL, sum)` errors.

- `rray_cast(x, NULL)` returns an array, for all seven vector types.

- `rray_ptype_common()` returns `NULL`, and `rray_cast_common()` returns `list()`.

- `NULL` is still rejected by `rray_size()`, with the same message as today. Pick
  one function from the Part 5 list as the guard against that check being lost.

---

# Part 8: Suggested pull requests

Four, stacked, in this order. Each one leaves the package passing its tests.

1. Add `RRAY_TYPE_null` to `enum rray_type`, teach `rray_typeof()` and
   `rray_type_as_c_string()` about it, and reject it in `arg_as_array()`. Nothing
   observable changes: `NULL` still errors everywhere, just by a different route.
   All existing tests pass untouched. This is the safe half of Part 5.

2. Add the null pairs to `enum rray_type2` and `rray_typeof2()`, with their side
   values. Still nothing observable changes, because `rray_ptype2()` does not
   look at the new pairs yet and its switch is still complete. The compiler will
   tell you if it is not.

3. Teach `rray_ptype()` and `rray_ptype2()` about null, then start
   `rray_ptype_common()` from `r_null` and delete its empty input error. This is
   where behaviour changes and where most of the test churn lands.

4. Teach `rray_cast()` about null. `rray_cast_common()` follows on its own.

Splitting 1 and 2 out matters. Together they are the change that could quietly
break ten unrelated functions, and on their own they are easy to review
against a green test suite.

---

# Part 9: Out of scope

Deliberately not part of this work:

- `rray_broadcast()` and `rray_broadcast_common()`.

- `rray_size()`, `rray_dimensions()`, `rray_dimensionality()` and the
  `rray_names()` family.

- `rray_split()` and `rray_sum()`.

All of those keep erroring on `NULL`, and the guard in `arg_as_array()` is what
keeps them that way. If we ever want `rray_size(NULL)` to be `0` like
`vec_size(NULL)`, that is a separate conversation about what dimensions `NULL`
has, and it is a harder question than this document answers.

---

# Part 10: Reading list

- `src/ptype-common.c`, the whole file. It is short and it is the reason for all
  of this.

- `vctrs/src/ptype-common.c`, the `ptype2_common()` function and the
  `reduce(r_null, ...)` call above it.

- `vctrs/src/ptype2.c`, the top of `vec_ptype2_impl()`, for how vctrs handles
  null and why we are not copying it.

- `vctrs/src/typeof2.c`, the top of the file, for the `left` values on the null
  rows. Ours are the same, under different names.
