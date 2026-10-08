# Supporting classed arrays

Future work. rray works on bare arrays today, and classed input is an error.

This document records a design for letting other packages plug their own array
classes in, why it is not being built yet, and the cases that must shape it when
it is.

---

# Part 1: Why this is deferred

The design below is complete and survives the objections we could think of. It
is deferred anyway, for three reasons.

## The user base is thin

vctrs pays for its type system because there are hundreds of vector classes and
it is the shared substrate under dplyr and tidyr. Arrays are different.

- `units` arrays fit well. The `units` attribute does not depend on length.

- A generic "array wrapper" class fits well.

- `Matrix` sparse matrices do not fit. The proxy would have to densify, silently
  and expensively.

- `torch` and `terra` do not fit at all. They are external pointers.

- `bit` fits, but needs a careful method. See Part 4.

That is a short list, and most of it is hypothetical.

## It is most of the work

The design costs roughly nine of the fifteen foundation pull requests the plan
originally had: proxy, restore, retrofit, ptype, ptype2, cast, and the operator
type hooks. On top of that comes C dispatch machinery, two documented recipes for
method authors, and a proxy test in every function pull request afterward.

## It gives class authors less than it looks like

To keep it honest we stripped it down to no inheritance, no fallbacks, no
same-class fallback, and no unspecified type. Methods are hand written against
every native type the class wants to interoperate with.

At that point a class author gets broadcasting and our C loops. They can get the
same thing by unclassing, calling rray, and re-wrapping, which is the same work
the recipes were asking of them.

## Deferring is safe

rray errors on classed input. Going from "errors" to "works" never breaks
anyone, so this can be added later with no compatibility cost.

The two alternatives we rejected both would have created one:

- **Silently drop the class and return a bare array.** If
  `rray_broadcast(units_array, d)` returns a bare double today and a units array
  later, that is a behavior change for anyone who built around the bare result.

- **Naively keep the attributes.** Actively produces corrupt objects. See Part 4.

---

# Part 2: Proxy and restore

The boundary between an R object and the C implementation.

```
x    <- rray_proxy(x)
out  <- <C implementation>
out  <- rray_restore(out, x)
```

## The proxy contract

`rray_proxy(x)` returns an unclassed array of a native type that has the **same
dimensions and the same names** as `x`.

Because of that, `rray_dimensions()`, `rray_names()`, `rray_size()` and
`rray_dimensionality()` all become "take the proxy, then read an attribute".

The normalization rules already in the plan fold into the default method. A bare
vector proxies to a one dimensional array, with `names` moved to `dimnames`.

## The restore contract

`rray_restore(x, to)` takes the **class and type defining attributes from `to`**,
and the **dimensions and names from `x`**.

Restore must never assume anything about `to`'s dimensions. It is routinely
called with a `to` that has a dimension of 0 while `x` has real data.

**Restore is also the validation point.** It is a real method that does real
work, not a mechanical attribute copy. It may error, and it may recompute
attributes from the new dimensions. Part 4 depends on this entirely.

## Dispatch

Both dispatch on `class(x)[[1]]` only, with **no inheritance**, exactly like
vctrs. Methods are ordinary S3 methods registered with `S3method()`:

```r
rray_proxy.my_array <- function(x) { ... }
rray_restore.my_array <- function(x, to) { ... }
```

Dispatch happens in C, with a fast path that skips it entirely when `x` has no
class attribute.

## No built in methods

Not for factor, Date, POSIXct or difftime. Those are not arrays.

---

# Part 3: The generic type system

Once classes exist, a type is no longer just an `enum r_type`. It has to carry
the class too, which is what turns the internal type rules into a dispatch
system.

## Ptypes

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

## The invariant that makes this work

> A ptype must fully determine storage. `rray_proxy(rray_ptype(x))` must give a
> zero length native array of exactly the storage type that `x` uses.

For a class with fixed storage, like a `dollars_array` that is always integer
backed, this is free.

For a class that is generic over storage, it means there is **no single ptype**
for that class. There is an integer backed one and a double backed one, and they
are different types. That is the honest answer, and once it holds everything
downstream works.

Document it. Do not check it.

## Ptype2 and cast

```r
rray_ptype2(x, y)        # methods: rray_ptype2.<x_class>.<y_class>
rray_cast(x, to)         # methods: rray_cast.<to_class>.<x_class>
rray_ptype_common(..., .ptype = NULL)
rray_cast_common(..., .ptype = NULL)
```

`rray_cast(x, to)` means "cast `x` to the type of `to`". It changes type only,
never dimensions or names.

`.ptype` runs through `rray_ptype()` first, so `.ptype = double()` works and
means "a double array".

There is **no fallback method of any kind**, not even for identical classes. If
`rray_ptype2.foo.foo` is missing, that is an error, and the message says which
method is missing.

This is stricter than vctrs on purpose. A class that is generic over storage
must not have its integer backed and double backed variants silently collapsed
by a same-class fallback.

The native rules stay as they are in the main plan, and become the fast path
taken when neither side has a class.

## Operator type hooks

The type system alone cannot answer "what type does this operator work in",
because `lgl + lgl` is `int` and `int / int` is `dbl`. The internal promotion
tables in the main plan become generics.

| family | hook | dispatch | ops |
|---|---|---|---|
| binary elementwise | `rray_binary_ptype2(op, x, y)` | double | `+ - * / ^ %% %/%`, `pmax`, `pmin` |
| reduction | `rray_reduction_ptype(op, x)` | single | `sum`, `prod`, `mean`, `max`, `min` |

One generic per family, matching the internal `rray_binary_type()` and
`rray_reduction_type()` they replace. The split that matters is elementwise
versus reduction, not arity: reductions accumulate, which raises overflow and
precision questions that elementwise operators never face, and a separate hook
leaves room to pass more than an operator through later.

If a unary elementwise family is ever added, it takes an
`rray_unary_ptype(op, x)` beside these two.

Each returns **one ptype**, used both to cast the inputs and to restore the
output. That works because we always promote before computing, so the input type
and the output type are the same.

`op` is a single string from a closed vocabulary that rray ships. Users cannot
invent new operators.

### The pipeline

```
p    <- rray_binary_ptype2(op, x, y)
x    <- rray_cast(x, p)
y    <- rray_cast(y, p)
px   <- rray_proxy(x)
py   <- rray_proxy(y)
dims <- rray_dimensions_common(px, py)
out  <- <C loop over two broadcast iterators>
out  <- rray_restore(out, p)
```

Unary and reduction operators use the same pipeline with one input.

### Operators that are not covered

An operator whose output type is fixed regardless of its input does not use
these hooks. It casts its inputs with plain `rray_ptype2()`, computes, and
returns a **bare** array with no class restored.

- Comparison returns a bare logical array.

- `rray_all()` and `rray_any()` return a bare logical array.

- `rray_locate_max()` and `rray_locate_min()` return a bare integer array.

A comparison of two `foo_array`s is a logical array, not a `foo_array`. Stated
once: **these hooks are only for operators whose output type equals their input
type.**

## Writing methods

Two recipes, both worth putting in the documentation.

**Fixed storage.** Enumerate the operators. Nothing is inferred, so nothing can
corrupt the class:

```r
rray_binary_ptype2.dollars_array.dollars_array <- function(op, x, y) {
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

**Generic over storage.** Defer to the native rule by proxying and recursing,
then put the class back:

```r
rray_binary_ptype2.wrapper_array.wrapper_array <- function(op, x, y) {
  ptype <- rray_binary_ptype2(op, rray_proxy(x), rray_proxy(y))
  rray_restore(ptype, x)
}
```

The proxies are bare native arrays, so the recursion lands on the native method
and terminates.

The same two shapes work for `rray_ptype2()` and `rray_reduction_ptype()`. It is
one pattern to learn, not three.

Because there is no fallback, a class that has not thought about arithmetic
simply errors. That is what protects `dollars_array` from silent promotion.

The cost, same as vctrs: interoperating with bare arrays needs methods against
them too, so `rray_add(dollars, 1L)` needs
`rray_binary_ptype2.dollars_array.integer`.

---

# Part 4: The cases that must shape the design

These are the reason the design should be validated against a real class rather
than a hypothetical one. Both are solvable, and both need a restore method that
thinks.

## A class with a dimensionality invariant

Consider `my_special_matrix`, which is only ever two dimensional.

`rray_broadcast(x, c(2, 3, 4))` would hand restore a three dimensional result.
Nothing in the proxy contract stops it.

The answer is that `rray_restore.my_special_matrix()` checks the dimensionality
and errors. Restore has a veto, and this is what it is for.

Note it can only veto on the shape of the result, not on the operation that
produced it. `rray_broadcast(x, c(3, 4))` stays two dimensional and restore
cannot tell it apart from any other reshape. That is correct behavior, but it
means restore is a shape check, not an operation check.

## A class with length dependent attributes

The `bit` package stores attributes derived from the length of the object.

A naive restore that copies attributes from `to` produces a corrupt object the
moment the result has a different size from the input, which is almost every
interesting operation.

The answer is that `rray_restore.bit()` recomputes those attributes from the new
dimensions rather than copying them.

## What both cases tell us

The contract phrasing "takes the class and type defining attributes from `to`"
undersells restore badly. It is not a mechanical attribute copy. Write the
contract as:

> `rray_restore(x, to)` produces a valid object of `to`'s class with `x`'s
> dimensions, names and data. It may error if that is impossible, and it may
> recompute any attribute that depends on size or shape.

That framing is what makes both cases fall out naturally instead of looking like
holes.

## Classes that will never fit

Worth saying out loud so nobody tries.

- **Sparse matrices.** The proxy contract forces a dense materialisation, which
  is silent and expensive. A sparse array system needs different primitives, not
  a proxy.

- **External pointer tensors**, like torch. There is no native R array to proxy
  to.

---

# Part 5: When to build this

Build it when a real class wants in, and use that class to validate the design.

Rough order, matching the pull request granularity of the main plan:

1. `rray_proxy()` and `rray_restore()`, with the C dispatch machinery and the
   native fast path.

2. Retrofit both into every existing function. All the structural functions
   restore to `x`'s own class.

3. `rray_ptype()`.

4. `rray_ptype2()` and `rray_ptype_common()`.

5. `rray_cast()` and `rray_cast_common()`, wrapping the internal cast.

6. `rray_binary_ptype2()` and `rray_reduction_ptype()`, each wrapping the
   internal promotion table it replaces.

The internal type rules in the main plan are already written against native
types, so each of these is a generic placed in front of a C function that
already exists. None of it is a rewrite.

## What happens to the boundary helpers

`arg_as_array()` should **not** become proxy aware. `rray_proxy()`'s default
method already turns a bare vector into a one dimensional array, which is the
only thing `arg_as_array()` does, so it becomes redundant and is deleted rather
than rewritten.

`check_unclassed()` goes too. Refusing classed input is exactly the behavior this
work replaces.

So step 2 is mostly a deletion. Each function swaps two boundary calls for one
`rray_proxy()`, and gains an `rray_restore()` on the way out.

Before starting, pick a real class and write its methods first. If the recipes
in Part 3 do not fall out cleanly for it, the design is wrong and this is the
moment to find out.
