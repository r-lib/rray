# Iterator follow ups

Two improvements to the array iterators. They are independent, and the first one
makes the second one smaller, so do them in order.

---

# Where things stand

`src/iterator.h` holds one struct and one step function, shared by everything
that walks an array.

```c
struct rray_iterator {
  int dimensionality;
  int v_point_dimensions[RRAY_MAX_DIMENSIONALITY];
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
  r_ssize v_location_strides[RRAY_MAX_DIMENSIONALITY];
  r_ssize location;
};
```

It walks one space, the point space, and reports a single 1-D offset into a
second space, the location space. The two spaces have the same axes. An axis of
the location space is either full, and carries a real stride, or collapsed to a
single index, and carries a stride of zero.

A stride of zero is the whole mechanism. Adding zero on a step is a no-op, and
the reset term is multiplied by that same zero, so `rray_iterator_next()` needs
no per axis test.

There are two ways to set one up.

- `rray_reduction_iterator_init()` in `src/reduction-iterator.h`. Walks the input,
  reports into the reduced output. Both spaces have the same dimensionality.

- `rray_broadcast_iterator_init()` in `src/broadcast-iterator.h`. Walks the
  broadcast view, reports back into the original input. The input may have fewer
  axes, and the missing trailing axes are treated as 1.

---

# Improvement 1: one stride builder for reduction and broadcast (done)

## Why

The two init functions now do the same thing. Both fill `v_point_dimensions`
from the space being walked, then build strides from the space being indexed,
zeroing any axis of size 1. The only real difference is that broadcast allows
the indexed space to have fewer axes and pads the tail with 1.

That means the same stride loop is written out twice, in two files. It was
edited in both places in the last change, which is the signal that it wants to
live in one place.

This is not about making reduction and broadcast into separate things. It is the
opposite. They are the same walk with different stride recipes, and the recipe is
straight line code with no tricky parts. The delicate code is the carry loop in
`rray_iterator_next()`, and that is already shared.

## What to do

Add a general init to `src/iterator.h`. Padding the tail with 1 covers the
reduction case for free, since there the two dimensionalities are equal and the
padding branch never fires.

```c
static inline void rray_iterator_init(
  struct rray_iterator* it,
  const int* v_point_dimensions,
  int point_dimensionality,
  const int* v_location_dimensions,
  int location_dimensionality
) {
  check_max_dimensionality(point_dimensionality);

  it->dimensionality = point_dimensionality;

  for (int i = 0; i < point_dimensionality; ++i) {
    it->v_point_dimensions[i] = v_point_dimensions[i];
  }

  r_ssize stride = 1;
  for (int i = 0; i < point_dimensionality; ++i) {
    const int dimension =
      (i < location_dimensionality) ? v_location_dimensions[i] : 1;
    it->v_location_strides[i] = (dimension == 1) ? 0 : stride;
    stride *= dimension;
  }

  memset(it->v_point, 0, sizeof(r_ssize) * point_dimensionality);
  it->location = 0;
}
```

Then both existing init functions become one line each, keeping their current
signatures so no call site changes.

```c
static inline void rray_reduction_iterator_init(
  struct rray_iterator* it,
  const int* v_dimensions,
  const int* v_out_dimensions,
  int dimensionality
) {
  rray_iterator_init(
    it,
    v_dimensions,
    dimensionality,
    v_out_dimensions,
    dimensionality
  );
}
```

```c
static inline void rray_broadcast_iterator_init(
  struct rray_iterator* it,
  const int* v_dimensions,
  int dimensionality,
  const int* v_view_dimensions,
  int view_dimensionality
) {
  rray_iterator_init(
    it,
    v_view_dimensions,
    view_dimensionality,
    v_dimensions,
    dimensionality
  );
}
```

## Watch out for

The broadcast wrapper flips its arguments. Its signature lists the input space
first and the walked view second, while the general init takes the walked space
first. Keep the wrapper signature as it is, since call sites depend on it, and
just pass the arguments across in the right order.

## Keep the named wrappers

Do not delete `rray_reduction_iterator_init()` and
`rray_broadcast_iterator_init()` in favour of calling the general init directly.
They cost one line each and they say what a call site is doing. Reading
`rray_broadcast_iterator_init` at a call site tells you more than five
positional arguments do.

---

# Improvement 2: `rray_iterator2`, two locations from one walk

## Why

Some operations need two offsets per step, not one. Today the only way to get
that is to run two `rray_iterator`s side by side over the same space. They keep
identical points and step in lockstep, so every element pays for the carry
arithmetic twice to read off two numbers.

`rray_split()` in `src/split.c` does exactly this now. It runs `out_it` and
`out_elt_it` together over the input, one giving the subarray to write to and
the other the slot inside it.

Binary broadcast operations will want the same thing, and there are a lot of
them still to write. The original rray exports `rray_add()`, `rray_subtract()`,
`rray_multiply()`, `rray_divide()`, `rray_pow()`, `rray_equal()`,
`rray_greater()`, and the `%b+%` family. Each one walks the common broadcast
shape while reading from two inputs of different shapes.

Note that two is a natural number here, not an arbitrary one. In both cases one
side is sequential and two sides are strided.

- Binary operation: the loop counter is the output index, so writes are
  sequential. The two tracked locations are the positions in `x` and in `y`.

- Split: the loop counter is the input index, so reads are sequential. The two
  tracked locations are which subarray, and which slot inside it.

## Build it generic, not split specific

An earlier sketch had a split specific iterator. A general two location struct
is better, because split's two spaces are complementary. Every axis carries a
real stride in exactly one of its two arrays. Binary broadcast is not like that.
Both inputs can carry full extent on the same axis, so both stride arrays are
non zero there.

A struct built around split's complementarity would not cover binary operations.
A general one covers both, and split simply passes complementary strides.

## What to do

Add `src/iterator2.h` beside `src/iterator.h`.

```c
struct rray_iterator2 {
  int dimensionality;
  int v_point_dimensions[RRAY_MAX_DIMENSIONALITY];
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
  r_ssize v_location1_strides[RRAY_MAX_DIMENSIONALITY];
  r_ssize v_location2_strides[RRAY_MAX_DIMENSIONALITY];
  r_ssize location1;
  r_ssize location2;
};
```

The step function is the same carry loop with one extra line in each branch.

```c
static inline void rray_iterator2_next(struct rray_iterator2* it) {
  for (int i = 0; i < it->dimensionality; ++i) {
    ++it->v_point[i];

    if (it->v_point[i] < it->v_point_dimensions[i]) {
      it->location1 += it->v_location1_strides[i];
      it->location2 += it->v_location2_strides[i];
      return;
    }

    it->v_point[i] = 0;

    it->location1 -=
      (it->v_point_dimensions[i] - 1) * it->v_location1_strides[i];
    it->location2 -=
      (it->v_point_dimensions[i] - 1) * it->v_location2_strides[i];
  }
}
```

Give it a general `rray_iterator2_init()` taking the walked space and both
location spaces, built the same way as Improvement 1. Then add named wrappers
for each use, following the existing file layout.

- `src/split-iterator.h` for `rray_split_iterator_init()`.

- A `rray_broadcast_iterator2_init()` in `src/broadcast-iterator.h` when binary
  operations land.

## Then convert `rray_split()`

`src/split.c` currently builds two dimension vectors and two iterators. After
this it needs one iterator, and the inner loop of both `RRAY_SPLIT_ATOMIC` and
`RRAY_SPLIT_BARRIER` reads two locations off one step instead of stepping twice.

There is a separate, agreed cleanup of the names inside `rray_split()`, since
`out_dimensions` and `out_elt_dimensions` are easy to mix up. That cleanup and
this conversion touch the same lines. Doing the naming first will make this
conversion much easier to read, so prefer that order.

## Do not fold `rray_iterator` into this

It is tempting to keep one struct and zero out the second stride array for the
single location case. Do not. It costs two dead `r_ssize` operations per element
in the innermost loop of every reduction and broadcast, and it makes the single
location case claim to track something it does not.

Two small step functions is the right trade here. The duplication buys a real
saving, which is not true of splitting reduction and broadcast apart.

## Where this stops

Three or more inputs. `rray_broadcast_common()` and
`rray_broadcast_names_common()` already exist, so n-ary broadcasting is on the
map, and `rray_iterator2` does not stretch to it. With
`RRAY_MAX_DIMENSIONALITY` at 64 each stride array is 512 bytes, and the two
location struct is already around 1.8KB on the stack.

One and two cover unary operations, binary operations, and split, which is
nearly the whole surface. Let the n-ary case allocate its stride arrays when it
actually lands, rather than paying for that generality now.

---

# Open question

The name `rray_iterator2`. In this codebase a trailing `2` currently means two
inputs, as in `rray_broadcast_names2(x, y, dimensions)`. Here it would mean two
location spaces. Those coincide for binary operations, but not for split, which
is one input with two output spaces. Worth settling before it is spelled that
way across several files.

---

# Verifying

Neither change alters behaviour, so the existing suite is the check.

```
Rscript -e "devtools::test()"
```

All tests should pass with no new ones needed. Run `clang-format -i src/*.c
src/*.h` over every file afterwards, not just the changed ones.

The performance argument for Improvement 2 is reasoning about work per element,
not a measurement. Nothing here has been benchmarked. If a number matters before
merging, benchmark `rray_split()` on a large array split along the first axis,
which is the case that steps the iterators hardest.
