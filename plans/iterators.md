# Iterator follow ups

Two improvements to the array iterators. Both are done. This records what they
were for and how they landed.

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

`rray_iterator_init()` sets one up. It takes the space to walk and the space to
index, each with its own dimensionality, and covers both existing uses.

- Reduction walks the input and reports into the reduced output. Both spaces
  have the same dimensionality.

- Broadcast walks the view and reports back into the original input. The input
  may have fewer axes, and the missing trailing axes are treated as 1.

`src/iterator2.h` holds `struct rray_iterator2`, which walks one space and
reports two locations. Both inits share `rray_location_strides_init()`, which
validates one location space against the walked space and fills its strides.

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

Broadcast reads backwards from how you might expect. The space it walks is the
broadcast view, and the space it indexes is the original input, so in
`src/broadcast.c` the view goes first and `x` second. Getting these the wrong
way round still compiles and still runs, it just reads the wrong array.

## The named wrappers were removed

This section originally argued for keeping `rray_reduction_iterator_init()` and
`rray_broadcast_iterator_init()` as one-line wrappers, on the grounds that the
names told a reader what a call site was doing.

That was overruled. Once they held no logic they were not worth two files, so
both wrappers and both headers are gone, and the five call sites call
`rray_iterator_init()` directly. Everything that walks an array now includes
`src/iterator.h` and nothing else.

The cost is that a call site is five positional arguments with no name saying
which kind of walk it is. Broadcast is the one to read carefully, since the
walked space is the view and the indexed space is the input.

---

# Improvement 2: `rray_iterator2`, two locations from one walk (done)

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
location spaces, built the same way as Improvement 1.

Do not add named wrappers per use. Improvement 1 ended with those removed, so
`src/iterator2.h` should hold the struct, the init, the accessors, and the step
function, and callers should use them directly.

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

# Settled question

The name `rray_iterator2`. In this codebase a trailing `2` elsewhere means two
inputs, as in `rray_broadcast_names2(x, y, dimensions)`. Here it means two
location spaces. Those coincide for binary operations, but not for split, which
is one input with two output spaces.

The name was kept. If it ever reads wrong, the thing to rename is the struct,
not the concept.

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
