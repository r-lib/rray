# N-ary addition with single-location iterator passes

## Status

This document is an implementation plan. It does not describe code that is
already present.

The plan branch is based on `main`, at the iterator-performance merge. A future
implementation should first incorporate the adjacent-axis coalescing work if it
has landed separately. The design below benefits from that work but does not
depend on it for correctness.

## Goal

Extend `rray_add()` from a binary operation to an operation that accepts two or
more arrays:

```r
rray_add(x, y, ...)
```

Keep the existing binary path unchanged when `...` is empty. For three or more
inputs, allocate one result, seed it from the first input, and accumulate each
remaining input into it in a separate pass. Every pass uses its own optimized
`rray_iterator`.

The goals are:

- preserve the current two-input throughput and small-call overhead;
- support an arbitrary number of broadcastable inputs;
- avoid a runtime operand loop inside the innermost element loop;
- avoid materialized broadcast inputs and full-size cast intermediates;
- allocate only the final output vector, apart from small setup objects;
- preserve deterministic left-to-right floating-point and integer arithmetic;
- reuse the existing common-dimension, common-type, and common-name machinery;
- leave `rray_iterator2` available for operations that intrinsically require
  two simultaneous locations, notably `rray_combine()`.

This is intentionally limited to addition. Subtraction, division, and
exponentiation have less natural N-ary APIs. Multiplication could adopt the
same architecture later, but should not be included merely to make the first
implementation more generic.

## Summary of the design

The public wrapper keeps `x` and `y` as required arguments and adds `...`:

```r
rray_add <- function(x, y, ...) {
  if (nargs() == 2L) {
    return(.Call(ffi_rray_add, x, y, environment()))
  }

  .Call(ffi_rray_add_n, list2(x, y, ...), environment())
}
```

The exact wrapper may need adjustment for project style and missing-argument
behavior, but the important property is that the binary call does not build a
list and reaches the existing native entry point unchanged.

The N-ary native path performs these phases:

1. Normalize and validate every input as an unclassed array.
2. Determine the final arithmetic output type across all inputs.
3. Determine the common broadcast dimensions across all inputs.
4. Allocate one output vector of the final type.
5. Copy and cast the first input into the output through one `rray_iterator`.
6. For each later input, add and cast it into the output through a fresh
   `rray_iterator`.
7. Attach the common dimensions and coalesced names.

In pseudocode:

```text
xs = normalize(x, y, ...)
output_type = add_common_type(xs)
dimensions = dimensions_common(xs)
out = allocate(output_type, size(dimensions))

copy_cast_pass(out, xs[0], dimensions)

for input in xs[1:]:
    add_cast_pass(out, input, dimensions)

set dimensions(out)
set names(out, broadcast_names_common(xs, dimensions))
```

Every pass writes the output with the flat iterator index and uses the mapped
location only to read its input. Because passes are independent, they do not
need synchronized point state or an N-location iterator.

## Why not a runtime-N iterator inner loop

A natural alternative is one iterator containing a runtime-sized array of
locations and strides:

```text
for each output element:
    combine values at locations[0:n]
    for location in locations[0:n]:
        advance location
```

That representation is logically general, but the dynamic location loop sits
inside the hottest loop. It prevents the compiler from seeing two scalar
locations and two fixed strides in the binary case.

A temporary experiment replaced the two scalar location fields with a runtime
location collection while leaving the arithmetic operation and type-specific
cores unchanged. The runtime location count was still exactly two. On an Apple
M2 Pro with R 4.6.0 alpha, Apple Clang 17, normal package flags including
`-O2`, and one million output elements, it produced these medians:

| Case | Fixed two locations | Runtime location collection | Slowdown |
|---|---:|---:|---:|
| Double identity shapes | 0.707 ns/element | 2.561 ns/element | 3.62 times |
| Double plus scalar | 0.725 ns/element | 2.569 ns/element | 3.54 times |
| Integer identity shapes | 1.027 ns/element | 2.549 ns/element | 2.48 times |
| First axis size one | 2.219 ns/element | 7.802 ns/element | 3.52 times |
| Crossed broadcast | 0.739 ns/element | 2.608 ns/element | 3.53 times |

This does not prove that every N-location iterator design must be slow. A
generic traversal plan that exposes runs to fixed-arity inner kernels can be
fast. It does show that a literal runtime operand loop must not replace the
current fixed-arity inner loop.

The accumulator design avoids that problem. Each pass has one location, one
fixed row stride, and one typed operation.

## Why not run two ordinary iterators in lockstep

Two `rray_iterator` instances cannot simply be nested or independently stepped
inside the current iterator macros. Each instance owns a complete traversal of
the point space. Coordinating them element by element would duplicate carry
work or require a new zip loop, which recreates the role of `rray_iterator2`.

It also complicates coalescing. Two independently initialized iterators can
choose different private computation shapes because coalescing depends on the
location mapping. A genuinely fused N-operand iterator must choose a shape
approved by every operand.

The accumulator avoids synchronization entirely. It completes one input pass
before starting the next, so each single-location iterator can use the best
private computation shape for that input alone.

## Do not initialize the result by adding to zero

The conceptual description may sound like this:

```text
fill output with zero
output += x
output += y
output += z
```

Do not implement it literally. Instead use:

```text
output = cast(x)
output += y
output += z
```

Seeding from the first input is important for both performance and semantics.

### Performance

A zero fill adds a complete output write. Adding the first input then requires
another output read and write. For integer addition it also performs an
unnecessary checked addition for `0 + x`.

In a temporary prototype, the seeded integer accumulator took about 1.19
nanoseconds per element for two identity-shaped integer inputs. The literal
zero accumulator took about 1.94 nanoseconds per element, versus about 1.03 for
the fused binary implementation.

### Signed zero

IEEE floating-point addition distinguishes positive and negative zero. The
existing binary implementation preserves the result of the actual binary
operation:

```r
rray_add(array(-0, 1L), array(-0, 1L))
# negative zero
```

Starting the accumulator from positive zero changes that result to positive
zero. Copying the first input and then adding the second preserves the binary
operation. The same caution applies to complex signed-zero components and may
also matter for NaN payload behavior.

### Empty input list

The public API continues to require `x` and `y`. There is therefore no need to
invent dimensions, type, or names for a zero-input additive identity.

## Arithmetic and type semantics

### Determine the final type before allocating

The N-ary path should determine one final output type from all inputs before
doing dimension work or allocation. Apply the addition-specific promotion
after finding the common native type:

| Common input type | Addition output type |
|---|---|
| logical | integer |
| integer | integer |
| double | double |
| complex | complex |

Character, raw, and list inputs remain unsupported.

The existing `rray_ptype_common()` machinery can provide the common native
type, but the error wording and error precedence must be reviewed. Existing
binary arithmetic reports an operator-specific unsupported-type error before a
dimension error. The N-ary path should preserve the analogous ordering and
identify the offending dots element.

Do not cast complete inputs up front. Dispatch each pass from the input storage
type to the already-known output type and cast one element at a time using the
existing scalar cast helpers.

This changes the shape of type specialization in a useful way. Binary addition
currently specializes on the pair of input types. N-ary accumulation only
needs pass kernels specialized on:

```text
(input storage type, output storage type, copy or add)
```

Only widening combinations admitted by the common type are needed. The number
of kernels grows with the type tower, not exponentially with the number of
inputs.

### Global promotion semantics

Compute the common type across the full input list before evaluating any
addition. This means:

```text
integer_max + 1L + (-1.0)
```

uses a double output from the beginning and does not first perform an
overflowing integer intermediate. This is common-type N-ary semantics, not a
literal reduction that materializes each binary result before discovering the
next input's type.

Document and test this choice because it differs from repeatedly calling the
existing binary `rray_add()` left to right when a later input raises the common
type.

### Evaluation order within the final type

After promotion, process inputs in their supplied order. At each output
element, the stored value evolves as:

```text
cast(x)
add y
add z
...
```

Although the implementation traverses the entire output between operands, the
per-element arithmetic order is still left to right. This preserves predictable
floating-point rounding and NaN behavior.

### Integer overflow

When the final type is integer:

- the first pass copies or casts into the output without addition;
- every later pass uses `rray_add_int_one()` against the current output value;
- overflow aborts at the first input, in supplied order, that makes an element
  overflow.

Do not perform a checked `0 + x` in the seed pass.

### Missing and special values

Reuse the existing scalar cast and addition helpers. Add explicit tests for:

- logical, integer, double, and complex missing values;
- `NaN`, `Inf`, and `-Inf`;
- signed zero;
- complex values with missing, NaN, infinite, and signed-zero components.

The seed pass must use the same scalar conversion rules as existing binary
arithmetic, especially the project's rule for casting missing logical or
integer values to complex.

## Dimensions and broadcasting

Normalize all inputs before computing their common dimensions. Keep the same
validation behavior as current binary arithmetic:

- classed inputs are rejected;
- bare vectors are converted to one-dimensional arrays;
- zero dimensions are valid where the existing dimension rules allow them;
- incompatible axes produce an error naming the relevant inputs.

Use `rray_dimensions_common()` or the underlying merge machinery to find one
common point space.

For each pass:

```text
point dimensions = common output dimensions
location dimensions = current input dimensions
```

Initialize one `rray_iterator` from those spaces. Its flat `INDEX` writes the
output in R column-major order and its mapped `LOCATION` reads the possibly
broadcast input.

No broadcasted input should be allocated.

### Coalescing interaction

If adjacent-axis coalescing is available, each pass may coalesce based solely
on the current input mapping. This is at least as permissive as a fused iterator
whose computation shape must work for all operands simultaneously.

Leading unit axes must not recreate the earlier short-row performance problem.
Benchmark shapes with a first dimension of one after integrating the coalescing
work.

## Names

Use `rray_broadcast_names_common(xs, dimensions)` after successful arithmetic.
It already walks inputs in order and fills each result axis from the first input
that contributes valid non-broadcast names.

Test that:

- names can come from the third or later input;
- names on broadcast axes are discarded;
- earlier contributing names win;
- unnamed axes do not prevent later inputs from contributing names;
- all-unnamed inputs produce no dimension names;
- zero-size results preserve the expected names.

## Proposed native organization

Keep the existing binary implementation and entry point intact:

```text
ffi_rray_add(x, y, frame)
    -> rray_add(...)
    -> rray_binary_arithmetic(...)
    -> existing iterator2 typed core
```

Add a separate N-ary entry point reached only when additional arguments exist:

```text
ffi_rray_add_n(xs, frame)
    -> rray_add_n(xs, arg, error_call)
```

The exact names can follow repository conventions.

`rray_add_n()` should own validation, common type, common dimensions, output
allocation, per-input iterator setup, names, attributes, and protection.

The typed work can be factored into two families:

```text
copy_cast_<input>_to_<output>(input, out, iterator)
add_cast_<input>_to_<output>(input, out, iterator, error_call)
```

Generate these with compact macros in the same style as current arithmetic
cores. Keep actual loop variables and strides visible to the compiler. Do not
route each element through a function pointer. Dispatch once per complete input
pass.

Likely files:

- `R/arithmetic.R`: add `...`, binary fast-path routing, documentation;
- `src/arithmetic-add.c`: N-ary entry point and addition-specific dispatch;
- `src/arithmetic.c` and `src/arithmetic.h`: only if shared shell helpers are
  genuinely clearer than keeping the work addition-specific;
- `src/decl/arithmetic-add-decl.h`: declarations for static pass kernels;
- `src/init.c`: native routine registration;
- `tests/testthat/test-arithmetic-add.R`: semantics and errors;
- generated documentation and namespace files if roxygen changes require them.

Avoid changing `rray_iterator2` as part of this feature. It remains useful to
binary arithmetic and `rray_combine()`, and its removal is an independent
design decision.

## R-level fast-path considerations

The two-input wrapper should reach the current `.Call` with the same arguments.
Do not unconditionally build `list2(x, y, ...)` merely to discover that the
list has length two.

Use a cheap arity decision such as `nargs()` if it behaves correctly for all
supported calling forms. Test positional and named calls. Confirm behavior for
missing arguments and any dots-splicing syntax accepted through `list2()`.

If `nargs()` introduces semantic complications, a small wrapper helper is
acceptable, but benchmark calls on one-element arrays because wrapper and
native setup dominate there.

## Performance model

Let `B` be the output element size and `N` the number of inputs. Ignore caches,
write allocation, and iterator metadata for a simple traffic model.

A hypothetical fused N-input kernel transfers approximately:

```text
(N + 1) * B
```

bytes per element: one read per input and one output write.

The seeded accumulator transfers approximately:

```text
(3 * N - 1) * B
```

bytes per element:

- first input read plus output write: `2 * B`;
- every later input read plus output read and write: `3 * B`.

For two inputs this is `5B` instead of `3B`, a theoretical traffic ratio of
1.67. In practice the output is commonly still cached for the next pass, so
the measured regression is smaller. For outputs much larger than cache, expect
the result to become more memory-bandwidth-bound and move toward the traffic
ratio.

As `N` grows, the accumulator's traffic ratio relative to a perfect fused
kernel approaches three. That comparison omits the fact that a truly dynamic
fused kernel may vectorize poorly, while every accumulator pass is a simple
streaming loop. Benchmark rather than assuming the fused dynamic design wins.

### Measured seeded-accumulator prototype

A temporary prototype kept all existing type-specific binary cores but
replaced their fused iterator2 loop with two single-iterator passes:

1. copy/cast `x` into the output;
2. add/cast `y` into the output.

It copied the already-computed point dimensions and each location stride map
into a separate `rray_iterator`, isolating the loop-structure question without
changing public setup behavior.

Environment:

- Apple M2 Pro;
- R 4.6.0 alpha, arm64;
- Apple Clang 17;
- normal package flags including `-O2`;
- one million output elements;
- multiple runs in alternating build order;
- values below are representative medians across runs.

| Case | Existing fused binary | Seeded accumulator | Expected effect |
|---|---:|---:|---:|
| Double identity shapes | about 0.72 ns/element | about 0.87 ns/element | about 20% slower |
| Double plus scalar | about 0.74 ns/element | about 1.00 ns/element | about 35% slower |
| Integer identity shapes | about 1.03 ns/element | about 1.19 ns/element | about 16% slower |
| Crossed broadcast | about 0.73 ns/element | about 0.92 ns/element | about 26% slower |
| Dimensions `[1, 1e6]` | about 2.20 ns/element | about 0.88 ns/element | accumulator about 2.5 times faster |

The first-axis-size-one result reflects the baseline iterator implementation's
short-row weakness. Re-run it after adjacent-axis coalescing; do not present it
as an inherent accumulator advantage.

At ten million doubles, observed results were noisy but generally put the
seeded accumulator roughly 10% to 25% behind the fused loop. This remains
hardware- and cache-dependent.

For one through one hundred elements, call times were effectively the same at
roughly 0.6 microseconds. Around one thousand elements, the seeded accumulator
was roughly 10% slower in the tested runs. Preserving the exact binary entry
point should eliminate even that distinction for public two-input calls.

### Performance expectations for the proposed implementation

Two-input public calls:

- should execute the current code path exactly;
- should retain current element throughput within benchmark noise;
- should not allocate a dots list;
- should retain current small-call latency within benchmark noise.

Three or more inputs:

- should scale linearly with input count;
- should perform one output allocation;
- should perform one streaming pass per input;
- should allocate no full-size cast or broadcast intermediates;
- should remain easy for the compiler to vectorize for double and simple
  integer cases;
- will become increasingly limited by repeated output traffic for large arrays;
- may compare favorably with a runtime-N inner loop despite higher theoretical
  memory traffic.

## Alternatives considered

### Repeatedly call binary `rray_add()`

An R- or C-level left fold over the existing binary operation preserves binary
kernel speed within each pass, but allocates `N - 1` full output arrays,
recomputes dimensions and names repeatedly, and may expose different global
promotion semantics.

It is an acceptable minimal prototype, not the desired implementation.

### Precast and prebroadcast every input

Casting and broadcasting every input to the final type and dimensions makes the
final arithmetic loop simple, but allocates `N` full-size intermediates and
does more copying than the accumulator. It discards a central benefit of the
iterator design and should not be used.

### One dynamic element-major fused loop

This writes the output once and minimizes theoretical memory traffic, but needs
runtime pointer, type, and stride selection for every input at every output
element. The location-only prototype showed a severe binary regression before
adding dynamic type handling. Do not use it for the common binary path.

### Generic traversal plan with fixed-arity run kernels

A more sophisticated future iterator could own arbitrary operands, coalesce
them jointly, and expose inner runs to fixed-arity kernels. This is compatible
with high binary performance but is a larger architectural project. It is not
needed for N-ary addition when independent accumulator passes are acceptable.

## Correctness tests

Add focused tests covering at least the following.

### Arity and compatibility

- two inputs still produce byte-for-byte identical results;
- three and several inputs with identical dimensions;
- positional and named dots inputs;
- incompatible dimensions involving the third and later inputs;
- classed third and later inputs;
- vectors normalized to one-dimensional arrays;
- zero-size arrays and zero dimensions on different axes.

### Broadcasting

- scalars represented as size-one arrays;
- a later input broadcast on each axis;
- crossed broadcast patterns across three or more inputs;
- differing dimensionalities with missing trailing axes;
- leading unit axes and high-dimensional shapes;
- shapes specifically chosen to exercise coalescing boundaries.

### Types

- all logical inputs produce integer output;
- all integer inputs produce integer output;
- logical, integer, double, and complex mixtures in different orders;
- a later double or complex input establishes the global output type;
- unsupported character, raw, and list inputs in the first, middle, and last
  positions;
- errors identify the correct dots element;
- global promotion is tested against the documented non-materialized semantics.

### Arithmetic edge cases

- integer overflow caused by the second, middle, and final input;
- no integer overflow when a later input establishes double output globally;
- missing values of every supported input type;
- `NaN`, infinities, and combinations of them;
- positive and negative zero, including `-0 + -0`;
- floating-point examples where left-to-right order is observable;
- complex missing-value and signed-zero behavior.

### Names and attributes

- names supplied only by a third or later input;
- first-valid-name-wins behavior across several inputs;
- broadcast axes discard input names;
- dimensions and dimension names on zero-size outputs;
- no unintended attributes copied from seed inputs.

### Allocation behavior

Use profiling or targeted instrumentation outside ordinary unit tests to
confirm:

- one full-size output allocation;
- no full-size cast inputs;
- no full-size broadcast inputs;
- no full-size binary intermediates.

## Benchmark plan

Run current binary `rray_add()` and the implementation from separately
installed package libraries. Alternate build order and benchmark order. Use
normal package optimization flags.

Measure at least:

- element counts `1`, `10`, `100`, `1000`, `1e6`, and `1e7`;
- logical, integer, double, and complex output;
- homogeneous and mixed input types;
- identity shapes;
- scalar broadcasting;
- axis broadcasting;
- crossed broadcasting;
- first dimension one;
- dimensionality sweeps;
- three, four, eight, and thirty-two inputs;
- output sizes below and above cache capacity.

Report both time per call for small inputs and nanoseconds per output element
for large inputs. For N-ary calls also report nanoseconds per input element so
scaling across operand counts is interpretable.

Compare these implementations where practical:

1. existing fused binary path;
2. public two-input fast path after adding `...`;
3. seeded single-iterator accumulator;
4. repeated public binary calls;
5. a simple runtime-N fused prototype, only as a diagnostic baseline.

Track allocations separately from elapsed time.

## Acceptance criteria

The feature is ready when:

- `rray_add(x, y)` uses the existing binary native path;
- binary correctness tests and snapshots are unchanged except for intentional
  documentation or signature updates;
- binary large-array performance is within 5% of the baseline across ordinary
  identity and broadcast cases, with differences investigated rather than
  automatically accepted;
- binary small-array call latency is within 5% of the baseline;
- three or more inputs allocate one full-size result and no full-size
  intermediates;
- N-ary results obey global common-type promotion and left-to-right arithmetic
  within that type;
- signed zero is preserved relative to the defined arithmetic order;
- integer overflow behavior is tested and documented;
- common dimensions and names include every input;
- all package tests pass under the normal test configuration;
- package checks introduce no new warnings or notes.

Performance thresholds are guardrails, not permission to hide benchmark noise.
Record the machine, compiler, R version, package flags, shapes, operand counts,
and full benchmark code with the implementation results.

## Suggested implementation sequence

1. Rebase onto the latest `main`, including iterator coalescing if it has
   landed.
2. Add the R signature and preserve an explicit two-input fast path.
3. Add tests proving the two-input path remains behaviorally identical.
4. Add N-ary input normalization and argument-tag handling.
5. Add operator-specific global type selection.
6. Add the typed seed copy/cast passes.
7. Add the typed accumulation add/cast passes.
8. Add common dimensions, one iterator per pass, and output attributes.
9. Add common names across all inputs.
10. Add arithmetic, broadcasting, type, error, zero-size, and name tests.
11. Run the complete test suite and package checks.
12. Run the benchmark matrix, comparing against an isolated baseline build.
13. Document actual results and any deviations from the expectations here.

## Final recommendation

Implement N-ary addition as an accumulator over independent optimized
single-location iterator passes, but seed from the first input rather than
zero. Preserve the existing fused `rray_iterator2` binary path when `...` is
empty.

This gives the public API arbitrary arity without placing operand-count
dynamism in the inner loop, without allocating broadcast or cast
intermediates, and without paying the measured accumulator cost for the common
two-input call. Keep `rray_iterator2` for binary arithmetic and for other
algorithms such as combine that genuinely need two simultaneous locations.
