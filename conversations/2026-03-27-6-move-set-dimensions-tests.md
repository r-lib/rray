# Move set-dimensions tests to test-dimensions.R

Running cost: ~$0.15

## Prompt

> move all of the set-dimensions tests to test-dimensions.R

## Actions

1. Read `test-set-dimensions.R`, `test-dimensions.R`, and both snapshot files.
2. Appended all `rray_set_dimensions()` tests to `test-dimensions.R` under a new `# rray_set_dimensions()` section header.
3. Appended all set-dimensions snapshots to `_snaps/dimensions.md`.
4. Deleted `test-set-dimensions.R` and `_snaps/set-dimensions.md`.
5. Ran `devtools::test(filter = '^dimensions')` - all 61 tests pass.
