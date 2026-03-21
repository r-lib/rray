# clang-format configuration

- Added `AllowShortFunctionsOnASingleLine: None` to `.clang-format` to prevent clangd from collapsing short function bodies (like FFI wrappers) onto a single line.
- Added `ContinuationIndentWidth: 2` to fix initializer lists (like `CallEntries[]`) using 4-space indent instead of 2. The Google base style defaults `ContinuationIndentWidth` to 4.
- Added `SpaceAfterCStyleCast: true` to insert a space after C-style casts, e.g. `(int) x` instead of `(int)x`.
