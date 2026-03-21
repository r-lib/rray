# Clang-format brace spacing

**Cost:** $0.06

## 2026-03-21

Davis wanted designated initializers formatted with spaces inside braces (`{ .x = frame, .env = r_null }` instead of `{.x = frame, .env = r_null}`). Added `Cpp11BracedListStyle: false` to `.clang-format` and reformatted all C files.
