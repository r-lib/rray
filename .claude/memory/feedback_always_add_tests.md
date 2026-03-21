---
name: Always add tests
description: User expects tests to be written immediately after any new feature, without being asked
type: feedback
---

ALWAYS add tests in `tests/testthat/test-{name}.R` after implementing any new feature in `R/{name}.R`. Do not wait for the user to ask.

**Why:** User explicitly stated "We ALWAYS add tests" — this is a non-negotiable part of the workflow.
**How to apply:** After writing any new R function, immediately create or update the corresponding test file and run the tests.
