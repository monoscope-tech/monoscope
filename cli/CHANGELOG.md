# Changelog

## Unreleased

- Breaking: `metrics query --assert` now exits with failure when the query does not return a numeric scalar. Previously it printed a warning and exited successfully. Use `summarize` without a `by` clause for assertion queries.
