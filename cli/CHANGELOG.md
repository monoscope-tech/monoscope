# Changelog

## Unreleased

- Breaking: `metrics query --assert` now exits with failure when the query does not return a numeric scalar. Previously it printed a warning and exited successfully. Use `summarize` without a `by` clause for assertion queries.

- Credential files are restricted to `0600` before writing, and the config directory is restricted to `0700`, including existing files and directories. Config init masks token entry.
