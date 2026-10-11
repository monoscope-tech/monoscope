# Changelog

## 0.6.31

- Breaking: `metrics query --assert` now exits with failure when the query does not return a numeric scalar. Previously it printed a warning and exited successfully. Use `summarize` without a `by` clause for assertion queries.

- Credential files are restricted to `0600` before writing, and the config directory is restricted to `0700`, including existing files and directories. Config init masks token entry.

- The CLI reports its release version, and the installer recognizes an already installed release without downloading it again.
