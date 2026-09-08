# Local CI

Run checks locally before pushing changes. Prefer laptop build caches and local services.
Use `make ci-signoff` to run checks and publish passing results for GitHub to reuse.
For a limited change, use `make ci-signoff CHECKS="..."` with the relevant checks from `ci/checks.tsv`.
The final status output lists every check that still needs GitHub.

Record the local commands, results, and outstanding checks in the PR description.
If a required service or tool is unavailable, record that limitation and let GitHub run the affected checks.
Never publish an attestation for a check that did not pass.

Use standard GitHub-hosted runners for remote jobs. Do not add Blacksmith runners or actions without an explicit user request.
See [the local CI guide](docs/local-ci.md) for setup, capabilities, and cache controls.
