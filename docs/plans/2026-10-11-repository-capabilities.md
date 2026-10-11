# Repository capabilities

## Agreed behavior

- One explicitly selected repository list per project. Repository detail owns all configuration; remove the global Configuration tab and duplicate add forms.
- Addition authorizes automatic setup and activation, including PR comments and configuration writes. Activate reliable matches; request input for uncertain settings. Later analysis suggests changes and respects manual overrides and explicit disables.
- Many-to-many service links shared by independently enabled source context and PR reviews.
- One managed resource folder per repository, containing dashboards and all monitor types. Preserve existing YAML formats and configured directory prefixes; extend the existing dashboards/ convention to monitors/.
- Monoscope edits take effect immediately and update one automated PR from monoscope-dashboards. Imports read the configured default branch after merge.
- Three-way reconciliation preserves pending local edits, imports independent remote changes, and resolves conflicts per resource by choosing local or Git. Deletions participate in the same workflow.
- Closing an automated PR unmerged retains active local resources and pauses export. Resumption is explicit.
- Disabling retains configuration. Removal retains local dashboards, monitors, and review history, closes the automated PR, and leaves Git files unchanged.
- Monitor definitions sync; incidents, evaluations, check-ins, and temporary mutes remain local.

## Implementation and verification

1. Consolidate navigation and repository configuration. Handler regression tests must show one list, repository-bound forms, separate capability controls, and retained settings.
2. Implement automatic analysis and activation using existing HTTP/LLM effects. Test reliable versus ambiguous matches, unavailable providers, retries, and preservation of user overrides.
3. Extend configuration ownership and codecs to query, uptime, and cron monitors. Test definition round trips without importing runtime state.
4. Replace direct dashboard export with automated PR reconciliation. Test repeated edits, target-branch imports, merge, unmerged closure, deletion, and per-resource conflicts.
5. Exercise editor/viewer permissions, keyboard navigation, narrow layouts, error recovery, and existing migration compatibility.
6. Run hs-distill, hs-evasion-review, and hs-lob-review until settled; simplify the final diff, run local signoff, and submit through a PR.

## Boundaries

No bulk configuration, multiple folder mappings per repository, or field-level conflict merging in this release. Existing integrations retain their paths and resource ownership during migration.
