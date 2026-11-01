# OTLP routing found during proactive Slack acceptance

The isolated QA service returned HTTP 200 for three protobuf log records authenticated only by `x-api-key`, but stored zero records. The monitor therefore could not exercise the intended telemetry-to-Slack flow.

## Change

Logs and traces now share one private `ProjectRouting` value and one project resolver. Verified resource credentials retain precedence over a resource project ID and the verified header project. Header authority is resolved once from the existing credential lookup; it is never inserted into telemetry attributes or disguised as an empty resource key.

Queued exports explicitly carry no header project. A resource cannot inherit a sibling resource's project. The former trace-only single-project fallback is removed. Existing cache membership, sampling, storage selection, effect rows, and operation ordering remain in place. There is no new module, package, configuration, migration, or executor. Runtime source grows by four lines.

## Failed-first verification

The existing header tests counted broad shared-project datasets, so unrelated fixture records could satisfy them. Their replacement sends credential-free resources through the actual HTTP exporter functions and queries their unique service.

- Header-only logs and traces: before the fix, expected two records and found one. Afterward, both persist.
- Resource-only sibling isolation: before the fix, expected two credentialed records and found three. Afterward, the two uncredentialed siblings are excluded. Distinct log content and span identity prevent deduplication from concealing misrouting.
- Two-project precedence: a resource key and header key belong to different projects. Exactly one log appears in the resource project; the header project has neither records nor a query error.
- All six focused authorization cases pass in 0.3604 seconds. The later broader run includes a concurrently edited API schema regression; that unrelated failure is not an ingestion-suite signoff.
- Actual loopback HTTP export against the updated QA application stores all three submitted logs with no resource credentials. Its PG-only fixture uses real protobuf decoding, authentication, conversion, and insertion. It does not establish TimeFusion acceptance.

Private, ignored receipts: `scripts/local/otlp-header-only-red.log`, `otlp-sibling-routing-red.log`, `otlp-routing-six-green.log`, `slack-telemetry-header-only-probe.json`, and `slack-telemetry-header-only-green-probe.json`.

## Scoped review

Repeated hs-distill review keeps the single resolver and existing pipeline; no parallel credential lookup or transport implementation is introduced. The constraint review covers both direct and queue callers: the new optional typed project represents actual verified header authority, and no sentinel, weakened effect, partial function, or suppression is added. The locality review finds no client-side change. Pinned Fourmolu passes. HLint reports no hints in runtime code or changed test code; five pre-existing hints remain outside this scope.

Full branch reviews, release checks, and telemetry-fed Slack acceptance remain separate requirements of the unified plan.
