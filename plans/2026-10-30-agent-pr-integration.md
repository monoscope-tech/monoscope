# Unified agent PR integration

Canonical integration PR: [#619](https://github.com/monoscope-tech/monoscope/pull/619).
The selected work is consolidated locally. Publication still requires final local image/signoff after cleanup.
This document records integration scope, not merge or deployment authorization.
The [unified plan](2026-10-30-unified-agent.md) defines the product contract.
The [current review](../docs/reviews/2026-11-01-agent-cross-module-review.md) records verification.

## Selected work

The recorded inventory covers 28 open PRs and selects twenty incoming heads.
All twenty selected heads and published #619 remain ancestors of the local branch after the actual rebase onto master `d842c8dc3`.
The sixteen-PR impact stack forms one parent-to-child chain through #709.
It supplies shared execution infrastructure without granting Slack code-publication or deployment authority.

| PR | Contribution | Treatment |
| --- | --- | --- |
| [#693](https://github.com/monoscope-tech/monoscope/pull/693) | Pi/Gondolin review and fix infrastructure, durable receipts, usage, artifacts, cache maintenance, and conversational wiki tools. | Reuse the shared infrastructure. Preserve the Slack branch's newer routine and authorization contracts. |
| [#694](https://github.com/monoscope-tech/monoscope/pull/694) | Haskell impact-evaluation orchestration. | Keep within the stack. Do not create another evaluation runner here. |
| [#696](https://github.com/monoscope-tech/monoscope/pull/696) | Haskell source-review comparisons and validation. | Keep its common validation and evidence contracts. |
| [#697](https://github.com/monoscope-tech/monoscope/pull/697) | Offline guest localhost resolution. | Retain the guest correction and regression. |
| [#698](https://github.com/monoscope-tech/monoscope/pull/698) | Host mappings and regression input guidance. | Preserve through runtime reconciliation. |
| [#699](https://github.com/monoscope-tech/monoscope/pull/699) | Complete regression patches and explicit parse failures. | Preserve complete input transport and useful failures. |
| [#700](https://github.com/monoscope-tech/monoscope/pull/700) | Caller tracing and review-premise guidance. | Reuse existing policy text rather than adding another prompt system. |
| [#701](https://github.com/monoscope-tech/monoscope/pull/701) | Frozen regression patches from complete test files. | Retain the Haskell input boundary and regression constraints. |
| [#702](https://github.com/monoscope-tech/monoscope/pull/702) | Isolated scratch clones of protected source. | Preserve source protection and private workspace behavior. |
| [#703](https://github.com/monoscope-tech/monoscope/pull/703) | Fingerprints for tracked submodule directories. | Retain the source inventory correction. |
| [#704](https://github.com/monoscope-tech/monoscope/pull/704) | Native advisory file tools selected by Haskell policy. | Preserve the role policy through the runtime merge. |
| [#705](https://github.com/monoscope-tech/monoscope/pull/705) | Native Pi guidance and one review response schema. | Retain the shared schema and native guidance. |
| [#706](https://github.com/monoscope-tech/monoscope/pull/706) | Shell deadlines that preserve the review workspace. | Required regression for the reconciled runtime. |
| [#707](https://github.com/monoscope-tech/monoscope/pull/707) | Typed evaluation modes and selectors. | Keep typed selectors and the shared runner. |
| [#708](https://github.com/monoscope-tech/monoscope/pull/708) | Screenshot image bytes through native file reads. | Required regression for the reconciled runtime. |
| [#709](https://github.com/monoscope-tech/monoscope/pull/709) | Review evidence guidance and recorded model comparisons. | Retain runtime guidance and comparison commands. Historical research exports remain in Git history; they do not prove general accuracy. |


| Additional PR | Integrated behavior |
| --- | --- |
| #692 | Haskell VM ownership, fix orchestration and SDK host protocol. Reconciled with later stack receipts and artifacts; replaced JavaScript workflow paths removed. |
| #661 | Matching and total production-evidence series through the existing scoped query tool and PNG renderer. No second model query planner. |
| #571 | Slack threadless fallback, bounded recovery, diagnostics, reconnect guidance and prompts through the current receipt/run path. |
| #568 | Five-minute infrastructure-health work budget and cancellation coverage. |

Published #619 retains its original ancestry, allowing a normal fast-forward branch update.
Other unrelated PRs remain separate, including OIDC, tooltip confinement, SARIF, hardware visualization and protobuf repair.
The unrelated icon-script rewrite from #692 is excluded; the existing Python helper is preserved.

## Reconciliation decisions

- Keep one Haskell owner for production guests, fix phases and private supervisor commands.
- Keep the later stack's outbox, Git isolation, chat memory, accounting and evaluation contracts.
- Retain complete regression files, source fingerprints, shell-deadline recovery and image transport.
- Share current member access across browser and background actors; only browser sessions retain demo/sudo behavior.
- Keep original requester, routine policy, evidence audience and operation identity through retries.
- Reuse the capability catalog, operation ledger, ProjectMemory and conversation inspector.
- Keep domain review/fix attempts and usage through typed links to shared runs.
- Preserve master issue lifecycles, signed SQL isolation, shared test helpers and canonical test prelude.
- Consolidate only branch feature migrations under the user's later explicit instruction. Preserve all master migration bytes.

## Evidence and remaining gates

The final actual rebase onto master PR #763 preserves selected ancestry and exactly matches an independent merge of the verified branch and the new master.
The previous branch remains available locally as `codex/slack-agent-before-master-763-cfb98f6df`.
Current canonical checks, native/KVM recovery and live Slack acceptance are summarized in the consolidated review.
Detailed merge previews, iteration logs and source hashes remain in Git history and ignored local receipts.

Finish migration/documentation cleanup and affected verification before final Mac-only image/signoff.
Then push this branch and update #619. Do not merge or deploy without separate authorization.
