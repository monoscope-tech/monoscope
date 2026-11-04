# Plans

This directory holds focused plans and unresolved investigations. Status statements belong to each document's recorded date.
The index groups documents by topic. It does not certify current deployment or test results.

## Start here

- [Product improvement program](2026-09-17-product-improvement-program.md): query trust, investigation flows, activation, scope, and billing follow-ups.
- [Unified agent](2026-10-30-unified-agent.md): the current agent contract, architecture, rollout, and remaining manual checks.
- [Retained context](context.md): decisions and follow-ups from completed work and superseded plans.
- [Customer flow coverage](customer-flow-test-coverage.md): coverage evidence and remaining test work.

## Query and investigation

- [Dashboard queries and panels](dashboard-query-and-panel-convergence.md)
- [Array membership lowering](array-has-lowering.md)
- [Log list performance](log-list-performance.md)
- [Live tail](07-live-tail.md)
- [Git hosts](07-git-hosts.md)
- [Scanner traffic and endpoint discovery](scanner-traffic-pollutes-endpoint-discovery.md)

## Infrastructure and presentation

- [Infrastructure experience](infrastructure-experience.md): product model and navigation.
- [Container monitoring](container-monitoring.md): collector, metric, and inventory contract.
- [Infrastructure, Sessions, and RUM performance](infra-sessions-rum-performance.md): query findings and implementation history.
- [Light-mode contrast](light-mode-color-contrast.md)
- [Color signal hierarchy](color-signal-hierarchy-v2.md)
- [Weekly reports](weekly-report-redesign.md): completed report contract and verification record.

## Billing and operations

- [Billing pricing v2](billing-pricing-v2.md): metering design. The [activation runbook](../docs/runbooks/activate-metered-billing.md) covers provider changes.
- [Job scheduler redesign](job-scheduler-redesign.md)
- [Self-chaining tickers](self-chaining-tickers-multiply.md): the scheduler investigation.
- [Firing alerts triage](firing-alerts-triage-2026-09-01.md)
- [Replay demo outage](session-replay-demo-outage-2026-09-01.md)
- [Talstack notification repair](talstack-notification-repair-2026-09-07.sql): an unexecuted, guarded repair, retained for operator review.

## Maintenance

Keep one plan per distinct workstream. Fold durable decisions and unresolved follow-ups into the existing plan before deleting superseded documents.
Remove completed task logs when the code, tests, runbooks, or review records hold the necessary evidence.
Git history retains removed documents: `git log --all -- plans/<filename>`.
Repository guidance in [AGENTS.md](../AGENTS.md) controls current development and release procedures.
