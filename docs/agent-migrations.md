# Consolidated agent migrations

The unreleased agent work uses five migrations after master's `0258`:

| File | Contents |
| --- | --- |
| `0259_native_agent_records.sql` | Native review/fix records, attempts, usage, allocations and current eligibility view. |
| `0260_shared_agent_runs.sql` | Routine/run authority, operations, evidence scopes, review policy and wiki authorship. |
| `0261_slack_agent_delivery.sql` | Shared checkpoints, OAuth credentials, incident delivery, reply publication and streaming. |
| `0262_legacy_slack_evidence.sql` | Conservative classification of previously retained Slack evidence. |
| `0263_native_agent_runs.sql` | Original-requester backfill and immutable native job/run linkage. |

Master's migration filenames and contents are unchanged.
The five files replace 48 branch-only feature migrations at the user's explicit request.
Fresh installations run them through the normal startup migration runner.

## Existing unreleased installations

Do not rerun consolidated DDL over an existing feature schema.
Do not delete schema, feature data or migration history to force a rerun.
The runner ignores historical filenames absent from its current migration directory.
Keeping those records preserves their audit trail.

Production inspection through `../monoscope/.env` found 21 original feature migrations, all with matching checksums.
The nine native feature tables contain no rows.
The complete native-record group is applied; only the two older memory changes exist from the shared-run group.
The guarded production transition then added only `0259_native_agent_records.sql` to the migration ledger.
All 273 previous records and timestamps remain unchanged; the ledger now has 274 records.
No schema DDL or feature-data mutation ran. The nine native feature tables remain empty.
The native-record file is now applied in production and must remain unchanged.

The helper checks every present original checksum and rejects duplicate or conflicting history.
It adopts complete groups by recording their consolidated file, preserving the old records.
It also recognizes production's two older memory changes, which the pending shared-run migration handles explicitly.
Other partially applied groups fail closed and require inspection.
A complete original 48-file installation adopts all five consolidated records.
Production's verified 21-file installation adopts only the native-record file, then runs the four pending files normally.
A repeat preserves the same records and timestamps.

Generate and inspect the SQL before applying it:

```sh
python3 scripts/baseline-agent-migrations.py > /private/tmp/agent-baseline.sql
# Set normal libpq connection variables for the intended database.
psql -X -v ON_ERROR_STOP=1 --single-transaction -f /private/tmp/agent-baseline.sql
```

Use this only on a verified installation matching a supported group shape.
Inspect and finish unsupported partial groups before adoption.
An installation without any of the original feature migrations uses normal startup instead.
Run adoption between deployments, while no startup migration runner is active.
Apply it before the merged version starts; the running application does not need a schema reset.
Local verification covers fresh installation, the original full schema and the observed production shape.
Production baseline adoption is complete. The next authorized deployment uses normal startup migrations to apply the remaining four files.
No production application deployment occurred during preparation.
Deployment remains subject to the normal PR and local signoff requirements.

Run the isolated baseline safety test against local PostgreSQL with the usual libpq variables:

```sh
PYTHONDONTWRITEBYTECODE=1 python3 scripts/baseline-agent-migrations-test.py
```

It checks repeat adoption, original history/data preservation, the observed production shape,
and rejection of missing, changed, null or duplicate checksums and empty history.
