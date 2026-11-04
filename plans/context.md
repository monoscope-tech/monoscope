# Retained decisions and follow-ups

Consolidated on 4 November 2026 from the August task batch, its follow-up lists, and the September Slack documents.
These notes preserve design constraints and historical findings. They do not establish current release status.
The [product program](2026-09-17-product-improvement-program.md) and [unified-agent plan](2026-10-30-unified-agent.md) contain newer work.

## Discarded migration proposals — 4 November 2026

The two inactive migration proposals are deleted rather than scheduled for startup execution:

- Retain the Git rename compatibility views from migration 0126. Dropping these views provides no functional benefit and requires deployment coordination.
- Discard the PostgreSQL telemetry event-identity unique index. It requires duplicate cleanup and a blocking index build on the telemetry hypertable.
  TimeFusion is the target telemetry store. PostgreSQL-specific deduplication is not a prerequisite for that migration.

No applied migration changes. PostgreSQL replay uniqueness remains unenforced by the discarded index.

## Explorer, source context, and dashboards

- **Environment scope:** August added the promoted environment column, a sticky cookie, and learned facet options. Historical rows received no backfill.
  The newer product program covers environment and service scope across charts, dashboards, APIs, and monitors.
  The old warning about an uncommitted TimeFusion schema edit was subsequently closed.
- **Service map:** logs and traces share `viz_type=logs`. Their links differ through `kind=="log"` and `kind!="log"`.
  The renderer owns filter interop. Missing optional template hooks must not blank the map.
- **Latency bars:** collapsed rows use their own duration as the axis. Expanded traces align all rows on the trace axis.
  Service colors identify services rather than operation names. Interval intersection excludes children entirely outside the parent window.
  Deep-trace readability remains a historical review item. The [color plan](color-signal-hierarchy-v2.md) contains later presentation work.
- **Identity:** session, user, and tenant fields share one field list for display and facets.
  A user ID must remain visible beside an email and must not carry the label “user name.”
- **Source context:** stack parsers preserve unrecognized lines. Mappings use the longest matching path prefix and resolve source at a known revision.
  Native disclosure and lazy snippet requests avoid fetching every frame. The first in-app frame opens by default.
  A later change added a successful-response cache keyed by owner, repository, revision, and path, with a 15-minute TTL.
  The earlier proposal to use the timeseries query cache is obsolete.
- **Required dashboard variables:** unresolved variables show an inline prompt and prevent widget queries.
  Defaulting Endpoint Analytics to `*` changes its meaning. It describes one endpoint, while the Endpoints page provides the aggregate view.
  Empty options need an explicit state. `tab.requires` and required variables share this behavior.
- **Dashboard copies:** an Explorer chart has no source dashboard. Copying its JSON into a destination must work without a source dashboard ID.

Historical source-context follow-ups need inspection before implementation:

- Issue stacks used a synthesized format rather than the span exception format. Reusing the span renderer requires actual payload evidence.
- SDK-attached snippets, a settings path tester, and source-map/debug-file symbolication did not ship in the August pass.
- The snippet cache gave immutable revisions and branch names the same TTL.

Implementation commits include `f78051166` (environments), `4615b0cc1` (service map), and `ab48f9c10` (latency).
Source context landed through `8b27dd49f`, `e292038e2`, and `a1b44e481`. Identity landed in `cc0c346bb`.

## Metric and trace correlation

The August correlation stream shipped without a schema migration. Exact exemplar IDs and service-plus-window matching represent different evidence strengths.

- Exemplar links use the exemplar's timestamp. A histogram row can contain an exemplar much older than the row itself.
- Exemplars are representative samples, not proof of the worst request. Missing retained traces need an explicit unavailable state.
- Both query directions use bounded windows and result limits. Matching a trace ID in serialized exemplars requires exact parsing afterward.
- Service charts reuse the metric catalog rather than scan raw metrics for names.
- Span charts use their own padded time window and highlight the span interval.
  The 26 August follow-up added widget time bounds and a dashed instant marker when the interval occupies less than 1% of the chart.
- Dashboard-wide exemplar overlays remain separate work. An arbitrary aggregate widget does not identify one metric family.

## TimeFusion correctness and testing

**Dedup projection incident:** TimeFusion `f8d7278` resolved the August cross-project failure, `DedupExec key 'id' not in input schema`.
The fix retained dedup keys in the scan projection. The discarded candidate patch did not establish the correct fix.

The reproduction selected distinct project IDs within a bounded timestamp window and restricted span kinds.
Project-scoped queries passed. Cross-project projection of only `project_id` failed, while adding `id` or `deleted` passed.
Regression coverage must reach the certified-partition skip path. A test without timestamp bounds can miss that path entirely.

**Integration reads:** tests retain TimeFusion reads when `TIMEFUSION_PG_TEST_URL` enables the real store.
Disabling reads hid a production-incompatible `mode() WITHIN GROUP` query, fixed in Monoscope `13982e099`.

- Shared-store assertions need isolated projects or deltas. A deterministic UUID stream that restarts per example cannot provide isolation.
- Attribute queries need the store's supported extraction operations, including `variant_get` on TimeFusion.
- Visibility delays and incorrect pruning are different failures. More polling cannot repair a pruned bucket.
- TimeFusion `bf3abc8` fixed a mem bucket that advertised its routing timestamp instead of its actual row range.
  Monoscope `a21485f30` restored the regression. A multi-timestamp batch and a query above its earliest row expose this bug.
- Local tests with TimeFusion reads disabled do not verify that path. A manually configured store with broken storage cannot provide reliable evidence.

Current watcher and service procedures belong to [AGENTS.md](../AGENTS.md) and [local CI](../docs/local-ci.md).
The removed test note's direct compiler commands and shared-storage wipe are obsolete procedures.

Historical certification follow-ups belonged to the TimeFusion repository's dedup coverage plan.
The August sweep covered today and yesterday while dashboards queried longer windows. Coverage needed separate work from projection correctness.
Historical controls were `timefusion_dedup_certification_persist=false`, then `timefusion_read_dedup_skip_swept=false`.
Current names and effects require inspection before operational use.
CapRover changed to `stop-first`, and probe timing plus scheduling-lag instrumentation landed in TimeFusion `d50d2e4`.
Reading that instrumentation under load remained a historical follow-up.

## Slack incident delivery

The [unified-agent plan](2026-10-30-unified-agent.md) supersedes the September proposal, implementation log, and repeated review log.
The [cross-module review](../docs/reviews/2026-11-01-agent-cross-module-review.md) records newer acceptance evidence and its limits.
Old test counts, local tool failures, and deployment restrictions in iteration logs do not certify the current tree.

The incident adapter retains these constraints alongside the shared agent contract:

- One incident episode has one root per workspace/channel destination and one investigation thread.
  Warning-to-critical transitions update that episode. A confirmed recovery followed by another alert creates a new linked episode.
- Recovery, reminders, manual resolution, and missing telemetry remain distinct events. Silence does not prove recovery.
- Durable delivery serializes root creation. Recovery waits for a pending root, and retries retain the original root timestamp.
  Ambiguous provider outcomes require reconciliation before another publication.
- A timestamp requires destination provenance. Workspace replacement invalidates old bindings.
  Private-channel webhooks retain notification delivery until the bot has the required access.
- Signed receipts, authorization at execution boundaries, role-preserving history, cancellation, and durable progress remain adapter requirements.
- Charts retain units, thresholds, actual time bounds, and text fallback. Missing measurements remain gaps.
  Query errors differ from successful empty results. Scalar monitor history needs an explicit measurement strategy.
- Related incidents supply comparison leads rather than proof of a common cause.
- Repository evidence uses authorized credentials and immutable revisions. Runbook paths reject traversal, and runbook content never grants execution authority.

The older proposal included action drafts and draft PRs. The unified plan explicitly excludes code changes and deployments from its first launch.
The newer plan controls that scope. Physical-device and screen-reader acceptance remain subject to its verification limits.

## Commercial and infrastructure follow-ups

The [billing activation runbook](../docs/runbooks/activate-metered-billing.md) owns the provider boundary checklist.
The August notes recorded these constraints:

- Stripe meter events alone do not charge a subscription without the corresponding price item.
  Attachment during a billing period can include earlier usage from that period.
- Product copy must agree with the prices that customers actually pay.
- Dormant usage is not automatically buffered for later billing. Any historical reconciliation requires an explicit operator decision.
- LemonSqueezy's single-item limitation used equivalent event units: ten metric datapoints per unit and 1,000 units per replay.
  The intended rates were $1 per million events, $1 per ten million metric datapoints, and $1 per thousand replays.

The August container follow-up required `k8s.cluster.name` for the cluster facet. Only `k8s.cluster.uid` existed in that recorded production sample.
The [container plan](container-monitoring.md) retains the detailed data contract and exclusions.
Old migration reservations, lint counts, local failures, and shared-checkout warnings describe past sessions rather than current work.
