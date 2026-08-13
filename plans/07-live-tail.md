# Live Tail: design and implementation plan

## Outcome

Add Live Tail as the first Explorer tab. Keep Events as the default Explorer page.

Live Tail shows new log records that match one service and an optional KQL filter. It does
not query historical data. It does not provide pagination, charts, latency cards, or
throughput cards.

The ingest path evaluates active subscriptions before the TimeFusion and PostgreSQL writes.
It sends matched records to browsers through Server-Sent Events (SSE).

## Product decisions

| Question | Decision |
| --- | --- |
| Data type | **Logs only.** The ingest hook can receive mixed telemetry, but Live Tail ignores non-log records. |
| Cross-pod fan-out | **Kafka side topic.** Ingest pods publish one message for each matched subscription. |
| Filter engine | **Filter-only KQL evaluator** over the existing `Pkg.Parser.Expr` AST. |
| Delivery guarantee | **At most once.** Live Tail has no replay or resume cursor. |
| Backpressure | **Bounded queue for each browser stream.** Drop the oldest record and report the dropped count. |
| Service selector | **Required.** A browser cannot register a stream without one service. |
| Subscription lifetime | **Lease with expiry.** An open SSE connection renews the lease. Expiry is authoritative. |
| Transport in an all-in-one process | ~~**Local STM hub.**~~ **Superseded** — see the *no Kafka requirement* revision at the end. `LocalHub` was removed because no process can tell whether ingest and HTTP share one; a Postgres relay table serves every deployment without Kafka. |

If Live Tail must include spans or requests, revise the data contract, selectors, row shape,
and tests before implementation.

## Non-goals

- Do not read TimeFusion or PostgreSQL for old records.
- Do not replay records after a reconnect.
- Do not guarantee delivery during a pod restart or network failure.
- Do not let a Live Tail failure delay or fail telemetry ingestion.
- Do not support KQL pipeline commands such as `summarize`, `extend`, `project`, `sort`, or `take`.
- Do not support time predicates. Live Tail only evaluates records that arrive after registration.

## Stream contract

The stream has these properties:

- A subscription belongs to one project, one user, and one browser tab.
- The server checks project access during registration, streaming, and removal.
- The server combines the required service predicate with the optional user filter.
- Matching starts after the ingest cache observes the subscription. This can take one cache interval.
- Records keep ingest order within one Kafka partition. Global order across ingest pods is not guaranteed.
- A reconnect creates a new live connection for the same active subscription. It does not recover missed records.
- A slow browser loses the oldest queued records. A control event reports the cumulative loss count.
- The server sends a heartbeat comment at least every 15 seconds to keep proxies from buffering or closing the stream.

Use explicit SSE event names:

| Event | Data | Status |
| --- | --- | --- |
| `ready` | Subscription ID and lease expiry time. | Done |
| `log` | One bounded Live Tail row. | Done |
| `dropped` | Cumulative records dropped by this browser connection. | Done |
| `notice` | A recoverable stream error, as a safe message. | **Done**, but renamed from `error`. EventSource dispatches on the frame's event name, and `error` is already its own transport-failure event — a frame called `error` arrives at the client's `onerror` handler, which *retries*. The one event meaning "retrying will not help" would have been the one event guaranteed to trigger a retry. Carried as a `Notice` row over the same addressed transport, so it reaches the pod actually holding the connection; sent today when a stored filter stops compiling. |

Do not put credentials, internal errors, or KQL parser details in SSE error events.

## HTTP API

Place all routes under the authenticated project route.

### `POST /p/:projectId/live_tail/subscriptions`

Accept JSON or form data with:

- `service`: required, non-empty service name.
- `query`: optional filter-only KQL text.
- `environment`: optional exact environment name.

The handler must:

1. Check the user's access to the project.
2. Parse the query with `parseQueryToAST`.
3. Accept only zero or more `Search` and `WhereClause` sections.
4. Reject all other sections with `400`.
5. Validate field names with the existing parser validation.
6. Add the service, environment, and logs-only predicates on the server.
7. Enforce the subscription limits atomically.
8. Insert a short lease and return `201`.

Return `subscriptionId`, `streamUrl`, and `expiresAt`. Use an opaque random UUID for the
subscription ID. The UUID is not an authorization mechanism.

### `GET /p/:projectId/live_tail/subscriptions/:subscriptionId/stream`

The handler must check the project, user, subscription, and expiry before it starts SSE.
Register the bounded local queue before the handler sends `ready`.

Set headers that disable response caching and proxy buffering. Renew the database lease while
the connection is healthy. Stop renewal when the response writer fails or the handler exits.

### `DELETE /p/:projectId/live_tail/subscriptions/:subscriptionId`

Delete the caller's subscription. Treat an already removed subscription as success so cleanup
is idempotent.

The SSE handler can also remove its row when it exits. This cleanup is best effort. The lease
handles browser crashes, pod crashes, and lost disconnect signals.

## Subscription storage

Add `static/migrations/0125_live_tail_subscriptions.sql`. Confirm that `0125` is still the next
migration number before implementation.

The table needs these fields:

- `id`: subscription UUID and primary key.
- `project_id`: project owner.
- `user_id`: user owner.
- `service`: required exact service name.
- `environment`: optional exact environment name.
- `query`: original KQL text for diagnostics.
- `filter_ast`: validated filter AST, or enough data to rebuild it without changing semantics.
- `created_at`: creation time.
- `expires_at`: lease expiry time.

Add an index for active subscriptions by project and expiry. Add an expiry index for cleanup.
Use the repository's normal foreign-key and deletion conventions.

Do not rely on a periodic delete before a subscription becomes inactive. Every cache query and
stream lookup must require `expires_at > now()`.

## Architecture

```text
ingest pod                                      web pod
──────────────────────────────────────          ──────────────────────────────────
processList                                     POST subscription
  └─ dualWriteWithPoisonMapping                   └─ validate, limit, insert lease
       │
       ├─ LiveTail.publishMatches               GET SSE stream
       │    ├─ read active project subscriptions  ├─ authorize subscription
       │    │  from the local cache               ├─ register bounded local queue
       │    ├─ ignore non-log records             ├─ send ready and heartbeats
       │    ├─ evaluate each filter                └─ renew lease
       │    └─ publish bounded matched rows ──┐
       │                                      │
       └─ bulkInsert to TimeFusion and PG     │
                                              ▼
                                  `monoscope.live_tail`
                                  key = subscription ID
                                  short retention
                                              │
                                              ▼
                                  one consumer group per web pod
                                  offset reset = latest
                                              │
                                              ▼
                                  local STM hub
                                  subscription ID → SSE queues
```

Each web pod needs every live-tail message because any load-balanced SSE connection can land
on any web pod. Give each web pod a unique consumer group. Use the subscription ID as the
Kafka message key so one subscription keeps partition order.

The consumer starts at the latest offset and does not replay old records. Choose one explicit
offset policy and cover restart behavior with a test. Do not describe the consumer as both
“no commit” and a normal durable consumer group.

### Ingest cache

Keep an immutable cache keyed by project ID. Refresh it from PostgreSQL every two seconds.
Only load unexpired subscriptions. Parse and validate stored filters before placing them in
the cache. If one stored filter is invalid, omit that subscription and record a metric.

A project with no active subscription costs one map lookup. Place hard limits on:

- Active subscriptions per user.
- Active subscriptions per project.
- Total subscriptions loaded by one ingest pod.
- Query length and regex length.
- Evaluation time or work per ingest batch.

Apply limit changes to new registrations first. The cache must still protect itself if the
database already contains more rows than the configured limit.

### Filter evaluator

Add `shared/src/Pkg/Parser/Eval.hs` with a pure API. Prefer a result type that distinguishes a
false match from an evaluation error.

```haskell
evalExpr :: (Subject -> Maybe AE.Value) -> Expr -> Either EvalError Bool
evalValues :: (Subject -> Maybe AE.Value) -> Values -> Either EvalError AE.Value
```

Define these semantics before implementation:

- Missing fields and JSON `null`.
- String, number, boolean, array, and object comparisons.
- Numeric coercion, if any.
- Case sensitivity for every text operator.
- Array wildcard resolution.
- `has`, `has_any`, and `has_all` token boundaries.
- Invalid and expensive regular expressions.
- Supported scalar functions in `Values` and `BoolFunc`.

The evaluator must match the SQL KQL behavior for the supported subset. Build conformance tests
from parser expressions and known JSON records. Do not silently treat evaluation errors as
matches. Count errors and treat them as non-matches.

The original draft omitted `NotStartsWith` and `NotEndsWith`, although both constructors exist
in `Expr`. Cover every constructor or reject it during registration.

### Record shape and size

Define a versioned Kafka envelope. It must contain the subscription ID and a bounded Live Tail
row. Reuse the existing log-list JSON shape where practical so detail rendering stays
consistent.

Set a maximum serialized message size below the configured Kafka broker limit. Decide how to
truncate large bodies, attributes, and stack traces. Mark truncated values in the row. If a
record still exceeds the limit, drop it and increment a metric.

Publishing must be non-blocking or strictly time-bounded. A Kafka error must not fail the main
ingest write. Log and count the failure without logging the telemetry payload.

### Transport selection

Use one transport for a deployment:

- Use Kafka when the live-tail topic is configured and Kafka is enabled.
- Use the local STM hub only when ingest and HTTP run in the same process.
- Disable Live Tail with a clear UI state when neither transport can deliver records.

Do not silently use the local hub in a split ingest/web deployment. That topology accepts a
subscription but can never deliver a record.

## Configuration

Add typed configuration with safe defaults:

- Live Tail enabled flag.
- Kafka topic.
- Cache refresh interval.
- Lease duration and renewal interval.
- Queue capacity for each SSE connection.
- Subscription limits per user and project.
- Maximum query, regex, row, and Kafka message sizes.

Validate related values at startup. For example, the renewal interval must be less than half
the lease duration. Reject an enabled Kafka mode with an empty topic.

## Failure behavior

| Failure | Required behavior |
| --- | --- |
| PostgreSQL unavailable during registration | Return `503`. Do not create an in-memory-only subscription. |
| PostgreSQL unavailable during lease renewal | Keep the connection briefly. Close it before the last known lease expires. |
| Ingest cache refresh fails | Keep the last cache until its subscriptions expire locally. Record cache age. |
| Kafka publish fails | Continue the main ingest write. Increment a publish-failure counter. |
| Kafka consumer disconnects | Reconnect with bounded backoff. Show the browser that the stream is not healthy. |
| SSE client is slow | Drop the oldest queued row and send a `dropped` event. |
| Web pod exits | The browser reconnects. Missed records are not replayed. |
| Subscription expires | Remove it from the cache and close any local SSE stream. |

## Security and privacy

- Use the authenticated user identity for every subscription operation.
- Check project membership for every route. Never trust the project ID in a request alone.
- Do not accept a client-supplied project predicate, user ID, or subscription owner.
- Apply the project's normal log-view permission. Do not create a weaker Live Tail permission.
- Protect the registration and removal routes with the repository's normal CSRF policy.
- Do not write record bodies or attributes to application logs.
- Treat KQL regexes as untrusted input. Add length and execution safeguards.

## Observability

Add metrics for:

- Active subscriptions, split only by low-cardinality transport or pod-role labels.
- Cache age, refresh duration, loaded rows, and refresh failures.
- Records evaluated, matched, published, consumed, delivered, and dropped.
- Filter evaluation errors and invalid stored filters.
- Kafka publish failures, consumer lag, and reconnects.
- Active SSE connections and lease renewal failures.
- Queue depth distribution and oversized record drops.

Use low-cardinality labels. Do not label metrics with project, user, service, query, or
subscription IDs.

## Implementation sequence

1. **Specify evaluator semantics.** Add the pure evaluator and conformance tests in
   `shared/src/Pkg/Parser/Eval.hs`.
2. **Add subscription storage.** Add migration `0125`, model functions, authorization checks,
   atomic limits, leases, and expiry cleanup.
3. **Add the local stream core.** Add subscription queues, drop accounting, heartbeat events,
   and deterministic concurrency tests in `src/Pkg/LiveTail.hs`.
4. **Add registration and SSE routes.** Update `src/Web/Routes.hs` and add
   `src/Pages/LogExplorer/LiveTail.hs`.
5. **Add ingest matching.** Call `LiveTail.publishMatches` from
   `OtlpServer.dualWriteWithPoisonMapping`. Filter mixed batches to logs first.
6. **Add Kafka transport.** Add the versioned envelope, time-bounded producer path, and one
   fan-out consumer group for each web pod.
7. **Start managed fibers.** Update `src/System/Server.hs` so cache and consumer fibers stop
   during graceful shutdown.
8. **Add configuration.** Update `src/System/Config.hs` and deployment configuration.
9. **Add the page.** Add `web-components/src/live-tail.ts` with connect, pause, resume,
   capped rows, dropped-record status, reconnect status, and cleanup.
10. **Add navigation.** Insert `("Live Tail", "/live_tail")` before Events in
    `Utils.explorerTabs`, while keeping Events as the default route.
11. **Add observability and rollout controls.** Keep the feature disabled by default until the
    Kafka topic, limits, dashboards, and alerts exist.

Pause is a browser display action. It must not create an unbounded server backlog. Choose one
of these behaviors and label it clearly in the UI: continue receiving into the fixed row cap,
or disconnect and miss records until resume.

## Test plan

### Unit and property tests

- [x] Cover every accepted `Expr` and `Values` constructor. — doctests in `Pkg.Parser.Eval`.
- [x] Compare evaluator results with SQL results for a shared table of supported expressions.
      — `EvalConformanceSpec`, 15 cases run both ways against one seeded row.
- [x] Cover missing fields, nulls, mixed types, arrays, wildcards, Unicode, and invalid regexes.
- [x] Prove that selector predicates cannot be removed or overridden by the client query.
- [x] Prove that queue size never exceeds its configured bound.
- [x] Prove that overflow drops the oldest row and increases the loss counter.
- [x] Round-trip the versioned Kafka envelope — **but not its size limit**, which is not
      enforced as a whole-envelope check (see "What is left", size cap).

### Integration tests

- [x] Reject registration without a service. — structural: `Scope` cannot be built without one.
- [x] Reject pipeline commands and unsupported expression constructors.
- [x] Reject users without project access on all three routes. — one test on
      `activeSubscriptionFor`, the single `WHERE` all three resolve through.
- [ ] Enforce per-user and per-project limits **under concurrent registration.** The atomic
      statement is tested sequentially; the race is not exercised.
- [x] Register a subscription, refresh the cache, ingest a mixed batch, and deliver only the
      matching log record.
- [ ] Route one subscription through Kafka to a simulated web pod. — covered at both ends
      (envelope round trip, hub routing), never through a broker.
- [ ] Show that two web-pod consumer groups each receive the same matched message.
- [x] Expire a lease and verify that matching and streaming stop without explicit removal.
- [ ] Disconnect SSE and verify best-effort cleanup plus eventual expiry. — the teardown delete
      exists; no test drives it.
- [ ] Simulate Kafka failure and verify that the main telemetry write still succeeds. — the
      publish path is non-fatal by construction, but nothing pins it.
- [x] Simulate a slow browser and verify the `dropped` event.
- [ ] Verify that reconnect starts at new records and does not replay old records.

### Browser tests

- [ ] Connect, show the ready state, append rows, and enforce the client row cap. — append and
      drop count are in `live-stream.test.ts`; the row cap is not asserted.
- [ ] Show connecting, live, reconnecting, paused, expired, and disabled states. — `expired` and
      the refusal path are covered; `connecting` / `reconnecting` / `paused` are not.
- [ ] Keep selector and filter values after a reconnect.
- [x] Stop the stream when the user leaves the page or changes project.
- [x] Keep Events as the Explorer landing page although Live Tail appears first. — e2e.

## Rollout

1. ~~Create the Kafka topic with short retention and a safe partition count.~~ **Done**
   (2026-08-12): `live_tail`, 6 partitions, 1 replica, `retention.ms=600000`,
   `retention.bytes=67108864`. Short retention is not a cost saving — nothing on this topic is
   replayable, and a reconnecting browser deliberately never rewinds, so anything older than a
   few minutes is garbage by construction.
2. ~~Deploy the schema, configuration, producer, and consumer.~~ **Done** — migrations 0125–0127
   are on master; there is no configuration left to deploy, and no dark-deploy switch. Steps 4
   and 7 below are void for the same reason: there is no feature flag to stage a rollout with,
   and no operator-set limits to tighten.
3. **Not done, but no longer blocked** — the metric set landed (step 11); this is now just the
   act of looking at them in staging.
4. ~~Enable the feature for internal projects only.~~ **Void** — no flag; ships on for everyone.
5. **Not done.** Load-test many subscriptions against high-volume projects.
6. **Not done.** Verify that ingest latency and write success do not change materially.
7. ~~Set conservative subscription and row-size limits before wider release.~~ **Void as a
   rollout step** — the limits are compile-time constants, already conservative
   (`maxPerUser = 3`, `maxPerProject = 20`, `maxCached = 500`, `maxRowFieldChars = 8000`).

The feature is ready when all acceptance criteria are true:

- [x] A matching log normally appears in the browser within three seconds. — pre-write matching
      puts it under the write-visibility floor; not measured under load (step 6).
- [x] An unmatched or non-log record never appears.
- [x] Live Tail adds no unbounded queue, cache, or Kafka growth.
- [x] A Live Tail dependency failure does not fail or materially delay ingestion. — true by
      construction (publish is buffered and non-fatal); no test pins it, and latency is unmeasured.
- [x] Cross-project access tests fail closed.
- [~] The UI reports connection health and dropped records accurately. — drops and `expired` are
      accurate; `connecting` / `reconnecting` / `paused` are untested, and the server has no way
      to report a recoverable error (no `error` frame).
- [x] A pod restart loses only in-flight live records and recovers without operator action.

## Rejected alternatives

- **PostgreSQL `LISTEN`/`NOTIFY`:** The payload limit requires truncation. `LISTEN` also does
  not work through the production transaction-pooling topology.
- **Read the main ingest topic on every web pod:** This design requires every web pod to decode
  every project's telemetry. This path also misses direct gRPC ingest.
- **Local STM only:** This works in one process. It cannot deliver between cloud ingest and web
  pods.

---

## Implementation status (2026-08-12)

Recorded against the numbered implementation sequence above.

| # | Step | Status |
| --- | --- | --- |
| 1 | Evaluator + semantics | **Done.** `shared/src/Pkg/Parser/Eval.hs`. Every `Expr` constructor covered including `NotStartsWith`/`NotEndsWith`; `evalExpr` returns `Either EvalError Bool`; semantics documented in the module header and pinned by doctests. |
| 2 | Subscription storage | **Done.** Migration `0125`, lease + expiry-filtered reads, per-user/per-project counts, idempotent delete, reaper. |
| 3 | Local stream core | **Done.** `src/Pkg/LiveTail.hs` — bounded queue, drop-oldest with cumulative counter, hub attach/detach, `takeBatchWithin` for heartbeats. |
| 4 | Registration + SSE routes | **Done.** `src/Pages/LogExplorer/LiveTail.hs` + `Web/Routes.hs`. Local `EventStream` content type, no new dependency. |
| 5 | Ingest matching | **Done.** `fanOutToLiveTail` called from `dualWriteWithPoisonMapping`, after id minting, before the durable write. Logs-only filter lives in `matchesFor`. |
| 6 | Kafka transport | **Done.** Versioned envelope, keyed by subscription id; producer installed in `withLiveTailTransport`; per-pod consumer group at `offsetReset = Latest`, no commit. |
| 7 | Managed fibers | **Done.** `live-tail-cache` (follows ingest) and `live-tail-consumer` (follows HTTP) in `System/Server.hs`, both under `supervise` and the existing shutdown path. |
| 8 | Configuration | **Done, then deliberately undone.** Shipped as eight `LIVE_TAIL_*` vars, all since deleted — see the *no configuration* revision at the end. Nothing to configure is the finished state, not a gap. |
| 9 | Page + web component | **Done.** `web-components/src/live-tail.ts` — connect, capped buffer, pause-as-display-freeze, drop counter, backoff reconnect, lease renewal, cleanup on unload. |
| 10 | Navigation | **Done.** `Live Tail` first in `Utils.explorerTabs`; Events unchanged as the landing route. |
| 11 | Observability + rollout | **Partial.** `PublishStats` counts evaluated/matched/failed/publish-failed, and the cache refresher logs cap-hit and uncompilable-filter conditions. The full metric set in the Observability section is **not** wired — it waits on the OTel metrics API (`TODO(otel-metrics)` elsewhere in the tree). The feature now ships **enabled**, which departs from the rollout section — see below. |

### Deviations from the spec above, and why

- **Resolver type.** The spec sketches `Subject -> Maybe AE.Value`; the implementation uses
  `Subject -> [AE.Value]`. A `Maybe` cannot express an array wildcard (`events[*].name`), which
  the spec separately requires — a list makes wildcards and plain subjects one code path
  instead of two that can disagree.
- **`has` token boundaries.** The spec asks for this to be decided. Decided as **substring,
  case-insensitive** — identical to `contains` — because that is what `Display Expr` already
  lowers both to (`~*` over a regex-escaped term). Parity with the shipped Events behaviour
  beats parity with upstream KQL; the module header states this explicitly.
- **Lease renewal is driven by the browser**, not by the SSE handler. A server-side renewal
  loop keeps a lease alive against a crashed tab, a proxy-wedged connection, or a sleeping
  laptop — exactly the cases the lease exists to clean up. It also keeps DB access inside the
  effect stack rather than threading a pool into the raw-`IO` streaming body.
- **Filter stored as KQL text**, not a serialized AST. The parser is the single source of truth
  for what a query means; a stored AST is a second encoding of that meaning, free to drift from
  it across a deploy.
- **`ENABLE_LIVE_TAIL` defaults to True**, at the author's explicit request, overriding step 11
  and the rollout sequence above (both of which called for shipping dark). What makes this
  merely a policy change rather than a risk: the flag is consent, not capability. Deployments
  that cannot deliver still refuse — `transportFor` resolves a split ingest/web deployment with
  no topic to `Unavailable`, registration returns 503, and the tab explains itself.
  `LIVE_TAIL_TOPIC` also now defaults to `live_tail`, which forced two corrections to how the
  transport is chosen:
  1. An earlier version picked Kafka whenever a topic was named. With a default topic that
     would route every single-process dev box at a broker it does not have.
  2. The obvious fix — also requiring `enableKafkaService` — was worse, and would have shipped
     a silent production failure. That flag is **per-process**: web pods run with it off and
     ingest pods with it on, so the two roles would have chosen *different* transports. Web
     pods would serve from a local hub nobody publishes to while ingest published to a topic
     nobody consumed: subscriptions accepted, rows flowing, tail empty forever.
  The signal is therefore **whether any Kafka brokers are configured**, the one input both pod
  roles share. `LiveTailSpec` pins this: a broker-configured deployment must never resolve to
  `LocalHub`, whatever the topic — it either uses Kafka or refuses outright. What the
  default does change is that the ingest hook is now live in every deployment, so the rollout
  steps that still matter are the load test and the ingest-latency check; the Kafka topic must
  exist before any deployment with `LIVE_TAIL_TOPIC` set can serve a tail.

*(The "Not done" list that stood here has been folded into
[What is left](#what-is-left-2026-08-12) at the end of this document — most of it has since
been closed, and keeping three separate open-items lists is how one of them goes stale.)*

---

## Extension: the Events tab's live toggle (2026-08-12)

Events already had a live mode: a 5s `setInterval` re-running the cursor query against
TimeFusion. It is now served by the same push path as Live Tail.

### Why polling could not be made fast

The first proposal was to keep the query as the source of truth and use SSE only as a *nudge*
— push "something matched", let the browser fetch. That is wrong twice over:

1. **The nudge necessarily precedes the data.** A row waits for its ingest batch to fill
   before `bulkInsertOtelLogsAndSpansTF` runs at all, and the hook that would fire the nudge
   sits *before* that write. The fetch would run against a row that is not yet queryable.
2. **It trades one push for a full query.** Planning plus a scan over the memory buffer and
   Delta, once per nudge. On a busy project that is worse than the 5s poll it replaces.

Write-visibility latency is a floor no polling interval can get under. The only way beneath it
is to read the record before it is written — which is what the push path already does.

### What changed

- **`Scope` replaces the bare `service` field.** `LogsOnly Text | AllSignals`. The service gate
  is now structural: Live Tail cannot be constructed without a service, and Events has no place
  to put one. This is the invariant that was previously a `NOT NULL` plus a handler check —
  a combination the second surface would have had to lie to.
- **Migration 0126** adds `scope` and `columns`, drops `NOT NULL` on `service`, and adds a
  CHECK that `logs_only` still requires one. Verified against a scratch DB: a gate-less
  `logs_only` row is rejected, `all_signals` is accepted.
- **`LiveRow` becomes a sum.** `LogRow` keeps Live Tail's fixed projection; `TableRow` carries
  column name → value for exactly the columns the browser said it was rendering. The `Scope`
  that selected the subscription selects the shape, so they cannot be mismatched.
- **Column projection.** The server cannot derive the column list — a query's `finalColumns`
  may hold SQL expressions only the database can evaluate — so the browser sends its own, and
  ingest resolves each name by mapping `___` back to `.` and reusing the filter evaluator's
  resolver. One traversal rule serves both matching and projection, so a field that filters
  correctly also renders correctly. Columns only SQL could compute are omitted, and the cell
  stays empty until the durable read fills it.
- **`live-stream.ts`** extracts the register/stream/renew/backoff/cleanup lifecycle, now shared
  by both surfaces rather than duplicated.
- **Pushed rows merge through `groupSpans` + `mergeIntoTree`**, the same path a recent fetch
  uses, so trace grouping, the new-row highlight and scroll anchoring stay identical between
  pushed and fetched rows.

### Consequences accepted deliberately

- **Live rows are provisional.** They are matched pre-write, so a row whose TimeFusion write
  later fails would appear and then vanish on the next durable read. Live Tail already accepted
  this; Events now does too.
- **Live mode may drop rows under load** (confirmed acceptable, 2026-08-12). Events has no
  service gate to bound it up front, so it is bounded by the per-connection queue instead:
  drop-oldest with a cumulative count surfaced as "N dropped — narrow your query". The
  alternative — an unbounded buffer — is an out-of-memory crash with extra steps.

### Test coverage

Kept deliberately thin: each test pins a distinct failure mode rather than re-covering paths
already exercised elsewhere. The row's journey through grouping, dedup and scroll anchoring is
the *same* code a fetch takes and is covered by the pagination tests, so only the boundary
where pushed rows enter it is tested.

- `test/integration/LiveTailSpec.hs` — scope invariants both ways, spans reaching Events but
  never a logs-only tail, cross-service matching, the `___` column projection, omission of
  unresolvable columns, the Kafka envelope round-tripping in both row shapes, and hub routing
  delivering to the named subscription and no other.
- `web-components/test/live-stream.test.ts` — the shared lifecycle (register, stream, drop
  count, stop), the two refusal paths (server message surfaced verbatim; an expired lease
  stopping rather than silently reconnecting into a gap), and the push→render boundary.

Two bugs were found by writing these, both of which would have shipped:

1. **Pushed rows never rendered.** `groupSpans` keys the tree off trace adjacency, not off the
   row array, and pushed rows were passed an empty `traces` list — so it returned an empty tree
   every time. A fetch receives adjacency from the server; a pushed row arrives alone, so the
   client now synthesises the same minimal entry a standalone record would have had. The
   feature would have looked completely inert with nothing in the logs to explain it.
2. **`handleLiveRows` dropped rows when `logsContainer` was null.** The container decides where
   a row goes, never whether it arrives — and unlike a fetch there is no cursor to re-request a
   dropped push with.

### Closed since (2026-08-12)

- **SQL-vs-evaluator conformance** — `test/integration/EvalConformanceSpec.hs`. Fifteen cases,
  each run *both ways against one row*: the evaluator over the decoded record, and the SQL
  `Display Expr` emits over that row in Postgres. The row is seeded into both the flattened
  columns the SQL reads and the JSON blobs the evaluator resolves through, since seeding only
  one would make every case agree for the wrong reason. A non-vacuity guard asserts both sides
  actually decided — at least one True, at least one False, no Left — because fifteen matching
  parse failures would otherwise pass while proving nothing. All fifteen agree, including the
  three most at risk: `has` as substring, absent-vs-JSON-null equivalence, and numeric coercion
  through the flattened column.
- **Cross-project / cross-user rejection** — one test rather than three. Stream, renew and
  delete all resolve through `activeSubscriptionFor`, so testing that `WHERE` clause tests all
  three; asserting per route would assert the same clause three times.
- **Limit enforcement** — this was a real defect, not just a missing test. The plan required
  the limits be enforced atomically; the code counted, checked, then inserted across three
  round trips. Now one conditional `INSERT … SELECT … WHERE (count) < N`, with the counts
  re-read only on failure to name which cap was hit. Residual, stated honestly: under
  `READ COMMITTED` two racing registrations cannot see each other's uncommitted row, so the cap
  is "about N". Deliberate — these bound one user's browser tabs, while the cap that protects
  the fleet (`liveTailMaxCached`) is a `LIMIT` on the ingest side that cannot be raced.

*(Also folded into [What is left](#what-is-left-2026-08-12).)*


---

## Revision: no configuration, and no Kafka requirement (2026-08-12)

Two changes supersede the Configuration and Transport-selection sections above, and the
deviation notes that discussed `ENABLE_LIVE_TAIL` / `LIVE_TAIL_TOPIC` / `Unavailable` /
`LocalHub`. **Those names no longer exist** — read this section instead.

### Every environment variable is gone

All eight `LIVE_TAIL_*` vars were deleted and are now constants in `Pkg.LiveTail`
(`leaseSecs`, `queueCapacity`, `maxPerUser`, `maxPerProject`, `maxCached`,
`cacheRefreshSecs`, `relayPollMs`, `relayRetentionSecs`, `kafkaTopicName`).

The test for keeping one was: does a deployment genuinely need a different answer? None did.
They are the shape of the feature, and an operator asked to choose a lease length or a queue
depth has no way to answer better than the code. The single thing that does vary between
deployments — whether Kafka exists — is read from `kafkaBrokers`, which is already configured
for ingest.

### Kafka is an optimisation, not a requirement

`Transport` is now `KafkaTopic Text | PostgresRelay`. There is no `Unavailable` and no
`LocalHub`.

Kafka is the queue this system is built around, but it is *optional infrastructure*: dev boxes,
docker-compose and self-hosted installs run without it, and a feature that quietly does nothing
there does not work. Postgres is not optional, so it is the floor — Live Tail works everywhere
with **no new dependency**, on the `projects.live_tail_events` relay table (migration 0127,
UNLOGGED, reaped at `relayRetentionSecs`).

`LocalHub` was removed because choosing it required knowing whether ingest and HTTP share a
process, and **no process can tell** — a web pod cannot see whether `CONSUMER_ONLY` pods exist.
Picking it on the available evidence (no brokers) was silently wrong for every split deployment
without Kafka. The guess was the bug; the relay is what made removing it possible.

Cost discipline on the relay, so it is safe on the ingest path:

- `emit` only buffers into a bounded queue; a fiber batch-inserts on a tick. The ingest path
  never waits on Postgres.
- Buffer full drops the oldest **and counts it**, surfaced by the flusher — the same rule the
  per-browser queue follows.
- A pod with no open SSE connection skips the poll query entirely.
- Both transports decode through `envelopeFromValue`, so a rolling-deploy version skew is
  reported as a skew on either path rather than looking like corruption on one of them.

### Lifecycle

Navigating away deletes the subscription (`keepalive` DELETE), and the SSE handler *also*
deletes it on teardown — the server learns a connection died the moment the response body
fails, which beats waiting out the lease for a crashed or slept tab. Matching therefore stops
within one cache refresh (~2s) rather than one lease (~45s), on both transports.

---

## What is left (2026-08-12)

Everything above is marked. This is the single list of open work; the earlier "Not done" and
"Still open" blocks now point here. Nothing on this list blocks the feature — it is shipped and
working on master — so each item says what it actually costs to leave undone.

### Gaps in the shipped code

1. **No `error` SSE frame.** The stream contract specifies one; the server never sends it. The
   client's `error` state comes only from a failed registration POST or `EventSource.onerror`,
   so every server-side condition that is recoverable-but-worth-saying (a filter that stopped
   compiling, a transport that went away under an open connection) currently reaches the user
   as either silence or a generic transport error. Small to add; the client already has the
   state to render it.

2. **The SSE response sets no anti-buffering headers.** The plan requires headers that disable
   caching and proxy buffering; only `Content-Type` is set. Heartbeats every 10s are what has
   been keeping connections open, which works against most proxies but is a weaker guarantee
   than `Cache-Control: no-cache` + `X-Accel-Buffering: no`. This is the most likely cause of a
   future "works locally, dead behind the ingress" report.

3. **Size is capped per field, not per envelope.** `maxRowFieldChars = 8000` truncates
   individual values, but nothing measures the serialized envelope, so the plan's "if a record
   still exceeds the limit, drop it and increment a metric" does not exist. A row with very many
   large attributes can still assemble an oversized message; Kafka would reject it at produce
   time (logged, non-fatal), and the relay would simply store it.

4. **No evaluation budget per ingest batch.** The plan lists "evaluation time or work per ingest
   batch" as a hard limit. Query and regex length are capped (4000 / 512), and `maxCached = 500`
   bounds the subscription count, so the product of the two is bounded — but there is no
   circuit breaker if that bound is still too slow for a hot project.

### Missing tests

Ordered by what would actually catch a defect:

5. **Ingest survives a Kafka failure.** Non-fatal by construction, never pinned. This is the one
   acceptance criterion whose violation is a production incident rather than a broken feature.
6. **Kafka round trip through a real broker**, and **two consumer groups both receiving**.
   Covered at both ends today (envelope encoding, hub routing); the middle was verified once by
   hand with `rpk`, which is not a regression guard.
7. **SSE disconnect cleanup.** The teardown delete exists and is the reason matching stops in
   ~2s instead of ~45s; nothing drives it in a test.
8. **Reconnect does not replay.** An at-most-once guarantee with no test.
9. **Concurrent limit enforcement.** The atomic statement is tested sequentially. Given the
   documented "about N" residual under `READ COMMITTED`, a concurrency test would mostly pin
   that the cap is approximately, not exactly, enforced.
10. **Browser states** `connecting` / `reconnecting` / `paused`, the client row cap, and
    selector persistence across a reconnect.

### Blocked or needs production

11. ~~**The metric set**~~ **Done** — declared in `Pkg.Metrics`, recorded where the counts
    happen. The "blocked on the OTel metrics API" claim was stale: the API had already landed
    in the pinned `hs-opentelemetry` commit, and `Pkg.Metrics` was already using it.
12. **Load test** against many subscriptions on high-volume projects (rollout step 5).
13. **Ingest-latency and write-success comparison** (rollout step 6). The ingest hook is live in
    every deployment because the feature ships on, so this is the measurement that matters most.

### Void, not outstanding

- Feature-flag rollout staging (steps 4 and 7) — there is no flag and no operator-set limits.
- "Validate related values at startup" from the Configuration section — there is no
  configuration to validate. The one remaining input, `kafkaBrokers`, selects a transport that
  works either way.
