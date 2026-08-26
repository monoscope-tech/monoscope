# Billing pricing v2 — three metered dimensions

Branch `ws-billing`. Migration number reserved: **0136**.

Target pricing:

| Dimension | Rate | Counting source today |
|---|---|---|
| Events (spans + logs) | $1 / 1,000,000 | `Telemetry.getUsageTotals` → `otel_logs_and_spans` count. **Submitted today.** |
| Metric datapoints | $1 / 10,000,000 | `Telemetry.getUsageTotals` → `otel_metrics` count; stored in `apis.daily_usage.total_metrics`. **Recorded, never submitted** (`src/BackgroundJobs.hs:503`). |
| Session replays (RUM) | $1 / 1,000 | **Nothing counts them today.** See below. |

---

## 1. Research findings

### 1.1 Where session replays live

There *is* a per-session Postgres row: **`projects.replay_sessions`**
(`static/migrations/0029_replay_sessions.sql`, extended by 0030 / 0063 / 0068 / 0106 / 0107).

- `session_id uuid` — **globally UNIQUE** (`idx_replay_sessions_session_id`), so
  `count(*) == count(DISTINCT session_id)`.
- `project_id uuid`, `created_at timestamptz DEFAULT now()`, `last_event_at timestamptz`.
- One row per *session*, not per chunk: ingestion upserts
  `ON CONFLICT (session_id) DO UPDATE` (`src/Pages/Replay.hs:698-711`), so `created_at`
  is written once, on the session's first chunk, and never mutated afterwards.
- rrweb payloads go to S3/R2; only this index row is in Postgres. Replay data is **not**
  in TimeFusion (`src/Models/Apis/LogQueries.hs:766-768` says so explicitly).
- Nothing in the codebase counts or aggregates this table today — the only reader is an
  existence lookup for the `hasReplay` flag (`LogQueries.hs:771`).

**Counting query** (billing counts sessions *started* in the window):

```sql
SELECT count(*)::bigint FROM projects.replay_sessions
WHERE project_id = $1 AND created_at > $2 AND created_at <= $3
```

`created_at` (immutable) rather than `last_event_at` (bumped by late chunks, so a window's
value could change after we billed it). `>`/`<=` matches the half-open convention
`getUsageTotals` already uses for events and metrics, so a session cannot be billed twice
across adjacent windows.

Caveats that shaped the design:

1. **30-day retention deletes rows.** `expireOldReplayData` (`src/Pages/Replay.hs:1198-1240`,
   `replayRetentionDays = 30`) DELETEs sessions with `last_event_at < now() - 30d`, but only
   for projects with `s3_bucket IS NULL`. So the count is only reliable inside the retention
   window. ReportUsage runs daily, so this is fine in practice — but the count **must be
   snapshotted** into `apis.daily_usage` at report time, which is what we do. Never
   recompute a historical window from this table.
2. There is no composite `(project_id, created_at)` index. Migration 0136 adds one.
3. Oversized/undecodable payloads never create a row, so we bill only successfully stored
   replays. Correct behaviour.

### 1.2 The subscription-item id problem

`projects.projects.first_sub_item_id` is singular. What it means differs per provider:

**Stripe does not need it at all.** `reportUsageToStripe` (`src/BackgroundJobs.hs:2772`)
POSTs `/v1/billing/meter_events` with `event_name` + `payload[stripe_customer_id]`. The
meter is addressed by **customer + meter event name**. Three Stripe meters = three
`event_name` strings against the same customer. No schema change needed for Stripe.

Note a latent oddity: `ReportUsage` gates the *entire* job on `first_sub_item_id` being
non-empty (`src/BackgroundJobs.hs:488`) even though the Stripe branch never reads `fSubId`.
Left as-is (it is a real "billing is configured" proxy), but the gate now lives per-meter.

**LemonSqueezy does need one per meter.** `reportUsageToLemonsqueezy`
(`src/BackgroundJobs.hs:2834`) POSTs `/v1/usage-records` with
`relationships["subscription-item"].data.id`. A usage record is addressed *only* by
subscription item, so three metered dimensions require three subscription items — i.e.
three metered variants on the LS subscription. Today the LS checkout is a single hosted
URL for a single variant (`src/Pages/Settings.hs:1209`), and the webhook only ever reads
`data.attributes.first_subscription_item` (`src/Pages/Settings.hs:1099`, `:1126`), so there
is exactly one item and nowhere to put a second id.

Where `first_sub_item_id` is written:

| Site | Provider | Source value |
|---|---|---|
| `Settings.hs:1099` (`upgradeToPaid`) | LS webhook | `first_subscription_item.id` |
| `Settings.hs:1126` (`updateProjectBilling`) | LS `subscription_created` | same |
| `Projects.hs:1405` (`updateProjectPricing`) | LS checkout callback | `firstSubscriptionItem.id` |
| `Settings.hs:1604` (`updateStripeProjectBilling`) | Stripe `checkout.session.completed` | `getStripeSubDetails` → `items.data[0].id` |
| `Settings.hs:1640` (`updateSubItemIdBySubId`) | Stripe `subscription.updated` | `items.data[0].id` |
| `Settings.hs:1670` (`setPlanBySubId`) | Stripe `subscription.resumed` | `items.data[0].id` |

(Stripe checkout already creates **two** line items — base + metered overage,
`Settings.hs:1477-1503` — and we only ever store item 0. Another reason not to hang
multi-meter addressing off this column.)

**Design:** a new table `projects.billing_meter_items (project_id, meter_kind, sub_item_id)`.
`first_sub_item_id` stays exactly as-is and remains the fallback for the `Events` meter, so
no webhook or checkout code changes and no behaviour change for existing subscriptions. A
LS project only becomes billable for metrics/replays once someone inserts rows for those
meters — which is precisely the dormancy we want.

### 1.3 Pricing copy that states the old single-dimension price

App (all events-only, "$1 per 1M"):

- `src/Pages/Settings.hs:1399` — "First 20M requests are included in the $29 plan price. Overage is $1 per 1M requests."
- `src/Pages/Settings.hs:1221-1226`, `:1285`, `:1310` — `basePriceNum` 29/199, `overageCost = n / 1_000_000`, 20M included threshold.
- `src/Pages/Components.hs:186-197` — pricing-slider JS: `29 + ((value - 20_000_000)/1_000_000)`, `199 + ((value - 100_000_000)/1_000_000)`.
- `src/Pages/Components.hs:289-321` — plan cards ($0 / $29 / $199, "10K events per day").
- `src/Pages/Onboarding.hs:310` — renders the same `paymentPlanPicker`.
- `src/Pkg/EmailTemplates.hs:1030-1047` — free-tier email; quantities dynamic, no hardcoded price.

Landing (`/Users/tonyalaribe/Projects/apitoolkit/apitoolkit-landing`):

- `pricing/index.md:8, 10, 18, 53-58, 66, 68, 70, 98-102, 110, 112, 179-186`
- `index.md:612, 614, 616, 645`
- `assets/js/main.js:8, 11, 55-62, 90-101`

**Out of scope for this stream.** Copy changes must land together with the provider-side
meters and prices being created, and the landing repo is a separate deploy. This document
is the inventory; nothing here is edited. The backend is built so that turning the meters
on is a config change, at which point the copy has to change in the same breath.

---

## 2. Design

### 2.1 `MeterKind` — one typed dimension, no stringly meter names

In `Models.Projects.Projects`:

```haskell
data MeterKind = Events | MetricDatapoints | SessionReplays
  deriving via WrappedEnumSC 'Nothing "" MeterKind
```

giving `events` / `metric_datapoints` / `session_replays` for JSON, DB, env vars and the
`CHECK` constraint alike — one spelling, derived, impossible to drift.

- `meterQuantity :: MeterKind -> UsageTotals -> Int` — the only place a dimension maps to a count.
- `stripeMeterEventName :: MeterKind -> Text` — `events_usage` (unchanged for
  backwards-compatibility with the live Stripe meter), `metric_datapoints_usage`,
  `session_replays_usage`.

`UsageTotals` gains `replays :: Int`.

### 2.2 Meter-kind dimension on chunking and submissions

```haskell
data UsageChunk = UsageChunk { meter :: MeterKind, quantity :: ChunkQuantity }
splitUsageIntoChunks :: MeterKind -> Int -> [UsageChunk]
```

`UsageSubmission` gains `meter :: MeterKind`. `ChunkQuantity`'s 900k cap is a LemonSqueezy
POST limit, not a pricing limit, so it applies unchanged to all three meters. Rates
($1/1M, $1/10M, $1/1k) live provider-side; we submit raw counts.

`splitUsageIntoChunks` is *extended*, not forked — the existing arity-1 call site becomes
`splitUsageIntoChunks Events`.

### 2.3 Dormancy — typed, not a buried boolean

A meter must not submit until it is confirmed to exist provider-side.

```haskell
data MeterTarget = StripeMeter Text Text | LemonSqueezyMeter Text   -- customer+event / sub-item
data DormantReason = MeterNotEnabled | NoStripeCustomer | NoSubscriptionItem | ProviderUnusable
resolveMeterTarget :: [MeterKind] -> BillingProvider -> ProjectMeterConfig -> MeterKind
                   -> Either DormantReason MeterTarget
```

`resolveMeterTarget` is a pure total function — the single decision point for
"may this meter submit, and where to". `EnvConfig.enabledUsageMeters :: [MeterKind]`
**Superseded 2026-08-27: the enable list was removed.** Metering is on by default and the only gate is addressability — a Stripe meter with no price attached bills nothing, which puts the off-switch provider-side. A LS project
additionally has to have a `billing_meter_items` row, so enabling the config alone cannot
POST to a meter that does not exist.

**Disabled and unaddressable are different things, and must not be conflated:**

`meterIsDormant :: DormantReason -> Bool` is the single predicate:

| Reason | Dormant? | Chunks cut? | Why |
|---|---|---|---|
| `MeterNotEnabled` | yes | **No** | Policy: this dimension does not bill yet. Buffering months of chunks and draining them the day the meter goes live is exactly the backlog-leak failure we have already hit on a provider switch. Enabling a meter bills from that day forward; the dormant period is reconciled by hand off `apis.daily_usage` if it is owed at all. |
| `NoSubscriptionItem` | yes | **No** | A Lemon Squeezy customer whose subscription has no metered variant for this dimension never agreed to that price. Cutting chunks would accrue failed rows forever against a product that does not exist. This is what keeps LS metrics/replays dormant even with the config switch on. |
| `NoStripeCustomer` | no | **Yes** | The project *is* on the plan; we simply cannot address it. Misconfig on money we are owed. |
| `ProviderUnusable` | no | **Yes** | Same. |

Cut chunks whose meter is unaddressable are marked `failed` by the drain, preserving the
pre-existing invariant that a paid project with an unusable provider leaves an auditable
failed row rather than a quietly-submitted one.

At drain time the same predicate applies: a chunk whose meter has since gone *dormant* is
left pending untouched (marking it `failed` would re-log the same row every tick forever),
while a misconfig reason marks it `failed` as before.

Totals for every dimension land in `apis.daily_usage` regardless, so a dormant window stays
reconcilable. `DormantReason` is logged per meter per run, so "why is nothing being billed"
is answerable from logs alone. `Events` resolving dormant on a paid project logs at
`logAttention` — a meter that silently stops billing is a revenue-off switch,
which is the shape of the five-week zero-usage incident.

### 2.4 Schema — migration `0136_billing_meters.sql`

Additive only:

1. `projects.usage_report_submissions` + `meter_kind text NOT NULL DEFAULT 'events'`, with a
   CHECK matching the three `MeterKind` spellings. The default backfills every existing row
   correctly — they were all events.
2. `apis.daily_usage` + `total_replays bigint NOT NULL DEFAULT 0`.
3. `projects.billing_meter_items (project_id, meter_kind, sub_item_id)`, PK
   `(project_id, meter_kind)`, FK to `projects.projects` ON DELETE CASCADE.
4. `idx_replay_sessions_project_created ON projects.replay_sessions (project_id, created_at)`.

`apis.daily_usage.total_requests` keeps its historical meaning (events + metrics) so
`getTotalUsage` and the Settings billing page are unchanged. Replays are **not** added to it
— they are priced 1000x differently and folding them in would corrupt the existing
"$1 per 1M requests" estimate. (That `total_requests` already includes metrics, and so
overstates the events-priced estimate on the Settings page, is a pre-existing issue noted
but not touched here.)

### 2.5 ReportUsage flow

1. `Telemetry.getUsageTotals` returns `Projects.UsageTotals` (was a 4-tuple) and now also
   counts replays from Postgres. Events/metrics keep following `enableTimefusionReads`;
   replays are Postgres-only because that is where the table is.
2. For each `MeterKind` in `[minBound ..]`: resolve the target. `Left reason` → log, no
   chunks. `Right target` → `splitUsageIntoChunks kind (meterQuantity kind totals)`.
3. `recordUsageWindow` writes `usage_last_reported`, one `apis.daily_usage` row, and the
   chunks for all live meters — in one transaction, before any HTTP.
4. Drain `pendingUsageSubmissions`: each row carries its `meter`, resolved to a target and
   POSTed. The existing per-row `tryAny` / mark-succeeded / mark-failed / double-submit
   logging is unchanged.

Exhaustive `case` on `MeterKind` everywhere — a fourth dimension will not compile until
every site handles it.

---

## 3. Tests

`test/integration/BackgroundJobs/ReportUsageSpec.hs` (the job already had a spec — extended,
not forked), driving the real `ReportUsage` job:

- three kinds recorded → chunked → drained → `submitted`, one Stripe meter each
- dormancy: metrics/replays off by default → totals in `daily_usage`, zero submission rows,
  and enabling them later does not bill the dormant window
- enabled-but-unaddressable → chunk still cut and marked `failed`
- LS with no `billing_meter_items` row → dormant even when config-enabled; billing resumes
  once the item is recorded
- a disabled meter's unsubmitted chunk is left untouched at drain, and drains on re-enable
- a succeeded chunk is never re-sent
- replay-only window (no events, no metrics) is recorded and billed
- replay counting: window boundaries `(wStart, wEnd]`, no double-count across windows

Provider POSTs move onto the `W.HTTP` effect and the spec runs them under a recording
interpreter (`runTestBgRecordingHTTP`). Under the default golden interpreter a cache miss
makes a **real** request to Stripe/Lemon Squeezy with whatever key the config carries, so a
billing test could not otherwise reach the `submitted` branch without hitting the network.

Plus doctests for `splitUsageIntoChunks`, `meterQuantity`, `stripeMeterEventName` and
`resolveMeterTarget`.
