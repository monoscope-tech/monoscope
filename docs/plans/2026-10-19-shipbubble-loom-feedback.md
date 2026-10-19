# Shipbubble Loom feedback: live tail, search, Databases tab

Source: customer Loom (2026-10-19), https://www.loom.com/share/aa6f8fc523ee409b87ff5fa7a30085e6.
Project: Shipbubble `28f62f01-46a1-400e-8195-da7bc3505b5b` (service `production-shipbubble-api`,
MySQL `ER_LOCK_DEADLOCK` issues exist for it). They have not confirmed the project. All prod
checks below were read-only.

| # | Symptom | Root cause (verified) | Sev | Repo |
|---|---|---|---|---|
| 1 | Live tail freezes the tab and the site; view stuck at 14:17 | MB-sized log bodies bypass the 8,000-char cap through `summary`; unbatched merge plus virtualizer remount on every SSE message | P1 | monoscope |
| 2 | Searching `deadlock` shows nothing / "not found" | No free-text search: a bare word is parsed as a field name → `Unknown field "deadlock"` | P1 | monoscope (+TF) |
| 3 | Databases → MySQL shows 0% errors today | 17 errors / 1,380,705 spans = 0.0012%, then `ROUND(…, 2)` = `0.00`. The same queries are also instrumented twice (`mysql` and `mysql2`) | P2 | monoscope |
| 4 | Errors counted but the error itself can't be seen | Nothing on the Databases tab shows `status_message` or exception text | P2 | monoscope |
| 5 | Clicking a query only highlights it; no way to copy | Every table row gets `hover cursor-pointer`, even with no `on_row_click`; no copy action; SQL truncates the text (`LEFT(…, 300/2000)`) | P3 | monoscope |
| ? | "…the usage as well" | Unclear from the recording. Ask the customer | — | — |

## Status (branch `fix/live-tail-freeze-kafka-retention`)

| # | Done | Regression test |
|---|---|---|
| 1 | Live-tail values are truncated inside lists and objects. A drain is split into SSE frames of at most 100 rows. The heartbeat is now a `ping` event, and the client reconnects after 25 s of silence. Pushed rows are merged once per animation frame. The list no longer remounts at the retention cap. Summary elements render capped at 8,000 chars. | `LiveTailSpec` `liveTail_summaryCarriesFullBody_isTruncated`, `liveTail_largeDrain_splitsIntoBoundedFrames`; vitest `liveTail_atRetentionCap_doesNotRemountPerMessage`, `liveTail_burstOfFrames_mergesOncePerAnimationFrame`, `liveStream_silentAfterPing_reconnectsInsteadOfFreezing`, `summary_multiMegabyteBody_rendersCapped` |
| 2 | A standalone quoted string (`"deadlock"`) is free text over `body`, `status_message` and `name`, in both the explorer and live tail. A bare unknown word gets a "quote it" hint. A gateway 504 is reported as a timeout. The KQL docs and the skill (branch `fix/kql-free-text-search` in monoscope-skills) now say `has` is a substring match. | `LogSpec` `freeTextSearch_findsSpanStatusMessageAndLogBody`; `LiveTailSpec` `liveTail_freeTextQuery_matchesLikeTheExplorer`; doctests in `Expr.hs`/`Stats.hs`; vitest `logWorker_gatewayTimeout_saysTimedOutNotParseError` |
| 3–5 | Databases tab: errors are shown as counts instead of rounded percentages. A new "Recent Database Errors" table links each error to its spans. Query and error cells can be copied. Rows that do nothing on click no longer look clickable. | `DashboardWidgetsSpec` `databasesTab_rareErrors_areCountedAndReadable`, copyable/hover render test |
| 6 | Kafka `live_tail` retention, below. | operational; verified on prod |

**TF correction:** `origin/master` already routes an OR across indexed text columns (`routed_tree_cases`: "both routed branches must union"). Prod numbers, Shipbubble, same 3-column OR:
- yesterday (indexed): 9.5 s
- 6 h of today: 38 s

The cost comes from `timefusion_tantivy_backfill_skip_today`: today's files are never indexed. The TF follow-up is therefore "index today's sealed files". That is an open CPU trade-off already listed in TF's long-window plan, not a routing change. The ngram3 indexes on `attributes___exception___message` and `db.query.text` are still open.

## Evidence (prod, 2026-10-19)

- **Volume**: Shipbubble wrote 8,720 rows and **42.7 MB of `body` in one minute**; the largest body in
  that minute was **6.2 MB**. Unfiltered live tail streams all of it.
- **Live tail on the demo project** (Playwright, 60s): stable. DOM 3.3–3.6k nodes, 3 long tasks ≤126 ms.
  So the freeze depends on volume and body size; it can't be reproduced on demo.
- **HTTP/2** is negotiated at `app.monoscope.tech`, so the browser's six-connection limit is ruled out.
  "Other tab hangs too" fits Chrome putting same-site tabs in one renderer process, which the frozen tab blocks.
- **Search**: `/log_explorer/validate`, demo project:
  - `deadlock` → `Unknown field "deadlock"`
  - `"deadlock"` → `Syntax error at column 1`
  - `body has "deadlock"` → valid
- **Where the deadlocks are** (TF, 24h): both are `kind=client`, `status_code=ERROR`, and the text
  is in **`status_message`** ("Deadlock found when trying to get lock; try restarting transaction").
  One is `db.system.name = mysql`, the other `mysql2`. The text is also in one `kind=log` row's `body`.
  So `body has "deadlock"` alone misses both spans.
- **Search cost on TF** (24h, Shipbubble):
  - `body::text ~* 'deadlock'`: 4.6 s
  - `attributes::text ~* 'deadlock'`: 20.8 s (never routed to tantivy)
  - `status_message ilike '%deadlock%'`: 1.5 s
- **Search OR cost**: TF does not route an OR across different indexed columns. Measured over 6h:
  - `status_message ilike` alone: 1.2 s
  - `body ILIKE OR status_message ILIKE OR name ILIKE`: **27.6 s**
  - The same OR over 1h: 5.7 s, with or without `exception.message`. The fallback is a raw scan either way.
  - `body ILIKE` type-checks on TF.
- **Databases tab**, 24h, spans with `duration`:
  - `mysql`: 1,380,705 spans, 17 errors
  - `mysql2`: 1,317,161 spans, 17 errors
  - In a 10-minute sample, all 6,772 traces with `mysql2` spans also have `mysql` spans, and both kinds are
    children of the HTTP server span. So it is two instrumentations of the same queries, not two databases.

## 1. Live tail freeze (P1)

Two entry points share `web-components/src/live-stream.ts` and the SSE endpoint:
- **A**: the "Stream new events" toggle in the Explorer (`log-list.ts`). This is the likely one in the video.
- **B**: the Live Tail tab (`live-tail.ts`).

Root causes, in order of impact:
1. **Bodies bypass the cap.** `toTableCols.truncateValue` (`src/Pkg/LiveTail.hs:999`) truncates only
   top-level `AE.String`. `summary` is a *list*, and its body element passes the raw body through
   (`src/Models/Telemetry/Telemetry.hs:2635`, `AE.String t -> Just t`). `summary` is a default column.
   Rows up to 900 KB (`maxEnvelopeBytes`) are sent in full. Larger rows are dropped and only counted
   server-side, so the UI's "N dropped" never learns of them.
2. **One SSE frame per drain, with no size bound.** `batchSTM` (`LiveTail.hs:1176`) drains up to 500 rows
   into one `log` event. The browser `JSON.parse`s it on the main thread (`live-stream.ts:113`).
   While the tab is frozen the queue refills, so the next frame is just as big.
3. **Path A does all its work per message.** `handleLiveRows` (`log-list.ts:791`) runs
   `groupSpans` + `mergeInTimeOrder` + `updateVisibleItems` over up to 2,500 rows on every message.
   Path B already batches per animation frame (`live-tail.ts:411`).
4. **Remount storm at the retention cap.** Once the list reaches `MAX_RETAINED_ROWS`, every merge
   bumps `virtualizerEpoch` (`log-list.ts:2276`). That value is in the `keyed()` key (`:2915`), so on a busy
   project every SSE message tears down and rebuilds the virtualizer. The same mechanism was fixed for
   history paging ([[log-list-scroll-jump-and-remount-storm]]); live-at-cap was never covered.
5. **Render cost grows with body size.** `renderSummaryElements` hashes every character of every
   summary per render (`log-list.ts:2992`) and puts the whole body in `title` (`:3053`). Its caches
   are bounded by entry count, not bytes.
6. **Stuck at "14:17" with no signal.** When the main thread starves, the stream stops advancing and
   nothing tells the user. Silent loss violates "never silently drop".

Fix steps (failing test first for each):
- [ ] **Cap what the stream sends.** Make `truncateValue` recurse into arrays and objects. Fix it there and
      not in the ingest-time summary builder: `summary` is stored and indexed, and capping it at ingest
      would change storage and still leave old rows uncapped. Whether 6 MB bodies should be truncated at
      ingest is a separate product decision ([[shipbubble-fat-log-bodies]]). The normal list is uncapped
      too (`Parser.hs:380` selects `to_jsonb(summary)` whole). It stays responsive because it fetches
      pages and parses them in a worker. Test: `LiveTailSpec`
      `liveTail_summaryCarriesFullBody_isTruncated`. Ingest a 2 MB string body, assert each
      projected value ≤ `maxRowFieldChars`. Fails today.
- [ ] **Report oversized drops.** Count dropped oversized rows into the client-visible `dropped` notice. Extend
      the existing oversized test at `LiveTailSpec:273`.
- [ ] **Bound frame size.** Have `rowsThen` split a drain into frames of ≤256 KB (or ≤50 rows). Test: a
      500-row drain arrives as several frames, each under the bound.
- [ ] **Batch Path A.** Buffer `handleLiveRows` per animation frame, as Path B does. Do the regrouping in the
      existing log worker. Test (`live-tail-in-list.test.ts`): 200 messages in one frame cause one merge.
- [ ] **Trim without remounting.** Trim at the cap without bumping `virtualizerEpoch` while the reader is at
      the live edge. Test (`log-list-scroll-position.test.ts`): at the cap, N live messages cause 0 remounts.
- [ ] **Cheaper summary rendering.** Key the summary caches on row id, not content. Truncate before tokenizing and
      before `title`.
- [ ] **Show a stalled stream.** If no frame or heartbeat arrives within 2× the heartbeat interval, show
      "stream paused, reconnecting" and reconnect.
- [ ] **Verify.** Replay a Shipbubble-shaped load (8–9k rows/min, 40 MB/min of body, some 1–6 MB bodies)
      on the e2e server (`scripts/e2e.sh`), never on :8080, which ingests into prod. Alternatively, mock the
      EventSource in the vitest harness. Use Playwright. Record long tasks, heap and remounts for 5 minutes on the toggle and on
      the tab. Pass: no long task over 200 ms, heap flat, the view keeps advancing.

## 2. Free-text search (P1)

- **Parser.** `shared/src/Pkg/Parser/Expr.hs:507` turns a standalone word into `isnotempty(<field>)`.
  Field validation then rejects it (`Stats.hs:1619`). A quoted string alone is a syntax error.
  The hint "wrap as `body has "x"`" exists only in the API/CLI path (`Log.hs:720`).
- **Wrong column even when phrased correctly.** On spans the text is in `status_message`, or in
  exception events (`events`), or in `attributes.exception.message`. It is not in `body`.

Fix steps:
- [ ] **Free-text term in the parser.** Two possible rules for a standalone term:
      - **(a) Recommended: only a quoted string is free text.** A bare word that isn't a field keeps erroring,
        but the message becomes `Unknown field "deadlock". To search text, use "deadlock"`.
        Typos like `statuscde` stay loud.
      - **(b) Any unknown bare word is free text.** This matches the customer's other tool, but every field
        typo silently becomes a substring search.

      Free text compiles to an OR of ILIKE literals: `body ILIKE '%t%' OR status_message ILIKE '%t%'
      OR name ILIKE '%t%'`. ILIKE literals route to tantivy; `::text ~*` on Variants does not.
      `attributes___exception___message` is added only after the TF index for it is deployed.
      Doctest in `Expr.hs`: `"deadlock"` parses to `FreeText`. A known field still gives `isnotempty`.
      Escaping of `%`, `_` and `'` is pinned.
- [ ] **TF: route an OR of indexed text predicates.** This blocks fast free text: the OR measured
      27.6 s over 6h, against 1.2 s for its fastest branch. Rewrite each routable branch to its own
      `text_match` and union the id sets. The rule today is that every branch must route to *the same*
      column. Until this ships, monoscope can run one query per column and merge the ids. Take that
      stopgap only if the TF fix slips.
- [ ] **Integration test.** `LogExplorerSpec` `freeTextSearch_findsSpanStatusMessage`: ingest a client
      span with an ERROR status and status_message "Deadlock found…", plus a log with "deadlock" in its
      body. Query `deadlock` and expect both rows. Fails today with `Unknown field`.
- [ ] **Free text and live tail.** Live tail evaluates queries itself (`Pkg.LiveTail` resolver), so
      `FreeText` needs an evaluator arm too. Otherwise the same query works in search but never matches live.
- [ ] **Docs.** `kql-reference` (monoscope-skills `SKILL.md:29,260`) says `has` is whole-word; the code does
      substring. Fix the wording and add a free-text section. Also update `docs/kql-reference.md`.
- [ ] **Timeouts.** TF's statement cap is 90 s and the gateway's is 60 s, so a slow search ends as a 504 with
      "Network error". The target is a ~45 s limit so the user sees "Query timed out".
      First confirm TF honours a session `SET statement_timeout` that *lowers* the cap
      (`pgwire_batch_statement_secs = 0` blocks raising it). If it doesn't, use a 45 s client-side fetch
      deadline instead.
- [ ] **TimeFusion schema.** Add ngram3 indexes on `attributes___exception___message` and
      `attributes___db___query___text`. Consider `events` with `flatten: json`. Allow `;`, `:` and `(` in routed
      literals by splitting into word conjunctions, so pasting a full MySQL error message doesn't fall back
      to a raw scan.
- [ ] **TimeFusion deployment.** First confirm which commit prod TF runs. The local TF checkout is 75 commits
      behind `origin/master`, and its staged changes appear to delete #405 (body text routing) work.
      Coordinate with whoever owns that tree; don't fork a parallel fix.
- [ ] **Verify.** On Shipbubble (24h), `"deadlock"` returns the 2 spans and 1 log. Expect under 5 s once the
      TF OR routing lands; until then, check a 1h window only.

## 3. Databases tab shows 0% errors (P2)

The tab is YAML: `static/public/dashboards/_overview.yaml:798-1099`, rendered by `Pkg/Components/Widget.hs`.

1. **Rounding.** Every error-rate cell is `ROUND(... * 100.0 / count, 2)`. The stat tile also uses
   `summarize_by: mean` over per-bin percentages. 17 errors in 1.38M rounds to `0.00`.
2. **Double instrumentation.** The same queries arrive as `mysql` (1.38M spans) and `mysql2` (1.32M),
   both under the HTTP span in the same traces, with identical error counts. It is one database
   instrumented twice on the customer's side (e.g. two OTel MySQL instrumentations enabled).

Fix steps:
- [ ] **Error counts.** Add an `errors` count column to "Query Optimization Targets", "Database Health",
      "Database Systems" and "Queries by Operation Type". A count never rounds to zero. Render rates
      under 0.01% as `<0.01%`, not `0.00`.
- [ ] **Stat tile.** Make the "Query Error Rate" tile compute `sum(errors)/sum(count)` over the window, not the
      mean of per-bin percentages.
- [ ] **Don't merge `mysql` and `mysql2`.** An `IN ('mysql','mysql2')` would double every count. Leave both options
      in the dropdown. Tell the customer in the reply that two instrumentations are active and one can be
      disabled.
- [ ] **Test.** `DashboardSpec` `databasesTab_rareErrors_notRoundedToZero`: 1 ERROR span among 100k
      mysql spans gives errors=1 and a displayed rate other than `0.00`.

## 4. Show the database error itself (P2)

- [ ] **"Recent Database Errors" table.** Add it under "Database Errors" (`_overview.yaml:1080`): group by
      `COALESCE(NULLIF(status_message,''), attributes___exception___message)` and the query pattern.
      Show count and last_seen.
- [ ] **Link to the Explorer.** Each row's existing column `link` opens the Explorer filtered to
      `attributes.db.system.name in ("mysql","mysql2") and status_code == "ERROR" and status_message == "<msg>"`.
      This needs `tableLink` (`Widget.hs:1721`) to substitute any row field, as `rowValue` (`:1613`)
      already does, with KQL quote escaping.
- [ ] **Test.** The widget returns the deadlock message, and the link's query parses and finds the span.

## 5. Copy / clickable query rows (P3)

- [ ] **No false affordance.** In `renderTableWithDataAndParams` (`Widget.hs:1581-1622`), add `hover cursor-pointer` only when
      `on_row_click` is set.
- [ ] **Copy button.** Turn the unused `TableColumn.clickable` field (`Widget.hs:440`) into `copyable`. Such a cell
      renders the existing `copyButton_` (`Pages/Components.hs:1067`), revealed on hover. Set it on the
      `query_pattern` columns (`_overview.yaml:858`, `:1025`). Don't add a new clipboard helper.
- [ ] **Full query text.** Truncation in SQL (`LEFT(…, 300)` at `:1065`, `LEFT(…, 2000)` at `:895`) means copy gets a cut-off query.
      Carry the full text in a hidden field for the copy source, or raise the limit and truncate
      only at render.
- [ ] **Row click.** Clicking a query row opens the Explorer filtered to that query pattern. Use `on_row_click`,
      matching on whichever field is shown (`db.query.summary` or `db.query.text`).

## 6. Kafka `live_tail` held rows for days after their tails closed

- **Cause.** The topic had `retention.ms=10min` and `retention.bytes=64MiB`, but `segment.ms` was the broker default of 14 days. Its `segment.bytes` (128 MiB) was also larger than `retention.bytes`. Retention only deletes *closed* segments, so nothing was ever deleted. On 2026-10-19 the oldest row was 9.3 days old (about 18 MB). The broker auto-creates topics (`auto_create_topics_enabled=true`), so a new environment gets the defaults again.
- **Fixed on prod at 07:33 UTC.** `rpk topic alter-config live_tail --set segment.ms=600000 --set segment.bytes=16777216` (incremental, so the retention settings are unchanged). About 35 min later every partition showed `LOG-START-OFFSET == HIGH-WATERMARK`, with 0 bytes on disk.
- **Automatic from here.** Redpanda now deletes rows at most about 20 min old: `log_segment_ms_min` is 10 min, plus 10 min of retention.
- **The config is now code.** `scripts/kafka-topics.sh` (`make kafka-topics`) is idempotent: it creates the topic or applies `alter-config --set`. `make ship` runs it best-effort after each deploy, and a failure only warns. `kafkaTopicName`'s comment points to it.
- **Production stops for dead tails.** Ingest matches only leases with `expires_at > now()`. The cache refreshes every 2 s, the lease is 45 s, and the SSE teardown deletes the lease at once. So rows for a closed tab stop within about 2 s, or 45 s if the DELETE is lost.
- **Consumer groups don't pile up.** Each web process has one `mono_live_tail_<nonce>` group (3 live), and the groups never commit offsets.
- **Not done.** Monoscope does not enforce the topic config itself at startup. hw-kafka-client (5.3.0, latest) has no admin API, so this would need our own librdkafka `IncrementalAlterConfigs` FFI. Plain `AlterConfigs` would *replace* all dynamic config and wipe retention.

## Rollout

Each item is a separate PR. Before pushing each one: run `/hs-distill`, `/hs-evasion-review`,
`/hs-lob-review` and `/simplify`, then `make ci-signoff`.

1. PR-1 live tail (items 1a–1c first: server-side cap and frame bound alone stop the freeze).
2. PR-2 free-text search (monoscope). In parallel, a TF PR for OR routing and the extra indexes, after deployment state is confirmed.
3. PR-3 Databases tab (items 3–5 together; all YAML + `Widget.hs`).
4. Reply to the customer:
   - Workaround until PR-2 ships: `status_message has "deadlock"`. It finds the span errors in about 1 s.
     Use `body has "deadlock"` for log lines.
   - Tell them about the double MySQL instrumentation.
   - Ask what "usage" referred to.
