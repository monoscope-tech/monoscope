# Monoscope and TimeFusion incident sweep, September 6, 2026

This journal contains historical observations. Read the dated checkpoints in order; later observations supersede earlier status and instructions. Session IDs and temporary mitigations describe the state at the recorded time.

Status recorded at September 7, 07:47 UTC: investigation remains active. Twenty-one software repairs have completed deployment or release. All four missing parquet paths are repaired; the latest post-deployment audit found all3,515 active objects present with zero duplicate paths. TimeFusion PR213 is deployed and verified: unchanged query results in3.31s versus33.95s, with dedup input20.36M→21.27K. All six monitors are normal after the transient MemBuffer alert cleared. Monoscope PR524 passed full CI and deployed as `e1f7e71`; deployment34096775206 succeeded, all three replicas healthy. Existing error/log vectors have reached2098/4000. A production reconciliation cursor stuck behind expired Delta history is reproduced; its conservative metadata recovery and periodic cursor maintenance are under test in an isolated worktree.

## Repair status recorded at September 7, 07:47 UTC

| Repair | Delivery | Verification or remaining check |
|---|---|---|
| Monitor queries and aggregate evaluation | PR 507 deployed; six monitor definitions corrected | All six active, unmuted, and normal in the latest snapshot |
| Embedding API URL | PR 508 deployed | URL 404 removed; small log-pattern batches saved vectors |
| Numeric JSON values in TimeFusion | PR 210 deployed as `f373d18` | Production returns JSON numbers; rollout soak had 43 probes and no failures |
| CLI explicit time windows | PR 510 merged; v0.6.26 released and installed | Published checksum verified; historical response bounds match the request |
| Scalar and grouped summary tables | PR 512 deployed | Production CLI retains numeric values and group names |
| Oversized embedding inputs | PR 511 deployed | Synthetic provider regression passes; production error vectors now persist incrementally (PR523), with temporary rate limits addressed in PR524 |
| Oversized Hasql diagnostic logs | PR 513 deployed | Regression preserves SQL diagnostics while omitting serialized parameter values |
| CLI severity shorthand | PR 514 deployed; v0.6.27 released and installed | 55 doctests and 16 tests pass; lowercase and uppercase input match the explicit production query |
| Project-ID-only queued ingestion | PR 515 deployment completed | Failed before; all 16 write-failure integration tests pass after, including persisted-row verification |
| `DEALLOCATE ALL` | TimeFusion PR 211 deployed as `25c2cce` | TCP and live SQL failures reproduced; live cleanup now succeeds, connection remains usable; 1,399 local tests and all CI checks pass |
| Project-less mobile navigation | PR 516 deployed as `bc91e908` | Projects-page regression passes before/after checks and full CI |
| Severity schema and documentation | PR 517 deployed as `d66d0f4` | All 118 schema doctests pass after review corrected uppercase level examples |
| CLI shorthand with pipelines | PR 518 deployed as `6f91d5b`; v0.6.27 released | 61 doctests, 16 CLI tests, published checksum and installed production count7 verified |
| Dashboard variable project context | PR 519 deployed as `c18eece` | Failed request regression now passes; all 931 frontend tests and production build pass |
| Initial error normalization performance | PR 520 deployed as `71caca9` | 647 doctests and 300 unit tests pass; real large inputs exposed additional quadratic copies addressed in522 |
| DV-aware maintenance freshness | TimeFusion PR 212 deployed as `5c30dfa` | Stale-DV corruption reproduced; all1,463 nextest tests, full lint and CI pass; live SQL healthy; all four missing paths reconstructed and179 duplicate identities relocated with exact byte/DV preservation |
| Disabled background worker isolation | PR 521 deployed as `e3d8c8e` on all3 replicas | Before/after queued-job regression,3 scheduler integration tests, lint and full CI pass |
| Remaining normalization suffix copies | PR 522 deployed as `131d24fd6`; deploy34082810456 succeeded | Full library build,650 doctests,300 units,6 benchmarks pass; real inputs23s/12s to0.64s/0.49s with byte-identical output |

| Incremental embedding durability | PR523 deployed as `43f2de56b` |303 units and CI pass; production error vectors500→1249, log vectors2000→3500 |
| DV-safe early scan filtering | TimeFusion PR213 deployed as `34f6e89`; deployment34093718832 succeeded |1,464 TF tests and113 Delta scan tests pass; live exact result21264/765504 preserved,33.95s→3.31s; dedup20.36M→21.27K input rows |
| Temporary embedding rate limits | PR524 deployed as `e1f7e71`; deployment34096775206 succeeded |12 HTTP regressions,303 app units and full CI pass; live retry verification pending |
| Broken demo Prometheus target | Exact demo-target configuration disabled; backup retained |Disabled state and unchanged07:13:04 last-scraped time independently verified at07:18:37 (interval60s) |

The detailed inventory and chronological notes below retain initial observations. Their initial hypotheses are superseded by the checkpoints and delivery status above.

## Initial scope and evidence

Initial window: September 1–6 UTC. Project: `87576849-4941-49d3-a15d-680fef88a1a8`.
Fetched four 100-row issue pages, stopping once a whole page predates September 1.
211 records updated in the window: 52 runtime exceptions, 11 log patterns,
148 API changes. Updated time is not occurrence time; the detail table below
records the latter. All 63 exception/pattern details were retrieved successfully.
Raw evidence is local at `/tmp/incident-sept6` (not committed because logs may
contain customer payloads). API-change titles are catalogued separately below;
these are discovery notifications, not automatically defects.

SSH verified Monoscope 3/3 replicas at image `4fb7aa0b86d3dcd1c2c6742d8538e255170b7a91`
and TimeFusion 1/1 at `a88933c`, running about eight hours. Local TimeFusion master
is `a1486730` and contains newer DV-dedup work, so local code is not deployment proof.
Host: `ubuntu@captain.s.past3.tech`. Docker service log captures cover the last 24h.

## Active failures found outside the issue list

1. **Ingestion dead letters.** Logs at 20:00 UTC report 2,991 messages parked in
   90 minutes, backlog 11,448,482; 5,910 DLQ entries in that window. Repeated
   `processList` messages say traces resolved to zero rows. Do not replay the
   backlog until representative payloads establish why project resolution fails.
   Code lead: `processBatchPipeline` skips both project-key resolution and project
   cache fetching when API keys are empty, even when `at-project-id` is present.
   This is an unverified production-cause hypothesis; reproduce and sample first.
2. **Broken TimeFusion monitors.** WAL `46b57972-3650-4a1b-9b7b-7de826a9cd2c`,
   MemBuffer `745d60c4-b89b-4c2b-9e3e-d22f9407bd64`, unsorted flush
   `fb7d109f-2197-490f-9613-112e06e29494` repeatedly fail KQL validation:
   unknown field `metric_value.contents.value`. The WAL monitor misleadingly
   remains normal with value zero. Fetch correct metrics schema, repair stored
   queries, verify actual evaluation; also examine error-status handling.
3. **Embedding jobs fail.** Production `OPENAI_BASE_URL=https://api.openai.com/`.
   `embeddingConfig` passes it verbatim; Docker logs show `//embeddings` and HTTP
   404. Default in code is `https://api.openai.com/v1`. Confirm library URL joining,
   repair configuration durably, and verify successful embedding jobs.
4. **Prometheus scrape target.** `d93ae374-4f17-49f4-8358-c65503c14a63` repeatedly
   requests `http://localhost:1/metrics`. Inspect stored target ownership/history;
   likely a test artifact but not yet proven.
5. **TimeFusion SQL session cleanup.** 132 `DEALLOCATE ALL` failures in captured
   logs: prepared statement `all` does not exist. Reproduce against pgwire and fix
   correct session-wide deallocation semantics, including quoted named statements.
6. **TimeFusion maintenance.** Repeated 900-second dedup coordinator timeouts and
   batch-probe timeouts across late-August/September partitions. Compare deployed
   code with newer DV-dedup work before selecting and validating a fix.
7. **Rollup fallback warnings.** Frequent unmatched residual-filter/count-measure
   warnings and service graph plan errors need distinct classification and query
   reproduction. A fallback warning alone does not prove wrong results.

## Prior work to verify, not redo blindly

`scripts/local/cross-service-issue-sweep-2026-09-04.md` records the sort-memory
investigation, FairSpill tests and fix. Verify ancestry and runtime configuration
of current production. It also identifies missing `sv` and
`attributes___telemetry___sdk___language` as ad-hoc invalid probes with supporting
SQL, and records successful reproduction of the exemplars query after recovery.

## Exception and log-pattern inventory

All entries remain under investigation unless evidence above says otherwise.
Counts for log patterns are lifetime counts, not counts within this window.

| ID | Service | Last occurrence | Count | Title |
|---|---|---|---:|---|
| 3f83033c-26ad-469b-bc95-61dbdaf87c29 | timefusion-dev | 2026-09-06T19:44:01.316181234Z | 1 | UUID SQL Type Unsupported in timefusion-dev PgWire Layer |
| cb06a35c-d070-4d68-a8f0-a987e1c1f4ae | timefusion-dev | 2026-09-06T17:24:31.115295167Z | 1 | New Error: Error - PgWire internal error: Invalid or Unsupported Configuration: could not find conf |
| 993cb9b8-7456-4d06-8493-5a908b41b323 | monoscope-ui | 2026-09-06T13:43:28.278009545Z | 1 | Data Fetch Fails with 400 Variable Options Request in Monoscope UI |
| 1f2f1d09-2c35-4499-843d-87540c2d3c97 | monoscope-ui | 2026-09-06T12:01:57.400547065Z | 1 | Session Expiration Prevents Data Fetches in monoscope-ui |
| 53edfe8a-4bcb-4366-b52d-9e01e08bdd5e | timefusion-dev | 2026-09-06T09:47:49.136950579Z | 1 | Missing ingested_at Field Causes PgWire Schema Error in timefusion-dev |
| eefd025a-cee4-4a28-9478-ac5704f849c7 | timefusion-dev | 2026-09-06T04:49:52.99849173Z | 1 | PgWire Invalid Batch Column Error in timefusion-dev |
| cc82c750-047d-4516-bb19-83084e43d122 | monoscope-ui | 2026-09-05T22:35:32.463949181Z | 1 | Data Fetch Fails with Widget Request Error 520 in monoscope-ui |
| 77a33f13-e46a-4fa6-9672-8fee2a858619 | monoscope-ui | 2026-09-05T22:35:26.034583586Z | 1 | 401 Variable Options Request Fails in Monoscope UI |
| 4cfe8e00-17ab-4e36-9102-c2d2d89d7626 | timefusion-dev | 2026-09-05T19:52:06.010808427Z | 1 | Query Planning Error: Ungrouped SELECT Column in timefusion-dev |
| 4de91601-c407-4f86-a0d3-c339f3f0ef00 | monoscope-dev | not supplied | 2512 | New runBackground ERROR Log Pattern Detected in monoscope-dev |
| f89d1c19-38c7-45f4-8da0-6c65f2cd8f48 | timefusion | 2026-09-05T18:29:31.631584581Z | 1 | Planning Error: Duplicate Expression Names in Timefusion PgWire Query |
| 210ad813-2a0c-46fc-84e5-d4776de13488 | monoscope-ui | 2026-09-05T18:18:46.550893165Z | 1 | Query Execution Failure in Monoscope UI Data Fetch |
| 64552251-b67c-4e98-846a-8fed26b31478 | monoscope-ui | 2026-09-05T17:28:36.425302407Z | 1 | AbortError: Aborted Signal in Monoscope UI |
| 172cb5ec-1f38-453a-ba7c-ff2694753411 | monoscope-ui | 2026-09-05T16:19:47.667449017Z | 1 | 400 Variable Options Request Failure in Monoscope UI |
| c293d180-53f1-4cc7-910f-d6f63ab389f9 | monoscope-dev | not supplied | 7 | New PostgreSQL Query Errors in Monoscope Dev Tracing Service |
| 2d27e770-9b74-4e82-9457-d8a5904d2939 | monoscope-dev | not supplied | 28594 | New Database Error Pattern: Failed Queries in Monoscope Dev Service |
| f5bfb461-5814-4275-a23b-4fd76d57f6ef | timefusion | 2026-09-04T18:08:49.603895126Z | 1 | New Error: Error - PgWire internal error: Schema error: No field named __delta_rs_file_id__. Valid  |
| 68a17301-c791-4481-8214-9d2f37c1ad3d | monoscope-dev | 2026-09-04T12:20:32.043473678Z | 1 | New Error: XX000 - XX000: Resources exhausted: Additional allocation failed for ExternalSorterMerge |
| f4a58877-d322-47ab-a3ad-47c84c7a64d1 | monoscope-ui | 2026-09-04T10:34:04.863640378Z | 1 | New Error: TypeError - Error fetching data for endpointHash: Cannot read properties of undefined (readi |
| 70122943-b5b4-4de9-97f2-68c8b33268d7 | monoscope-dev | 2026-09-04T10:01:12.54588748Z | 1 | New Error: Error - SqlError {sqlState = "XX000", sqlExecStatus = FatalError, sqlErrorMsg = "Resourc |
| 343aa779-58d6-48ad-82b1-f2b8405cdc47 | monoscope-dev | 2026-09-04T10:00:21.46942856Z | 1 | Critical: SQL Query Fails from Insufficient Memory in Monoscope Dev |
| 46091817-b342-4257-854a-60c25fa4b7d5 | monoscope-dev | 2026-09-04T10:00:01.959692917Z | 1 | New Error: Error - SqlError {sqlState = "XX000", sqlExecStatus = FatalError, sqlErrorMsg = "Not eno |
| bc7a5efc-be47-40aa-8d14-50d7c8fb94d6 | monoscope-dev | 2026-09-04T09:59:41.605217828Z | 1 | Critical: Fatal SQL Error from Insufficient Memory in Monoscope-Dev |
| ead84afb-6230-43b0-a43d-02feb1162fd1 | monoscope-dev | 2026-09-04T09:59:26.456361245Z | 1 | New Error: Error - SqlError {sqlState = "XX000", sqlExecStatus = FatalError, sqlErrorMsg = "Not eno |
| 2e861aaf-5d05-4882-ac2a-5470d3968a44 | monoscope-dev | 2026-09-04T09:58:55.515340415Z | 1 | Critical: SQL Resources Exhausted in Monoscope Dev Service |
| 6067ab56-5c36-4e74-8261-562599dc00a4 | monoscope-dev | 2026-09-04T09:58:46.303402093Z | 1 | Critical: Fatal SQL Error from Insufficient Memory in Monoscope Dev |
| ecba0c0e-3ae1-4a80-97da-06c001c81eeb | timefusion | 2026-08-31T22:42:42.580586986Z | 1 | Schema Error: Missing Field sv in Timefusion PgWire Query |
| 6fa75bf1-700c-4dd5-9688-9b327d9cb026 | timefusion | 2026-09-01T20:29:41.20489636Z | 1 | Schema Error: Missing attributes___telemetry___sdk___language Field in Timefusion |
| 3b70d4aa-f27c-45e2-af8d-46412526e201 | monoscope-ui | 2026-09-03T19:41:11.488545188Z | 1 | AbortError: HTMX Request Signal Aborted in Monoscope UI |
| b761a67f-bc8e-49d8-9308-551bb8f04d8a | monoscope-ui | 2026-09-03T19:39:20.05616097Z | 1 | Unknown HTMX Swap Style "morph" in Monoscope UI |
| 5aa24466-6ca8-4a37-a79a-34331cf984b9 | monoscope-ui | 2026-09-03T19:39:11.654216942Z | 1 | Runtime Error: Null #sidenav-toggle Element in Monoscope UI |
| e1766a3a-2584-45f7-b96c-a14945dfacdd | monoscope-ui | 2026-09-03T17:28:10.69081318Z | 1 | AbortError: User Aborted Request in monoscope-ui |
| af252cac-b3d7-4950-a34f-c6985eac9e5a | monoscope-ui | 2026-09-03T13:04:04.07090741Z | 1 | TypeError: Service Worker Registration Fails in Monoscope UI |
| 70b113f1-e89f-4e4d-bfd3-6652872f706a | monoscope-ui | 2026-09-03T10:20:10.410707851Z | 1 | Scroll Restore Fails After Disconnection in Monoscope UI |
| 8c72aa62-3043-4429-950a-d31ddb818382 | monoscope-ui | 2026-09-03T08:08:01.402823827Z | 1 | Asset Load Failure for roma-echarts JavaScript in monoscope-ui |
| 2f3bcd63-734c-4d32-b9d5-0d589124c2f7 | monoscope-dev | not supplied | 906 | New PostgreSQL Query Error Pattern Detected in monoscope-dev |
| 2bbd6b43-ef59-4629-ba66-a94a3e3d5d16 | monoscope-dev | not supplied | 6 | New ERROR Log Pattern in runBackground Span on monoscope-dev |
| ebbb7fc3-332c-442b-80a4-24772fed4e25 | monoscope-dev | not supplied | 8790 | New Log Pattern: kind database db.system postgresql db.query.text;text-textStrong⇒SELECT users."id", users."created_a |
| fca52256-1caf-4c86-a5ca-9088f6461429 | monoscope-dev | not supplied | 8790 | New Log Pattern: kind database db.system postgresql db.query.text;text-textStrong⇒ insert into users.persistent_sessi |
| b750151f-f194-454b-97b6-5766031307ab | monoscope-dev | not supplied | 28305 | New Log Pattern: kind database db.system postgresql db.query.text;text-textStrong⇒WITH bucket_digests AS (SELECT extr |
| c815aa2e-fe92-4628-9a1a-c081b3e8d7c1 | monoscope-dev | not supplied | 1813 | New Log Pattern: kind internal span_name containersGetH status ERROR status;right-badge-error⇒ERROR duration;right-ba |
| 28295f89-99ff-4a37-94ab-b51b5e79f692 | monoscope-dev | not supplied | 9934 | New Log Pattern: kind database db.system postgresql db.query.text;text-textStrong⇒ WITH latest AS ( SELECT CASE WHEN  |
| c616a98b-84ae-456e-a94d-54a331aa326f | monoscope-ui | 2026-09-02T15:39:46.527555643Z | 1 | New Error: TypeError - Failed to fetch dynamically imported module: https://app.monoscope.tech/public/a |
| da05fdb0-fca2-4b6a-846d-7709c02b4561 | monoscope-ui | 2026-09-02T15:39:40.351668156Z | 1 | New Error: TypeError - Failed to fetch dynamically imported module: https://app.monoscope.tech/public/a |
| bb1e2dea-44a1-459c-93d6-8b1d88812098 | timefusion | 2026-09-02T11:17:56.610886198Z | 1 | New Error: Error - PgWire internal error: Resources exhausted: Additional allocation failed for Ext |
| 44c1577a-8157-4bd6-8e36-2cd7ebc991da | monoscope-dev | 2026-09-02T09:42:47.665339239Z | 1 | Critical: Resource Exhaustion in monoscope-dev External Sorter Merge |
| 2a5a7043-234f-4973-9fa9-0306fce16eef | monoscope-ui | 2026-09-02T00:44:45.008151761Z | 1 | TypeError: Service Worker Registration Fails in Monoscope UI |
| 49818a8b-c902-41a0-93eb-1162193d6a29 | monoscope-dev | 2026-09-01T23:06:21.670660656Z | 1 | Driver Cleanup Failure Leaves Transaction Status Unknown in Monoscope-dev |
| f30f8098-da53-4bc8-aa12-c67f0d59bf20 | monoscope-dev | 2026-09-01T23:06:27.166295039Z | 1 | Hasql Session Cleanup Failure After Transaction Interruption in Monoscope Dev |
| b52362cc-e189-4652-a0e2-dbd067035819 | monoscope-ui | 2026-09-01T21:59:05.902694227Z | 1 | New Error: Error - Failed to fetch new data: widget request failed: 403 Forbidden |
| e1897ac6-4881-4c5a-a1b6-26b73e6b013e | monoscope-dev | 2026-09-01T21:43:18.551858803Z | 1 | New Error: HasqlException - Hasql session error: Server error
  totalStatements: 1
  statementIndex: 0
  sql |
| 62ffdca2-723c-4302-acb9-c2c22ba20404 | monoscope-dev | 2026-09-01T21:11:54.871197319Z | 1 | New Error: SomeAsyncException - Effectful.Error.Static.ErrorWrapper: ServerError {errHTTPCode = 400, errReasonPh |
| 6bef3528-78ff-46fc-a1a8-10670b3bc11b | timefusion | 2026-09-01T19:30:14.528564875Z | 1 | New Error: Error - Error while loading searcher after commit was detected. OpenReadError(FileDoesNo |
| da75f2d8-9473-453b-8bac-62758a32320c | monoscope-ui | 2026-09-01T18:27:38.548528643Z | 1 | TypeError Illegal Constructor in Monoscope UI |
| 0929a223-a6bb-4f67-a8f3-09071aa56de4 | monoscope-ui | 2026-09-01T18:27:32.604789909Z | 1 | TypeError: htmx.registerExtension Undefined in monoscope-ui |
| 7dae9cd9-84c8-4678-8b43-3d1699965a35 | timefusion | not supplied | 11298 | New Error Log Pattern in Timefusion Flush Completed Buckets(span) |
| 519f123c-bd0b-46e6-a214-26124c9aa8d9 | monoscope-ui | 2026-09-01T13:44:16.210471411Z | 1 | TypeError: monoscope-ui Fails Mapping Undefined Database Data |
| d38ca147-dc2a-4e51-b524-7620a09187a6 | monoscope-ui | 2026-09-01T13:44:35.337441458Z | 1 | TypeError: Undefined Value Read in monoscope-ui Data Fetching |
| f0b0a551-ed3c-4119-bfb5-eb8297f98765 | monoscope-ui | 2026-09-01T13:44:02.847367759Z | 1 | TypeError: Undefined Resource Data Causes .map Failure in monoscope-ui |
| f1df2199-6e86-418c-92b2-cf4ab7579198 | monoscope-dev | 2026-03-29T16:37:12.496966688Z | 2 | Runtime Error: Timeout Kills Async Thread in Monoscope-Dev Service |
| 736f9b26-ff65-4f6b-8351-969d4a8162ae | monoscope-ui | 2026-09-01T09:17:01.199116734Z | 1 | New Error: Error - Server error (504) |
| 5d6aa23b-bf13-46d8-870e-575e007ed4ae | monoscope-ui | 2026-09-01T08:32:54.543689523Z | 1 | New Error: Error - Failed to fetch new data: widget request failed: 500 Internal Server Error |
| 3bd99508-c256-4147-a199-d9470150856d | monoscope-ui | 2026-09-01T06:44:49.461808283Z | 1 | monoscope-ui Fails to Fetch New Data Due to Widget Request Timeout |

## API-change inventory

| ID | Updated UTC | Title |
|---|---|---|
| dee07edb-5a85-4ef8-bdaf-23cdba9cf59b | 2026-09-05T09:45:58.112295Z | New endpoint detected: GET /p/{param}/metrics/details/{param}/ on monoscope-dev |
| 37316687-a2a9-47b7-ad84-7414be527055 | 2026-09-05T09:45:58.112295Z | New endpoint detected: GET /p/{uuid}/metrics/details/aspnetcore.memory_pool.allocated/exemplars on monoscope-dev |
| e8f64e0a-8192-48ce-9761-7d402c3e5d6a | 2026-09-05T09:45:58.112295Z | New endpoint detected: GET /p/{uuid}/metrics/details/{param}/exemplars on monoscope-dev |
| 396d5fce-d0be-425e-a6f7-a2e355c3e4e4 | 2026-09-05T09:45:58.112295Z | New endpoint detected: GET /p/{uuid}/metrics/datapoints/counts on monoscope-dev |
| a3d9ea3e-f571-4f6c-84a3-a95888d58242 | 2026-09-05T09:45:58.112295Z | New endpoint detected: GET /p/{uuid}/metrics/details/app.cart.get_cart.latency/exemplars on monoscope-dev |
| 9f24132a-5aa7-4f6a-9003-53ea554dc444 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.Bxvd_2SP.js.map on monoscope-dev |
| 2418ef94-7cea-4b6d-aa82-ecc5d6a619a4 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.74JFH_Fb.js on monoscope-dev |
| 57cb9054-01a6-4e50-b938-47a1165f49c7 | 2026-09-03T13:00:11.705477Z | New GET Endpoint Added: Retrieve Dashboard Latency Percentiles PNG Image |
| 9bbcea30-bcb8-4407-aee9-11820dd1dbd8 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/query-editor-config.BKy_7E1b.js.map on monoscope-dev |
| c934ed74-9ac3-4005-a906-745288ae3c74 | 2026-09-03T13:00:11.705477Z | New GET Endpoint Added to P-Service for All Requests Dashboards PNG |
| d6f156b7-6847-45ae-8a25-c7b83b3160d4 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/live-stream.oFD_XLIq.js on monoscope-dev |
| f6c03f8d-0e9d-4ff1-9524-fa2411b6f19e | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/assets.zWAGh_7L.js.map on monoscope-dev |
| 1d1f8f3e-89c9-4ef8-98a1-50ab0128c294 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/dashboards/Errors%20(4xx%20&%205xx).png on monoscope |
| 1e27fba6-7b2b-43e4-b53a-2919cca7bded | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/dashboards/Time%20Spent%20by%20Downstream%20Operations.png on Unknown |
| 6b388bf0-8905-4707-8fa1-38531ed2603b | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.DKEHEW__.js on monoscope-dev |
| 454e6fc9-0b75-48ba-a848-a99fa1cfdb80 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/query-editor-config.BKy_7E1b.js on monoscope-dev |
| 128385ad-e2e3-4205-89e4-44e02919f7d5 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.B65Z_nzM.js on monoscope-dev |
| cd862e3d-1cab-47af-b47b-0d72af7dac47 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.C9Il_68g.js.map on monoscope-dev |
| e7606886-28ad-4a57-8eee-b53320757623 | 2026-09-03T13:00:11.705477Z | New p-service Endpoint Added: Download HTTP Requests by Status Code PNG |
| 1be51e07-9d40-42dd-b3ad-9cb309c3947b | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/index.AE_mqwJ0.js on monoscope-dev |
| c6dbc420-0619-4427-8abe-0c7ded83ed17 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.Bb_IAMLW.js on monoscope-dev |
| d9bccedf-6d5d-4672-b5b2-a950d7854a77 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/dashboards/Latency%20percentiles%20(ms).png on monoscope |
| 49dccefa-04fb-4c62-963e-e69c56d26717 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.DGPi_EKJ.js.map on monoscope-dev |
| e62a5322-11da-480d-baa3-567cb213e246 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.C56c_x8I.js on monoscope-dev |
| b0339fee-b2fd-40e5-bc0d-23b3442ad21f | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.wY_tDcW9.js on monoscope-dev |
| 8e409119-fdd1-4a8a-8e07-1fb639590003 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/live-tail.B-D_Doav.js on monoscope-dev |
| 62938f76-a69c-4c2f-8e26-798b162cc663 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/query-editor-config.FdJGJ_rB.js.map on monoscope-dev |
| 6ebe7a60-06f3-4ab2-877f-6236d1ff88f2 | 2026-09-03T13:00:11.705477Z | New p-service Endpoint Added: GET /p/{uuid}/dashboards/Downstream Dependencies.png |
| ef22a34d-febc-40fd-a016-36e08f2b3d05 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.C9Il_68g.js on monoscope-dev |
| cef448bc-7b30-4569-a6c1-8bde6b75c861 | 2026-09-03T13:00:11.705477Z | New p-service Endpoint Added: GET /p/{uuid}/dashboards HTTP Errors PNG Image |
| 2fd1ceb7-961e-4949-883e-92b22a753acc | 2026-09-03T13:00:11.705477Z | New `Database Health` Dashboard PNG Endpoint Added to p-service |
| 78aa5678-f6b8-4c02-8e3b-79cb51051ca1 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/dashboards/New%20Widget.png on monoscope |
| de51fb52-4695-41f5-818c-d42a7870d8e6 | 2026-09-03T13:00:11.705477Z | New p-service Endpoint Added: GET PNG Error Dashboards for UUID Resource |
| 7b0868cb-cef3-4a6c-9eba-245202acd3b6 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.Bb_IAMLW.js.map on monoscope-dev |
| 0c473230-2fbe-4240-968b-7f057d1c7ac1 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.E_ntgWkO.js.map on monoscope-dev |
| 2d20cb26-ea93-43f1-992c-72466ee04739 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.Bxvd_2SP.js on monoscope-dev |
| fa009fd6-83f6-4d86-a175-9dbf2231bd34 | 2026-09-03T13:00:11.705477Z | New P-Service Endpoint Added: Error Rates by Service PNG Dashboards |
| 46284a24-a5df-4c3f-aa53-a1681e5b9ff5 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.CO_SWGLE.js on monoscope-dev |
| d8d7f36b-ce6c-4865-b1e0-72d4cb696852 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.0Ht7_iBI.js on monoscope-dev |
| 74324025-3e53-4a15-b4cc-75892fe0627b | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.CXNk_ctm.js on monoscope-dev |
| f896f172-b68c-413a-a010-57b07f9c1106 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.BdrjJu_L.js on monoscope-dev |
| ef460f01-fe24-4e16-a636-47fa68677ee0 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/dashboards/Requests%20by%20Status%20Code.png on monoscope |
| 8415d3f2-f39c-4d8a-a1a7-e3c1a092ab4f | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.CYE0_VZK.js.map on monoscope-dev |
| 92ff262f-6efc-4cc8-b024-1e8178ee2099 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/dashboards/Status%20Code%20Breakdown.png on Unknown |
| 3f8ff738-fd3e-48c2-842b-522acb94157e | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/dashboards/Recent%20Traces.png on monoscope |
| e1eceb67-63a4-4fc6-8120-44fbc796ae87 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/service-map.C-7UBW7_.js on monoscope-dev |
| de61153a-7606-4e34-8bc5-d99000fb4b6c | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.fmKG_lnT.js on monoscope-dev |
| a82c01ab-57e9-46bc-9646-acd8b8fa17dc | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/dashboards/Status%20Code%20Breakdown.png on monoscope |
| 45177a35-318d-4c8c-9b67-e431e921d650 | 2026-09-03T13:00:11.705477Z | New Dashboards Retrieval Endpoint Added: GET /p/{uuid}/dashboards/ in P-Service |
| 23acfe61-7537-452e-895b-6762ef33ac40 | 2026-09-03T13:00:11.705477Z | New p-service Endpoint Detected: Get Request Latency Percentiles PNG Image |
| 6a573c8c-5886-4f7a-9a70-d71b8723a722 | 2026-09-03T13:00:11.705477Z | New P-Service Endpoint Added: Get Request Dashboards by Endpoint PNG |
| 76b4b292-36b0-4027-aaa6-4d99bc04a2df | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/index.lMehKbu_.js on monoscope-dev |
| 4af75774-df6e-4e99-907d-924931f99671 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/dashboards/Downstream%20Dependencies.png on monoscope |
| a271fb16-1734-4db0-8bff-40cad4c202ed | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/index.B_l3-N5p.js on monoscope-dev |
| 70ce5213-fbd1-401f-8243-e54dc360586a | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.74JFH_Fb.js.map on monoscope-dev |
| 41697e2e-29e8-412b-a39f-41d8f172b3a9 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.f8K_ja18.js.map on monoscope-dev |
| 3dbeb428-f3eb-4ccd-bc3b-10b7272d352b | 2026-09-03T13:00:11.705477Z | New p-service Dashboard Requests-By-Status PNG Download Endpoint Added |
| 00e1b2c8-8cd8-48be-ae97-6d577419449a | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.wY_tDcW9.js.map on monoscope-dev |
| 15e28727-6391-4cf5-a0a3-df5acc9dc13c | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/index.vA-_oPDC.js on monoscope-dev |
| 6e3a09c9-00dd-4d23-a9c3-9ac3e41e99c8 | 2026-09-03T13:00:11.705477Z | New p-service Endpoint Added: GET /p/{uuid}/dashboards/Services Health.png |
| 6c9c6131-6dc5-4d60-9ac2-d2ac19ddab2f | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/query-editor-config.FdJGJ_rB.js on monoscope-dev |
| c0243e0b-1917-4519-a1c9-6a59332cbcf9 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.CP_ko3bN.js.map on monoscope-dev |
| acbd4df3-ccc3-459a-9d5a-9d15ec3e5081 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.C_pv_3W8.js on monoscope-dev |
| a736f391-7219-4fcc-8efa-71a8828ac199 | 2026-09-03T13:00:11.705477Z | New p-service Dashboard Image Endpoint Added: HTTP Request Volume by Service |
| 5b34fc0e-7071-43d1-8887-1c6d8bc31427 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/dashboards/HTTP%20Requests%20by%20Status%20Code.png on monoscope |
| 956fcf3e-6747-4f7d-bf19-461a81097412 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.CkZ_2XHt.js on monoscope-dev |
| d241fd41-654f-4c69-bf80-2c1d44f6742c | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.E_ntgWkO.js on monoscope-dev |
| 62938f76-a69c-4c2f-8e26-798b162cc663 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/query-editor-config.FdJGJ_rB.js.map on monoscope-dev |
| e62a5322-11da-480d-baa3-567cb213e246 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.C56c_x8I.js on monoscope-dev |
| 9f24132a-5aa7-4f6a-9003-53ea554dc444 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.Bxvd_2SP.js.map on monoscope-dev |
| 7df6e76b-4176-4b9b-893f-274e7e6da575 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.0Tn_chon.js on monoscope-dev |
| 904dfc0c-6286-40b0-aa64-6558a1da175c | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.BFDEP_AF.js on monoscope-dev |
| acbd4df3-ccc3-459a-9d5a-9d15ec3e5081 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.C_pv_3W8.js on monoscope-dev |
| e08bb278-b467-4969-821d-c77bbaf9b978 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.D856e_GA.js on monoscope-dev |
| f6c03f8d-0e9d-4ff1-9524-fa2411b6f19e | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/assets.zWAGh_7L.js.map on monoscope-dev |
| 62a5570e-9994-4af9-b6bb-d7fd5fcddda4 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/index.CIe_hCCL.js on monoscope-dev |
| 95bf74e3-89d3-4f42-a926-5cc543637a35 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/query-builder.DU0P_4jF.js on monoscope-dev |
| c0243e0b-1917-4519-a1c9-6a59332cbcf9 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.CP_ko3bN.js.map on monoscope-dev |
| fa009fd6-83f6-4d86-a175-9dbf2231bd34 | 2026-09-03T13:00:11.705477Z | New P-Service Endpoint Added: Error Rates by Service PNG Dashboards |
| c6dbc420-0619-4427-8abe-0c7ded83ed17 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.Bb_IAMLW.js on monoscope-dev |
| d9bccedf-6d5d-4672-b5b2-a950d7854a77 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/dashboards/Latency%20percentiles%20(ms).png on monoscope |
| 1be51e07-9d40-42dd-b3ad-9cb309c3947b | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/index.AE_mqwJ0.js on monoscope-dev |
| 8badd713-687c-424f-9cfa-2bb017d79f77 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.DVM_pG9t.js on monoscope-dev |
| 00e1b2c8-8cd8-48be-ae97-6d577419449a | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.wY_tDcW9.js.map on monoscope-dev |
| f7fc4c70-ca27-48cf-bb80-4e6e588b8d13 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/metrics/details/app.cart.get_cart.latency/ on monoscope-dev |
| 13d220c2-4307-41cf-9321-6ff8cdf76b21 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/index.Bl_Cb5Bi.js on monoscope-dev |
| 1e27fba6-7b2b-43e4-b53a-2919cca7bded | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/dashboards/Time%20Spent%20by%20Downstream%20Operations.png on Unknown |
| f048da70-10d6-4d37-bcc6-34df2cc33ce1 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/dashboards/New%20Widget.png on monoscope |
| e7606886-28ad-4a57-8eee-b53320757623 | 2026-09-03T13:00:11.705477Z | New p-service Endpoint Added: Download HTTP Requests by Status Code PNG |
| d4bef02b-5e71-40f0-833e-dda0ec634afb | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/metrics/details/kafka.consumer_group.lag_sum/ on monoscope-dev |
| 8415d3f2-f39c-4d8a-a1a7-e3c1a092ab4f | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.CYE0_VZK.js.map on monoscope-dev |
| 23acfe61-7537-452e-895b-6762ef33ac40 | 2026-09-03T13:00:11.705477Z | New p-service Endpoint Detected: Get Request Latency Percentiles PNG Image |
| 4af75774-df6e-4e99-907d-924931f99671 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/dashboards/Downstream%20Dependencies.png on monoscope |
| 003f7271-f8e6-4430-a15f-906b6ddfb809 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.C_IfdGp0.js on monoscope-dev |
| 2418ef94-7cea-4b6d-aa82-ecc5d6a619a4 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.74JFH_Fb.js on monoscope-dev |
| 74324025-3e53-4a15-b4cc-75892fe0627b | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.CXNk_ctm.js on monoscope-dev |
| 57cb9054-01a6-4e50-b938-47a1165f49c7 | 2026-09-03T13:00:11.705477Z | New GET Endpoint Added: Retrieve Dashboard Latency Percentiles PNG Image |
| 41b503a4-e360-4e5b-a8e0-936116639701 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/metrics/details/otel.logs.log_processor.queue.limit/ on monoscope-dev |
| ac345b2b-8e99-4be9-ada1-b4b5cc966635 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.HoKu_H3f.js on monoscope-dev |
| eb9e7d6f-1b01-4536-b37b-6e3dd8982d42 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/metrics/details/k8s.container.memory_limit/ on monoscope-dev |
| a271fb16-1734-4db0-8bff-40cad4c202ed | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/index.B_l3-N5p.js on monoscope-dev |
| 2d20cb26-ea93-43f1-992c-72466ee04739 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.Bxvd_2SP.js on monoscope-dev |
| 45177a35-318d-4c8c-9b67-e431e921d650 | 2026-09-03T13:00:11.705477Z | New Dashboards Retrieval Endpoint Added: GET /p/{uuid}/dashboards/ in P-Service |
| d8d7f36b-ce6c-4865-b1e0-72d4cb696852 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.0Ht7_iBI.js on monoscope-dev |
| 518caba7-5ae8-4fe8-b4db-bae469a53a77 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/metrics/details/app.cart.add_item.latency/ on monoscope-dev |
| 0d71ebe3-8225-401a-96e6-d95e9709f1d0 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.CHs_nI2Q.js on monoscope-dev |
| 49dccefa-04fb-4c62-963e-e69c56d26717 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.DGPi_EKJ.js.map on monoscope-dev |
| 92ff262f-6efc-4cc8-b024-1e8178ee2099 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/dashboards/Status%20Code%20Breakdown.png on Unknown |
| 3f8ff738-fd3e-48c2-842b-522acb94157e | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /p/{uuid}/dashboards/Recent%20Traces.png on monoscope |
| a736f391-7219-4fcc-8efa-71a8828ac199 | 2026-09-03T13:00:11.705477Z | New p-service Dashboard Image Endpoint Added: HTTP Request Volume by Service |
| 454e6fc9-0b75-48ba-a848-a99fa1cfdb80 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/query-editor-config.BKy_7E1b.js on monoscope-dev |
| 46284a24-a5df-4c3f-aa53-a1681e5b9ff5 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.CO_SWGLE.js on monoscope-dev |
| cd862e3d-1cab-47af-b47b-0d72af7dac47 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.C9Il_68g.js.map on monoscope-dev |
| 0c473230-2fbe-4240-968b-7f057d1c7ac1 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.E_ntgWkO.js.map on monoscope-dev |
| c934ed74-9ac3-4005-a906-745288ae3c74 | 2026-09-03T13:00:11.705477Z | New GET Endpoint Added to P-Service for All Requests Dashboards PNG |
| d241fd41-654f-4c69-bf80-2c1d44f6742c | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/log-list.E_ntgWkO.js on monoscope-dev |
| 956fcf3e-6747-4f7d-bf19-461a81097412 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/widgets.CkZ_2XHt.js on monoscope-dev |
| d938d92b-1b27-4980-a90d-88c95b340834 | 2026-09-03T13:00:11.705477Z | New endpoint detected: GET /public/assets/web-components/dist/js/assets.zWAGh_7L.js on monoscope-dev |
| 2d9e5165-4221-48ba-af74-d2fbfe826c51 | 2026-09-02T16:46:34.710231Z | New endpoint detected: GET /api/v1/schema on monoscope-dev |
| b18c2011-370a-46d6-b573-1444800ce32d | 2026-09-02T16:46:34.710231Z | New endpoint detected: GET /p/{uuid}/manage_teams on monoscope-dev |
| 8d06cc47-4966-4faa-b855-0da33db8f45e | 2026-09-02T16:46:34.710231Z | New endpoint detected: GET /p/{uuid}/issues/{uuid_1}/unarchive on monoscope-dev |
| 545dfc6d-bbf7-4fe7-9800-eeb595c39020 | 2026-09-02T16:46:34.710231Z | New endpoint detected: GET /p/{uuid}/manage_subscription on monoscope-dev |
| 01b9f782-a473-42bb-af49-9ff3e5d8f63d | 2026-09-02T12:00:08.283444Z | New endpoint detected: DELETE /p/{uuid}/apis/{uuid_1} on monoscope-dev |
| 098b4991-448c-49fa-8146-3242c03a34c8 | 2026-09-02T12:00:08.283444Z | New endpoint detected: GET /api/v1/endpoints on monoscope-dev |
| 8aa49887-43e1-410e-a4f2-5dee8d65ad9e | 2026-09-02T12:00:08.283444Z | New endpoint detected: GET /api/v1/api_keys on monoscope-dev |
| f7daf81f-13e2-45a0-be0e-3ba98dccbd80 | 2026-09-02T12:00:08.283444Z | New endpoint detected: GET /api/v1/teams on monoscope-dev |
| 0e31d1b1-50ca-497c-a1fd-3b5cb7625e6e | 2026-09-02T12:00:08.283444Z | New endpoint detected: GET /api/v1/project on monoscope-dev |
| 5c9c26a3-f16b-4565-9bc8-a535e100bf9a | 2026-09-02T12:00:08.283444Z | New endpoint detected: GET /api/v1/log_patterns on monoscope-dev |
| ad4f58d6-3250-43b3-b86e-41ca4ba4595f | 2026-09-02T10:24:33.576877Z | New endpoint detected: POST /p/{uuid}/dashboards/{uuid_1}/duplicate on monoscope-dev |
| 184b031e-7c3a-4a1e-8a85-7effc3362a43 | 2026-09-02T10:24:33.576877Z | New endpoint detected: POST /p/{uuid}/apis on monoscope-dev |
| d9bafaf7-7d4e-450d-ab43-88a5e3134c50 | 2026-09-02T10:24:33.576877Z | New endpoint detected: GET /p/{uuid}/issues/{uuid_1}/acknowledge on monoscope-dev |
| 072663b1-7e7c-43e1-82d2-59b833edc919 | 2026-09-02T10:24:18.5263Z | New endpoint detected: POST /p/{uuid}/dashboards/{uuid_1}/star on monoscope-dev |
| 0d57542b-d4de-40b8-9392-a1e3d6fe84be | 2026-09-02T10:24:18.5263Z | New endpoint detected: PATCH /p/{uuid}/apis/{uuid_1} on monoscope-dev |
| a8bb8e5d-d0d1-4c0a-a679-00ce51ad942d | 2026-09-02T10:24:13.483974Z | New endpoint detected: DELETE /p/{uuid}/log_explorer/queries/{uuid_1} on monoscope-dev |
| bf95901a-32b0-42f4-9743-47570b315a9b | 2026-09-02T10:24:13.483974Z | New endpoint detected: POST /p/{uuid}/log_explorer/queries on monoscope-dev |
| df9fd6fb-1a84-4339-98b5-11725eccd3ce | 2026-09-02T10:23:13.077252Z | New endpoint detected: POST /p/{uuid}/monitors/alerts/{uuid_1}/toggle_active on monoscope-dev |
| 4c462598-b006-4ac7-a8dd-4e575bba229b | 2026-09-02T10:21:05.525996Z | New endpoint detected: GET /p/{uuid}/widget/sql-preview on monoscope-dev |
| 6d86a4da-e504-4e4d-947c-60eaa74f6de7 | 2026-09-02T10:21:05.525996Z | New endpoint detected: GET /p/{uuid}/widget/sql-text on monoscope-dev |
| 06927cab-531d-4e10-91ba-5ed869a2219d | 2026-09-01T21:12:58.784786Z | New endpoint detected: GET /p/{uuid}/metrics/card/app.cart.add_item.latency on monoscope-dev |
| 56d73c92-1a1f-4ea4-9195-dc815be1a035 | 2026-09-01T21:12:58.784786Z | New endpoint detected: GET /p/{uuid}/metrics/details/app.cart.add_item.latency on monoscope-dev |
| dbf914d1-bfa2-41ee-91dc-c3dfdcc715c6 | 2026-09-01T21:12:58.784786Z | New endpoint detected: GET /p/{uuid}/metrics/details/app.cart.add_item.latency/breakdown on monoscope-dev |
| f3b0c3e3-6ff1-4730-8799-e236fb3c8f7e | 2026-09-01T21:12:58.784786Z | New endpoint detected: GET /p/{uuid}/monitors/alerts/{uuid_1} on monoscope-dev |
| 5a1ccb92-2204-48cc-a8ce-03e6f6a3336e | 2026-09-01T19:15:06.456275Z | New endpoint detected: GET /p/{uuid}/metrics/details/app.shipping.items_count/exemplars on monoscope-dev |
| b0f5544b-6b42-402c-8a0e-b5733dcb4a1b | 2026-09-01T19:15:06.456275Z | New endpoint detected: GET /p/{uuid}/metrics/details/app.cart.add_item.latency/exemplars on monoscope-dev |
| 4c6e0dc1-596f-4497-a071-86cebf596c15 | 2026-09-01T17:07:46.852844Z | New endpoint detected: GET /p/{uuid}/metrics/details/app_recommendations_counter/exemplars on monoscope-dev |
| 13361aee-c858-4a15-9f21-1c02c32da3c5 | 2026-09-01T17:07:46.843222Z | New endpoint detected: GET /p/{uuid}/metrics/details/aspnetcore.memory_pool.rented/exemplars on monoscope-dev |
| c2faaa4b-d233-4c80-99e2-50c2de45c486 | 2026-09-01T17:07:46.837844Z | New endpoint detected: GET /p/{uuid}/metrics/details/app_ai_assistant_counter/exemplars on monoscope-dev |
| f306b387-4b35-47f3-8723-033e04185a28 | 2026-09-01T17:07:46.832733Z | New endpoint detected: GET /p/{uuid}/metrics/details/aspnetcore.routing.match_attempts/exemplars on monoscope-dev |

## Continuation checkpoint

This turn made progress by fetching the issue inventory and discovering active
production failures that change priorities. No blocker or approval is pending;
the user authorized implementation and deployment across both services.

Next action: inspect representative parked-message headers and project attributes
without printing payloads or API keys; reproduce the keyless `at-project-id`
cache path in `Opentelemetry.TimefusionWriteFailureSpec`. The current
`processBatchPipeline` guard at `src/Opentelemetry/OtlpServer.hs:482` ignores
`atIds` when deciding whether to load project caches. Preserve auth boundaries
and real empty-payload semantics when fixing it.

TimeFusion Docker capture completed successfully. Monoscope Docker capture is
still live as exec session `77855`, SSH PID 24533 (verified by process inspection);
poll this handle rather than starting another capture. Details fetch session
81385 completed: all 63 successful. Raw API error search used uppercase ERROR
but discovered facet values are lowercase `error`; the zero result is not a
health check. Schema field is `severity.severity_text`, not `severity.text`.

Production OPENAI_BASE_URL was inspected with secret filtering and confirmed
`https://api.openai.com/`. This config is used only by `embeddingConfig` in
current source. Changes to configuration should survive the next CapRover deploy.

`git fetch origin` completed; origin/master has work beyond local master. Avoid
rolling back those changes during deployment. Local monoscope started clean at
`d1344345b`; this catalog is the only modification from this turn. TimeFusion
started clean at `a1486730`. No AGENTS.md was found in these repo file listings.

## Follow-up: inactive-project payloads and incorrect monitor semantics

The previous turn was **progress**. This follow-up also made progress:

- Captured 20 recent parked records read-only with `rpk topic consume` (no consumer
  group, no offsets committed). Parsed raw protobuf locally rather than trusting
  UTF-8 rendering. The sampled spans carry a 48-character key and service `engine`.
  A read-only metadata lookup resolves that key to project
  `d062e010-e3d0-4673-8dfe-d652d6826f49`: key active, project inactive, neither
  deleted, plan GraduatedPricing. Thus this sample is correctly rejected by the
  active-project guard. Do not activate the project or replay these records.
  This does not classify the entire 11-million-record historical backlog.
- The independent keyless `at-project-id` hypothesis remains untested; it is NOT
  the explanation of these sampled production failures. `withinQuota` can also
  produce zero records and currently shares the unresolved-project error reason.
- Patched the three monitor queries through the authenticated API, replacing
  `metric_value.contents.value` with `value`. All three PATCH calls succeeded.
  Saved the complete resulting definitions to `observability/monitors/`.
  Query-only patches preserved destinations, thresholds, timing and identity.
- Validated corrected queries through `monoscope metrics query`: 30-minute WAL
  maximum 20,971,522,000 bytes; MemBuffer maximum 28%; unsorted-fallback maximum
  null (counter exists in source, may only emit on occurrence; verify this).
- **Monitors are NOT repaired yet.** Fetching the patched row proves compiled SQL
  still uses `GREATEST(count(*)::float8)` for `summarize max(value)`.
  `src/Pkg/Parser.hs:501` contains an explicit FIXME and hard-coded count.
  `evaluateQueryMonitor` also routes only SQL containing `otel_logs_and_spans`
  to TF; metrics remain on PG despite TF reads being enabled. The MemBuffer
  monitor evaluated at 20:50:06 UTC and still reports zero, versus the actual
  metric maximum 28. Fix both aggregate generation and source routing before
  claiming success. Consider missing aggregate values and HAVING semantics.

Added the regression `alert queries evaluate requested aggregates instead of
counting samples` to `test/unit/Pkg/ParserSpec.hs`: scalar, aliased and grouped
max(value) must preserve max and must not count rows or retain SQL aliases.
The pre-fix test command is live as **exec session 21150**:

```
cabal test unit-tests --ghc-options=-O0 --test-show-details=direct --test-options='--match requested'
```

Output: `/tmp/incident-sept6/monitor-aggregate-before.log`. At last poll it was
compiling module 60/196 of test-dev (cabal.project.local enables tests broadly).
It has not run the assertion yet. Poll the existing handle; do not start another
build. Production parser/evaluator code has not been edited, so this remains a
valid before-fix regression run. Existing helpers `colsNoAsClause` strip trailing
aliases safely (including preserving CAST AS). Use the existing parsed
`QueryComponents.fromTable` to route metrics/spans, rather than inferring source
from substring matches in generated SQL. Monitor decoding expects Double.

Raw local evidence/scripts (not for git): `parking-payload.bin`,
`parking-records.json`, `inspect-proto.py`, `check-parking-project.py`,
`*-monitor-patched.json`, `monitor-after-patch.log` under `/tmp/incident-sept6`.
The scripts read credentials without printing them. Rpk requires SCRAM-SHA-256;
unauthenticated invocation failed and was not evidence of a broker fault.

No code fix has been deployed. Next: finish failing regression run, implement
and test aggregate/source repair, review the Haskell diff with
haskell-constraint-review, integrate with current origin/master, deploy, and
verify actual monitor values and logs. Other catalogued issue families remain.

## Aggregate repair implementation checkpoint

This turn made **progress**, including a failing regression and code changes.
Pre-fix session 21150 finished with exactly 1 example / 1 failure. Failure text
shows `summarize max(value)` generated `select greatest(count(*)::float8)`.
Evidence: `/tmp/incident-sept6/monitor-aggregate-before.log`.

Implemented (not deployed yet):
- `src/Pkg/Parser.hs`: alert SQL now renders requested aggregations using existing
  `colsNoAsClause`, combines measures with GREATEST, casts the scalar to float8,
  and retains HAVING. Filter-only monitors still use count(*).
- `src/BackgroundJobs.hs`: both supported KQL sources (SSpans, SMetrics) follow
  enableTimefusionReads, with metricJsonAsVariant matching that backend. Removed
  SQL-text substring routing. Decode nullable aggregates as Maybe Double and
  use the existing no-data branch after catMaybes.
- Unit test covers scalar/aliased/grouped max(value), HAVING retention, and keeps
  the existing count-by-kind/time-window assertion aligned with float8 output.
- Integration test in `MonitoringSpec`: ingest two metric samples 28 and 80,
  evaluate max(value) against threshold 70, assert value 80 and alerting. This is
  a real ingestion/evaluation test, not evaluateQueryMonitorValue injection.
  It follows the test environment's configured telemetry backend; run under a
  real-TF integration configuration to prove the TF routing part.

The first post-fix run (session 91910) is TERMINAL FAILED, due to a verified
concurrent-build collision: rename BackgroundJobs.dyn_o.tmp failed. Other ghcid
processes are writing root dist-newstyle/test-dev. Do NOT repeat builds there.
Replacement validation is running in a separate build directory:

```
cabal test unit-tests --builddir=/tmp/incident-sept6/isolated-build --ghc-options=-O0 --test-show-details=direct --test-options='--match Pkg.Parser'
```

**Live exec session: 33918.** Log: `/tmp/incident-sept6/monitor-isolated.log`.
Poll this handle; it is a new run because the earlier process exited with a
concrete file-write collision, not because an observation timeout expired.
`git diff --check` passes. Haskell constraint review traced the parser's
QueryComponents/Sources, aggregate alias helper, evaluator and callers. No new
suppression, unsafe/partial function, or overloaded field. `[Maybe Double]`
represents SQL NULL explicitly rather than pretending missing max() is zero.
Full verification, formatter and integration run remain outstanding.

Separately reproduced `DEALLOCATE ALL` on production TF through psql: exit 1,
`Prepared statement 'all' does not exist`. DataFusion 54.1.0 context/mod.rs sends
all deallocation to remove_prepared(name); session_state.rs exposes that method
only crate-internally. TimeFusion has no DEALLOCATE interception. A correct fix
must actually clear both relevant prepared-statement stores and respect quoted
named statements; returning success without clearing them would be a hack.
No TF source has been changed by this sweep yet.

Embedding cause also corroborated from the pinned Langchain dependency:
`baseUrl <> "/embeddings"`, with library default https://api.openai.com/v1.
Saved CapRover machine `tf-captain` at ~/.config/configstore/caprover.json was
tried via the installed CLI's documented API/header shape. Server returns
status 1106, "Auth token corrupted" (NOT proven expired). No config changed.
The failed response has no app data. This access issue does not block code
repairs/deployment through existing CI, so the full goal remains active.

Next: poll 33918, fix any parser failures, run Monitoring integration tests on
isolated build with an actual test database/TF, integrate latest origin/master
without rolling back other sessions, deploy, confirm actual monitor values.
Other catalogued issues, including embedding configuration, remain in scope.

## PR and real-TimeFusion verification checkpoint (21:18 UTC)

This turn made **progress**. The initial target integration test passed (1/1),
then the entire Monitoring module passed (12/12) against real local TimeFusion:
`/tmp/incident-sept6/monitor-real-tf-integration.log` and
`/tmp/incident-sept6/monitor-real-tf-suite.log`. Executed the completed isolated
`test-dev` binary with `USE_EXTERNAL_DB=true` and
`TIMEFUSION_PG_TEST_URL=postgresql://postgres:postgres@localhost:12345/postgres`.
Local PostgreSQL at localhost:5432 has both timescaledb and timescaledb_toolkit;
TimeFusion at localhost:12345 was confirmed listening and answering queries.
Tests ingest actual metrics and evaluate the saved KQL. The broader module
covers count monitors, recovery, hysteresis, and notification suppression.

Fourmolu full-file checks pass for changed Haskell files. A range-only format
attempt reported an AST mismatch; it was not applied or overridden with unsafe.
Full-file formatting showed only one whitespace change in Parser.hs, applied
normally. The Haskell constraint review remains clean.

The all-target local build 33918 is **terminal failed**: another session edited
ReportsSpec while the shared checkout was compiling, so the report function and
its new call arity disagreed. This is not a parser test failure; parser tests
had not executed. Do not restart from the shared checkout again.

**Authoritative incident implementation now lives at:**
- Worktree `/tmp/monoscope-incident-repair`
- Branch `fix/incident-sweep-20260906`
- Commit `d69f4938cc2e1c804d37d4dd92ac878365266a82`
- Base `017d00d9f` from freshly fetched origin/master
- Draft PR https://github.com/monoscope-tech/monoscope/pull/507

Only the four incident Haskell/test files, six saved monitor YAML definitions,
and incident catalog were committed. Other concurrent changes in the original
checkout (Containers, Reports, EmailTemplates, monoscope.cabal, report tests,
weekly-report docs) are NOT ours and were left untouched. The newer base's
BackgroundJobs changes are unrelated drain/pattern work and applied cleanly.
The original checkout still holds our uncommitted four-file patch; use the
incident worktree for further edits/builds/commits to prevent source races.

**Live local unit run:** exec session `70392`, log
`/tmp/incident-sept6/monitor-branch-unit.log`, started from the incident worktree:
```
cabal test monoscope:test:unit-tests --builddir=/tmp/incident-sept6/isolated-build --ghc-options=-O0 --test-show-details=direct --test-options='--match Pkg.Parser'
```
The previous isolated build is stopped; this run reuses its directory but reads
an immutable committed source tree. Revalidate and poll this handle.

**CI run:** `34060480374` (Build and Test PR), verified in_progress.
Frontend, UI tests, Gate and format already succeeded. Build/Test and hlint were
running at last inspection. Review run `34060480270` is also active. PR is draft;
ready/merge/deploy only after required checks and review findings are handled.
No code deployment has happened from this sweep yet.

**Important temporary production mitigation:** all FOUR metrics monitors are
muted for 30 minutes, approximately until 21:40 UTC (read actual muted_until).
After query-field correction the broken count-based evaluator caused the
unsorted monitor to become alerting, so muting prevents misleading notifications
while the code repair is tested. Monitor actual values directly during this
window; do not call the underlying condition repaired. Reassess before expiry;
restore notifications after verified deployment and correct evaluation.

Full monitor inventory contains SIX entries, all active. Also corrected:
- `c7830a38-9bec-4192-85b0-314b2f645fd9`: oldest bucket query now max(value).
  Direct 30-minute metric max was 945 seconds.
- `70b9c7dd-add4-4a1d-9ff5-d07feb8cbe7f` (flush failures) and
  `d47e3766-b689-41c3-b65d-4d4f57b46e9c` (WAL corruption): old
  `severity.text == "ERROR"` now `severity.severity_text == "error"`, matching
  the discovered schema/facet values. Direct corrected flush count is zero.
All PATCH/mute calls succeeded. Definitions for all six are committed under
observability/monitors. Existing notification destinations and thresholds were
preserved. Raw snapshot: `/tmp/incident-sept6/monitors-all.json`.

Older-fix evidence: FairSpill commit `47291d21` is an ancestor of deployed TF
`a88933c`, whose config explicitly defaults to FairSpill. Monoscope fixes
`15e0e0872` (service worker removal/stale-chunk reload) and `8a61f2e60` (image
build and expected-4xx reporting) are ancestors of deployed `4fb7aa0b`.
Do not blindly reimplement them; verify remaining recurrence separately.

UI variable-error context searches saved under `/tmp/incident-sept6/variable-*`.
The exact window around 13:43:28 mostly contains E2E long-animation-frame spans;
no error body matched in a 5-minute search, so this is inconclusive. A mistaken
unbounded event get for a non-error span is still tracked as session `52531`
(last verified live); poll it if needed, do not repeat it. Use `events get --at`
for known timestamps to avoid its default 90-day scan.

Next: poll local unit run and CI; fix failures in the incident worktree; update
PR validation; ready/merge and verify deployment image, then actual monitor
values and unmute. Continue the full issue inventory and embedding/TF fixes.


## 21:34 UTC review and validation checkpoint

PR #507 review identified unrelated reporting edits accidentally captured from the shared working directory. The follow-up removes those edits; the incident branch now retains only monitor behavior changes, regression tests, monitor definitions, this journal, and behavior-preserving fixes for 17 inherited HLint hints. No lint suppression was added. The HAVING regression now checks aggregate expansion and removal of the alias reference.

The monitor integration suite passed all 12 examples against local PostgreSQL and real TimeFusion before this cleanup. A clean-branch rebuild is still running; no application deployment has occurred. Full-source HLint left two Charts hints, now corrected with a clean targeted HLint result. CI on the original commit failed; the follow-up must pass checks before deployment.

All 132 actual ERROR-level entries in the captured TimeFusion Docker log concern DEALLOCATE ALL compatibility. This remains unresolved. Four metrics monitors were temporarily muted while their corrected queries still run through the old count-based evaluator; inspect and renew only for a bounded interval while checking metrics directly. The alert implementation must be deployed and verified before normal evaluation resumes.

## 21:38 UTC isolated branch checkpoint

Authoritative branch remains /tmp/monoscope-incident-repair. PR #507 now heads 6f659931f. Review found reporting edits accidentally included in the original d69f4938c patch; they are removed in 6f659931f. The first CI build confirmed the missing RP.collectSystemReport symbol. Do not repeat the earlier claim that the first patch contained only monitor edits. The follow-up also resolves inherited HLint hints with no suppressions and strengthens aggregate-alias HAVING assertions.

Clean-branch parser validation passed 128 examples. Reusing the shared local TF instance at :12345 gave a widget count of 2 instead of 1 and a nonempty RUM project, so that run is not clean evidence. A separate TF instance on :12346 with prefix incident-20260906-2136 and data /tmp/incident-sept6/tf-clean-data passed all 12 Monitoring tests. Logs: monitor-clean-branch-unit.log, monitor-fresh-tf-integration.log under /tmp/incident-sept6. The isolated TF runs in exec session 34443; stop it after testing rather than the shared :12345 process. Targeted RUM empty-project validation is also running (81470). Latest PR CI run 34061405906 passes lint/UI/format; Build and Test pending. No code deployment yet.

Four metric monitor mutes were renewed for 45 minutes around 21:34 UTC because deployed evaluator still counts samples. Verify exact muted_until before expiry (about 22:19 UTC); do not restore automatic evaluation until deployment is checked. Direct 30-minute metric samples still returned WAL 23,068,674,000 bytes, pressure 12%, oldest age 1104 seconds, no unsorted series. CLI events output discards grouped metric labels, so query each series separately for reliable attribution.

TF warning summary saved in /tmp/incident-sept6/tf-warning-summary.json: 11,638 rollup_promotion_unmatched, 1,830 scheduling-lag warnings, 985 dedup_batch_probe_timeout, 471 journal_hold blocking warnings, 319 coverage_ledger_disagreement, 199 maintenance_coordinator_unit_timed_out. Full counts and samples available; these remain untriaged beyond inventory. All 132 ERROR-level log entries are DEALLOCATE ALL compatibility failures. Embedding configuration and remaining issue families remain unresolved.


## 21:47 UTC embedding regression and new deployment evidence

Production recheck now reports Monoscope 017d00d9f (3/3) and TF a148673 (1/1), deployed by other ongoing work. TF a148673 merges DV-dedup changes; re-evaluate old maintenance warnings against it. Do not deploy stale code over these versions. Monitor PR507 still checks 6f659931f; run34061405906 Build and Test confirmed live in Build. Nonblocking review recommends mixed aggregates; direct SELECT GREATEST(count(*)::float,max(value)::float)::float8 over (28),(80) returns80 on local TF. Four monitor mutes verified expire22:18:43–45 UTC.

The SAME incident worktree is now on follow-up branch fix/embedding-base-url-20260906, based on6f659931f, with uncommitted embedding work. No changes to monitor PR head. Extracted existing client config unchanged into Data.Effectful.LLM.openAIEmbeddings, BackgroundJobs delegates to it; added7 unit cases covering empty, official root, official /v1, trailing slashes, custom proxy paths. The repair is not applied yet: first demonstrate expected failures. Initial builds failed because new module/dependency needed declaration; hpack regenerated manifest and unit tests now explicitly depend on langchain-hs. Current build session54216, log embedding-before-final.log. Do not restart unless terminal. Once failing regression observed, normalize trailing slashes and map the official https://api.openai.com root to its /v1 API base while preserving explicit custom paths. Then test, review, commit separate PR. OpenAI Docs skill read and official embeddings reference fetched.

Fresh Docker snapshot /tmp/incident-sept6/monoscope-docker-latest.log (~250MB) includes58 parsed TF_WRITE_FAILED entries between21:31:47 and21:32:04, all draining-for-deployment (43 spans,15 metrics). Queue source routes WriteFailure to DLQ with flush-before-offset-commit;168 write-failure routing entries appear in capture. Replay completion not yet verified. SSH log capture session22194 may still need polling; do not fetch identical30min again. Large log size is caused by exception formatting including full serialized telemetry batches. Catalogue parameter-safe exception logging as a separate fix; avoid printing raw batch data.

The RUM empty-project test passed1/1 on isolatedTF after previous checkpoint. IsolatedTF :12346 pid29514 was stopped with SIGTERM; shared :12345 untouched. Last embedding failure in new log still21:20:13, errorPatternEmbedding, api.openai.com //embeddings.


Embedding checkpoint21:49 UTC: baseline now reproduced with7 examples/4 failures, log embedding-before-typed.log. Added concrete [(Text,String)] test-case annotation after the overloaded-list inference error; no weakened types. Repair now applied in Data.Effectful.LLM.openAIEmbeddings: trim trailing slash and map official root to /v1; custom API paths retained. After-fix test run session38220, log embedding-after.log, is live. TargetedHLint started separately. All changes remain uncommitted on fix/embedding-base-url-20260906 in /tmp/monoscope-incident-repair. hpack also added previously missing existing0146 migration to generated manifest. MonitorPR507 CI still Build; do not restart livejob.


21:49 further evidence: targeted embedding HLint clean ([]). After-fix session38220 still compiling/linking. Manual metrics over15min now WAL8,388,611,000, pressure22%, oldest839seconds, no unsortedseries. Queried each metric with summarize max(value) by metric_name for attribution. WITHOUT by metric_name, events endpoint reproducibly400s selecting context___trace_id and other span-only fields from otel_metrics; scalar summarization is dropped on this API path. Add scalar metric events-summary bug to catalogue; separate from tested finalAlertQuery monitor path. Confirmed on currentproduction017d00d9f. Do not confuse these generated unknown-field requests with customer errors.


Embedding fix complete locally:7/7 regression cases pass in embedding-after.log; HLint[] and fourmolu/diff checks pass. Committed/pushed e477b7b5d40f934b66f16ff73b62bc17423021c7, draft PR508 https://github.com/monoscope-tech/monoscope/pull/508, stacked on fix/incident-sweep-20260906 (PR507). Worktree /tmp/monoscope-incident-repair clean on embedding branch. After PR507 merges, rebase embedding commit onto currentmaster and update PR508 base to master if necessary; keep only embedding diff. No live embedding success/deployment yet. PR507 run34061405906 now passed Build and is live in Run doctests. Docker capture22194 still reports running when polled; file bounded snapshot was enough for recent failure classification but capture not yet terminal.


## 22:01 UTC monitor merge and summary-table repair

PR507 all checks passed, including e2e; marked ready and squash-merged to90b23980d2c5d562b9908da980e12aca38b24a71. Deploy run34062540931 confirmed live building Docker image. Gate accepted previous attestations, Build/Test skipped by its authorized gate. Application rollout not yet verified. Metric mutes expire22:18:43–45; inspect after rollout before unmuting.

PR508 now rebased to master90b23980d in separate /tmp/monoscope-embedding-rebase, commit17ad5a1e1, pushed force-with-lease, PR base master and body updated. New CI34062591161 Build/Test pending, lint passed. This worktree is clean and holds the embedding branch. The old embedding commit e477b7b5d is still the parent of the ongoing summary branch; rebase summary onto17ad5 after committing its patch.

/tmp/monoscope-incident-repair is now on fix/summary-table-results-20260906 with uncommitted 4-file repair: Parser, LogQueries, ParserSpec, LogSpec. Reproduced scalar table-summary fallthrough:1 regressionexample1failure (summary-table-before-direct.log); cabal's first attempt used a bad --match argument, so directbinary ran the meaningful baseline. The table path required both aggregates and grouping, selected defaultspan columns for scalars, and mislabeled grouped rows with defaultevent columns. Fix renders scalar/grouped aggregate values (including group labels), supplies matching finalColumns, suppresses invalid implicit ORDER BY timestamp for scalars, and passes metricJsonAsVariant=useTimefusion in selectLogTable. Parser tests129/129pass; targetedHLint[]; no suppressions/unsafe/type weakening. First build caught extendedColumns being list rather than Map; fixed via Map.fromList.

Integration test ingests28/80 into a fresh project, calls real queryEvents for scalar and grouped variants, checks80 and the metriclabel. Initial run returnedNULL because testclock=2025 while samplesusedrealnow; now explicit from/to around inserted timestamps. Rebuild live session76658, log summary-table-integration-build.log. After terminal success run integration binary --match 'metric table summaries' with USE_EXTERNAL_DB=true TIMEFUSION_PG_TEST_URL=postgresql://postgres:postgres@localhost:12345/postgres LOG_LEVEL=attention. Unique project perrun avoids previoussharedTFcontamination. Earlier log summary-table-integration.log is failed clock-mismatch evidence, not afterfixpass. No summarypatchcommit or PR yet.


22:04 UTC serialization dependency: explicit-time table integration now returnsString80.0 instead ofNumber80. Do NOT weaken this assertion to make it pass. Direct localTF SQL SELECT jsonb_build_array(80::float8,80::float4,80::bigint,80::numeric) returns[80.0,"80.0",80,"80.0000000000"]. src/read/functions.rs array_to_json_values_inner (~1113) only matches Int64,Float64,Boolean;Float32 and other numerictypes hit generictextfallback. This is the actual reason integration remainsred, separate from MonoscopeSQL bug. Parser castsvalue::float (TF Float32), so implement numericJSON support inTF, test it then rerunMonoscope. Existing serde macro json_primitives supportsnormalfinitevalues;handle specialfloat semantics deliberately. Current numericassertionmustremain. Rustskills andrs-minimal-tests file read attempted but combinedoutputtruncated; read relevant instructions fully beforeRustedit. NoTFedits yet. TFroot concurrentwork onmastera148673, create isolatedworktree forrepairs. SourceJsonBuildArrayUDF lines930+, array_to_json_values_inner1094+, tests2436. Current Monoscope summarybranch patchuncommitted, baseline1failure, parser129pass, integrationfails numericJSONtype. Casewithbin stillneeds check its metadata/output semantics; existing binbranch ignores non-timegroups, an additionalpre-existing bug.


## 22:16 UTC production monitor and TimeFusion JSON checkpoint

Monitor deploy34062540931 succeeded; SSH confirmsMonoscope90b23980d 3/3, TF remainsa148673. Fresh metricsevaluations: oldest955 at22:07 then810 at22:16; pressure8 at22:07. Unmuted oldestc7830a38 andpressure745d60c4 after verification. WAL46b... stilllast22:01, due15min; unsortedfb7... last22:07 alerting0. Both stillmuted until22:18:43–45, checkbeforeexpiry andextendonlyboundedifneeded.

Unsorted monitor threshold0 contradictedits"page if >0" description: monitorStatusbreachisinclusive<=, sozero couldneverrecover. Patched onlyalert_threshold to1 throughCLI monitorspatch, confirmedresponse1. YAMLtimefusion-unsorted.yaml modified in SAMEsummarybranch, uncommitted; mustcommitthispersistentdefinitionseparately fromsummarycode. Do not change evaluator boundarysemantics: existingdoctests/tests intentionallyinclusive. Waitfornormalstateafterfresh evaluation thenunmute.

Historicalmetric comparisons initiallylookeddifferentbecause CLI metricsParams alwaysaddsdefaultsince1h alongsideexplicitfrom/to; API TimePickerprioritizessince, so explicitboundswereignored! Reproducedresponsewindow1hour versusrequested10min. --since '' removesdefault andresponseboundscorrect. pressurehistoricaldirect9 versusmonitor8 mayreflectlate-arrivals/query-timing; no evidenceofnewaggregationbug. Add CLI explicitfrom/to overriddenbydefaultsince defect tofixqueue.

NewTFworktree /tmp/timefusion-incident-repair, branchfix/json-numeric-values-20260906 baseda148673. rust-skills/rs-minimal-tests applied, noAGENTSfound, CLAUDEmandatesnextestandSQLbugSLTbaseline. Added2queries atstarttests/slt/json_functions.slt. Baseline runsession86257 terminalexit100, logtf-json-before.log:expected[80.0,-7,17,null],actual["80.0","-7","17",null];9tests8pass1fail. Fixedarray_to_json_values_inner usingexistingjson_primitivesfor allintegerwidths, Float32/64 viajson_floatsmacro retaining PostgreSQLnonfinite"NaN"/"Infinity" strings. LocalPGoracleconfirmednonfinite strings. Targetedafterrun99802 terminal0, tf-json-after.log9/9pass. Source/testonly2filesdirty; cargofmtallalsotouched3preexistingdatabasefilesbutthoseFORMATONLYchangeswererestored. Fullfmtcheckmayrevealbaselineformatdrift; handlenecessaryprepushcleanly, nosuppression.

UsingexistingTFtargetdir /Users/tonyalaribe/Projects/apitoolkit/timefusion/target withCargobuildlock (nootherRustbuildwhenstarted), avoids55GBduplication on71GBfree disk. Currentserverbuildsession36019, tf-json-build-server.log, livecargo build--bintimefusion. Afterterminalsuccessrunfixeddebugserver isolatedport12346 anduniqueprefix/localMinIO, thenMonoscope integrationnumericassertionunchanged. Testrequiresnumeric80, notstring. Fullcargo lint+suite requiredbeforepush, noTFcommityet. DecimalJSONconversionstilltext andFloat(None)mapsFloat32inTF unlikePGFloat64: cataloguefurthernumericcompatibilitywork, notcoveredbythisfix.

Monoscopesummarybranch5filesdirty:Parser/LogQueries/ParserSpec/LogSpec + unsortedYAML. Parser129pass, integrationfailedsolelyTFnumericstring afterexplicitbounds; rerunwithfixedTF. EmbeddingPR508 rebasedhead17ad5a1e1 CI34062591161stillBuild. Logcapture22194lastlive; checkhandlebeforeanothercapture.


## 22:23 UTC verified repairs and pending gates

All6productionmonitorsverifiedactive/unmuted/normal. WALfresh22:17:01 value14,680,067,020 (14.68GB), unsortedrecoverednormal0 at22:16:20 withthreshold1. WAL/unsortedunmutedbeforeexpiry; noremainingmutes. Snapshotmonitors-restored.json confirmsall6normal. Monitorfixdeployment90b23980dverified3/3. ThresholdYAMLcommittedseparatelyonsummarybranch.

TFfixeddebugserver on12346 returned[80.0,-7,17,null] and Monoscope metric-tableintegrationpassed1/1 againstit, numericassertionunchanged. Logs summary-table-fixed-tf-integration.log andtf-json-server.log. Isolatedserverpid78021 stoppedSIGTERM aftertests, shared12345untouched. TargetedJSON9/9pass,fullcargo lintsession84472terminal0. FullTFnextestsuitecurrent51707, logtf-json-full-suite.log;compilefinished1m46s,lastconfirmedlive. UseslocalMinIOendpointcredentials, createdtimefusion-testsbucket withmc--ignore-existing. Needfullsuiteoutcome+prepush beforeTFcommit/push/deploy. cargofmtallreapplied3baselineformattingdrifts (database/{compact,maintain,mod}.rs), shouldcommitformattingseparatefromJSONfix ifrequired; nosemanticchangesintended. TFworktree5filesdirty:3formatonly,functions.rs,SLTfile.

EmbeddingPR508allchecksgreen,markedreadyandmerged3318622da15223d02482f78e9a479c9025cdf00c. Deploy34063807503live; verifyruntimeandliveembeddingjobafterrollout. Goalembeddingnotcompleteuntilthatverified.

Summarybranchin/tmp/monoscope-incident-repairnowcleanandrebasedontomaster3318622da (twoowncommits:unsortedthreshold andsummaryrepair; inspectgitlogfornewhashes). Priorpre-rebasehashes98dc7466d/7ea933bccchanged. NotpushedorPRyet:integrationdependsTFJSONfix, avoidpointlessCIrunagainstoldlatestTF. OnceTFpublished/deployed, pushsummaryPRandtestCI. NumericJSONcorrection alsoexposesCLIeventsresponsemappingpossibleissue (CLI/LogView.hs+Commands.hs normalizeEventsResponse); inspectbeforeclaimingCLIaggregatevaluesfixedend-to-end. ExistingCLIhistoricalboundsbug remainsqueued. RemainingcatalogueincludingDEALLOCATEALL,DLQclassification,maintenancewarningsstillactive.


### 2026-09-06 22:52 UTC checkpoint

- Embedding URL PR508 deployed as3318622da on all three production replicas. Post-deploy logs now show HTTP400 input-token-limit rejections in both error and log embedding jobs (22:45:05/22:45:14); the URL404 is resolved, embedding backlog is not. The own-project first500 errors total93,109,761 bytes; max7,433,484. Own-project embeddings remain absent. Candidate normalization standalone probe copied the exact function, compiled GHC9.12.2 -O2: 16KB1.99ms,160KB11.3ms,1.6MB108.8ms. Do not blame normalization complexity from source length guards; optimized measurements are linear on this synthetic case. Long-input chunking and bounded request batching now under development on fix/embedding-input-limits-20260906. Evidence: embedding-postdeploy-2246.log, NormalizerProbe.hs, normalizer-probe (local only).
- TimeFusion JSON fix PR210 is open at92965589. Local full nextest1399/1399 passed (16 skipped); full cargo lint and targetedSLT9/9 passed. CI Clippy, both test shards, E2E, formatting and automated review passed. RustCodeQL still running. Review mentions separate existing Variant nonfinite and Decimal string handling; catalogue for follow-up, not fixed by this scalar primitive patch. Production stilla148673 until merge/build/deploy.
- CLI explicit time range fix committed3fd762a3e, draftPR510. Rebuilt localCLI sent exact10-minute interval without empty-since workaround;16CLItests passed. CI build/test still running. Installed CLI not updated. Summary table fix remains committed on fix/summary-table-results-20260906, awaiting TF latest image so integrationCI sees numeric JSON. Fixed localTF integration passed1/1.


### 2026-09-06 23:02 UTC checkpoint

- TF PR210 merged as f373d182591d76c95cd11812ccd47c120b241994 after every CI check passed. Production deployment run34065441146 is building the image; do not restart it or claim production fixed yet.
- Embedding input-limit repair committed b83697fb3, draft PR511. Three baseline tests failed, then all12 embedding tests and296 full unit tests passed. Targeted hlint[] and formatcheck passed. Live synthetic OpenAI request reproduced8192-token rejection with unbounded call and returned one1536-dimensional embedding with bounded code. UTF-8 byte ceilings bound both per-input and request tokens; long documents are preserved and combined with a byte-weighted normalized mean, using strict unboxed accumulated vectors. Logs: embedding-limits-before.log, embedding-limits-after.log, embedding-limits-unit-all.log, embedding-live-result.log. Production stillawaitsCI/deploy; backlog not resolved.
- Six monitors normal and unmuted at22:56 capture: pressure9%, oldestbucket964s, WAL17,825,794,048 bytes, three error-count monitors0. Fresh evaluations22:47–22:51. Evidence monitors-2256.json.
- New branch fix/hasql-parameter-logging-20260906 now checked out in incident worktree. Root cause of huge diagnostics: derived Show HasqlException includes Hasql StatementSessionError parameter values; displayException also uses toDetailedText over the full session error. TF_WRITE_FAILED logs show SomeException, which retains all batch parameters. Reproduction test with a synthetic1.9MB parameter is building before the rendering fix. Keep original typed errors for retry classification; change formatting only, retaining SQLSTATE, SQL template and server diagnostics.


### 2026-09-06 23:20 UTC checkpoint

- TF deployment34065441146 completed successfully, image f373d18. Production SQL numeric JSON regression returned[80.0,-7,17,null]. Rollout: total18,194ms, old-to-new query2,046ms, continuous unready1,945ms, WAL recovery0ms; sustained soak43probes0failures. Saved tf-json-deployment.log. Container0e09a7f683c2 afterrollout log snapshot10,000lines/~4.5MB starts23:14:17: noERRORlevel entries; many snapshot_duplicate_adds warnings (code drops duplicate file additions), and DV-dedup scan/oracle mismatches. Still catalogue these as unresolved background-maintenance work; no claim whole system clean.
- CLI PR510 merged1f8471569; server deploy34065766364 success. Tagv0.6.26 pushed to the tested merge commit; CLI release34065815357 running. macOSarm64 andLinuxx86_64 built; macOSx86_64/Linuxarm64 pending. Release not yet published/installed. Root concurrent master644a2a24a added host lifecycle maintenance; preserve it.
- Summary table branch593768ce3 pushed as draftPR512; CI inprogress, lintpassed. Hasql logging branch now14afbd6c5 (functional e589d7342 rebasedonto1f; plushostdurationlintfix), draftPR513. Syntheticlarge-parameter regression failedbefore,8Hasqltests and292fullunit tests passedafter, hlint[]. Original typed UsageError remains unchanged. Rendering retains SQLSTATE,SQLtemplate,pipelineposition,server/decoder diagnostics, omits SQLparameters from Show/displayException/SomeException.
- Embedding PR511 updated3f9b0290b afterbyte-aware chunk refinement (Text.Foreign.takeWord8/dropWord8 round forward at UTF8 boundaries; reserve3bytes below8191 to keep bounds).298fullunit tests passed, including request-count and cross-batch weightedmean; live synthetic OpenAI beforestill8192-token rejection, after1536-dim vector. hlint[]. Earlier CI failure was unrelatedBackgroundJobs hostduration unarynegation; fixed89ba3e26f andalsocherry-pickedintoPR513as14afbd6c5. Both newCIruns pending. /tmp/monoscope-embedding-rebase nowcheckedoutembeddingbranchclean; /tmp/monoscope-incident-repair Hasqlbranchclean. Shared isolatedcabalbuild mostrecentlyfromembeddingworktree. Copied previouslybuiltisolated web-components/dist assets intoembeddingworktree to satisfyTHmanifest; no frontend source edits.
- Started /tmp/datafusion-deallocate-all clone (sharedobjects fromcargo cache; do noteditcache). Branchfix/deallocate-all-54.1 basedupstreamtag54.1.0(0d1f2ebe). origin ton yalaribe/datafusion URL actuallytonyalaribe; upstreamapache; cargo-cachelocal. Onlyuncommittedchange iscore SQLAPI regressionforDEALLOCATEALL withquoted"all",idempotence,sessionconfig andsessionisolation. Noimplementationyet. AGENTS.md read: MUSTfmt,clippyalltargetsallfeatures-Dwarnings beforecommit; docs requireappropriatecore/CLItests andrust_lint.sh beforePR. No delegation requirement. DataFusionSLTharness explicitlydeclares--listunsupported(nextestskips), so coreasync integrationtest usedwithnextest. Initialcargoattemptblockedbybrokenlocal1.95.0rustuptoolchain conflictbin/cargo-fmt; inspectinstalledworkingtoolchains andusecompatibleone ratherthanremoveuserfiles.
- DataFusioncore prepared_plans isprivate withno publicclear. Properfix mustrepresentALL explicitly (e.g.DeallocateenumAll/Named), SQLplannerrecognizesunquotedALL vsquotedname, coreclearsjustitsplanmap. PgwirePortalStore alsohasnobulkstatementclear (onlyclear_portals,rm_statement); protocolstatement cleanupstillrequiresproperadapter/APIwork. Do notsilentlyno-op orresetwholesession. ExistingDFfork5aa045c based54.0 onlyaddsUPDATEFROM+publishedstandaloneSQLmanifest; preserveSQLpatch when integrating54.1fix. No newDF/Rustcodecompiledyet.

### 2026-09-06 23:52 UTC checkpoint

- PR 513 merged as `7b12ca9847d035e03c3e736655ce9f5335835632`; deployment is running. Summary tables (PR 512) and embedding input limits (PR 511) also await completion of their deployment checks. Production still runs `0189e4cff` at this checkpoint.
- The installed CLI artifact from release v0.6.26 passes the explicit historical time-window probe. Its existing package version string still reads 0.1.0.0; release identity was verified through the published checksum.
- The TimeFusion TCP regression reproduces `DEALLOCATE ALL` failing with “Prepared statement all does not exist.” The DataFusion fix passes its focused regression and all-target, all-feature Clippy. Broader tests and the remaining required lint suite are running.
- The pgwire statement-store change passes default and minimal/server/ring/client feature tests and their CI-equivalent lint checks. TimeFusion already vendors pgwire; the tested change has been applied to that vendor copy, retaining its existing protocol negotiation fixes. No additional pgwire fork is needed. The SQL and protocol cleanup hook is implemented locally; dependency integration and full TimeFusion verification remain.

### Fresh demo-project query failures

TimeFusion logs collected after 23:45 UTC contain four unordered merge-on-read dedup memory-limit errors (23:47:23, 23:48:24, 23:49:49, 23:50:24). Correlated Monoscope logs identify demo-project log-explorer queries, with ascending timestamp order over roughly three hours. The read-only production EXPLAIN of the same shape reproduces `DedupExec mode=full-set/greatest` over `CoalescePartitionsExec`: the memory leg declares descending ordering, the Delta leg declares none. This is consistent with the existing footer-repair problem, not proof that raising the 2 GiB guard is appropriate. No memory limit changed. Evidence: `tf-2358.log`, `monoscope-2359.log`, `demo-ascending-plan.txt` under the local incident directory. File-level cause and repair still need investigation.

All six monitors remain normal at 23:47–23:49 evaluations: flush errors 0, WAL corruption 0, oldest bucket 596 s, unsorted flushes 0, MemBuffer pressure 8%, WAL bytes 19,922,946,048. These conditions do not cover every query failure.

DataFusion broader nextest run passed 2,776/2,776 tests. Full lint passed Rust and TOML checks, then failed because the unpinned local installer selected incompatible hawkeye; installed CI-pinned hawkeye 6.2.0 under the incident tools directory and reran the full suite.

### 2026-09-07 00:11 UTC checkpoint

- Summary tables PR 512 deployed successfully. Released CLI v0.6.26 now returns a numeric scalar `max_value: 9.0` and a grouped row with the metric name and numeric `average: 2.5` for a fixed production window. The suspected CLI aggregate-column loss is not reproduced after the server projection fix. Evidence: `summary-live-scalar.json`, `summary-live-grouped.json`.
- Embedding limits PR 511 deployed as `b24004b8`, verified 3/3 healthy. Own-project log embeddings now number 1,000, latest timestamp 23:54:56; these precede that rollout and therefore do not prove the oversized-input fix. Error embeddings remain zero at this checkpoint. Wait for the next production job cycle. Database logging PR 513 deployment also succeeded.
- New confirmed CLI defect: `--level error` returned zero rows while the explicit `severity.severity_text == "error"` filter found rows in the same 23:40–23:45 window. Its implementation used `severity.text` and upper-case enum values. Fix in `/tmp/monoscope-embedding-rebase`, branch `fix/cli-severity-filter-20260906`: correct field, lower-case normalization, corrected help examples. Sixteen CLI tests and 55 command doctests pass; both `--level error` and `--level ERROR` now exactly match the explicit production query. Not yet committed/published. Separate observation: shorthand filters wrapping a piped query can produce invalid syntax; not fixed by the severity change.
- New confirmed ingestion defect: project-ID-only batches skipped cache resolution because the empty-API-key shortcut ignored `at-project-id`. Regression failed with no ack and DLQ routing, then passed after requiring both keys and IDs to be absent before skipping lookup. Full write-failure integration suite is running. Branch `fix/project-id-only-ingestion-20260906` in `/tmp/monoscope-incident-repair`; only OtlpServer and its integration spec changed.
- Pure DataFusion DEALLOCATE fix committed/pushed to own fork as `3fb603e95` on `fix/deallocate-all-54.1`, after 2,776 tests and full lint passed. Consumer branch `timefusion-deallocate-all-54.1` preserves UPDATE FROM through an explicit planner capability: default DataFusion continues rejecting it, TimeFusion’s DmlQueryPlanner opts in. This replaces the old unconditional guard removal, which failed an existing upstream safety regression. Capability changes remain uncommitted while full checks run. TimeFusion currently uses temporary local path patches for 33 DataFusion crates; replace with the tested immutable fork revision before committing its manifest/lockfile.

### 2026-09-07 00:32 UTC checkpoint

- TimeFusion is pinned to tested DataFusion fork revision `a442435c7a4781ea4cc5b9043e2f0634d603705a`; all 33 workspace patches now use the immutable git revision. The consumer fork passed 2,776 tests, all-feature Clippy, and the complete lint suite.
- The production TCP regression passes for both simple and extended DEALLOCATE ALL, preserving another connection’s prepared statement and connection reuse. The local TimeFusion full `cargo lint` passed; full nextest is running. The dependency version check required a PgCoalesceUdf audit: upstream ScalarUDFImpl and coalesce sources are byte-identical between 54.0 and 54.1, but the wrapper lacked conditional-argument and documentation forwarding. Those methods are now forwarded and the audited version is 54.1.0. Evidence diffs and source hashes were captured locally.
- CLI severity PR 514 (`6be31c1e4`) and ID-only ingestion PR 515 (`8a156ba7f`) are in CI. Both have reproduced failures and passing local regressions. No parameter-suppression or weakened tests introduced.
- Mobile sidebar error `5aa24466` reproduced in rendered projects-page markup: an authenticated page without a project still emitted a toggle whose load handler dereferenced the absent sidebar checkbox. Fix `8a9440a87` renders toggle and backdrop only when the sidebar exists. All three projects-page integration tests, source HLint, and formatting pass; PR submission is in progress. Worktree `/tmp/monoscope-incident-repair` is now on `fix/mobile-sidebar-without-project-20260907`.
- Production Monoscope is healthy 3/3 at `2528c5a9`, which includes all shipped fixes. Six monitors remain normal at 00:21–00:25: pressure 10%, oldest bucket 1,015 s, WAL 18,874,371,020 bytes, flush errors/corruption/unsorted flushes zero.
- The 00:30 embedding job is confirmed locked by deployed production worker `82ac0bc19425:1` for our project (job 38819884). Follow this run for vector persistence; prior checks still showed zero error embeddings and 1,000 log embeddings last updated at 23:54:56. Do not claim oversized-error recovery until those timestamps move.

### 2026-09-07 00:52 UTC checkpoint

- PR514 merged as `04cbfd4b3bfda7f3056590a12835c821762529b9`; deployment34070581767 succeeded and three production containers were observed on this image.
- PR515 merged as `b362814a7ec51b20c66f31296cf8c39eec41ae79`; deployment34070585350 still running at last check.
- PR516 fixes the project-less mobile sidebar null target; local projects integration tests pass, CI now running integrations after successful build/doctests/unit tests.
- PR517 corrects severity schema metadata and CLI/MCP documentation. Commit197a4613e; all117 schema doctests pass, HLint and formatting clean.
- TimeFusion PR211 completed both nextest shards, E2E, PostgreSQL18 client smoke, Clippy and formatting. Rust CodeQL analysis remains pending before merge.
- Fresh production cleanup error at00:44:15 again reports prepared statement all missing after timeout; PR211 addresses this exact path.
- Embedding recovery remains incomplete:0 error vectors,1000 log vectors unchanged since23:54:56. Job38819884 retried on another worker at00:40:06; database last_error is TimeoutException, attempts2. The current embedding path saves vectors only after the entire500-document operation. Inspect normalization/runtime and checkpointing before declaring the oversized-document fix operationally recovered.
- CLI pipeline composition separately reproduced with failing doctest: shorthand filters wrapped pipelines in parentheses. Candidate emits a separate filter stage and removes only an optional leading pipe, preserving quoted pipes. All60 CLI Commands doctests now pass; binary build in progress in `/tmp/monoscope-embedding-rebase`, branch `fix/cli-filter-pipelines-20260907` (not committed yet).

### 2026-09-07 00:59 UTC checkpoint

- PR516 merged as `bc91e9081cb9c0ed5b65d97f3c979dcce256cd46`; deployment34071243345 queued behind515.
- PR518 opened, stacked on517. Commit03d3b9554; pipeline binary successfully returns count7 for23:40–23:45 with `--level error`, equivalent to explicit severity filter.16 CLI tests and60 Commands doctests pass. Evidence cli-pipeline-live.json, cli-pipeline-{before,after,tests,build}.log.
- New embedding root-cause evidence: `normalizeErrorForEmbedding` runtime on synthetic z-text grows near-quadratically:50k0.0346s,100k0.1072s,200k0.3910s. `Utils.replaceAllFormats.scanMons` evaluates `T.length` of the complete remaining text on every character just to check length<3. This can dominate processing of multi-megabyte error messages before any provider call.
- Isolated branch `fix/linear-error-normalization-20260907` in `/tmp/monoscope-incident-repair` replaces three unbounded prefix-length checks with `T.compareLength`. HLint/formatting pass. Full library build is running in session25271; optimization settings changed from previous test build, so dependencies are rebuilding. Do not cancel it. Before benchmark binary and logs: `/tmp/incident-sept6/normalization-scaling*`. Rebuild benchmark against the new library only after build completes, then run existing normalization doctests/unit tests. Long numeric runs in scanMons still merit inspection: `T.span isDigit t` followed by <=2 can rescan long suffixes. No performance-recovery claim yet.
- Six monitors freshly normal at00:52–00:56: flush0, replay0, age674sec, pressure10%, WAL17,825,794,048bytes, unsorted0. Evidence monitors-0057.json.
- Duplicate snapshot investigation remains a hypothesis, not a deployed fix. DeltaScanExec consumes DV masks keyed by physical path; a duplicated Add can therefore scan extra unmasked copies while metadata COUNT applies a mask per duplicate. Kernel checkpoint-crossing guard already exists. Cached snapshot restore materializes with reconcile=false, while periodic reconciliation only runs on active commits. Check for persisted pre-fix duplicates, reproduce, then rebuild from log truth rather than arbitrarily retaining a duplicate. Delta protocol requires snapshot Add paths to be unique: https://github.com/delta-io/delta/blob/master/PROTOCOL.md#action-reconciliation . No delta-rs cache edits made.

### 2026-09-07 01:06 UTC checkpoint

- TimeFusion PR211 merged as25c2ccea67d426021370678acffcfb46dea80b05 after Rust CodeQL passed22m50s. Deployment34071717314 is building. Read-only production SQL probe still fails before rollout with Prepared statement all missing; evidence tf-deallocate-live-before.log.
- PR515 deployment34070585350 completed successfully. PR516 deployment34071243345 is running. PR517 CI34070970331 and PR518 CI34071245659 are still pending. Retarget518 to master once517 merges; release v0.6.27 has not been tagged yet.
- PR519 opened from branchfix/dashboard-variable-project-context-20260907 in `/tmp/monoscope-embedding-rebase`, clean pushed commit5ae59f395. It fixes historical error993cb9b8: reloadVarWhitelist copied location.search but omitted the required chart_data pid. Dashboard input now carries data-project-id and request sets pid explicitly. Regression fails before/passes after, all931 frontend tests pass, Vite build passes, Haskell HLint/format pass. Logs dashboard-variable-*.
- Frontend test runner must use Node25 explicitly. Default npm used old Node18/x64, which first lacked styleText and then installed wrong optional bindings. Resolved with `/Users/tonyalaribe/.nvm/versions/node/v25.2.1/bin/node /Users/tonyalaribe/.nvm/versions/node/v25.2.1/lib/node_modules/npm/bin/npm-cli.js ci --ignore-scripts`, then explicit Node25 node_modules/vitest/vitest.mjs and vite/bin/vite.js. No lockfile edits.

### 2026-09-07 01:25 UTC checkpoint

- TF deployment34071717314 succeeded. Production container56a77b7d0f12 is25c2cce. Live same-connection PREPARE / DEALLOCATE ALL / DEALLOCATE PREPARE ALL / SELECT42 succeeds; numeric JSON remains[80.0,-7,17,null]. Evidence tf-deallocate-live-after.log. Monoscope is3/3bc91e908 (f9b7aac890e3,2d11bddeff44,e92db960114a), mobile deployment34071243345 succeeded.
- PR517 review correctly caught my accidental lowercase `level` examples; reverted to uppercase and clarified description. Commitd45374968,118 schema doctests pass. Pushed correction; CI reruns. PR518 merged that correction without force-push, newHEADfd397fc4488e1f0531a5c2faa448ca2f01a133fc. Still stacked on517; retarget to master after517 merges. CLIv0.6.27 not released yet.
- PR520 (055b4d44a) contains only three Utils.hs length-check replacements. All647 relevant doctests and300 unit tests pass. Scaling before50k/100k/200k:0.0346/0.1072/0.3910sec; after0.0044/0.0055/0.0110sec. Candidate5.4MB JSON-like input0.376sec. Baseline previous test build vs candidate default library optimization differs; use scaling rather than an exact speedup factor. Actual next500 unembedded errors:93,074,225 first-line bytes,24 first lines over1MB,max7,433,484. No message contents retrieved for this measurement. Production recovery still needs post-deploy verification.
- Disk fell to2.4GiB during concurrent local work. Removed only own completed `/tmp/incident-sept6/pgwire-target` (1.6GB); all evidence retained. Latestfree5.4GiB. SharedTFtarget52GB, incidentartifacts6.4GB, isolatedHaskellbuild3.9GB. Monitor before further builds; never delete user caches indiscriminately.
- New snapshot evidence: copied six small zstd snapshot files read-only with sudo tar to `/tmp/incident-sept6/snapshot-cache.tar` (chmod600), from`/home/ubuntu/timefusion-data/.timefusion_meta/delta_snapshots`. Parsed locally usingzstd CLI + installedpyarrow. Metadata audit `/tmp/incident-sept6/snapshot-duplicate-audit.json`:04fbddf62e47997b v529528 has1620entries/1499paths,28duplicatedpaths/121extras;95437d149806a356 v169092 has1746entries/1737paths,8duplicatedpaths/9extras. Every duplicate path has DIFFERENT deletion-vector descriptors. Other four snapshots unique. Do not arbitrarily select/drop a descriptor.
- New isolateddelta-rs checkout `/tmp/delta-rs-snapshot-repair`, branchfix/intervening-commit-removals, cloned--shared from untouchedCargo cachea0f68b0. NoAGENTS found, readCONTRIBUTING; use nextest. Suspectedpostcommithook bug:kernel/transaction/mod.rs run_post_commit_hook advances snapshot over all intervening commits but passes only this commit's removed_paths toadvance_with_removes; missing intervening removals can retain stale files/DVs. A regression `incremental_commit_applies_intervening_removes` added in crates/core/src/kernel/snapshot/mod.rs: clonev1, overwriteexternallyv2, blindappendfromstalev1=>v3, compare hookfilelist withauthoritativev3. IMPLEMENTATIONNOTCHANGEDYET.
- Regression build running session45999, logdelta-snapshot-before.log. Target sharedTFtarget, Rust1.91.1, RUSTFLAGS lldsameTF, debug0/incrementalfalse DEV+TEST (may rebuild dependencies vs TFline-tables-only). Temporaryconfig `/tmp/incident-sept6/delta-snapshot-build-config.toml` patches only33DataFusionworkspace crates to samea442435c fork asTF; seededCargo.lock fromTF forreuse, untrackedignored. Need watchdisk. Next: finish failing regression, fix intervening-remove handling properly, verify; thendetermine durablelog vs cache damage and repair from authoritativehistory. Forkremote currently pointslocalcache; beforepushsetownGitHubforktonyalaribe/delta-rs-timefusion, neverupstreammessages.


### 02:00 UTC checkpoint — DV identity reproduction and delivery status

- PR519 (`c18eece4a717366831da63294cfa708ccd666f0e`) deployed successfully; current Monoscope containers are `142c2e92e7db`, `e96b894c11e9`, `587ec154b697`. This delivers the dashboard variable request's missing `pid`.
- PR517 merged as `d66d0f4217852088681b18ebd22b4d58c3bd9b1b`; deploy34073705511 still running. PR520 merged as `71caca919dd9d79418568d3c0df6e20bf73e5cc7`; deploy34074032798 still running. Before normalization rollout, project error vectors remain0, log vectors1000 (latest23:54:56). Do not claim production embedding recovery yet.
- PR518 now targets master after rebase onto517. Review caught another real case: operator-free leading stages (`| take 10`, `| sort by timestamp`) were rewritten as search text. Added a failing parser doctest, preserved leading pipes in bareword classification, then61/61 Commands doctests and16/16 CLI tests passed. Updated push/CI pending; CLIv0.6.27 not yet released.
- The proposed Delta post-commit/intervening-removal bug was DISPROVED: PreparedCommit refreshes its read snapshot on conflict before the post-commit hook. Isolated Delta regression passes unchanged. No Delta implementation patch or upstream PR. Also corrected validation provenance: Cargo global `--config` was not forwarded through nextest, so the standalone Delta test used registry DataFusion54.1.0, not the git fork. The actual TimeFusion regression uses its locked git-fork dependencies.
- Durable audit independently loaded the cached table versions from object storage: logs v529528 has1664 entries/1501 physical paths/45 duplicate paths; metrics v169092 has1746 entries/1737 paths/8 duplicate paths. Duplicates therefore exist in durable history; rebuilding only local snapshot caches is insufficient. Evidence: `durable-snapshot-audit.json` and `compare-durable-snapshots.py` under the private incident directory. No production table mutation or arbitrary mask selection performed.
- TimeFusion regression `wave_rejects_a_superseded_deletion_vector` used real in-memory Delta writes and two independently generated DV updates against the same original file. Before fix, landing probe wrongly returned true and stale wave committed version3 instead of preserving version2: `(true,1,0,Some(3))` versus expected `(false,0,1,Some(2))`. This reproduces the same-path stale-DV corruption mechanism without modifying production.
- Local fix on `fix/dv-aware-wave-liveness-20260907`: compare staged Removes and Adds against exact active path+DV descriptors; refuse ambiguous duplicate paths; include live DV sidecars in discard protection. Initial targeted suite10/10 passes. Extended regression verifies orphan sidecar deletion, live sidecar/parquet preservation, and exact prior-commit recognition. Full cargo lint in progress after correcting two needless borrows; full nextest follows before push.
- Removed only our completed disposable caches/executables, preserved evidence and the CLI candidate. Old huge Docker logs are now `.log.gz`; pgwire-target and cli-severity-build removed, candidate binary retained at `/tmp/incident-sept6/monoscope-pipeline-candidate`. Disk approximately7GiB free. Concurrent user GHC PID20261 and user source/worktrees remain untouched.


### 02:30 UTC checkpoint — normalization rollout, missing parquet, full Rust validation

- PR517 deploy34073705511 and PR520 deploy34074032798 both succeeded. All Monoscope replicas now use71caca: `7e23277ed25a`, `7ea8baff6e92`, `c4a693f6e8a8`. Error embeddings remained0 immediately before/through rollout; next completed embedding cycle still needs verification. Current own-project jobs included01:55 retry and02:00 first attempt timing out on earlier replicas. Do not infer recovery from deployment success.
- PR518 HEAD `657d0147b1a550c8c84c9ea33be1d3017735a937`; final review approves, CI Build and Test pending. Candidate CLI now also verified `| take 10 --level error` in production for23:40–23:45: count7 (`cli-leading-pipe-live.json`). All61 doctests and16 CLI tests pass. Release remains pending.
- Latest monitor snapshot `monitors-0218.json`: all6 normal/active/unmuted. Values: flush0, replay0, oldest bucket743s, buffer9%, WAL13,631,491,072bytes, unsorted0. Evaluation timestamps02:09–02:10. This does not clear maintenance or metadata defects.
- New high-priority production failure at02:15:11/23/44: endpoint served-evidence jobs get HEAD404 for `project_id=be87ebc1-08b9-4293-a390-283460fa6202/date=2026-09-06/part-00000-27c85910-bfeb-4809-a575-7e9273d15ccb-c000.zstd.parquet`. Independent durable Delta loads at versions530346 and530424 each still list exactly one active entry, while direct S3 HEAD returns404. Bucket versioning is not enabled and object-version listing is empty. This is a missing live object, not merely a stale process cache. Evidence: `embedding-normalization-rollout.log`, `missing-parquet-audit.json`, `check-missing-parquet.py`. No production mutation performed; commit-history/recovery investigation remains necessary.
- DV fix refined to retain original `Vec<Add>` inputs in StagedBin rather than only paths: DV dedup can read survivor files it does not Remove, so all reads need freshness checks. The probe now reports Inconclusive if any expected physical path remains live but exact identity/whole-wave landing is unproven, preventing callers from deleting partially landed or subsequently DV-updated parquet. No new lint suppression/unsafe/weakened types.
- Full `cargo lint` passes. First all-features nextest:1458/1463 passed,5 failures (4 MinIO disk reserve errors + new cleanup test's unprefixed store fixture). Fixed fixture to use `log_store.object_store(None)` and asserted object existence before cleanup. Second all-features run:1459/1463 passed, only the4 MinIO disk reserve errors; the new regression passes, including orphan cleanup/live sidecar preservation. Third full run active after freeing disk to16GiB. No tests skipped or suppressed to hide failures;17 existing configured skips remain.
- Disk cleanup: removed obsolete incident-build executables (8 files,4.48GB, names recorded in `obsolete-incident-executables-removed.txt`), completed incident Haskell utils-bench/doctests/server outputs, and registry DataFusion build artifacts. The registry cleanup matched older cached registry versions too, not solely this turn's54.1 build; only reproducible compiled artifacts were removed, no sources/data. Current TimeFusion git-fork test executables and user running services/compilers preserved. Standalone Delta test binary77d59 removed after evidence retained. MinIO reserve settings remain unchanged.


### 03:05 UTC checkpoint — CLI release, stale DV history, remaining embedding work

- PR518 merged as6f91d5b6a; all three Monoscope replicas run it (5739f07d7ece,50f35eee6398,1daaaae38654). CLI v0.6.27 published for four platforms. macOS arm64 binary checksum verified, installed, and production leading-pipe/severity query returns the expected7 events.
- TF212 merged as5c30dfa42ebb7fb6b186bdbb4a829f3cabbf3999. Full cargo lint, all1463 nextest tests (17 existing skips), and full CI passed. Deploy34077693066 remains in progress; production verification pending. Existing duplicate metadata is not repaired by this prevention fix.
- Missing parquet history confirms stale DV updates:527171 replaces242 with539;527178 incorrectly removes242 again and adds315.527405 later replaces the696 branch physically, leaving539.528303/528338 update that leftover branch to545/5041. PostgreSQL dual writes are disabled and there are zero backup rows for the project/day. Replacement lineage and retained ingestion sources still need investigation before any repair; no arbitrary deletion or mask selection is safe.
- Embedding verification job38826824 was picked up by5739f07d7ece at02:43:21 and subsequently disappeared; error vectors remain0. Its terminal reason was not captured. Actual two large inputs still take23.34s/12.12s to normalize after520. Follow-up builder/constant-lookahead changes are isolated in /tmp/monoscope-normalization-followup, validation underway. Private baseline outputs allow exact byte comparison.
- Disabled-worker regression reproduced queue consumption despite enableBackgroundJobs=False; initialization and supervisor guards now preserve the queued job. All3 DailySchedule integration tests, HLint and formatting pass. Production replicas have this setting enabled; the concurrent user GHC connects to a local database, so no claim that it consumed production jobs. User process remains untouched.

### 03:15 UTC recovery evidence

- Worker isolation fix published as PR521, commit56e30ce18. Review has no blockers; full Build and Test still running.
- Read-only lineage scan through531030 captured80 commits for the affected project/day in replacement-partition-commits.json. Original replacement c9c18781 proceeds through0998b89c,87836dfe,45a10bac,bb1a006d. All five objects remain present;45a10bac andbb1a006d remain active, showing additional stale branches. Filename-based matching corrected an initial URI-prefix mismatch in the audit helper; initial false active flags were not reliable.
- All five inputs to original missing-file creation at527051 remain present. Preserved20 objects (inputs plus relevant DV sidecars),109,154,906bytes, locally under recovery-inputs with SHA256 manifest. Physical input rows1,077,600 minus recorded2521-row DV exactly equals missing output1,075,079rows. This is promising reconstruction evidence, not a completed repair. Original output was sorted; exact row order matters because surviving DVs refer to row positions. No production mutation.

### 03:27 UTC rollout and reconstruction constraints

- TF212 deployment34077693066 succeeded. Production1a0a15730a23 runs5c30dfa; same-connection PREPARE/DEALLOCATE ALL/DEALLOCATE PREPARE ALL/SELECT42 and numeric JSON probe pass. Initial1500-line log capture still contains historical duplicate-snapshot and404 failures, no panic. Prevention is deployed; historical repair remains open. check-missing-parquet.py now points to the new container.
- Reconstruction audit applied the preserved2521-row bitmap using the same compiled Rust RoaringTreemap decoder.1,075,079rows have21,425 tied sort-key pairs; all differ in full row values.21,073 pairs are internal to one input,352 span inputs. The surviving5041-row DV partially masks197 cross-input pairs, so arbitrary stable sorting is insufficient: exact merge tie ordering or a separately proven logical reconstruction is required. Evidence reconstruction-{order,tie,tie-sources,mask-ambiguity}*.json and helper sources. No production data changed.
- Normalization follow-up library build reached BodyWrapper then failed on the fresh worktree's missing generated Vite manifest. Copied the existing generated dist from the prior isolated worktree; resumed build31337 is progressing. Source lint/format pass. Semantic tests and private byte comparison remain pending.

### 03:39 UTC — worker merge, dominant normalization cause, recovery proof underway

- PR521 passed full CI and merged as e3d8c8e7985aa43f0a404dbaacf1ab42726d4eb6. Deploy34080123535 is running.
- PR522 draft opened, commitda1989ae4 on fix/linear-pattern-prepasses-20260907 in /tmp/monoscope-normalization-followup. Main scanner hexPart <> rest copies the entire remaining message repeatedly. Reusing T.drop (T.length safePre) txt removes this copy. Isolated exact-source functions at identical-O1 show actual corpus23.506877s/13.870836s before vs0.662462s/0.507532s after. All8 output comparisons are byte-identical, including two captured large messages and email/hex scaling cases. Evidence scanner-isolation/{before,after}.log and scanner-{before,after}-*.normalized (private).
- Prepass-only full-library benchmark remained23.479s/12.322s: it was insufficient on the real inputs, though email scaling became linear. That version passed300 unit tests. Initial649-doctest run failed because prose immediately followed an expected output; inserted blank comment separator. Unicode doctest changed to equality because Show escapes lambda. Combined scanner+prepass build28041 is running;650 relevant doctests and final unit rerun still required. HLint/format/diff checks pass. No production embedding recovery claim.
- Six monitors snapshotmonitors-0330.json normal, newest03:25: flush0/replay0/oldest449s/pressure12%/WAL16,777,219,020bytes/unsorted0.
- Preserved first replacement c9c18781 and its two other inputs:3objects110,871,620bytes, replacement-manifest.json. Across ALL580 partially masked sort-tie pairs (not only cross-input pairs),311 retain both rows in first replacement and269 retain one; none lose both. Neither extra input contains any of these580 keys. Current proof script prove-reconstruction-tie-order.py is matching full row contents and preserved physical order to infer surviving row identity; active session72427, outputreconstruction-tie-order-proof-summary.json. No production table mutation.

### 04:14 UTC — final normalization validation and production recovery in progress

- PR522 now includes repository scaling benchmarks for hex suffixes, emails, malformed JWT headers and digit runs. Benchmark commit a77ba4839. Full650 doctests pass; full300 units pass after annotating benchmark tuple/list types; all6 benchmarks pass. Hex repeats5k/10k/20k take5/10/19ms; email7/14/27ms; JWT1/2/4ms. No new timing thresholds. Final full-library actual inputs0.641013s/0.494084s; five outputs byte-identical to baseline. CI34081307260 still running doctests.
- PR521 deploy34080123535 succeeded; production Monoscope containers0e44207f785d,4cc17fffaaf3,a90e2e4d1efd all run e3d8c8e. TF remains1a0a15730a23/5c30dfa.
- Missing-file recovery proof covers all580 partially masked sort ties. Every full row matches the first retained replacement with known surviving positions; extra replacement inputs contain none of these keys. Reconstructed1,070,038 visible rows are private in recovery-inputs/reconstructed-visible-527051-dv5041.parquet. Full PyArrow value/schema roundtrip and actual TimeFusion/DataFusion reader pass across all91 physical columns.
- Temporary Rust recovery utilities are untracked under /tmp/timefusion-incident-repair/examples and archived under /tmp/incident-sept6/recovery-tools. They are not product changes and must not be pushed accidentally. Local Delta transaction rehearsal regenerates stats with RecordBatchWriter, preserves sort/compression metadata and row-group bounds, and replaces the exact missing Add with dataChange=false. Independent final validation confirms1,070,038rows, one live file, identical values/metadata/sort order,10row groups. The first baseline validation falsely expected DataFusion to preserve Variant field metadata: the original file fails the same expectation; corrected validation compares native and DataFusion schemas to the original and passes.
- Recovery utility rechecks source path/DV/size/partition and table schema after staging, refreshes with update_state, then uses CommitBuilder with max_retries0. Local race test removes source during staging: no repair commit occurs. Repeated invocation refuses before staging. Clippy for both tools passes on the final source. No suppressions or unsafe rename enabled.
- Production repair now running in session80546, frozen validated binary and SHA256 plan recorded in production-repair-plan.json; output production-parquet-repair-summary.jsonl/log. It staged from version531991; outcome and post-commit query checks still pending. Do not repeat blindly. Wrapper uses captured production AWS credentials only in subprocess memory, forces unsafe rename off, and never prints them.
- Delta protocol was rechecked: new DV versions must remove the prior active identity, snapshots have unique Add path keys, and dataChange=false is valid for a row-preserving rearrangement. Source: https://raw.githubusercontent.com/delta-io/delta/master/PROTOCOL.md (Add/Remove and Action Reconciliation).

## September 7, 04:34 UTC: missing parquet repair verified

Production repair committed at version532000 (04:12:22 UTC). Independent snapshot532039 contains zero active old entries and exactly one replacement. The replacement is118,038,893 bytes and its full streamed SHA256 equals the locally validated final file. It contains1,070,038 visible rows. A physical production query for September6 noon returns5,284 rows and190,224 total ID characters in1.447 seconds. The latest10,000 TimeFusion log lines contain no old filename and no panic. Raw404 substring matches were unrelated to parquet filenames and are not evidence of additional missing files.

PR522 passed all CI and merged as131d24fd62c6f6d5018978573d73925eaea13342; deploy34082810456 remains in progress. Actual embedding persistence still requires observation after rollout.

A local-only duplicate-path recovery prototype is being tested. It copies each extra physical reference to a fresh object while retaining the exact deletion vector and metadata, then commits one Remove/Add pair. This preserves the existing logical row multiplicity. No duplicate-path repair has been applied to production.

## September 7, 04:52 UTC: object audit and duplicate recovery

PR522 deployment34082810456 succeeded. Production replicas a5cc77647a90, efefab40c132, and70e601159774 run131d24fd and are healthy. Error vectors still0/log vectors1000 at the pre-rollout baseline. Next own-project embedding job38819902 is scheduled05:00 UTC; persistence verification remains open.

All active object HEADs were checked: logs snapshot532423 has1511 unique paths,1508 present and3 missing; metrics169676 has1740 paths, all present. Missing logs are4c45c2d5 (2 logical entries),6d86dade, and8e94414c. The original27c85910 repair remains verified and inactive. The latter two files predate the6-hour Delta-log retention horizon; available history has been preserved locally (4892 commits). Commit531853 is an observed FSCK action removing these paths with no DV; it did not remove their actual DV-bearing identities. No such FSCK was used for the verified repair.

The duplicate relocation tool passes its real Delta/DataFusion row-preservation test, concurrent-repair refusal, clippy, and build. A production conditional multipart-copy probe verified exact bytes and overwrite refusal, then removed its test objects. First relocation532456 is independently proven byte-identical and snapshot-equivalent except for path; exact DV preserved. Fourteen further relocations passed per-commit action checks before a concurrent commit caused a safe OCC failure. Independent log scan532519..532535 proves the failed repair did not land. A fresh-state batch resumed. One uncommitted copied object may remain from the conflict; do not delete unidentified objects. Original full estimate was39.7GB of server-side copies; missing sources are excluded and retained for reconstruction.

The missing4c45c2d5 originated at530547 from two retained files (3351 rows), then split into DV53 andDV59 through stale updates before the prevention deployment. Its inputs, replacement witness, and DV sidecars are preserved. Reconstruction and full-row tie-order proof are underway; no missing-file mutation has been applied for it.

## September 7, 05:10 UTC: three missing paths repaired; final reconstruction in progress

Missing4c45c2d5 was repaired at532642 and532644, preserving its separate DV53/DV59 visible sets (3298/3292 rows). All68 partial tie groups were resolved using a retained sorted replacement witness with no overlapping keys from its other input. Both native DataFusion reads and the complete local Delta rewrite preserve all91 fields. Independent production hashes equal the rehearsed files; old path inactive at532679. The first verification incorrectly required replacements to remain active after ordinary maintenance; corrected verification checks each committed snapshot and separately confirms the old path is inactive.

Checkpoint532607 retained33,318 tombstones despite expired historical logs. Exact deletion-time groups identify six candidate inputs for6d86dade and four for8e94414c; their full physical row counts match1,662,166 and39,528 respectively, and all carry the expected sort order. Inputs and sidecars preserved. 8e94414c has zero partially masked tie groups; reconstruction and native/full-schema rehearsal pass. Production commit532768 restores39,475 visible rows. Independent hashes match the rehearsal; old path inactive at532811.

6d86dade remains unrepaired. There are670 partially masked ties,333 across inputs, all cross-input full rows differ. A229.9MB retained sorted file74239613 created22:21:51 is being checked as a witness; nearby tombstones identify its other two inputs. Do not choose an arbitrary tie order.

Duplicate relocation has65 verified commits including the initial proof, about7.6GB copied so far. Batch3 now verifies every intervening transaction after a failed attempt before retrying from fresh state, with a limit of3 attempts. It refuses a retry if any committed Add/Remove touches the source. Uncommitted copies remain for normal cleanup, not blind deletion.

All six monitors remain normal. The scheduled05:00 embedding job disappeared without error vectors and without a captured terminal reason. Controlled verification job38830873, scheduled05:07:19, was observed locked by current replica efefab40c132:1 at05:07:21 and remains running. Error vectors remain0 at this checkpoint; no claim of embedding recovery yet.

## September 7, 05:18 UTC: production error embeddings now persist

Controlled job38830873 was picked by efefab40c132:1 at05:07:21. Production now has500 error-pattern vectors, timestamp05:16:06.692850 UTC, versus0 before the normalization rollout. The complete500-document private corpus (108.5MB JSONL) normalizes in9.674387s locally; slowest document0.803632s. Normalized text totals125,959,816 characters and needs at least15,834 provider chunks, explaining the long network phase. The job then hit its10-minute limit in later stages and was retried by70e601159774:1 at05:17:25; durable vector progress is proven, full job completion remains to monitor.

Read-only socket inspection found three existing local GHC processes29690,30333,49967 connected to production PgDog6432. Their root checkout has ENABLE_BACKGROUND_JOBS=False and still has unguarded jobsWorkerInit, matching the disabled-worker bug fixed in deployed PR521. This provides a likely route for jobs disappearing without executing. These are pre-existing user development processes; they were not stopped, reloaded, or modified. Previous investigation of PID20261 did not cover these three.

The final missing-file6d86dade reconstruction resolves all670 partial ties against retained74239613; its1,680,856 rows equal the original1,662,166 plus18,690 from two other inputs, which have no overlap with the needed tie keys. Full-row identity/order proof has0 failures. Native read and complete local Delta rewrite preserve1,631,464 visible rows, all91 fields and metadata. Production repair is now running with zero commit retries and exact DV/source validation.

## September 7, 05:44 UTC: all missing active log objects repaired

The initial single-request230MB upload for6d86dade hit the600-second orchestration timeout without committing or producing a matching completed object. The storage client defaults to a30-second request deadline; this is a likely transfer limitation, not evidence of another database-corruption issue. The prevalidated local final file was then uploaded in28 parts of8MB (two concurrent transfers), each checked with Content-MD5 and completed conditionally with If-None-Match. Upload succeeded.

A staged-commit utility streams and verifies the full SHA256, validates exact source DV/metadata, checks destination identity and schema stability, refreshes the table, and uses zero commit retries. Local regressions prove checksum mismatch refusal, successful commit, repeated-commit refusal, and concurrent-source-removal refusal. Full clippy/build pass. Production final repair533279 preserves1,631,464 visible rows in230,162,273 bytes. Independent full SHA verification matches the rehearsal. Snapshot533285 has1661 unique active log objects, all present, and all four old paths are inactive. The first live query still returns5284/190224 rows/ID characters.

Duplicate relocation continues; over138 commits have been independently checked. A follow-up is isolated in /tmp/monoscope-embedding-progress, branchfix/durable-embedding-progress-20260907. It saves complete embedding documents in groups bounded by the existing provider chunk budget; oversized individual documents remain indivisible. Before reproduction shows two provider requests with the second failing leave0 saved documents. New tests cover durable earlier documents, refusal to save an unfinished multi-request document, and order/identity preservation. Full local unit build is running; HLint and formatting pass. No PR or deployment for this follow-up yet.


## Checkpoint: September 7, 06:12 UTC

- Historical duplicate-path repair completed: 179 exact-byte/DV-preserving relocations in total (initial proof 1, batches 14 + 15 + 148, final metrics copy 1). The last metrics copy exceeded the repair client's default request timeout; a verified fresh attempt with a 120-second client request timeout committed version 169924. No production server timeout was changed.
- Independent object audit: logs version 533575, 1,699 entries/unique paths; metrics version 169924, 1,760 entries/unique paths. Every object HEAD succeeded; both tables have zero duplicate paths and zero extra entries. Evidence: `/tmp/incident-sept6/final-active-object-audit.json`. This establishes physical path integrity; normal logical row deduplication remains a separate concern.
- PR 523 is ready for review, local final 303 unit tests pass, automated review found no correctness issue, full CI pending. It saves completed documents before requesting later groups, retaining earlier progress if a later provider request fails. Oversized documents remain indivisible to avoid saving incomplete chunk averages.
- Live demo query for September 6 12:00–12:05 UTC returns 21,264 rows but takes 33.951 seconds, with warm EXPLAIN ANALYZE 30.929 seconds. Delta scans 20.36 million physical rows, emits 5.45 million after dedup, and the timestamp filter above dedup emits 21,264. It reads 478.1 MB; all 250 row groups matched. Evidence: `recovered-demo-query-plan.log`.
- Local real-DV regression reproduces excessive dedup input: a 30-row file with two masked rows returns the correct four selected IDs, but dedup consumes all 28 visible rows. Testing immutable filtering after Delta applies row masks and before dedup. Delta's physical optimizer filter pushdown also needs checking to ensure it cannot shift DV row positions.


## Checkpoint: September 7, 06:20 UTC

- PR 523 passed every required CI check, including the full build/test job (22m11s), and merged as `43f2de56b08504db345538ae9a9ec112f5573bcf`. Deploy run `34089847919` is building the image. Baseline: 500 error vectors, latest 05:16:06.692850; 2,000 log vectors, latest 05:20:29.989403. Pending unembedded rows: 11,608 error patterns and 2,903,055 log patterns.
- Six monitors are normal at the 06:12 snapshot; latest evaluations 06:07–06:09. Own-project ERROR query over the preceding 30 minutes returned zero events.
- TimeFusion regression before any fix: correct four IDs, but 28 rows reach DedupExec. Adding the earlier filter alone causes deleted `k12` to reappear, proving that DeltaScanExec's physical filter pushdown crosses the DV row-position boundary. With the Delta guard plus the earlier immutable filter, the same real-DV regression passes: correct four IDs and four dedup inputs.
- Delta fork guard rejects both static and dynamic physical filters before DV masks, retained row indices, or row tracking have consumed physical positions. All 113 scan tests pass. Fork commit `a17379858779815fd93bac998910c9e506a6dc8d` pushed to own fork branch `fix/dv-physical-filter-boundary-20260907`. TimeFusion dependency pin and full checks are next in `/tmp/timefusion-filter-before-dedup`; no TimeFusion PR or deployment yet.


## Checkpoint: September 7, 06:25 UTC

- PR 523 deployment succeeded: all three healthy replicas run `43f2de56b08504db345538ae9a9ec112f5573bcf` (`f7f1507c9ec3`, `8c464db844df`, `efd608c8f3a4`). This is the nineteenth deployed/released software repair.
- Controlled job 38832870 disappeared before a worker was captured; it is not recovery evidence. Second job 38832916 was captured locked by `f7f1507c9ec3:1` at 06:24:26.282889. The worker logs incremental `Saved pattern embeddings` events. Independent database check at 06:25:12 shows 542 error vectors (up from 500), while the same job remains locked/running. This proves the new implementation saves completed groups before the overall job finishes.
- TimeFusion full lint passed. Full nextest is building/running with the final pinned fork. Cargo.lock retains every existing unrelated dependency resolution; only the four Delta git source entries changed, and locked metadata validation succeeds.


## Checkpoint: September 7, 06:41 UTC

- TimeFusion PR 213 is open: https://github.com/monoscope-tech/timefusion/pull/213, head `f48569a0`. Final full lint passes. Default nextest: 1,401/1,401, 16 existing skips. E2E: 62 passed, one cache-test race; the test now cancels unrelated boot preload before measuring global S3 counters, preserving its assertions. Both affected cache tests pass on rerun. Combined coverage covers all 1,464 enabled tests, plus 113 Delta scan tests. CI is running; no TF deployment yet.
- Current maintenance state required replaying the WAL: the 63 MB snapshot was last checkpointed at 03:18:52, so reading it alone gave obsolete failures. Applied all 111,521 complete JSON WAL records (97,379 task updates and 14,142 removes) to its keyed task map. Current state: 77,707 complete, 19,919 superseded, 454 pending, 203 retry, 7 running. Dedup: 321 pending, 175 retry, 7 running. No missing-file retries remain. Seven DV accounting retries and eleven memory retries remain recorded; the memory retries' latest scheduled deadline is September 5, while the DV accounting retries' latest is September 7 05:14. These are recorded retry states, not evidence that all failures recurred at capture time. Evidence: `maintenance-tasks-replayed-0636.json`.
- Embedding baseline-to-progress: errors 500 → 834; logs 2,000 → 3,396 at 06:38–06:39. The controlled job retried around its ten-minute worker limit and retained earlier persisted groups. Its terminal exception was not captured (`last_error` was null), so do not attribute the retry to a specific exception.


## Checkpoint: September 7, 06:48 UTC

- PR 213 CI formatting, autofix, Clippy, both test shards, E2E, test attestation, and automated review are green. Rust CodeQL analysis remains running. Head `f48569a07e1b17707605f787595c8e1e115f7f98`, worktree clean.
- Both normal scheduled embedding jobs observed after rollout used deployed workers; the 06:45 job 38819909 is locked by `f7f1507c9ec3:1`. Latest database snapshot: 835 error vectors and 3,500 log vectors. Controlled verification job 38832916 has left the queue. Its terminal success/error was not captured; durable intermediate persistence was independently verified while it ran.
- Pre-213 maintenance baseline from live `timefusion_stats`: 562 pending, 16 running, 199 retry; dedup worker_seconds 182,439, killed_seconds 49,780, rows_dropped 2,651,691, progress proxy 45,116,410,512. These are cumulative counters since the current process started, not counts over the snapshot interval. `maintenance-pre-213.json` retains the complete baseline for post-rollout rate comparisons.


## September 7, 07:09 UTC — early-filter deployment and embedding rate limits

TimeFusion PR 213 passed Rust CodeQL plus all tests, lint, E2E, and review. Merged exact reviewed head as `34f6e89c56f23ef2e04c3d2aa79569c6e0099ceb`. Deployment34093718832 is building; production verification has not yet run against the new image.

Production embedding errors at06:33:17,06:35:42,06:38:44,06:38:45 were HTTP429 with `rate_limit_exceeded`, token rate limit, and requested retry delays101–604ms. These are actual errors, independently observed from job lifecycle timing. Langchain's embedding transport returned the first429 immediately. Added typed response classification, at most two retries starting within30s, Retry-After seconds/date handling, exponential fallback plus jitter, and preservation of terminal error responses. Quota/input/unknown errors and unreplayable streams are not retried; exceptions propagate. Twelve real local HTTP cases pass, including unchanged POST bodies, retry bound, minimum delays, invalid headers, future server dates, and terminal errors. Before fix the two retry regressions failed. Fork `5595b365639676702c52c4fd2f15e5815d4bc330` is pushed to the owned langchain-hs repository; Monoscope pin is under full unit validation in `/tmp/monoscope-embedding-retry`. No retry-fix PR/deployment yet.

Evidence: `embedding-retry-before.log`, `embedding-retry-final.log`, `embedding-post-523-0652.log`, and private `523-embedding-errors-*.json` under `/tmp/incident-sept6`.


## September 7, 07:15 UTC — demo scrape corrected and retry PR opened

Read-only metadata established the failing scrape target `d93ae374-4f17-49f4-8358-c65503c14a63` belongs to the all-zero demo project, is named `demo-target`, and requests `http://localhost:1/metrics`. It was created June28 and still received connection refusals at07:13. Preserved the complete original row in private mode0600 `disabled-demo-scrape-backup.json`; atomically disabled only this exact ID/project/name/URL while retaining history and configuration. Returned enabled=false, last_scraped_at07:13:04. Verify it stays unclaimed after the next scheduler interval.

Monoscope PR524 opened from `59d44fb16` (one Langchain pin change). Fork twelve HTTP regressions and lint passed; application build/test remains running. PR lint, format, frontend gate, CodeQL and review pass; main build/test is running. Local build initially lacked the new worktree's generated Vite manifest; copied existing compiled assets after confirming frontend source identity with the previously validated worktree, then resumed.

Fresh07:10 monitoring: six monitors normal; own-project ERROR count0 over30min. Pre-TF213 log capture contains no missing-object or accounting errors. One07:10 HotPacking capacity failure hit the8GB pack pool; coordinator capacity splitting already exists, so record and observe recovery rather than infer a new missing mechanism. Most other warnings report active scan-pressure throttling, inline Tantivy segment merging, unsupported rollup shape fallback, or base tiers waiting for dedup.


## September 7, 07:21 UTC — transient MemBuffer dwell alert

Fresh issue inventory found the existing MemBuffer alert updated at07:12 with actual1506s above1200s; monitor was alerting at07:18. This supersedes the earlier normal snapshot. Live stats at07:20 show oldest bucket299s,1345 completed flushes, zero failed flushes,7% buffer pressure, and392,433 buffered rows. WAL18GB, no quarantined files. The gauge measures dwell (source and regression confirmed), not event-time age. Flushes continued and drained the delayed bucket; the monitor's ten-minute max window retains the high sample. Verify natural clearance and absence of recurrence after TF213; do not mute or manually resolve the alert. No new runtime exception was created after02:36 in the latest100-issue page.

Evidence: `issues-followup-0719.json`, `membuffer-alert-0719.json`, `membuffer-monitor-0720.json`, `membuffer-stats-0720.json`, `tf-membuffer-0720.log`.


## September 7, 07:26 UTC — application retry validation passed

All303 Monoscope unit examples pass with pinned Langchain5595b36; full application and test binaries built. PR524 description updated with final local validation and unchanged library package version0.0.3.0 (freeze-compatible). CI build remains running. TF213 release image passed its smoke test and entered production handoff; live query verification awaits the replacement container.


## September 7, 07:29 UTC — TimeFusion production improvement verified

Replacement container `474b31443e15` is healthy on `34f6e89`. Exact production baseline query returns unchanged21264 rows/765504 ID characters in3.305s (before33.951s). Warm EXPLAIN ANALYZE takes1.890s (before30.929s). DedupExec input is21.27K instead of20.36M; elapsed dedup compute3.48ms. The lower FilterExec selects21.27K from20.29M rows after deletion-vector masking. This eliminates excess dedup/sort work; DV files still decode physical positions, so it does not claim elimination of scan IO. Full evidence `filter-production-after.log` and `filter-production-plan-after.log`. Readiness soak remains running; fresh full active-object audit is running against the new container without overwriting prior audit evidence.


## September 7, 07:47 UTC — retry deployment and expired-history regression

PR524 full CI passed (build/test24m45s plus E2E, lint, format, review and CodeQL). Exact reviewed head59d44fb1632f3dfe84ce74ab6b13dc0d0f93ed0b merged as e1f7e71cc14427fe87308b9fb007078942b26bcf. Deployment34096775206 is building. Baseline immediately before rollout:2098 error vectors,4000 log vectors, with efd608c8f3a4 holding the07:30 retry and07:45 regular job.

Post-TF213 restart logged `maintenance_task_reconcile_failed`: source commit492767 unavailable, durable cursor492766. Reconcile currently runs only at startup and cannot advance through expired log retention. Local regression removes a commit while retaining the live snapshot and fails with the identical unavailable-commit error (commit1, cursor0). Initial compilation needed ObjectStoreExt imported; one overlapping old build appended its compiler-error trailer to the reused log, but the final nextest result independently confirms the actual missing-commit failure.

Candidate fix in `/tmp/timefusion-reconcile-history`, branch `fix/reconcile-expired-history-20260907`, based on34f6e89. On an absent commit, collect metadata partitions from the loaded source and its loaded rollup tables, including output-only partitions left by missed deletes; retain partitions discovered before the gap. Requeue whole-day dedup and tier work, checkpoint it before advancing the cursor, and yield between partitions. Coarse invalidation reuses normal invalidation state resets rather than manufacturing hundreds of hourly tasks per partition. Reconciliation repeats every60s to keep healthy cursors inside retention. Network/parse failures still propagate; no cold-loading or blind cursor reset. Extended regression includes an output-only Delta metadata fixture with intentionally unreadable parquet to prove metadata-only recovery, and reloads the durable journal to verify tasks plus cursor survived. After tests/lint/CI and deployment remain pending; no PR yet.


## September 7, 07:57 UTC — embedding retry deployed; maintenance checks continue

PR524 deployment34096775206 succeeded. Three healthy replicas on e1f7e71:bd834bc1c110,f14156c42d80,c0853558e731. Next regular embedding cycle08:00 will provide live progress/error verification. No claim of an observed production retry counter: the helper returns successful retries transparently, and the real HTTP regressions verify the retry mechanism.

History-gap recovery passes all five focused reconciliation tests, including output-only metadata, durable journal reload, precise retained-history hours, and no cold loading. Full lint found an unnecessary iterator clone; corrected to borrowed iteration and rerunning lint. No history-gap PR yet.

At07:49, TimeFusion had two Dedup worker_error retries. Captured one actual timeout07:44:03 on demo-project slice1788732060000000..1788732420000000, input4files, estimated470041153B; it ran901s after starting07:29:01. The progress watcher is wired, but a separate code inspection found that plans completing before its15s sampling tick can drop the watcher without ever reporting their completed rows. This is a hypothesis for false timeouts, not yet proven as the production task's cause. A regression using40 real short SQL queries over40 virtual seconds against a30s idle window is prepared in `/tmp/timefusion-short-plan-progress`, branch `fix/report-short-plan-progress-20260907`; it has not yet run. Only its test is changed, no implementation yet.


## September 7, 08:10 UTC — tracked post524 progress

The08:00 regular embedding job38819914 disappeared before a new worker claim was observed; no success is inferred from that. All three new replicas explicitly have ENABLE_BACKGROUND_JOBS=True. Old job38819913 was still locked to replaced8c464db844df until normal stale-lock recovery removed it; no manual unlock or local-process interference was used.

After no embedding job remained locked, queued one controlled verification job38836217 for08:06:53.413619. Fast polling captured f14156c42d80:1 claiming it at08:06:53.621919, attempts1. The same worker emitted15 Saved pattern embeddings records through08:07:55, with no embedding failure/terminal429 in the captured log. DB error vectors2312→2371 while that exact job remained locked to the new worker. Log vectors4608; later progress and error soak remain to check. Evidence `verify524-job.log`, `524-tracked-worker.log`, `embedding-post524-tracked.json`. Do not queue another verifier while this one runs.

History-gap full lint passed; all1402 default tests are executing (16existing skips). The short-query progress regression is compiling independently; no progress-watcher implementation has been edited yet. Other pre-existing local Claude/Rust work is also consuming resources in separate worktrees; leave it untouched and avoid unnecessary additional concurrent builds.


## September 7, 08:17 UTC — short-query timeout reproduced; combined maintenance release planned

History recovery passed full lint and all1402 default tests (16 existing skips,342.608s). All-feature E2E is compiling with two test threads to limit local resource use. Short-query regression failed as predicted:40 real SELECT1 queries separated by1s over40 virtual seconds hit the30s idle timeout because none survived the15s watcher tick. Evidence `short-plan-progress-before.log`.

Candidate progress fix now in `/tmp/timefusion-short-plan-progress`: PlanProgress keeps shared plan/progress/high-water state, samples on guard close, and uses atomic fetch_max to count each row interval once if periodic and final samples race. Existing sampled-plan test now checks final sampling does not double-count. All liveness tests are queued behind the E2E build. No progress fix has been pushed/deployed yet.

Deliver these two maintenance corrections together after combined final checks. Apply the tested progress diff onto `/tmp/timefusion-reconcile-history` after its current E2E run and the progress-focused test finish; then run final combined lint/tests, create one PR and deploy/monitor. No maintenance PR has been created yet.

Tracked post524 job38836217 saved500 error and500 log embeddings by08:14:02 on f14156c42d80,64 save groups, zero terminal429 or embedding failures in the captured8-minute worker log. Error vectors2812; log vectors4932 at08:13:56 before its final log save. It still held the tracked job then. This verifies selected-batch progress on the deployed code, not that the entire historical backlog is exhausted.

### 2026-09-07 08:30 UTC checkpoint

History-gap recovery passed all 1,402 default and 63 all-feature E2E tests (existing skips unchanged). Short-query watcher final sampling passed all 11 liveness tests after the real 40-query regression failed before the fix. Changes are now combined in `/tmp/timefusion-reconcile-history`; final lint/all-feature tests running (`maintenance-combined-{lint,tests}.log`). Neither change is published yet.

Tracked embedding job 38836217 saved its 500 error + 500 log vectors, then recorded `TimeoutException`; its retry was claimed by 6cbf9c6ea393 (same e1f7e71 release). The original worker log at 08:18:43 records `LLM judge failed for log patterns` with `AsyncCancelled`. Thus full job completion is not proven. At 08:27 durable totals reached 3,378 error / 5,608 log embeddings. OddJobs defaults to a 600-second limit; Langchain OpenAI-compatible generate/chat catch SomeException and turn cancellation into a provider error. Local actual HTTP cancellation regressions are being run before changing this behavior. No additional verification job inserted.

External concurrent Monoscope commit 0e3ae850 (vendor browser SDK/cache constraint) is deploying; do not interfere.

### 2026-09-07 08:40 UTC checkpoint

Main TimeFusion advanced to 6867aa23 (external certify-on-completion work). Combined fixes applied without conflicts in `/tmp/timefusion-maintenance-recovery`, branch `fix/maintenance-recovery-20260907`, based on that commit. Integrated lint/full all-feature tests are running (`maintenance-integrated-{lint,tests}.log`, session 25297). The earlier combined check based on TF213 passed lint and is running 1,466 enabled tests, 17 existing skips (session 1529); not yet a final result. Neither TimeFusion branch is pushed.

OpenAI cancellation: actual local HTTP regressions reproduced cancellation being returned as a provider error for generate and chat. Both now pass, plus synchronous HTTP500 behavior and all 12 prior retry regressions (14 tests total, 1.51s). HLint clean. Library fix committed/pushed as ad74fb7f1663349993238d5da99ca05e3bf68334 on `fix/openai-cancellation-20260907` in the owned Langchain fork. App pin in `/tmp/monoscope-openai-cancellation`, same branch name, based on external Monoscope 0e3ae850; app build/unit checks running (session 82318; `openai-cancellation-app-units.log`). New app branch not committed/pushed. Source comparison confirmed web-components unchanged since e1f7e71 before copying real compiled assets from the prior validated worktree. Monoscope's enclosing tryStep uses UnliftIO.Exception.catch, which preserves async exceptions; no extra catch change is needed.

At 08:35 all six monitors remain normal (some values evaluated earlier); maintenance retries include 9 Dedup.worker_error and 95 dedup_incomplete, so post-fix soak is still required. New evidence `monitors-0835.json`, `maintenance-0835.json`.

### 2026-09-07 08:45 UTC checkpoint

Integrated TF lint passed. The superseded combined run on TF213 exposed an existing DV fixture race: at 08:39:26 a background flush wrote one row between INSERTs, producing four files where the fixture expected two. Final integrated worktree now uses the existing one-hour test flush interval for that fixture so its two explicit flushes define the boundaries. All original two-file, same-path, exact-DV, row-count and no-resurrection assertions remain. Superseded nextest PID58570 was verified in `/tmp/timefusion-reconcile-history` and interrupted; integrated build PID69234 was verified and interrupted before editing the fixture. Final lint passed again (`maintenance-integrated-lint-final.log`); full all-feature tests rebuilding/running (`maintenance-integrated-tests-final.log`, session38934). No test failure is claimed as fixed until that result.

External deployments complete: TF6867aa2 container29050d2b92dc, Monoscope0e3ae850 replicas bc9dd1b0ce4d/cd2beee4ad79/fe5d55a93799 healthy. TF startup at08:34:35 independently confirms history-gap bug for metrics: source commit152985 unavailable, cursor152984. `tf-cert-start.log`. No newly created issues in latest100-page since07:19. Embeddings3423errors/5608logs; tracked38836217 no longer listed, but removal is not whole-job success proof because stale retries may skip. No new verification job. Docker die events show exit137 for several old Monoscope containers, but no matching OOM event in retained event output; do not infer OOM from137 alone.

### 2026-09-07 09:00 UTC checkpoint

Monoscope cancellation pin is now draft PR527, reviewed head8169795754d100b0ea1ab829040d7762e487ce3a. Formatting, HLint, frontend, CodeQL and automated review pass; Build and Test is still running (34102401843). Review confirms UnliftIO wrapper is already async-safe. Local application unit build continues (session82318). Do not mark ready/merge until application checks pass.

Final integrated TF build finished after15m30s; 1,477 enabled all-feature tests are now running, with17 existing skips. Final lint already passed. Session38934, `maintenance-integrated-tests-final.log`. No result yet. Both maintenance fixes plus the DV fixture race correction are uncommitted in `/tmp/timefusion-maintenance-recovery` on top of6867aa23. Do not publish an earlier worktree by accident.

Fresh monitor evaluations at08:46–08:50 are all normal: MemBuffer dwell569s, pressure13%, WAL22,020,098,048bytes; other values0. Evidence `monitors-0850.json`.

### 2026-09-07 09:13 UTC checkpoint

Final integrated TimeFusion lint and all1,477 tests PASS (557.973s,11slow,17existing skips). The DV fixture race correction is verified in that complete run. Committed/pushed as4334f6e05c6a9dbaa61ffd84fffb82d287788afc; PR214 open, CI/review pending. Worktree `/tmp/timefusion-maintenance-recovery` clean. CI34104554365, CodeQL34104551861, review34104554323. Merge only the reviewed exact head after all checks pass.

Monoscope PR527 is now ready. CI34102401843 Build/weeder/doctests/unit/CLI/integration steps all passed, E2E finishing. Other checks/review already green. Local unit build82318 still compiling duplicate validation; CI already supplies application test evidence. PR head8169795754d100b0ea1ab829040d7762e487ce3a. Neither214 nor527 merged yet.

Production before maintenance release: one Dedup.worker_error since external6867aa2 restart,5,696,512 rows deduplicated and28,399,943,263 observed progress rows. These are per-container counters, not comparable directly with previous totals. Evidence `maintenance-pre-recovery-0907.json`.

### 2026-09-07 09:20 UTC checkpoint

Monoscope527 all CI checks passed (34102401843), including E2E. Merged exact reviewed head816979575 as616656462146318df4732d0ea7abc5b6668bf1ef. Deploy34104889701 is building/pushing the image; dependency-image34104889791 is separate. This is the22nd delivered software fix once deployment is verified (currently merged, not yet live). Local duplicate unit build PID75338 was verified with cwd `/tmp/monoscope-openai-cancellation` and interrupted after complete CI passed; do not report that local build as passing. Fork14HTTP tests and full application CI are the validation evidence.

TF214's initial Format check failed only because rustfmt collapses the fixture builder chain. Applied cargo+1.91fmt and checked it. Autofix bot concurrently pushed657bb1b1 with the exact same resulting tree as local0dfa6cbb. Reconciled with a normal merge (no force-push): final head4e639eb66b07bdfa102d1ff79571e9a23f1a4bfd, clean worktree. New CI/review running on that head. The only change after1477PASS is whitespace in the fixture. Do not merge old4334f6e0 or assume old failedFormat applies to final head.

Latest monitors all normal; dwell1078s/pressure14% (09:16 evaluation), WAL11,534,339,072bytes (09:05 evaluation), others0. `monitors-0920.json`.

### 2026-09-07 09:37 UTC checkpoint — temporary profiling MUST be restored

527 deployed successfully (34104889701), initial three61665646 replicas healthy. Subsequent memory observation shows two new replicas near10GB, so cancellation does not resolve the separate memory issue. Old47b49ab624f7 Docker State confirms OOMKilled=true,exit137,12GB cgroup limit (swap24GB),09:16:56–09:23:49. Host kernel journal had no OOM entries; Docker state is decisive. No mounts. `/opt/monoscope/profiles` was empty because GHCRTS override was only `-M10G` (replaced the Dockerfile profiler defaults). Private evidence `replica-state-0927.json`, `replica-memory-mounts-0928.log`, `oom-47b49ab-final.log`. One widgetPng request09:21:32, three replay merges09:22–09:23, maxKafka inflight476,378,868bytes; no attribution yet. Replay merger already streams25files/byte-bounded shards, PNG dimensions alreadyclamped2000 and process deadline30s; do not invent missing guards.

Authorized TEMPORARY service diagnostic at09:37: `docker service update --detach=true --env-add 'GHCRTS=-M10G -p -poprofiles/monoscope -l-a --eventlog-flush-interval=10 -hc -i60' srv-captain--monoscope`. Original exactGHCRTS=`-M10G`. This is a rolling diagnostic, not a CapRover persistent-config edit. **Restore `GHCRTS=-M10G` after collecting profiles/diagnosing (or verify a later CapRover deploy restored it).** Profile paths `/opt/monoscope/profiles/`, Dockerfile late-cost-centre profiling already enabled. Capture profiles before Swarm discards old containers. Do not overwrite user config changes blindly.

214 review found one bad partition aborting other sources. New local tests pending in `/tmp/timefusion-maintenance-recovery/src/database/mod.rs`; PRODUCTION FIX NOT YET WRITTEN. Parameterized using existing test_case (not rstest). Valid metadata-only path test genuinely failed before (`cannot reconcile partition for metadata-only.parquet`). Initial null-date fixture failed during Delta snapshot construction, so it did not reproduce the intended isolation issue. Corrected fixture emits Metadata action with partitionColumns=[project_id] for a legitimate legacy layout, keeping date as a data column, then Add without date metadata. Before-v2 test compiling, session99181, `reconcile-partition-isolation-before-v2.log`. Do not change production code until before-v2 finishes. Earlier orphaned failed build PID32634 and descendants32641/32642 were verified in our worktree and terminated; other user builds untouched.

Planned production recovery adjustment: use `LogicalFileView.partition_values()` with `deltalake::kernel::ScalarExt::serialize` to recover stored partition metadata (avoid deprecated add_action and avoid stats materialization). Label outer source loop; if partition still unknown, warn and continue to next source with failed source cursor unchanged. Do NOT skip an unknown file then advance its cursor. Validate date before enqueue if needed. After focused before/after, fmt/lint/full all-feature tests, push updated214, await exact-head CI/review before merge. Currently published214head4e639eb6 is green except ongoingRust analysis but MUST NOT MERGE while review fix remains.

### 2026-09-07 09:49 UTC checkpoint

214 review correction implemented locally and all5 focused reconciliation tests PASS (2.351s). Before-v2 BOTH cases failed with `cannot reconcile partition for metadata-only.parquet`; the corrected legacy layout fixture uses Metadata::try_new preserving table ID and omitting date from partition columns, avoiding the invalid-null-schema setup. Production code uses parsed partition scalars via `deltalake::kernel::scalars::ScalarExt::serialize`, validates dates, and labeled source continuation leaves unknown-source cursor unchanged while healthy source baseline proceeds. No deprecated add_action/stats serialization. New full lint/all-feature suite running session55858, logs `reconcile-partition-isolation-{lint,full-tests}.log`,8testthreads. Current uncommitted changes maintain.rs/mod.rs; do not merge214oldhead4e639eb6.

Profiling diagnosis: `src/System/Server.hs:444` explicitly stops CPU and heap timers at startup; SIGUSR2 toggles them. Thus empty .hp/initial-only eventlog was expected with timers parked, not an event parser failure. Installed ghc-events0.21.0.0 locally in `/tmp/incident-sept6/bin/ghc-events` (fixed its macOS /tmp relative symlink to absolute .cabal/store target). 5d18e4f6c184 OOMed before profiling was armed; preserved14MBinitial eventlog in `oom-5d18e4f6-profiles.tar`. No useful heap attribution yet.

At09:48:57 sent USR2 ONCE to20966cf0979f (replacementslot3),78150782e6e7,142c8431575a. These still have temporary profilerGHCRTS set at09:37; all signal handlers confirmedknowncode, 209log confirms `enabled via SIGUSR2`. CPU records60s window every15min, heap every60s. DO NOT send USR2 twice to same current container unintentionally (toggles off). New containers startdisabled and may needoneUSR2 whilediagnosing. Capture profilefiles beforeoldcontainersdisappear. **Restore GHCRTS=-M10G after diagnostic/fix verification**, as notedabove.


### 2026-09-07 10:05 UTC checkpoint

TF214 partition metadata/isolation correction: full 1,478 enabled tests passed (17 existing skips), lint and formatting passed; committed 26918fa1 and publishing for CI. Temporary profiling watcher PID 67774 stopped, and Swarm GHCRTS restored to original -M10G at 10:04 UTC. Saved four final profile archives under /tmp/incident-sept6/profile-final-*.tar. Failed ba7be5a36e24 retained 2.882 GB sampled live heap, with OTLP conversion/JSON/column building major contributors; healthy replicas typically 0.45–1.0 GB. Replacement 40639715face sampled 2.318/2.888/1.471 GB. CPU profile confirms PatternMerge Jaccard allocation work under flushDrainTask, but this does not prove the OOM root cause. Further memory investigation remains open.


### 2026-09-07 10:12 UTC checkpoint

External TimeFusion master advanced to cf197f83 (DV-dedup self-remint suppression and staging counter). Merged into TF214 locally, resolving overlap by preserving per-hour mint_dedup for retained commits and always minting conservative Dedup+Rollup on history gaps. Combined lint passed; all-feature 1,479-test run is active (/tmp/incident-sept6/recovery-remint-integration-tests.log, session 19450). Merge is not committed/pushed yet.

Follow-up issues found one new JSONB SQL-type error at 08:58:42 (issue 01020f90-b322-4ab0-a01b-d965ba6705ac). Captured query is an error-chart shape but includes CASE WHEN events IS NULL OR events = null::jsonb, absent from current and deployed parser source. Caller origin remains unproven; do not claim current application emitter creates it. New clean investigation worktree /tmp/monoscope-error-chart-jsonb has no edits. Existing TimeFusion tests cover current exception COALESCE expression. All six monitors remain normal at 10:10; embedded counts 4776 errors / 6504 logs. External Monoscope report rollout ebb0e4fd completed around 10:11, resetting replicas during normal-runtime memory observation; GHCRTS remains restored -M10G. Before that rollout replicas were about 1.3/2.8/2.1 GiB, without observed OOM during the short window.


### 2026-09-07 10:20 UTC checkpoint

Combined TF214/current master tests passed: 1,479 enabled, 17 existing skips, 507.529 seconds; lint and formatting passed. Publishing resolved merge, then await exact-head CI before deployment. Pre-deployment maintenance snapshot still has source cursors logs=492766 and metrics=152984 (snapshot mtime 1788765311.2273083). Read-only verification scripts ready: active-object-audit-post214.py and verify-filter-production-post214.py. Current exception expression returned 21 events in 0.263 seconds; JSONB variant caller remains unproven. Normal-runtime memory watcher session 28074, script watch-normal-memory.py, log memory-normal-ebb0e4fd.log, scheduled ~15 minutes from 10:14. No production diagnostic override remains.


### 2026-09-07 10:28 UTC checkpoint

TF214 published combined head a0bdfb251e018fc024d1525c053431fcf0dd5098, full local 1,479 tests/lint/fmt passed. Review flagged inherited RECONCILE-DBG stderr dump now repeated by our minute loop. Removed it locally and corrected coarse recovery count to one shared Dedup plus every tier per partition; tightened existing history fixture count accordingly. Cleanup is UNCOMMITTED: session 12373 runs lint then three focused history/dedup regressions; logs recovery-review-cleanup-{lint,tests}.log. Do not merge published head until this cleanup is validated, committed/pushed and exact-head checks pass.

New Monoscope PR528: https://github.com/monoscope-tech/monoscope/pull/528, head 669293c08, branch fix/jaccard-allocation-20260907, worktree /tmp/monoscope-jaccard-allocation (base 6f897652e). Jaccard union cardinality now uses input sizes minus existing intersection, avoiding union allocation. Exact 65,536 set-pair equivalence passed. Optimized GHC 9.12.2 standalone production-helper benchmark: 499,500 comparisons, allocation 1,251,894,624 -> 472,532,736 bytes, CPU 0.441 -> 0.227 seconds; identical score. Artifacts Jaccard{Before,After}.hs, jaccard-{before,after}-bench.log, jaccard-equivalence.log. HLint/fourmolu/diff checks pass, constraint review no weakening or suppression. PR review no correctness concerns; optional extra doctests not required given exhaustive equivalence and existing merge coverage. App CI 34111285161 running. Not merged/deployed; do not claim OOM fix.

Read-only cursor helper check-maintenance-cursors.py now folds snapshot plus source_cursor WAL records (checks snapshot mtime across read). Baseline remains logs=492766, metrics=152984, saved maintenance-cursors-with-wal-pre214.json. Normal-runtime memory watcher 28074 continues; all replicas unchanged and healthy since external report deployment at 10:11. Peak observed ~5.7 GiB then fell; current ~3/4.7/2.7 GiB. No OOM observed in this window.


### TEMPORARY PRODUCTION OVERRIDE — 2026-09-07 10:33 UTC

Enabled lightweight GC-only event logging via Swarm service update: GHCRTS=-M10G -l-ag --eventlog-flush-interval=10. MUST RESTORE original GHCRTS=-M10G after collecting evidence (or verify subsequent CapRover deployment restored it). No CPU profiling, heap census, SIGUSR2 watcher or limit change. Existing boot command chooses profiles/monoscope-<epoch>.eventlog under /opt/monoscope. App stopProfTimer/stopHeapProfTimer do not stop GC event logging. Previous normal-runtime window showed no OOM but one replica reached 7.718 GiB at 10:31:15. Goal is distinguish managed live/allocated heap from total RSS; earlier -hc sample and RSS were not sufficient. Capture files before stopped containers are removed. New override log enable-gc-events-1033.log.


### Runtime override RESTORED by concurrent deploy — 10:33 UTC

External Monoscope docs rollout 6f897652eb5cb8625e2b4baa91ae75969fa32ef1 superseded the GC diagnostic after about 20–30 seconds. Service inspection confirms GHCRTS=-M10G again: NO ACTIVE PRODUCTION OVERRIDE. Captured gc-events-1033/{24bb830420ec,5e424bd98f6c}.{json,tar}; both OOMKilled=false, rollout exits (0 and 137), not OOM evidence. GC watcher session33258/PID93069 stopped at10:35 to avoid pointless polling. Normal-memory watcher28074 completed its interval. Resume low-overhead GC diagnostic after deployments settle; do not repeatedly race external rollouts.


### 2026-09-07 10:41 UTC handoff

Still active: TF214 final cleanup session12373, lint passed in5m13s; focused nextest recompilation remains active under concurrent local build load, no result yet. Do not change source while it builds. Published a0bdfb25 has all CI tests/E2E passing; final cleanup still needs commit/push/checks. Monoscope528 head669293c082fdc296edb49dd40f26bdfc9fd7ac23 has all review/lint/format/CodeQL checks passing; Build and Test workflow34111285161 remains in Build step (started10:27:37). Neither PR is merged. Both must be delivered and monitored. No active production runtime override: external6f897652e deployment restored -M10G. GC-only archive24bb830420ec parsed successfully: live data, heap size, block size and fragmentation events present, but only22 seconds before rollout, so not OOM evidence. Parsed text gc-events-1033/24bb830420ec.events.txt. Previousnormalruntimewatch recorded20min withoutOOM, rising to7.718GiB beforediagnosticrestart; historicalOOMrootcause remains unproven.

### 2026-09-07 11:29 UTC checkpoint

TF214 final head30756b2c passed all CI including Rust CodeQL; squash merged as9fca1ccbb4c25a65347c3759cd3c98aec19454ce. Deployment34116665191 pending. Production TF currently f1831ac from concurrent changes; post214 cursor/object/query verification remains required. Final cleanup lint and three focused tests passed; combined pre-cleanup full run1479 passed, and final-head CI passed.

M528 squash merged e4e90620c76c08fa5b56763432e7b1661e436e31. Deployment34113831607 failed: Docker frontend could not resolve @codemirror/state; concurrent a30fdeab7 fixes lockfile installation. Integration also failed on obsolete query skeleton opacity-60 assertion after native textarea conversion. Latest master02474f672 deployment34116451534 running. M528 is NOT yet verified deployed; production remains6f897652e.

Normal-runtime OOM0f5299d053f7 confirmed10:47:09, State preserved in oom-0f5299-final-state.json. Replacement290cd55898fb disappeared before State collection; cannot classify its exit, object already pruned. Current replicas f5f613ba294b/6d2aef638980/2d9ccf30af53. GHCRTS remains-M10G, no diagnostic override.

Read-only named ELF RTS counters via /proc/PID/mem now available in read-live-rts-stats.py; exact-image guards prevent reuse of addresses on new binaries. Unlocked snapshots, not atomic measurements. live-rts-stats-1045.jsonl shows hot0f max live6.897GB, RTS heap10.738GB, block fragmentation5.502GB, RSS12,764,912KiB before OOM. Replacement290 at10:55 maxlive7.393GB. RSS is not equivalent to cgroup charge. Hot Kafka inflight552,387,098 bytes. Actual batch config5000, concurrency default4; poll admission checks occur before full count-bounded poll, allowing byte overshoot. No memory-budget code changes implemented yet.

Read-only Kafka sample10:38–10:40:5000 records186,427,206bytes, largest945,213bytes. Largest sample29 log records,239 resource bytes; body449707 and attributes493866 bytes dominate. Resource sharing alone is not supported as primary cause by this sample. No consumer group/offset commit or payload logging. Private artifacts ingest-metadata-1038-1040*, ingest-large-79976734*. Live counter refresh live-rts-stats-1128.jsonl collected without restart/pause.

### 2026-09-07 11:45 UTC checkpoint

TF214 deployment34116665191 is building its image after previous0af67498 rollout completed. No post214 verification yet. M528 still not verified live: external master pushes superseded deployment02474; newest run34117567691 head44b2bf594 in progress.

Opened Monoscope531 https://github.com/monoscope-tech/monoscope/pull/531, worktree/tmp/monoscope-query-placeholder-test, branchfix/query-placeholder-contract-20260907, latesthead9f081248f. Replaces obsolete opacity-60 integration assertion with native textarea placeholder plus empty editable value checks. CI discovered another inherited runner issue: Vitest tried importing Playwright bench/editor.spec.ts; excludes bench/** while retaining configDefaults.exclude. All55Vitest suites/911tests pass locally with Node25; final-head UI CI passes, lint/format/review/CodeQL pass, app Build and Test pending. Native default Node20 was too old; reran npm ci with installedNode25 to obtain correct native binding. No dependency/version edits. Actual frontend build passed and generated assets copied into isolated ingestion worktree for Template Haskell manifest requirement.

Confirmed normal-runtime OOMf5f613ba294b at11:28:36, statefileoom-f5f613-final-state.json. Just before exit liveRTSmax7.120GB/currentlive7.188GB. Monitors11:41 allnormal: oldestbucket794s, pressure24%,WAL18,874,370,048bytes. Latest100openissues include no creation after08:58JSONBcaller issue; this is not an exhaustive all-pages claim.

New uncommitted worktree/tmp/monoscope-ingest-memory-budget, branchfix/ingest-memory-budget-20260907 base02474f672. Adds Pkg.IngestBudget (FIFO QSemN shared64MiB raw processing reservation; oversizedvalidbatch reservesfullcapacity/runsalone), AuthContext and testcontext wiring. Budget NOT USED BY WORKERS YET. Queue.pollKafkaBatch currently wraps OLD count-only poll for before-regression; consumerloop still calls old API directly. New integration case32shared8MiBrecords,5000countlimit expects only8consumed and no commit. First before-build failed on missing generated frontend manifest (not behavioral failure). Assets built/copied, before-v2 build session5536 active; DO NOT EDIT SOURCE until it ends. /tmp/incident-sept6/ingest-poll-before-v2.log. Earlier80872terminalexit1.

Standalone tests against actual Pkg.IngestBudget source passed2cases (smallsharing/oversizedsolo; cancellation of holder/waiter and exception restoration), artifactsIngestBudgetSpec.hs,ingest-budget-build.log,ingest-budget-tests.log,binarybin/ingest-budget-tests. These tests are NOT yet copied into unit suite. Next implement bounded single-record polling (first100ms,subsequent0; countandbytes; catchKafkaError preserveserrorlist), wire budget around entire worker processing/outcome/ack, and avoid data polls while paused (async callback loop preserves heartbeats). Preserve offsets,DLQhold/reseek,durability gates. Need before/after integration result, production throughput/memory verification before calling OOM resolved.

### 2026-09-07 12:00 UTC checkpoint

TF2149fca1cc deployed successfully, workflow34116665191 success; production3eb1767da780 started11:48:51. Metrics history-gap recovery logged152984→170427 across480partitions; durable cursor subsequently170437, confirming minute reconciliation advances beyond boot. Logs cursor remains492766: recovery correctly rejects active null-date metadata. Eight source Parquet files under project_id=__HIVE_DEFAULT_PARTITION__/date=__HIVE_DEFAULT_PARTITION__ contain11physicalrows, timestamp/id nonnull datedAug2–3. Read-only inspection null-partition-inspection.jsonl, privateidentity follow-upinprogress; NO mutations/deletes/guessedtenantassignment. Need proper nullable partition handling or evidence-backed data repair; do not silently skip unknown partitions or claim logs recovery complete.

Post214 base-table audit PASS:logs v5355611705paths,metricsv1704271783paths;3488/3488present,zero duplicates. Helper's initialobject_status404 refers previouslyretiredmissingpath withactive_entries0, expectednotnewmissingfile. active-object-audit-post214.json/log. Fixed5minquery21264rows/765504idchars unchanged,1.056sec;EXPLAIN1.489sec,Dedup21.27Kinput. Maintenance at~11:53:21dedupbins,18dedupwaves,9packingbins;no dedup failure/timeout,provesworkadvancingbutnotallbacklogcomplete.

M531 finalhead9f081248f all checks except Build and Test passed; currentstep integration-tests. M528 still not verifiedlive until deployment includes it. M532 published https://github.com/monoscope-tech/monoscope/pull/532 headde0cc11d7a5a9db688a3c94d50342f3c10e37ae9; worktree/tmp/monoscope-ingest-memory-budget clean. Contains memoryfix3c5717fe3, mergedcurrentmaster44b2bf594 and531 branch fixes. Merge531first, then rebase/mergeupdatedmaster into532 asneeded; awaitexactheadCI/review beforedeploy.

M532 implementation now complete: count+64MiB polling via KE.pollMessage (first100ms/subsequent0) withtryError(_,KafkaError) preservingerrorlist; bytesincludevalue,key,headers; no data pollswhilepaused (clientasync callbackloopcontinues). Shared AuthContextIngestBudget FIFOQSemN64MiB wraps entireworkerprocessing/outcome/ack; oversizedbatch reservesfullcapacity/runsalone. No runtimeoverrideor limitraise. Before actualtest-devbinary failed1case (32records/256MiBconsumedvs8expected), ingest-poll-before-direct.log. After actualcompiledtest-devbinary13Kafka-relatedintegrationexamplesPASS67.6s, including5LiveTail+8Kafka tests, ingest-memory-after-direct.log. Newsharedbudget2standalonecasesPASS; copiedintounit/Pkg/IngestBudgetSpec.hs. HLint/fmt/diffchecksPASS. Finalpure$>stylecleanup occurredafter13testpass; CI validates combinedbranch. Two transienttestcompileerrorsfixed(explicitnumeric/listtypes,name-shadowing), no warnings suppressed. Cabal builds allcomponents eventargettest-dev: usedlinkedactualtest-devbinaryforfocusedtests, then stoppedownunneededbuilds28939/39423 aftercwdverification. Sessions5536/62094exit130intentionally; noactiveownlocalbuild. Consumer test6365terminalPASS. NewPRCIpending; OOMnotdeclaredresolveduntilproductioncomparison.

### 2026-09-07 12:15 UTC checkpoint — continue with guarded null-partition repair

M531 all checks passed (BuildandTest23m22), squashmerged58877a42482edea3a81a026f074fdd1dbc112ec2. Deployment34119960020: image/frontend/gatepass; BuildandTeststillBuild. Do notclaimM528or531productionrolloutcomplete untilimagesverified.

M532 review correctly raised fullchunkserialization whenchunk=budget64MiB. Fixed by dividingnormalworkchunk target byconfiguredconcurrency (default4→16MiB each), totalsharedbudget stays64MiB, pollbudgetstays64MiB, highwater256MiB/low64MiB. Oversizedrecordsrunalone. Newbarriertestrequiresfourfullnormalchunksconcurrentlycomplete beforeacks. Actualcompiledtest-dev14Kafka-relatedtestsPASS61.46sec; HLint/fmt/diffpass. Commit3d69ae986; merged531/master58877; publishedfinalhead9ad733efbebe48b563808a16537da24c14a07ba2. CI34120389113, review34120389347, CodeQL34120386934 pending. PRbodyupdated. Worktreeclean. Testbuildsession62136finishedbuildand14tests; noactiveownlocalbuild. Explicitcabal build monoscope:test:test-dev avoids earlierall-component rebuild. Runtimeper-recordpollthroughput remainsproductionverification item. BudgetNaturaltypedAPI retained; review's Int-only suggestion would lose nonnegative boundary and riskoverflow beforeclamping.

NULL PARTITION BREAKTHROUGH: all11physicalrows have nonnulltrace/span IDs and UUIDv5eventIDs. `prove-null-partition-projects.py` enumerated2656existingprojectUUIDs and reproduced Monoscope's exact deterministicOtelId span algorithm: namespace6f1a7c30-9b2d-5e84-8a3f-0c1d2e3f4a5b; JSON-quotedprojectUUID,trace_id,span_id joined0x1f. EACH row uniquelymatchesoneproject. No service-name inference. Proofprivate0600null-partition-project-proof.json, conciseproof.log. Allrowsineachfileagreeononeproject+UTCtimestampdate. Mappings:
- cf1e4407...1row,d062e010-e3d0-4673-8dfe-d652d6826f49,2026-08-03
- 07c6b89f...1row,samed062,2026-08-03
- 8c887f46...3rows,28f62f01-46a1-400e-8195-da7bc3505b5b,2026-08-03
- 3380b56a...1row,same28f62,2026-08-03
- 0a60caef...2rows,same28f62,2026-08-03
- 8c24f112...1row,same28f62,2026-08-03
- 67046f70...1row,d062,2026-08-02
- 56ee6d9a...1row,dcad860a-9a98-4c9e-9e69-20d52dcf90e2,2026-08-03.

All8filesphysicallyOMITpartitioncolumnsproject_id/date, so exactParquetbytescanbe relocated undercorrectmetadata without re-encoding. sourcefooters11rowsid/timestampnonNull,deletedallNULL (not tombstones). Checkpoint535641 confirmsall8AddpartitionValuesNULL, deletionVectorNONE,tagsNONE,statsnumRecords1/3/2matchingphysical. SourceobjectmodificationtimesAug4~22:40–22:45,updated_atstatsAug4 (suggesthistoricalDMLrewritebug,NOTyetprovenexactcommit). Existing schema dateDate32 NONNULL;project_idnullableforDelta-rsstatsbuildercompat. SQLproject_idISNULL AND dateISNULL optimizedtoEmptyExec because datenonNull: read-onlyqueryreturns0, notproofemptyphysicalfiles. Date-boundedidwitnessqueryalso0. Filesmustnotbedroppedassupposedempty.

Artifacts: null-partition-inspection-v3.jsonl(footer/deleted), null-partition-identities.json(privateIDs), null-partition-actions.jsonl(flattenedAddstats), null-partition-dv.jsonl(checkpointfullAddselected), null-partition-query.json, null-partition-explain.json(EmptyExec), null-partition-witnesses.json(0rows), prove-null-partition-projects.py + proofJSON/log. Existingcheck-missing-parquet-post214.py helperread-onlysafe; printsretiredoldmissingpath404with0activeentries(expected). No repairmutationhasoccurred.

Next authorized action: prepare dedicated guardedmetadatarepair for8files/11rows. Newisolatedworktree/tmp/timefusion-null-partition-repair branchfix/null-partition-repair-20260907 base9fca1ccb exists,CLEAN,NOcodeyet. Use newone-offexample/tool, DON'Trerun frozenoldrepairutilities. Can READ `/tmp/timefusion-incident-repair/examples/repair_duplicate_paths.rs` for DeltaCommitBuilder API/protocolguards only. Generate sealedmanifestwithsourceSHA256+fullAddidentity+UUIDproofproject/date+numrows. Verify noDV,rowTracking/columnMappingunsupported,sourcepartitionvaluesNULL,exact8paths,fullphysicalrowsmatchproof,allfile rows agreeproject/date,partitioncolsabsent. Copybyte-identicalobjects tocorrectproject/date/newUUIDpaths;verifySHA256bothcopiesandunchangedsource. RefreshDelta state,recheckexactsourceAddanduniqueidentity; atomicallyRemoveold/AddnewwithCORRECTpartitionvalues,dataChange=true(logicalrepair),max_retries0,explicitrepairmetadata. OnOCCconflictreload/reverify; no blindoverwrite/no oldfiledelete. Preserveallnonpartitiondata. A correct repair mayincludeinactiveprojectd062's HISTORICALrows; thisdoesNOTactivateprojectorreplayDLQ andmustnotdoeither. Aftercommitverify11visibleIDs/rows,zero activeoldnullpaths,allreplacementbytes/objects,thenminute logs recoverycursoradvance+maintenanceprogress. Finishbroaderbaseobjectaudit/monitor. Do notchangevalidateto silentlyskipnullpartitions. ConsiderseparatepreventiveDMLregression oncehistoricalcauseidentified.

### 2026-09-07 12:35 UTC checkpoint — null-partition repair COMPLETE; NEVER REAPPLY

Applied the dedicated guarded repair as Delta logs version535817. Eight old null-project/null-date Add entries atomically removed, eight byte-identical Parquet copies added under UUID-proved project and UTC timestamp date; dataChange=true because logical partition metadata is restored. No old objects deleted, no DLQ replay or project activation. Sealedmanifest SHA25651f29b2e0965db46b8eb81635b359e5e869b0601084604b7436c95eb49fb58dd. Executable archivedbin/repair-null-partitions-20260907 SHA25616acf3f793e7aed117193ee6fbb58262ad97e058c6ed250dd2e49051f9b64665. Source example/tmp/timefusion-null-partition-repair/examples/repair_null_partitions.rs (untracked, DO NOT publish as normal product feature), alsoarchivedrecovery-tools/repair_null_partitions.rs. BuildallfeaturesPASS2m49,clippy-DwarningsPASS1m24(existingvendor4warningsonly). No Cargo/production code change. Initialmissingexamplesdir/fmtCLI invocation failedbeforeanysource/build; corrected, subsequentbuildv2passed.

Independentlocalfixturetest-null-partition-repair.py PASS sixguards: badhash, changedAddsize, nonnullsourcepartition, duplicateproofpath, changedphysicalbytes, staleinventory afterdelete. Atomicpositivefixturepreserves11rows/allphysicalbytes; secondapplyfailsbeforestaging. Fixture/tmp/incident-sept6/null-partition-fixture-iz_saszh and-stale are LOCALonly. Productionprepare-null-partition-repair.py re-provedall11UUIDs fromfreshobjects across2656projectIDs, checkedpartitioncolumnsabsent/oneproject+dateperfile/fullSHA, sourceAddfullidentity andnumRecords. Production--checkpassedv535811. Repair--applywrapperassertssealedmanifesthash andexistingresultfileguardsrerun. Successfulresultnull-partition-repair-result.json/privateapply.log/status.log. NEVER rerunapply-null-partition-repair.py or tool--apply onproduction; repairdone.

VerificationPASSv535819: zeroactiveold/null-datepaths, all8replacementobjectsSHA256match18,700,549bytes,11physicalrows. ActualTFSQLqueryreturns11rows0.875sec; exact(id,timestamp)->(project,date) matches sealedexpectedrows. null-partition-repair-verification.log, null-partition-repaired-row-query.json, null-partition-repair-expected-rows.json. At12:28:09logs recoverylogged392partitions,492766→535819;12:28:12coordinatorreported1234tasks. Subsequentminute106tasks andsourcecursor535827 then535852;metrics170512→170520→170538. Both sources now recovered and progressing; no unknownpartitionerrorafterrepair. timefusion-after-null-repair.log includespre-repairwarnings12:26/12:27; don'tmisclassifyasnew. FullpostrepairactiveauditPASSlogs5358671689files,metrics1705481784files:3473/3473present,zero duplicates; active-object-audit-after-null-repair.json/log. Oldpost214audit3488baselinepreservedseparately.

M528andM531nowproductionverified: all3replicas58877a42482edea3a81a026f074fdd1dbc112ec2 healthy at~12:32 (08249b276be9,d0b9e2b1204a,058887d741ea). Deploy34119960020SUCCESS. Earlier35177deploywasinterleaved; finalall58877nowconfirmed.

M532head9ad733 CI34120389113failedwhilebuildingunitcomponentduringdocteststep: newunitfixtureimportedUnliftIO.Async/Exception withouttest-targetdependency. Fixed IO-onlytest toalready-declaredControl.Concurrent.Async + qualifiedControl.Exception.try@IOException/throwIO; productionbudgetunchanged. Published f6a8c36e18c3b7a153184a3d57f4852b2c918f87. LatestCI34122334029/job101743354100pending; review/CodeQLnewrunsmustcheckexacthead. Local ACTUALunitcomponentbuildsession28706 active: cabal build monoscope:test:unit-tests --builddir=/tmp/incident-sept6/isolated-build; logingest-budget-unit-component-build.log currentlylib55/130rebuild. Do not editworktreesourcewhilebuildactive. Oncebuilt runactualunitbinaryIngestBudgetmatch (andrequiredunit/doctestschecks); previousstandalone+14Kafka testsstillpassbutdon'tclaimnewunitcomponentpassuntilobserved.

LiveRTSreaderupdatedfor58877ONLYafterELF/symbolverification: EXECnonPIEAMD64,stats0x16a784c0size0x178,mblocks0x16a791d0size8. old6faddressesretainedkeyedexactimage. Addedread-onlycgroupv2memory.current/events alongsideRSS (RSS!=cgroupcharge). rts-symbols-58877.txt,live-rts-stats-58877-initial.jsonl. Watcher/tmp/incident-sept6/watch-rts-58877.py session51847 active45samples@20s (~15min), writesrts-58877-before-memory-fix.jsonl; no runtimeoverride,no signals/restart/debugger. Helper skipsnewunknownimagesuntilsymbolsverified. Need memory/throughputcompareafterM532deployment; noOOMresolutionclaimyet.

Historicalpartitioncorruptionintroducerstillunproven: all8objects/updated_atAug4~22:40–45, contextstronglyMORenrichmentrewritesbutdon'tassertexactcodecause. ExistingDMLtestscoverUPDATE/projectandoldtimestamps; inspectphysicalpost-flushpartitionassertion ifneeded. Currentfiles/src/write/mod.rs section6592insert_coerce,src/database/write.rs stagedwrite~690/fallback~875,src/dml.rs perform_version_append. No newpreventiveRustcode/testimplemented. Repairwasfullproof-backedmetadata restoration, notsilentmaintenancevalidationbypass.

### 2026-09-07 12:48 UTC follow-up

M532 remains OPEN exact f6a8c36e18c3b7a153184a3d57f4852b2c918f87. Review, formatting, HLint, frontend and CodeQL passed; CI34122334029 now passed Build/weeder and is running doctests. Local actual unit-component build28706 still active (GHC60202 consuming CPU, last module101/130); no source changes during build. Review nonblocking notes: benchmark per-message polling throughput; optional first-error bailout (current loop bounded5000 errors) and reservation doc wording. No additional production change yet.

External M530 deployment replaced all replicas with69766950bbf0c8da0c0592af3c679cbaa9da32a3 (saved reports/email charts), preserving previous fixes. Verified new ELF non-PIE AMD64; stats0x16a8c200 size376, mblocks_allocated0x16a8cf10 size8. Added exact image allowlist entry to read-live-rts-stats.py. Evidence rts-symbols-697669.txt and live-rts-stats-697669-initial.jsonl. Existing watcher51847 can now also capture697669; its filename rts-58877-before-memory-fix.jsonl contains per-row image so filter by actual image. Previous58877 samples: hot replicas peak cgroup11.995/11.997GiB, max-live6.246/5.205GiB, no sampled oom_kill; rollout truncated observation. New697669 replica f8984bf2949d exited137 at12:46:00, OOMKilledFALSE, after three5s health timeouts. Preserve distinction: not confirmedOOM. State state-f8984bf2949d.json and final logs archived. Replacement dacebc4b6b38.

TimeFusion cursors12:45 logs535916 metrics170581, continuous minute reconcile events (e.g.308tasks12:43). Maintenance counters264dedup bins223waves,215packing bins, zero dedup/packing failure and timeout counters. Logs timefusion-followup-1243.log ten-minute window classified timefusion-followup-1243-summary.json: zero error-level entries;14slowrollups,13journal_hold/12journal_lock_wait(max921ms),17rollup_declined_shape,6dirty_bins_retired_undrainable,4DV scan/oracle mismatch warnings,1tantivy_wave_reindex_failed. Index failure12:37:46 on part68c7af24... only logs upload context because %error drops anyhow cause chain; exact underlying cause not proven. Journal checkpoint fsync already uses without_blocking_the_worker, but journal() mutex lock itself synchronous; investigate before claiming fix. Do not equate timed guard hold with actual runtime block duration because inner fsync is block_in_place.

Embedding read-only followup: errors6262embedded(latest11:04:42),5851pending vs earlier4776/7333; logs6504embedded(latest09:19:15),2898793pending. Scheduled jobs next12:45/13:00/13:15; no new controlled job, replay, or project activation. Repair remains complete and must NEVER reapply.

### 2026-09-07 12:53 UTC checkpoint

Previous turn classified progress (new production observations and diagnostic image mapping); current turn completed actual local verification. M532 exactf6a8c36e actual unit-component build28706 EXIT0. Actual unit binary IngestBudget2examplesPASS0.0722s (ingest-budget-actual-unit-tests.log), then entire unit suite305examplesPASS2.7915s (ingest-budget-all-unit-tests.log). Test sessions89632 and13930 terminalPASS. CI34122334029 now passed doctests/unit/CLI and running integration-tests12:52:50; no failed step. PRstillOPEN, notmerged/deployed. Existing14Kafka integration evidence remains. No source modifications this turn.

Refreshed ALL13pages of open issues:1272rows/1272unique, newestcreated08:58:46 JSONBissue; nonecreatedsince09:00. files issues-followup-1250.json, issues-followup-1250-page1..12.json, issues-followup-1250-all.json. Pagination observations not an atomic snapshot but no duplicate/missing count at fetch. All6monitorsnormal at12:48/49 evaluations; oldestbucket768s,pressure17%,WAL25,165,828,096B(last12:34eval),flush/corrupt/unsorted0. monitors-1253.json.

OldRTSwatcher51847 terminalSUCCESS. Newread-onlywatcher51120 ACTIVE30samples20s (~10min), outputrts-697669-before-memory-fix.jsonl. Script run via python heredoc, no runtime overrides; exact-image symbol helper remains. f8984bf2949d finalmetrics had inflight_bytes474,981,734 and0committed_partitions repeatedly12:45:54–57; healthreplaced, OOMKilledfalse, so don'tclaimOOM. Memorypressurefixstillrequiresdeployment+throughput/memoryvalidation beyondpriorfailurewindow. No repairreapply/newcontrolledjobs/outboundmessages.

### 2026-09-07 12:57 UTC — M532 MERGED; deployment pending

M532 all exact-head checks passed. BuildandTest34122334029 terminalSUCCESS23m3s includesdoctests,305units,CLI,integration,e2e; review/CodeQL/lint/fmt allpass,UIjobskippedbygate. Worktreeclean. Squashmerged with --match-head-commit f6a8c36e18c3b7a153184a3d57f4852b2c918f87. Authoritative mergecommit e3ec85f6680aa57e7419a830132c2bc1c6471d89, merged12:56:34UTC. Merge session78479 EXIT0. Deploy34124628975 queued exacte3ec85. Other concurrent externalDeploy34123379525(head115795d67...),34122556762(head1f81794...) inprogress. Do not restart/cancel on observation timeout; inspect actual final rollout images. M532 NOT yet productionverified. Need followdeploy and update RTS symbol map ONLY after verifying exact new ELF/symbol addresses; then monitor memory/cgroup/health plus ingestion commits/lag/throughput beyond old~5–15min failure window. Baseline697669 hotreplica23b36cef06cb already11.816GiB duringwatcher51120. No configoverride or memorylimitincrease.

### 2026-09-07 13:05 UTC follow-up — deployment and broker baselines

Prior turn progress: completed305unit suite, allissuepages, mergedM532. CurrentDeploy34124628975 exacte3ec85 stilllive: Gate/frontendpass, Dockerbuild and BuildandTestinprogress (lastInstallhpack). No restart/cancel/manualredeploy. Productionstill697669, frequenthotreplicareplacements. Confirmed23b36cef06cb OOMKilledTRUEexit137 at12:55:18, start12:49:03 (6m15s); state-23b36cef06cb.json. Previousf898washealthtimeout/OOMfalse; keep distinction. Currenthoteba043cc599f, others3a6a5465eb72/e2a465c7c394. RTSwatcher51120 maystillactive, no overrides.

Addedread-onlyread-kafka-group-progress.py. Credentials parsed from.env in memory, JSON rpk profileconfig sent via SSH/stdin to dockerexec-i rpk --config /dev/stdin (no credentials in command args or output). Initialunauth rpk list and legacyconfigshape failedSASL; version6/current_profile/profiles shape works. Reads only group list/describe; never commits/seeks/joins/replays. Groupapitoolkit_eu PreparingRebalance12–13members, so rpk printsTOTAL-LAG0 withNOpartitionrows: NOTvalidzero-lagproof. DLQstable3memberslag7→2. kafka-group-baseline-1300/1302.txt.

Readbrokerpublic_metrics (unauth endpoint localcontainer) across3brokers; coordinatorbrokerf7a5968cd30f exposes committed offsets duringrebalance plus maxoffsets. Otherbrokeridsb44b51c30971,26550399cdef. Rawredpanda-public-metrics*-1303.txt. Createdwatch-kafka-progress.py ACTIVE session73386:30samples20s, readsall3brokersinparallel, filtersotlp_* committed offsets/maxoffset/requestbytes, storeskafka-progress-before-memory-fix.jsonl. Per-topic/partition committedoffsetdelta is durableingestprogress; requestconsumebytes caninclude rereads, don'tequatewithsuccess. Maxoffset metric HELP latestreadableoffset/highwatermark; beexplicitaboutoffsetconvention when computinglag. ThesearebaselinepreM532; compareafterrollout. Existinguserlocalconsumersmayparticipate, don'tkillthem orclaimallbrokerthroughputfromoneimage.

### 2026-09-07 13:06:36 UTC checkpoint (clock verified)

Deploy34124628975 exacte3ec85 stilllive Dockerbuild+BuildandTestBuild, Gate/frontendpass. Allcurrentproductionreplicas now external115795d67cc8495eda7e948d97247a9cf425dbca (Dockerfile chart renderer fix):3fab64a2eac5,dc0e358a6f71,0390a236fcf4, healthywhenread. M532 NOTdeployed. OldRTSbaseline51120 terminalSUCCESS; reader skips115795 because noverifiedsymbolmap (correct). Don'tinterpretlackofsamplesaslowmemory. Prior checkpoint13:05/1307filenames approximate; authoritativecurrentclock13:06:36.

Newread-only rolloutwatcher98706 ACTIVE watch-rollout-health.py,90samples30s(~45min), memory-fix-rollout-health.jsonl. Records currentimage,fullState/health,containercap,cgroupmemory/events andpreviousreplicaexitstate beforepruning; no /proc/mem or ELFdependency so survivesimagechanges. No mutation/restart/configoverride. Kafka watcher73386 continuesbaseline. First8samples145.5s primarycommittedoffsetdeltas logs10500/spans18831/metrics10058; mixedprefixedimages/rebalancesandpossiblylocalconsumers, notcontrolledthroughputbenchmark. kafka-progress-baseline-summary.json. Needafterrolloutmatchedcomparison.

TimeFusion cursors535989/170609 keepadvancing. Embeddingreadembedding-progress-1307.jsonl remains errors6262latest11:04:42,pending5851;logs6504latest09:19:15,pending2898800. No newprogresssincepreviousquant; nextscheduled13:15. No controlledjobenqueued. Sourceerrors500thenlogs500 sequential; do notassertwhybacklogstalledwithoutlogs/ownershipproof. Read0390replica20minlogsfoundnoembeddingevents (onlyonecurrentreplica, notglobalabsence). HistoricalAug3DMLcommit382ea718 fixesguardloss/idempotency, but no evidenceitcausedAug4nullpartitionmetadata; introducerstillunproven. No newproductioncode/testthisturn; meaningfulprogressisrollback-safeobservabilitythroughrollout and authoritativebaseline.

### 2026-09-07 13:17 UTC follow-up

Deploy34124628975 Dockerimage+frontend+Gate nowSUCCESS. BuildandTest compilationpassed, currentlydoctests, nofailure. Stillnotrolledout. Rolloutobserver98706 caught3fab64a2eac5 on115795 imageOOMKilledTRUE, peak11.95GiB; replacement2a77c79034b1 already11.48GiB. Otherreplicasdc0e358a6f71/0390a236fcf4 lowermemory. Observercontinues45min; preserveallrowsperimage.

Embeddinginvestigation narrowed: central events12:45successfulsaves areOTHERproject28f62f01..., example5167fcf2 saved36logembeddings from3a6a5465. Target87576849 exactattributes.data.project_id query11:00–13:10 returned15saveevents latest11:04:43 only. Failurebodyquery("Failed to embed"/errorPatternEmbedding)returned0; no proofabsenceoflocalworkerfailures. AllcurrentproductionreplicashaveOPENAI_API_KEYpresent andENABLE_BACKGROUND_JOBSTrue (valuesnotprinted). Rootandmonoscope-2 envfileshavekeysandENABLE_BACKGROUND_JOBSFalse,butfilesdon'tprovealreadyrunningcontext. Do notkilluserprocesses.

ObservedNATURALscheduledjob38819935 (13:15) viareadonlyPKquery1s inwatch-natural-embedding-job.py/session23024: queued→missing13:15:02.618, no lockedstatecaptured, so owner/resultUNKNOWN; don'tcallcompletedusefulwork. Exactfuturejob38819936run_at13:30matchespayloadscheduled13:30target87576849, no stale timestampbug apparent. Old12:45/13:00 idsabsent too. getUnembeddedError/Log SQL hasexactembeddingNULL+merge_overrideFALSE predicate,limit500(nohiddeneligibilityexplains5851/2.898M). No controlledjob/replay/enqueue. Naturaljobobserverterminal(missingrow), evidence natural-embedding-job-38819935.jsonl0600. Newcentral13:15queryreturns11saveevents, ownership/projectnotyetchecked. Artifacts embedding-events-1240-1308-full.json, embedding-save-attributes-1245.json, embedding-target-events-1100-1310.json, embedding-failure-events-1100-1310.json, embedding-events-1315.json. Querywith--fieldsbodyonlyreturnedtimestampbecause listendpointsummaryshape, notprovenproductbug. Broad"stale"queryalsohitsourownHTTPaccesslogquerytext/WALGC; don'tclassifyasappfailure.

### 2026-09-07 13:25:13 UTC — M532 DEPLOYED, observation in progress

Deploy34124628975 terminalSUCCESS alljobs (Docker/frontend/BuildandTest incldoctests/unit/CLI/integration/e2e/CapRover). All3replicas nowexacte3ec85f6680aa57e7419a830132c2bc1c6471d89 healthy:6bcec5b87564,07d46c128089,fa119e48ef44 (firststart~13:24). RuntimeconfigurationUNCHANGED: containercap12884901888,GHCRTS=-M10G,countpoll5000,concurrencyenvunset(default4). No overrides/signals. Do notclaimOOMresolutionbefore sustainedloadedobservationbeyond5–15min priorwindow.

VerifiednewELF AMD64EXECnonPIE, stats0x16a94800size376,mblocks_allocated0x16a95510size8; rts-symbols-e3ec85.txt. Exactnewimageallowlistaddedto read-live-rts-stats.py. NewRTSwatcher19083 ACTIVE90samples20s(~30min), rts-e3ec85-after-memory-fix.jsonl. Generichealthobserver98706 stillACTIVE from13:06for45min to~13:51; memory-fix-rollout-health.jsonl catchesexits+cgroup+imagewithoutELFdependency. Kafkaoldwatcher73386 terminalSUCCESS; newwatcher87222 ACTIVE90samples20s(~30min) from13:22; kafka-progress-rollout.jsonl. Compareafterfullrolloutto13:24+; don'tmixrollingupgradebaseline. Groupmetricsincludesanyexistinglocalconsumers; perreplicahealth/memoryauthoritative.

Embeddingnextnaturaljob13:30 observer89566 ACTIVEwatch-natural-embedding-job-1330.py forID38819936, file natural-embedding-job-38819936.jsonl0600. Poll1s exceptshort13:29:59–13:30:10 window30ms sleeps (network adds latency), readonlysinglePKquery,no newjob/claim/trigger. Old38819935tracequerybyattributes.job.id returned0, inconclusive (disabledcallbackreturnsbeforewithSpan or localnon-export). SourcejobsRunner haswhen enableBackgroundJobs guard returningimmediatelyifFalse; historicunconditionalrunnerregistrationpreviouslyreproduced/fixedM521. ExistingoldlocalGHC workerconsumptionremainsplausiblebutCURRENTjobownerunproven. Needcapturenextclaimbeforeattributing. Leaveuserprocessesuntouched. EarlierallcurrentprodkeypresenceTrue; no credentialvaluesexposed. Central13:15saveeventsneedprojectattribution; targetbackloglast11:04stillunresolved.

### 2026-09-07 13:31 UTC — early post-M532 evidence and natural embedding claim

All3e3ec85 replicas remainhealthy/~7minuptime; RTSsamplewindow5.9min peaks6bcec5b875645.89GiB/live2.23GiB,07d46c1280896.18GiB/live2.12GiB,fa119e48ef443.46GiB/live1.09GiB. No observedOOM/restartyet, stillshorterthanlongerpriorfailurewindow; memorynotflatclaim. Genericobserver98706/RTS19083/Kafka87222 continuereadonly.

Newread-onlyread-kafka-replica-metrics.py capturesonlykafka.consumer.metrics JSON from5mincurrentlogs,image-tagged. kafka-replica-metrics-1326.jsonl:peak inflight10.4/6.0/5.5MiB,commit-bearingticks89/303/200 respectively(earlylifetimesvary). No payload/logenvironmentprinted. Postfullrollout brokerwindow125.49s from13:25+ commitdeltaslogs4456,spans21416,metrics10243; approximateoffsetgaplogs18/spans630/metrics260(maxreadableoffset-vsnextcommitconvention ±1perpartition). Notcontrolledbenchmark: groupcanincludelocalconsumers andchanginginput; showsdurableprogresswithsmalllagcurrently.

NATURAL13:30job38819936 ownershipCAPTURED: locked13:30:01.136804 tofa119e48ef44:1, attempt1. Newimageproductionworkerstartedtargetembedding; logs13:30:40saved1error,13:30:42saved6error for87576849. Quant1330 errors6290embedded(+28from6262),pending5824/latest13:30:42;logs6504/pending2898830/latest09:19unchanged(yet). Actualjobstilllockedat13:31, observer89566continues. Thisrulesoutlocalstealingforthisjob; olderfastmissingjobsremainunproven. Sourceframeinitialsequentialerrorstagecanlastlong; waitoutcomebeforefairness/timingchange. Artifactsembedding-fa119-1330.log,embedding-progress-1330.jsonl,natural-embedding-job-38819936.jsonl. Otherproject28f62alsoactivelysavedlogs. No newjob/DBmutations/replay. Needfollowjobthroughlogsstep/cancellation andlongermemorywindow.

### 2026-09-07 13:46:11 UTC — memory validation and embedding fairness regression

M532 e3ec85 remainsall3healthy after~22minuptime/21minRTSsamplewindow,beyondallprevious5–15minfailurewindows. Peaks/currentcgroupGiB6bcec5:5.95/5.56,07d46c:7.05/7.05,fa119:6.60/6.02; maxlive2.23/2.12/2.37GiB. NoobservedOOM/restarts. Continue30minobserversanddurableoffset/lagcomparison; don'tclaimunboundedfuturestability. No runtimeknobchange.

NATURAL13:30job38819936 ranproductionfa119:1 for600s, queued→locked13:30:01.136→missing13:40:01.510. Targeterrors6262→6721(+459of500requested),latest13:39:59.6989;logs6504unchanged/latest09:19.15. No logstageprogress. OddJobspinnedConfigBuilder.hs sets cfgDefaultJobTimeout=Seconds600; jobsRunnerdefaultusesit. Rowdisappearedexactbudgetboundary; timeoutlognotcaptured(jobloggerfiltersinfo), so distinguishdirectobservedtiming+incompleteerrorstagefromunseenexception. Sequentialerror-then-log implementation createsrealstarvationriskunderlongerrorbacklog; previousfastmissingjobsstillseparate/unprovenlocalownerissue.

NEWisolatedworktree/tmp/monoscope-embedding-stage-fairness,branchfix/embedding-stage-fairness-20260907,basee3ec85. AddedONLY regressiontest test/integration/Pkg/EmbeddingFairnessSpec.hs +hpackmonoscope.cabalgenerated. ProductionBackgroundJobsUNCHANGED. Test usesrealtestPostgresandLLMeffectinterposition: errorproviderwaitsonMVar,logproviderwaitserrorstartedthenreturnsvalidvector; actualpatternEmbeddingAndMerge mustpersistlogembeddingwhileerrorblocked, cancellationmustpreserveitanderrorremainNULL. ExpectedBEFOREtestFAIL(on3sboundedconditionwait), notyetobserved. No externalAPIcalls: all3LLMconstructorshandled. Testdatafixtureonly, updatesmergeoverrideinisolatedtestDB. ProviderdiscriminatorEmbeddingStageBlockedError; possiblecompilefixesstillneeded. No warning suppression/newdependencies.

Buildsession74734 ACTIVE cabal build monoscope:test:test-dev --builddir=/tmp/incident-sept6/isolated-build, logembedding-fairness-before-build.log. At105/199EmailTemplatescompilation; DON'TEDITsourceswhilebuildrunning. CopiedactualexistingbuiltfrontenddistfromM532worktree(notstub),fourmolupassed. Oncebuilt runactualtest-dev --match 'Pattern embedding stage fairness' withsafeexternalLOCALtestDBenv as earlier(USE_EXTERNAL_DB=true,DB_HOST127.0.0.1,PG16postgres/postgres); recordBEFOREfailure. Proposedfix: runindependenterror/logembeddingstagesconcurrentlywithinexistingboundedjoblifetime, keepingdurablebatchsavesandcancellation. Evaluateproviderconcurrency/ratebudget,teststageindependence/cancel, nojobtimeoutincrease. Notimplemented/committed/PRyet. ExistingLLMeffectalreadypublicandinterposeavoidschangingtesthelpers/addingtest-onlyproductionAPI.

TFmonitoroldestbucketalertvalue1359 at13:37; liveTFstats13:38oldest789,bufferedflushcompleted778/failed0/rows12,809,448; noERRORlevelin10minlogs. Othermonitorspressure14%,WAL31.457GB,flush/corruption/unsortednormal. Needconfirmnextmonitorrecovery; don'tsilence/raisethreshold. Artifactsmonitors-1337.json,timefusion-stats-1338.json,timefusion-followup-1338.log. Genericrollout98706,RTS19083,Kafka87222 stillactiveuntil~13:51/13:55/13:53; naturaljobobserver89566 terminalmissingrow(no reapply/restart). No datarepairmutation/outboundmessages/userprocesskills.

### 2026-09-07 13:55 UTC — embedding fairness fix published as M534

Beforebuild74734 EXIT0. Actualtest-devregression session11841 EXIT1: expectedJust(), gotNothing,1example1failure8.6298s (embedding-fairness-before-test.log). Failureisdesiredstarvationreproduction, notcompiler/setupfailure. Productionchange nowusesEffectful.Concurrent.Async.concurrently_ onexistingtryStep error/log actions underunchangedjobtimeout. Bothpersistcompletedbatchesasbefore; cancellationstopsboth. Perjobcanissue2providerrequestsconcurrently; existingrequestlimits/retrypolicyretained. No newconfig/dependencies/sentinels/typeweakening.

Afterbuild19773 EXIT0. Actualsameintegrationregression40894 PASS1example4.6906s: logsavedwhileerrorblocked, survivesparentcancellation,errorstillNULL. Logembedding-fairness-after-test.log. HLint94342 PASSNoHints,fourmolupass,diffcheckpass. Constraintreview tracedpatternEmbeddingAndMerge/typedIDs/LLMconstructors/embedAndMerge+persist/tryStep: nofindings, embedding-fairness-constraint-review.txt. Only3fileschange:BackgroundJobs(2stagecomposition),newPkg.EmbeddingFairnessSpec(realtestDB+LLMeffectinterposition),generatedcabalmodulelists. TestfixtureusesexistingpublicLLMeffect; noexternalAPIcalls ornewtest-onlyproductionAPI.

Publishedcommitd2143ae546b5cb354cac588951c8d91fb1f61c7f,PR534 https://github.com/monoscope-tech/monoscope/pull/534,worktree/tmp/monoscope-embedding-stage-fairness branchfix/embedding-stage-fairness-20260907. PRbodyembedding-fairness-pr-body.md. CI/reviewinprogress,notmerged/deployed. Needexactheadchecks,reviewanyconcurrencyconcerns,merge/deploy/observeLOGbacklogprogressonactualscheduledjob. Alllocalbuild/testprocessesterminal; no sourcesbeingcompilednow.

Externalrolloutat13:46replacede3replicaswith4b5e072a87f6452195b5a8973b6b87348e14a676. GHcompareprovesaheadbye3+2commits:a1b6095M533reportchartURLversionand4b5incidentcheckpointpreservation. ThusM532retained. Olde3RTSwindow21.0–21.3min, NOT30min; lastmaxcgroup5.95/7.05/6.60GiB, live2.23/2.12/2.37, noOOMobserved. Readercorrectlyskippedunmapped4buntilnow. Current4breplicas3f85881c4962,35398f8f8b43,f40032e0c53f allhealthy~9minuptime. VerifiedELFAMD64EXECnonPIE stats0x16a94880size376,mblocks0x16a95590size8; rts-symbols-4b5e07.txt. Addedexact4bimageallowlist, initialrts-4b5e07-initial.jsonl. OldRTSwatcher19083stillactiveatlastpoll13:54(nearing90samplesend); itmayappend4browsto e3filenameAFTERmapping, filteractualper-rowimage. Startnew4bspecificwatcherAFTER19083terminalratherthanassumestopped.

Genericobserver98706 terminalSUCCESS; restartedread-onlysameappendfileas63507 ACTIVE90samples30s(~45min)from13:53. Kafka87222 terminalSUCCESS; restartedwatch-kafka-progress-rollout.py sameappendfileas75256 ACTIVE90samples20s(~30min). No duplicateactiveoldwatchersforhealth/Kafka. nm1714terminalsymbols. Primaryruntimeconfigurationunchanged,noRS signals/profiling/configoverride.

All6TFmonitorsNORMALagain(monitors-1355.json,actualclock13:54:37):oldestbucket1065(last13:52),pressure7%,WAL22,020,100,096B(last13:39),flush/corrupt/unsorted0. Oldestbucketalertrecoveredwithoutthreshold/mutechanges. ContinuewatchwhileM534CIruns; outstandinghistoricnullpartitionintroducer and fastmissingoldembeddingjobownerremainunproven,separatefromnewverifiedstage-starvationfix.

### 2026-09-07 14:03 UTC — M534 cancellation assertion and renewed object audit

M534 review ond214pass/no blockers. Reviewer suggestedtryStepmight swallowasync; sourceimportsUnliftIO.Exception.catch (async-safe), so strengthenedactualregressionratherthanassuming. Testnowexplicitlycancelworker,waitCatchmustLeftSomeAsyncException (Rightisfailure),thenassertsavedlogpersists/errorNULL. Build80786PASS, test76943PASS1example3.5767s (embedding-fairness-cancellation-test.log). HLintflaggedredundantControl.Exception.fromException import; removed(useReludeexport), finalbuild31297PASS,HLintNoHints/fmt/diffcheckpass. No productionchangeafterinitialconcurrently_. Pushed01690d872075ff02881b7263e7049c7a5be756b1 toPR534; bodyupdatedwithasync-resultassertion. Commit27056terminalpushsuccess,bodyedit95342success. CurrentexactheadCI/reviewinprogress, format/lint/frontendpass; no localbuildactive. Do notmergeoldheadchecks.

RTSwatcher19083 terminalSUCCESS. Started4bspecificRTSwatcher38974 at13:56,90samples20s(~30min), rts-4b5e07-after-memory-fix.jsonl. Externaldeploymentthenreplaced4bat~13:56withae571327309624df6c020150b3fea22f6b07ac0b,allhealthy6a47eed2c906/d5e77cad6e2d/5460f577f20b. GHcompareprovesae aheadbycommit'Reconcile local work, update monitor recipients and compact chart retry control', so M532retained. NoaeRTSsymbolmapYET; 38974correctlyskipsunknownimage, do NOTtreatabsenceaslowmemory. Genericobserver63507+Kafka75256 continueandcoverallimages. 4bcompleted~10minwindowbeforeexternalrollout,noOOM,peaks2.29/6.34/5.69GiB. E3~22minwindowbeforeexternalrollout,noOOM,peaks5.95/7.05/6.60GiB. No forcedrestartsbyincidentwork.

FULLactiveobjectre-audit78045terminalPASS: logsDelta5361681743objects,metrics1706741817objects;3560/3560present,0duplicatepaths. active-object-audit-1401.py/json/log preserveoldbaselinefiles. Initialhelperlineversion536168active_entries0object404 refersRETIREDOld27c859filename, expected—notactivecorruption. Auditreadonly,no repairreapply/sourceobjectdelete. Maintenancehascontinuedafter12:28repairwithallcurrentbaseobjectsintact.

Lastmonitorsallnormalat13:54; liveoldestbucketrecoveredasrecorded. No newcontrolledembeddingjobs,replay,projectactivation,notificationmessages,userprocesskills,orproductionconfigoverrides. M534stillrequiresCImerge/deployandactualtargetLOGprogressverification. Historicalnullpartitionintroducer/fastmissingolderjobownerstillnotproven; do notreplace evidencewithassumptions.
