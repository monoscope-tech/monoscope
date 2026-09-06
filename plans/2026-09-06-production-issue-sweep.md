# Monoscope and TimeFusion incident sweep, September 6, 2026

Status: investigation in progress; no fixes deployed by this sweep yet.

## Scope and evidence

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
