# Container monitoring (Kubernetes + Docker/Swarm)

Stream: `ws-containers`. Assigned migration prefix `0137_` — **not used**, see "No migration" below.

The standing instruction is to copy Datadog precisely where we can and to reuse our existing
components rather than grow parallel ones. This plan does both: the view is Datadog's Containers
Explorer, and it is built entirely out of `Pkg.Components.Table` + the Explorer nav tabs that
Service Map already uses.

---

## 1. Datadog survey

Sourced from `docs.datadoghq.com/containers/monitoring/*` (the `/infrastructure/containers/*`
paths are aliases) and `DataDog/integrations-core@master:{container,kubelet}/metadata.csv`.

### 1.1 Containers Explorer (formerly Live Containers)

`app.datadoghq.com/containers`.

- **One table for every runtime.** Docker, ECS and Kubernetes containers share the view; the
  runtime is conveyed by a per-row icon, not by a separate page. This is the single most
  important shape decision to copy.
- Docs describe it as "a continuously updated table with resource metrics at two-second
  resolution", organised "by fields such as container name, status, and start time". The framing
  is explicitly *htop / ctop / kubectl*.
- **The literal column list is not published.** Datadog documents the view's behaviour, not its
  columns. Anything claiming to be "Datadog's container columns" is reconstruction. What the docs
  do commit to: "RSS and CPU utilization on containers is reported compared to the provisioned
  limits on the containers, when they exist" — usage against limits is the load-bearing idea.
- **Search is substring, not key:value**: matches container name, container ID, or image, with
  `AND` / `OR` / `NOT` (`!`) and parentheses.
- **Summary Graphs** collapse above the table, with a Scatter Plot and a Timeseries tab. The
  scatter plot groups by `short_image` by default; dot size is the number of containers in the
  group.
- **Detail panel documents exactly one tab: Logs** — live tail (streams like `kubectl logs -f`,
  not persisted) and indexed logs.
- Stated gotcha: "In Kubernetes the `health` value is the containers' readiness probe, not its
  liveness probe."

### 1.2 Kubernetes Explorer (formerly Orchestrator Explorer)

`app.datadoghq.com/orchestration/overview`. A **Select Resources** dropdown switches between
Pods, Clusters, Namespaces, Deployments, ReplicaSets, Nodes, Services, DaemonSets, StatefulSets,
Jobs, CronJobs, CRDs/CRs, PVs, PVCs. Per-resource column lists are also **not documented** — only
"fields such as status, name, and Kubernetes labels".

Side-panel tabs (this list *is* literal): YAML (with 7 days of definition history and diffing),
Logs, APM (date/service/duration/method/status code), Metrics, Processes, Network, Events,
Monitors, plus a "View Dashboard" button. **What query each pivot pre-fills is not documented.**

### 1.3 Kubernetes Resource Utilization page

This is the one place Datadog publishes column names verbatim, so it is the best available model
for how usage-vs-requests-vs-limits should read. A **CPU / Memory toggle** splits the table:

- CPU: **Pod group** · **CPU idle** (sum of usage−requests) · **CPU usage/requests** (%) ·
  **CPU usage/limits** (%) · **CPU graph** (usage, requests and limits over time).
- Memory: **Pod group** · **Memory unused** · **Memory usage/requests** · **Memory usage/limits**
  · **Memory graph**.

Utilization enrichment exists for **Clusters, Nodes and Pods only**, as 15-minute averages
computed at collection time: `cpu_usage_avg15`, `cpu_requests_avg15`, `cpu_limits_avg15`,
`cpu_usage_pct_requests_avg15`, `cpu_usage_pct_limits_avg15`, `cpu_waste_avg15` and the `mem_*`
equivalents.

**Units, stated explicitly by Datadog:** CPU is a **number of cores**; memory is **bytes**;
percentages are floats where `1.0` = 100%, and **values routinely exceed 100%** (usage over
*requests* especially). Color coding reflects degree of over/under-provisioning.

Documented limitation worth copying: groups containing a pod whose containers set no
requests/limits show **no** percentage — "Datadog cannot infer the usage percentage without them"
— and sort last regardless of sort direction. We do the same: a missing limit renders `—`, never
a fabricated 0%.

### 1.4 Tagging model

- Kubernetes, Datadog-extracted: `kube_cluster_name` (all resources), `kube_namespace` (all
  namespaced resources), `kube_<api_kind>:<metadata.name>` (e.g. `kube_deployment:web-server-2`),
  with pods as the exception — `pod_name`, plus `pod_phase` and `pod_status` (the latter
  "calculated similarly to `kubectl`").
- Image identity is deliberately tiered by cardinality: `docker_image` (full `repo/name:tag`),
  `image_name` (repo/name), **`short_image` (last path segment — the lowest-cardinality useful
  image identity, which is why the scatter plot defaults to grouping on it)**, `image_tag`.
- High-cardinality: `container_name`, `container_id`. Low: `image_name`, `short_image`,
  `image_tag`, `swarm_service`, `swarm_namespace`, `env`/`service`/`version`.

### 1.5 CPU/memory metric families and throttling

- Container runtime: `container.cpu.usage`/`.user`/`.system`/`.limit` in **nanocores**;
  `container.cpu.throttled` (ns) and `container.cpu.throttled.periods`; memory in bytes including
  `.working_set`, `.rss`, `.limit`, `.oom_events`.
- Kubelet/cAdvisor: `kubernetes.cpu.cfs.throttled.periods`/`.seconds`, `kubernetes.cpu.usage.total`,
  `kubernetes.cpu.requests`/`.limits`, `kubernetes.memory.working_set`/`.requests`/`.limits`.
- Datadog's own guidance where the two families disagree: "For the most precision, refer to
  `container.cpu.*` metrics over `kubernetes.cpu.*`." We follow this — our usage numbers come
  from the kubeletstats `container.*` family and only the requests/limits come from `k8s.*`.

### 1.6 Docker and Swarm — the honest finding

**Datadog has no Swarm view, no Swarm explorer and no Swarm resource type.** Swarm support is
*tags only* — `swarm_service` and `swarm_namespace` — which let you filter and group the generic
Containers Explorer. Plain Docker hosts get the same Containers Explorer table (with a Docker
icon on the row), `container.*`/`docker.*` metrics, and Docker events.

This is a useful negative result: building a Swarm-specific page would *diverge* from Datadog,
not copy it. A single container list with a runtime facet is the faithful design.

### 1.7 Three things the docs genuinely do not specify

Not to be assumed when designing against this survey: the Containers Explorer table's column
list; the per-resource column lists in the Kubernetes/ECS Explorer; and the query each side-panel
pivot pre-fills.

---

## 2. Our data model

Ingestion is OpenTelemetry, so the realistic source is an OTel Collector running the
`kubeletstats`, `k8s_cluster`, `docker_stats` and `hostmetrics` receivers. **This was verified,
not assumed** — see §3.

`otel_metrics` (both TimeFusion and Postgres, migration `0108_otel_metrics.sql`) already carries
dedicated flattened resource columns:

```
resource___host___name              resource___k8s___cluster___name
resource___container___name         resource___k8s___namespace___name
resource___service___name           resource___k8s___pod___name
                                    resource___k8s___container___name
```

Everything else lives in the `resource` blob (`JSONB` in Postgres, a Variant in TimeFusion):
`k8s.node.name`, `k8s.deployment.name`, `k8s.daemonset.name`, `k8s.replicaset.name`,
`k8s.pod.start_time`, `k8s.pod.uid`, `container.id`, `container.runtime`, `container.image.name`,
`container.image.tag`.

Identity and facets are therefore built **only on the flattened columns**, which are identical in
both stores. Node / deployment / image need one small per-store extraction expression.

### 2.1 Unified row

Datadog's single-table-per-runtime shape maps onto our two receiver families like this:

| Field | kubeletstats / k8s_cluster | docker_stats |
|---|---|---|
| Container | `k8s.container.name` | `container.name` |
| Runtime | `kubernetes` | `docker` |
| Pod | `k8s.pod.name` | — |
| Namespace | `k8s.namespace.name` | — |
| Node / Host | `k8s.node.name` | `host.name` |
| Image | `container.image.name` (+`.tag`) | `container.image.name` (carries the tag inline) |
| CPU (cores) | `container.cpu.usage` (unit `{cpu}`) | `container.cpu.utilization / 100` |
| CPU limit | `k8s.container.cpu_limit` | — (docker_stats emits no CPU limit) |
| CPU request | `k8s.container.cpu_request` | — |
| Memory | `container.memory.working_set` | `container.memory.usage.total` |
| Memory limit | `k8s.container.memory_limit` | `container.memory.usage.limit` |
| Memory request | `k8s.container.memory_request` | — |
| Restarts | `k8s.container.restarts` | — |
| Ready | `k8s.container.ready` | — |

Two normalisations deserve calling out:

1. **`container.cpu.utilization / 100` really is cores.** The docker_stats receiver reproduces
   Docker's own formula, `(cpu_delta / system_delta) * online_cpus * 100`, which is "percent of a
   single core" — 200% means two full cores. Dividing by 100 puts Docker on the same
   cores axis as Kubernetes, so one `CPU (cores)` column is honest for both. Spot-checked against
   prod: prod TimescaleDB reads 79.3 → 0.79 cores, Redpanda 16.3 → 0.16 cores, versus the
   Kubernetes demo cluster's Prometheus at 0.15 cores. Same magnitudes.
2. **`container.memory.working_set` for Kubernetes, not `container.memory.usage`.** Working set
   is what the kubelet's OOM killer and eviction manager act on, and it is what Datadog surfaces.

Where a denominator is absent — Docker has no CPU limit, and most Kubernetes containers set no
CPU limit at all — the percentage column renders `—`. Copying Datadog's stance: never infer a
percentage without its denominator.

### 2.2 Naming collision to avoid

kubeletstats emits `container.memory.usage`; docker_stats emits `container.memory.usage.total`.
These are different metrics whose names are prefixes of one another. All predicates use
`metric_name IN (...)` with exact names — never `LIKE 'container.%'`, and never an OR-chain of
equalities (the `Utf8View` OR-evaluation bug returns wrong, near-empty results on TimeFusion).

### 2.3 Series identity

The partition/group key includes the node or host, not just the container name: Docker container
names collide across hosts in a Swarm, and pod names collide across clusters.

- Kubernetes: `(namespace, pod, container, node)`
- Docker: `(host, container)`

---

## 3. Do we already receive this? Yes.

Read-only, time-bounded query against production TimeFusion:

```sql
SELECT project_id, metric_name, count(*) AS n
FROM otel_metrics
WHERE timestamp > (now() - interval '2 hours')::text
  AND (metric_name LIKE 'k8s.%' OR metric_name LIKE 'container.%')
GROUP BY 1,2 ORDER BY n DESC LIMIT 40;
```

Three projects are already emitting container telemetry today:

- **`00000000-…0000` (demo)** — a full kubeletstats + k8s_cluster feed from a two-node cluster
  (`vps-d6d7e318`, `vps-708640dc`) running the OpenTelemetry demo. 60 distinct metric names
  including `container.cpu.usage`, `container.memory.working_set`, `k8s.container.cpu_limit`,
  `k8s.container.memory_limit`, `k8s.container.restarts`, `k8s.container.ready`, `k8s.pod.phase`,
  `k8s.deployment.available`/`desired`, `k8s.node.*`, `k8s.replicaset.*`, `k8s.statefulset.*`,
  `k8s.daemonset.*`, `k8s.job.*`.
- **`87576849-…` (monoscope self-telemetry)** and **`6297304f-…`** — docker_stats from our own
  CapRover Swarm: `container.cpu.utilization`, `container.memory.usage.total`/`.limit`,
  `container.memory.percent`, `container.network.io.usage.*`, `container.blockio.*`.

The resource blob is fully populated, e.g. for a Kubernetes container:

```json
{"container":{"image":{"name":"quay.io/prometheus/prometheus","tag":"v3.9.0"}},
 "k8s":{"cluster":{"uid":"13269e1e-…"},"container":{"name":"prometheus-server"},
        "deployment":{"name":"prometheus"},"namespace":{"name":"default"},
        "node":{"name":"vps-d6d7e318"},
        "pod":{"name":"prometheus-658c66794d-lrkrd","start_time":"2026-07-31T08:26:10Z","uid":"…"},
        "replicaset":{"name":"prometheus-658c66794d","uid":"…"}}}
```

and for a Swarm container:

```json
{"container":{"hostname":"26550399cdef","id":"26550399cdef…","runtime":"docker",
              "image":{"name":"docker.redpanda.com/redpandadata/redpanda:v25.1.4"},
              "name":"srv-captain--redpanda-0.1.tt13bkp5vyz5upo49yapxldyu"},
 "host":{"name":"0ce201583b04"},"os":{"type":"linux"}}
```

The full list-query shape was prototyped end-to-end against production and returns a correct,
Datadog-shaped container list in **~0.4 s** over a 10-minute window.

Two findings that constrain the design:

- **`resource___k8s___cluster___name` is empty** in practice — the collector emits
  `k8s.cluster.uid` but no cluster name unless the operator configures one. There is no cluster
  facet in v1; the onboarding config sets the name so it becomes available going forward.
- **The metrics rollup tables are unusable here.** `otel_metrics_rollup_metrics_1m_v2` keeps only
  `metric_name` and `resource___service___name` — every container dimension is aggregated away.
  The list query must read `otel_metrics` directly, which the timing above shows is fine.

### 3.1 Blob extraction per store

| | TimeFusion | Postgres |
|---|---|---|
| expression | `variant_get(resource,'k8s.node.name','Utf8')` | `resource #>> '{k8s,node,name}'` |

Verified on TimeFusion: `variant_get(…,'Utf8')` works and `json_get_str(variant_to_json(…),…)`
also works; `get_field`, `variant_get(…,'string')` and a `::jsonb` cast all error. One query
builder emits the right expression based on the existing `useTimefusion` flag — a dialect switch
inside one function, not a forked query.

---

## 4. v1 scope

### 4.1 In scope

A **Containers page** — one table for every runtime, mirroring the Containers Explorer.

- Route `GET /p/:pid/containers`, added to `explorerTabs` in `Utils.hs` alongside Live Tail,
  Events, Metrics and Service Map. Those tabs already implement the documented HTMX
  tab/nav swap pattern, so nav integration is one list entry plus `explorerNavTabs_` in the
  page's `BWConfig` — exactly how `Pages/ServiceMap.hs` does it. No new top-level BodyWrapper item.
- Handler returns a **typed newtype carrying `PageCtx` + data** (`ContainersGet`), never
  `Html ()`, with the row-fragment constructor split out for HTMX partial swaps the way
  `Pages/Endpoints.hs` splits `CatalogListPage` / `CatalogListRows`.
- Columns: Container · Runtime · Pod · Namespace · Node/Host · Image · CPU (cores) · CPU % limit ·
  Memory · Mem % limit · Restarts · Ready. Kubernetes-only cells render `—` on Docker rows.
- Facets via the table component's existing `FilterMenu` / `activeFilters`: runtime, namespace,
  node/host, image. Substring search over container/pod/image via `SearchMode ClientSide`,
  matching Datadog's substring-search behaviour.
- Zero state via the component's `ZeroState`, pointing at the collector setup docs.
- Detail drawer per container: requests and limits for CPU and memory, image and tag, pod UID and
  start time, workload owner (deployment/daemonset/statefulset), and pivots into Log Explorer and
  the Metrics explorer pre-filtered on `resource.k8s.pod.name` / `resource.container.name`.
- Onboarding: extend `docs/kubernetes.md` (which already ships a `k8s_cluster` + `kubeletstats`
  collector ConfigMap) with the resource attributes we depend on, and add the Docker/Swarm
  `docker_stats` equivalent.

### 4.1.1 What already exists that we build on rather than duplicate

- **`static/public/dashboards/kubernetes.yaml` and `docker.yaml`** already ship full KQL widget
  sets over the `metrics` source, auto-provisioned per project by `DashboardsAutoProvision`
  (`src/BackgroundJobs.hs`) when a project's metric names match their `discovery_metrics`
  prefixes. The container page does not re-implement charts; the drawer links to these dashboards
  and reuses their query idiom verbatim, e.g.
  `metrics | where metric_name == "container.cpu.utilization" | summarize avg(value) * 100 by bin_auto(timestamp), resource.container.name`.
- **The route belongs in the existing `TelemetryRoutes'` record** in `src/Web/Routes.hs`, next to
  `serviceMapGetH` and the `metrics*` handlers — that record has already captured `pid`.
- **Adding `("Containers", "/containers")` to `explorerTabs` yields both the tab strip and the
  sidebar flyout entry** (`navFlyoutItems "Explorer"` reads the same list), so nav is one edit.
- **Onboarding snippets are not in this repo.** `src/Pages/Onboarding.hs` fetches them over HTMX
  from `monoscope.tech/docs/sdks/...` via `proxyLandingH`, and Docker and Kubernetes are already
  listed in its Infrastructure group. Our collector-config work is therefore the `docs/` content,
  not an in-app snippet component.

### 4.1.2 KQL field lowering — verified, and why it is fine

`flattenedOtelAttributes` is seeded only from `otel_logs_and_spans` columns, which have no
`resource___k8s___*`. So in a KQL `metrics` query, `resource.k8s.pod.name` lowers to a JSON probe
(`resource->'k8s'->'namespace'->>'name'`, or `variant_to_json(resource)->…` on TimeFusion) rather
than to the indexed flattened column. The shipped dashboards already rely on this and work.

The container **list** query does not go through KQL — it is a hand-written model function that
names the flattened columns directly, which is both indexed and identical across the two stores.
No parser change is needed, and none is made.

### 4.2 Explicitly out of scope for v1

Being honest about this matters more than breadth:

- **No agent.** No live process inspection, no exec-into-container, no `docker logs -f` live tail
  in the panel. We have no agent and are not building one.
- No Kubernetes/Orchestrator Explorer resource views — Pods, Deployments, ReplicaSets, Nodes,
  Services, DaemonSets, StatefulSets, Jobs and CRDs as their own browsable pages.
- No Cluster Map, no scatter-plot/timeseries Summary Graphs, no group-by rollup table.
- No Container Images Explorer, no SBOM, no vulnerability counts.
- No YAML/manifest tab and no definition history or diffing (we ingest metrics, not manifests).
- No Processes, Network, or Events tabs; no Docker events; no `docker.exit` service check.
- No Swarm service aggregation and no ECS — matching Datadog, which has neither.
- No throttling metrics (`container.cpu.throttled`, `kubernetes.cpu.cfs.throttled.*`): the
  receivers we recommend do not emit them today.
- No alerts or monitors on container metrics.
- No 2-second live resolution. Our ingestion path is batched; the page refreshes on the existing
  time-picker/refresh affordance.
- No cluster facet until collectors are configured to emit `k8s.cluster.name` (§3).
- No CPU-% -of-requests column in the table; requests live in the detail drawer. Datadog splits
  requests and limits behind a CPU/Memory toggle for the same reason — four percentage columns do
  not fit a scannable row.

### 4.3 No migration

Container monitoring reads `otel_metrics`, which already exists in both stores with the columns
we need. **The assigned `0137_` prefix is deliberately left unused** — a migration file should not
be created merely because a number was reserved.

### 4.4 Performance guardrails

Baked into the model function, given TimeFusion's history of being OOM-killed by unbounded and
wide-window aggregates:

- Always bound by `project_id` **and** `timestamp`, always with a `LIMIT`.
- The list query uses a short fixed window (~15 min) — a container list is a "what is running
  right now" view, so it never accepts a multi-day range.
- Exact `metric_name IN (...)`; no `LIKE`, no OR-chains.
- One pass over the window with a window function, pivoted with conditional aggregates — no
  self-joins, which is the shape that has repeatedly killed TF.

---

## 5. Collector configuration handed to users

### Kubernetes — added to `docs/kubernetes.md`

The existing ConfigMap already runs `k8s_cluster` and `kubeletstats`. What it must additionally
guarantee is the resource attributes the page keys on:

```yaml
receivers:
  kubeletstats:
    collection_interval: 30s
    auth_type: serviceAccount
    endpoint: "https://${K8S_NODE_NAME}:10250"
    insecure_skip_verify: true
    # cpu_request/cpu_limit/memory_request/memory_limit drive the "% of limit" columns
    metric_groups: [container, pod, node]
    k8s_api_config:
      auth_type: serviceAccount
    extra_metadata_labels:
      - container.id
  k8s_cluster:
    auth_type: serviceAccount
    node_conditions_to_report: [Ready, MemoryPressure, DiskPressure]

processors:
  k8sattributes:
    auth_type: serviceAccount
    extract:
      metadata:
        - k8s.namespace.name
        - k8s.pod.name
        - k8s.pod.uid
        - k8s.pod.start_time
        - k8s.deployment.name
        - k8s.node.name
        - container.id
        - container.image.name
        - container.image.tag
  resource:
    attributes:
      # Not emitted by any receiver — set it so the cluster is nameable in the UI.
      - key: k8s.cluster.name
        value: "my-cluster"
        action: upsert
```

### Docker and Swarm — new section

```yaml
receivers:
  docker_stats:
    endpoint: unix:///var/run/docker.sock
    collection_interval: 30s
    timeout: 20s
    api_version: "1.44"
    metrics:
      container.cpu.utilization: {enabled: true}
      container.memory.usage.total: {enabled: true}
      container.memory.usage.limit: {enabled: true}
      container.memory.percent: {enabled: true}
      container.network.io.usage.rx_bytes: {enabled: true}
      container.network.io.usage.tx_bytes: {enabled: true}
  hostmetrics:
    collection_interval: 30s
    scrapers: {cpu: {}, memory: {}, filesystem: {}, load: {}, network: {}}

service:
  pipelines:
    metrics:
      receivers: [docker_stats, hostmetrics]
      processors: [batch]
      exporters: [otlp]
```

The collector needs the Docker socket mounted read-only. On Swarm it runs as a global-mode
service so every node reports its own containers; `container.name` then carries the Swarm task
name (`srv-captain--monoscope.1.chuu5qe…`), which is what the Container column shows.

---

## 6. Tests

Integration tests in `test/integration/`, ingesting through `Pkg.TestUtils`:

- `ingestMetric` / `createGaugeMetricAtTime` currently take no resource attributes, so they can
  only produce `service.name`-tagged metrics. They are **extended** with a resource-attribute
  parameter (~10 call sites, mechanical) rather than forked into a parallel
  `ingestMetricWithResource` — extending the original is the project rule.
- Cases: a Kubernetes-shaped container set and a Docker-shaped set render the expected rows with
  correct CPU/memory/limit values; a facet filter narrows the list; the empty state renders when
  a project has no container metrics.
- `Telemetry.flushMetricCatalog` must be called explicitly after ingest — the metric catalog is
  buffered in a `TVar` and `runAllBackgroundJobs` does not flush it. `GrpcIngestionSpec` already
  does this and is the model to follow.
- Assertions are against the handler's typed payload structurally, not against HTML strings —
  which is the reason the handler returns a typed newtype in the first place.
