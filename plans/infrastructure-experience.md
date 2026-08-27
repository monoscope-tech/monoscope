# Infrastructure experience

## Original brief (verbatim)

> You will run the work i assign to you next until completion and verification, without my input. i'll trust your decisions. Use chrome to verify things. Atm, we have containers as a sub tab in explorer. Instead, i want us to implement datadogs infrastructure features and have containers moved there. Then hosts should be added (https://app.monoscope.tech/p/00000000-0000-0000-0000-000000000000/containers <- our currnet containers). improve the containers row for not ready, so its not broken as in the screenshot: /var/folders/nt/myzgjzxx7vb4g3j1gv7g52nw0000gn/T/pi-clipboard-8325fe5d-f1e0-4140-bf3d-2330b1171999.png then, our hosts tab under infrastructure shold be implemented similarly to our explorer page, using our components, but with all the features of datadog host screen: /var/folders/nt/myzgjzxx7vb4g3j1gv7g52nw0000gn/T/pi-clipboard-956b7596-766f-41cf-9321-92067e75ba61.png notice columns like integrations, system cpu, memory, and others like storage etc which can be added as columns. also note the facets. notice the search filter query by and group by. its basically the explorer page but with changes and richer virtual list with icons and others. /var/folders/nt/myzgjzxx7vb4g3j1gv7g52nw0000gn/T/pi-clipboard-119be1fb-fc5b-4617-9d20-f65fcccc5258.png notice the inspect a host feature when a host is clicked: /var/folders/nt/myzgjzxx7vb4g3j1gv7g52nw0000gn/T/pi-clipboard-2c12ac4d-2024-4a97-bb4a-33769e9996e2.png then containers which have charts, we would have same chart widgets /var/folders/nt/myzgjzxx7vb4g3j1gv7g52nw0000gn/T/pi-clipboard-90c0f81b-5031-4fd5-b059-68adc6465ccf.png <- containers sub tab under infrastructure, like hosts. then as sub tab, container images: /var/folders/nt/myzgjzxx7vb4g3j1gv7g52nw0000gn/T/pi-clipboard-096f6c01-f588-45bb-8b53-a0ff01d662d1.png then kubernetes: /var/folders/nt/myzgjzxx7vb4g3j1gv7g52nw0000gn/T/pi-clipboard-fb337300-9873-4651-a5a5-d33fbbc2dad2.png Then a host map sub tab, which shows hexagons like datadog /var/folders/nt/myzgjzxx7vb4g3j1gv7g52nw0000gn/T/pi-clipboard-d0be6633-9a87-444e-a094-26c39766b00d.png https://docs.datadoghq.com/infrastructure/hostmap/ . I will trust you to complete this task and carry it to completion. First, write the entire prompt i shared to a markdown file, then do extensive research to how datadog and other players like newrelic do all these and then document findings to the md file, then create a todo list of tasks out of them, and then implement the todo list. before pushing code, run hs-distill and hs-evasion-review and hs-lob-review, fix changes and then push.

## Research findings

### Sources reviewed

- The supplied Monoscope and Datadog screenshots.
- Datadog: [Host List](https://docs.datadoghq.com/infrastructure/list/), [Host Map](https://docs.datadoghq.com/infrastructure/hostmap/), [Containers Explorer](https://docs.datadoghq.com/containers/monitoring/containers_explorer/), [Container Images Explorer](https://docs.datadoghq.com/containers/monitoring/container_images/), and [Kubernetes Explorer](https://docs.datadoghq.com/containers/monitoring/kubernetes_explorer/).
- New Relic: [Infrastructure hosts UI](https://docs.newrelic.com/docs/infrastructure/infrastructure-data/infrastructure-ui-pages/infra-hosts-ui-page/).
- Elastic: [Infrastructure inventory](https://www.elastic.co/guide/en/observability/current/view-infrastructure-metrics.html).
- Dynatrace: [Hosts](https://docs.dynatrace.com/docs/observe/infrastructure-observability/hosts).
- The existing Monoscope Explorer, table, widget, drawer, container-query, Kubernetes dashboard, Docker dashboard, navigation, and integration-test implementations.

### Shared product model

The products converge on one interaction model rather than several unrelated pages:

1. **Inventory first.** Hosts, containers, images, and Kubernetes resources are dense, searchable inventories. A row is a launch point, not a dead report.
2. **Facets plus a query.** Search handles precise investigation; low-cardinality facets provide fast narrowing; group-by changes the inventory's structure without changing the underlying set.
3. **Metrics in context.** CPU, memory, storage, load, and network are visible in the inventory and expand into time-series charts in the detail view.
4. **Progressive disclosure.** Clicking a row opens an in-context side panel. The list remains visible, preserving investigation context.
5. **One infrastructure information architecture.** Hosts, containers, images, Kubernetes, and the map are sibling views. Containers do not belong under logs/traces Explorer.
6. **Customisable density.** Datadog lets operators add metric, tag, and host-attribute columns. The useful principle is not arbitrary schema plumbing; it is letting each operator expose the signals relevant to an incident without leaving the inventory.
7. **No fabricated telemetry.** All competitors show absent values distinctly. Elastic explicitly documents null map values; Datadog does not infer utilization when the denominator is missing. Monoscope must continue to render `—`, not `0`.

### Datadog details worth matching

#### Hosts

- The list is a live inventory with a single query bar, group-by, facets, export, and selectable columns.
- Default scan columns combine identity and context: host, cloud/configuration icons, software/OS, environment/service, monitor state, CPU, and integrations.
- Optional metric columns include memory and disk usage. Host attributes and tags can also become columns.
- The row opens an inspector containing tags, account/region/environment/service, health, enabled products, and charts for CPU, memory, disk, network, queues, and uptime.
- Integrations are compact labelled chips/icons. Their value is source awareness: the reader can tell why the platform knows about the host.

#### Containers

- Kubernetes, Docker, and other runtimes share one explorer. Runtime is a facet and an icon, not a separate silo.
- Search covers name, ID, and image and supports boolean composition.
- Summary graphs are collapsible and support time-series and scatter-plot analysis. The default scatter grouping is the low-cardinality `short_image` identity; dot size is the container count.
- CPU and RSS memory are the primary list metrics. Usage is compared with provisioned limits when those limits exist.
- Kubernetes `health` means readiness, not liveness. A not-ready state must remain a single, legible status label and must not wrap into broken text.

#### Container images

- Images are grouped independently from running container instances.
- Core columns are image/repository, running-container count, source/registry, tags/digests, size, and vulnerability severity.
- Facets include running state, source, image name, tag, ID, registry, repository, and digest.
- Vulnerabilities require SBOM collection and a vulnerability scanner. Monoscope currently receives neither; the UI must state that security data is unavailable rather than invent counts.

#### Kubernetes

- A resource selector covers pods, clusters, namespaces, nodes, workloads, networking, storage, and access-control resources.
- Common controls are query, group-by, facets, visualization mode, and a resource-specific table.
- Pod rows prioritize status, cluster, namespace, age, readiness, restarts, CPU, and memory.
- The inspector pivots to YAML/history, logs, APM, metrics, processes, network, events, and monitors. Monoscope can implement the metrics/log pivots now; YAML history, process inspection, and runtime network topology require telemetry it does not ingest.

#### Host map

- Each resource is one hexagon. Fill represents a metric or status. Groups are spatially separated and labelled.
- Users choose the resource, fill metric, grouping key, and filter. Hover gives exact identity/value; click opens the same host inspector as the table.
- Grouping is usually more valuable than elaborate geography: availability zone, region, cloud account, service, or environment turn a large fleet into incident-sized clusters.
- Color is quantitative signal. A visible legend and text/tooltips are required so the map does not rely on color alone.

### New Relic, Elastic, and Dynatrace lessons

- **New Relic** separates Summary, System, Storage, Network, Processes, Inventory, Events, and Alerts while retaining the selected host/time context. Its strongest contribution is the explicit system/storage/network breakdown and the use of overview tiles to narrow the rest of the page.
- **Elastic** makes the waffle map and table two views of the same inventory. Both hosts and containers open an overlay, with an option to expand to a full page. It explicitly supports OpenTelemetry as an infrastructure schema. This validates using one Monoscope data model for the list, map, and inspector.
- **Dynatrace** emphasizes host health and drill-down from fleet status to host-level CPU, memory, disk, network, processes, and logs. Health must remain a textual state, not only a color.

### Monoscope data and implementation constraints

- `otel_metrics` already carries the identity needed for containers, pods, and hosts. The current bounded 15-minute query normalizes Kubernetes, Docker/Swarm, pod-only collectors, and bare-host `hostmetrics` data.
- The shipped `kubernetes.yaml` and `docker.yaml` dashboards already prove the KQL queries needed for CPU, memory, pod phases, and network charts. Infrastructure pages should reuse the same widget and KQL path.
- Host CPU and memory are data-backed when `hostmetrics` reports them. Kubernetes nodes without host-level telemetry remain identifiable from their containers, but host percentages must stay absent.
- Storage, load, uptime, cloud provider/region, OS, and architecture can be shown when standard OpenTelemetry host metrics/resource attributes are present. Missing data remains `—`.
- Integrations can be derived honestly from telemetry sources (`OpenTelemetry`, `Kubernetes`, `Docker`), not from an agent inventory Monoscope does not have.
- Container images can be grouped from existing container resource attributes. Vulnerability, SBOM, manifest history, package lists, and image size are not currently ingested.
- A full Kubernetes object explorer needs object metrics/manifests that may not exist in every project. The first implementation can provide data-backed Pods, Workloads, and Nodes views and preserve the same route/query model for later resource kinds.
- The existing server-rendered `Pkg.Components.Table`, `Widget`, drawer, HTMX nav, semantic tokens, and responsive shell are the correct primitives. A parallel frontend system would increase drift.

## Implementation plan

- [x] Add a top-level **Infrastructure** destination and shared sub-tabs: Hosts, Containers, Images, Kubernetes, Host Map.
- [x] Remove Containers from Explorer while preserving `/containers` as a compatibility route.
- [x] Add standard host metadata and host signals (provider, region, OS, architecture, load, storage, uptime) to the bounded infrastructure query.
- [x] Implement a Hosts inventory with search, facets, group-by, source/integration chips, utilization bars, and optional columns.
- [x] Implement a host inspector with metadata, capacity facts, logs/metrics pivots, and reusable CPU/memory/storage/load chart widgets.
- [x] Move the Containers experience into Infrastructure and add collapsible CPU and memory summary widgets.
- [x] Fix the Not ready badge so it never wraps or clips.
- [x] Implement the Images inventory by grouping observed container image identities, with registry/runtime facets and an honest SBOM/security state.
- [x] Implement the Kubernetes inventory with Pods, Workloads, and Nodes resource views, facets, readiness/restart/utilization columns, and row inspection.
- [x] Implement a Host Map with hexagonal host cells, metric fill, grouping, legend, tooltips, keyboard activation, and host inspection.
- [x] Add regression and integration tests before each behavior change, then verify through the live test watcher.
- [x] Run formatter, linter, Impeccable detector, Haskell constraint/distillation reviews, and large-object review; fix findings.
- [x] Verify the complete desktop interaction model in Chrome and responsive classes/overflow behavior in code; commit and push.
