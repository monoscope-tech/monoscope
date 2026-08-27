# Container monitoring

The **Explorer → Containers** page lists everything reporting to a project that runs and burns
CPU and memory — Kubernetes containers, plain Docker and Swarm containers, and bare nodes with
no containers at all — in one table, each shown against whatever limit it was given.

Monoscope adapts to the collector you already run rather than requiring the one below. Where a
receiver leaves something out, the page falls back rather than blanking a column; where nothing
can supply a value, the cell reads `—` instead of a fabricated zero.

Monoscope has no agent. Everything on the page comes from OpenTelemetry metrics, so the only
setup is pointing a collector at your project.

## What the page shows

| Column | Where it comes from |
| --- | --- |
| Container | `k8s.container.name`, `container.name`, else the pod or host name |
| Pod, Namespace | `k8s.pod.name`, `k8s.namespace.name` |
| Node / Host | `k8s.node.name`, else `host.name` |
| Cluster | `k8s.cluster.name`, else `k8s.cluster.uid` |
| Workload | the controller: Deployment, StatefulSet, DaemonSet, CronJob, Job, ReplicaSet — or the Swarm service |
| Image, Tag | `container.image.name` / `container.image.tag`, splitting `repo:tag` when the tag is not sent separately |
| CPU | `container.cpu.usage` (cores), `container.cpu.utilization` ÷ 100 for Docker, `k8s.pod.cpu.usage`, or non-idle `system.cpu.utilization` summed across cores |
| CPU % lim | CPU divided by `k8s.container.cpu_limit`, or by the node's logical core count |
| Memory | `container.memory.working_set`, `container.memory.usage.total`, `container.memory.usage`, `k8s.pod.memory.*`, or `system.memory.usage{state=used}` |
| Mem % lim | memory divided by `k8s.container.memory_limit`, `container.memory.usage.limit`, or the node's total memory |
| Restarts, Ready | `k8s.container.restarts`, `k8s.container.ready` |

The **Runtime** facet separates `kubernetes`, `docker` and `host` rows.

A percentage column shows `—` when the container has no limit set. Monoscope does not guess a
denominator: a blank cell means "this container is unbounded", which is itself worth knowing.

Docker reports CPU with Docker's own convention, where 200% means two full cores. Monoscope
divides by 100 so Docker and Kubernetes containers share one cores column.

### If your collector sends less than this

- **`kubeletstats` without the `container` metric group.** Only `k8s.pod.*` arrives. The pod
  becomes the row and stands in for its own containers; Image is blank, because a pod covers
  more than one. As soon as container metrics do arrive the pod row yields to them, so you
  never see a pod and its containers double-counted.
- **No `k8s.cluster.name`.** Nothing can emit it — a cluster does not know its own name through
  the Kubernetes API — so the Cluster facet falls back to `k8s.cluster.uid`. Set the name with
  the `resource` processor below to make it readable.
- **Only `k8s.deployment.name` extracted.** `k8sattributes` extracts exactly the metadata you
  list, and names the controller after its kind, so a DaemonSet or StatefulSet pod has no
  `k8s.deployment.name` at all. List every kind you run or Workload will be blank for them.
- **No `container.image.tag`.** `docker_stats` puts the tag inside `container.image.name`;
  Monoscope splits it, leaving registry ports and `@sha256:` digests alone.
- **No Swarm service attribute.** Swarm names a task `<service>.<slot>.<taskid>` and sends no
  service name, so Monoscope reads the service back out of the task name into Workload.

## Kubernetes

Run the `kubeletstats` and `k8s_cluster` receivers. `kubeletstats` provides usage;
`k8s_cluster` provides the requests, limits, restart counts and readiness that the percentage
and status columns need.

```yaml
receivers:
  kubeletstats:
    collection_interval: 30s
    auth_type: serviceAccount
    endpoint: "https://${K8S_NODE_NAME}:10250"
    insecure_skip_verify: true
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
        - k8s.statefulset.name
        - k8s.daemonset.name
        - k8s.cronjob.name
        - k8s.job.name
        - k8s.node.name
        - k8s.cluster.uid
        - container.id
        - container.image.name
        - container.image.tag
  resource:
    attributes:
      # No receiver emits this. Set it so the cluster is nameable in queries and filters —
      # without it the Cluster facet falls back to the opaque k8s.cluster.uid above.
      - key: k8s.cluster.name
        value: "my-cluster"
        action: upsert
  batch:
    timeout: 10s

exporters:
  otlp:
    endpoint: <your-monoscope-host>:4317
    headers:
      x-api-key: ${MONOSCOPE_API_KEY}

service:
  pipelines:
    metrics:
      receivers: [kubeletstats, k8s_cluster]
      processors: [k8sattributes, resource, batch]
      exporters: [otlp]
```

Deploy this as a DaemonSet so each node reports its own kubelet. A full DaemonSet manifest,
the RBAC it needs, and the rest of the Kubernetes setup are in
[kubernetes.md](kubernetes.md#monitoring-your-kubernetes-cluster).

`k8s.node.name` must reach the collector as an environment variable:

```yaml
env:
  - name: K8S_NODE_NAME
    valueFrom:
      fieldRef:
        fieldPath: spec.nodeName
```

## Docker and Docker Swarm

Run the `docker_stats` receiver with the Docker socket mounted read only. Add `hostmetrics` if
you also want the host itself.

```yaml
receivers:
  docker_stats:
    endpoint: unix:///var/run/docker.sock
    collection_interval: 30s
    timeout: 20s
    api_version: "1.44"
    metrics:
      container.cpu.utilization:
        enabled: true
      container.memory.usage.total:
        enabled: true
      container.memory.usage.limit:
        enabled: true
      container.memory.percent:
        enabled: true
      container.network.io.usage.rx_bytes:
        enabled: true
      container.network.io.usage.tx_bytes:
        enabled: true
  hostmetrics:
    collection_interval: 30s
    scrapers:
      cpu:
      memory:
      filesystem:
      load:
      network:

processors:
  batch:
    timeout: 10s

exporters:
  otlp:
    endpoint: <your-monoscope-host>:4317
    headers:
      x-api-key: ${MONOSCOPE_API_KEY}

service:
  pipelines:
    metrics:
      receivers: [docker_stats, hostmetrics]
      processors: [batch]
      exporters: [otlp]
```

Compose service:

```yaml
services:
  otel-collector:
    image: otel/opentelemetry-collector-contrib:latest
    command: ["--config=/etc/otelcol/config.yaml"]
    environment:
      MONOSCOPE_API_KEY: ${MONOSCOPE_API_KEY}
    volumes:
      - ./otel-collector.yaml:/etc/otelcol/config.yaml:ro
      - /var/run/docker.sock:/var/run/docker.sock:ro
```

On Swarm, deploy the collector in **global** mode so every node reports the containers running
on it:

```yaml
    deploy:
      mode: global
```

Swarm task names arrive in `container.name` — for example
`srv-captain--monoscope.1.chuu5qe4qhbhxdhi0indeexfg` — and that is what the Container column
shows. `host.name` is the node. Monoscope derives the **service** (`srv-captain--monoscope`)
from the task name and shows it as the Workload, so the replicas of one service group together
even though `docker_stats` sends no service attribute.

## A bare node or VM

Nothing containerised — a single application installed straight onto a host. Run `hostmetrics`
alone and the node appears as its own row: what it burns is the usage, what it has is the limit.

```yaml
receivers:
  hostmetrics:
    collection_interval: 30s
    scrapers:
      cpu:
        metrics:
          # NOT enabled by default, and the only CPU metric the page can read: the
          # default system.cpu.time is a counter, not a current reading.
          system.cpu.utilization:
            enabled: true
      memory:
      filesystem:
      load:
      network:

processors:
  # hostmetrics does not set host.name on its own in every environment. Without it the node
  # has no identity and cannot become a row.
  resourcedetection:
    detectors: [env, system]
    system:
      hostname_sources: [os]
  batch:
    timeout: 10s

exporters:
  otlp:
    endpoint: <your-monoscope-host>:4317
    headers:
      x-api-key: ${MONOSCOPE_API_KEY}

service:
  pipelines:
    metrics:
      receivers: [hostmetrics]
      processors: [resourcedetection, batch]
      exporters: [otlp]
```

CPU is the sum of every non-idle mode across every logical core, so a node showing `0.5 / 4`
has half a core busy out of four. Memory is `system.memory.usage{state=used}` against the sum
of all states. Requests, limits, restarts and readiness stay blank: a bare node has no
scheduler to set them.

The same block works alongside `docker_stats` — add `hostmetrics` to the receiver list and you
get the node and its containers as separate rows.

## Verifying

After a minute or two, open **Explorer → Containers**. If it is still empty:

- Check the collector logs for export errors.
- Confirm the API key is correct: a rejected key fails the whole export, not just metrics.
- In **Explorer → Metrics**, look for `container.cpu.usage` (Kubernetes), `k8s.pod.cpu.usage`
  (Kubernetes, pod group only), `container.cpu.utilization` (Docker) or
  `system.cpu.utilization` (bare node). If none are present the receiver is not collecting.
- If the metrics are there but the page is empty, the identifying resource attribute is
  missing. A row needs one of `k8s.container.name`, `container.name`, `k8s.pod.name` or
  `host.name` — check that `k8sattributes` (Kubernetes) or `resourcedetection` (bare node) is
  in the metrics pipeline.
- A bare node with a blank CPU column is almost always `system.cpu.utilization` left disabled;
  it is off by default.

The page shows containers seen in the last 15 minutes, so a container that stopped reporting
drops off rather than lingering as a stale row.

## Not included

Monoscope reads metrics, not the container runtime. There is no process list, no exec into a
container, no live `docker logs -f` tail, no image vulnerability scanning, and no manifest or
YAML history. Kubernetes objects other than containers — Deployments, Nodes, Services and the
rest — are not browsable as their own views; the shipped `kubernetes.yaml` dashboard covers
them at the cluster level instead.
