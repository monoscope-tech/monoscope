# Container monitoring

The **Explorer → Containers** page lists every container reporting to a project — Kubernetes
pods and plain Docker or Swarm containers in one table — with CPU and memory shown against the
limits each container was given.

Monoscope has no agent. Everything on the page comes from OpenTelemetry metrics, so the only
setup is pointing a collector at your project.

## What the page shows

| Column | Where it comes from |
| --- | --- |
| Container | `k8s.container.name`, or `container.name` for Docker |
| Pod, Namespace | `k8s.pod.name`, `k8s.namespace.name` |
| Node / Host | `k8s.node.name`, or `host.name` for Docker |
| CPU | `container.cpu.usage` (cores), or `container.cpu.utilization` for Docker |
| CPU % lim | CPU divided by `k8s.container.cpu_limit` |
| Memory | `container.memory.working_set`, or `container.memory.usage.total` for Docker |
| Mem % lim | memory divided by `k8s.container.memory_limit` or `container.memory.usage.limit` |
| Restarts, Ready | `k8s.container.restarts`, `k8s.container.ready` |

A percentage column shows `—` when the container has no limit set. Monoscope does not guess a
denominator: a blank cell means "this container is unbounded", which is itself worth knowing.

Docker reports CPU with Docker's own convention, where 200% means two full cores. Monoscope
divides by 100 so Docker and Kubernetes containers share one cores column.

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
        - k8s.node.name
        - container.id
        - container.image.name
        - container.image.tag
  resource:
    attributes:
      # No receiver emits this. Set it so the cluster is nameable in queries and filters.
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
shows. `host.name` is the node.

## Verifying

After a minute or two, open **Explorer → Containers**. If it is still empty:

- Check the collector logs for export errors.
- Confirm the API key is correct: a rejected key fails the whole export, not just metrics.
- In **Explorer → Metrics**, look for `container.cpu.usage` (Kubernetes) or
  `container.cpu.utilization` (Docker). If those are absent the receiver is not collecting; if
  they are present but the page is empty, the resource attributes are missing — check that
  `k8sattributes` is in the metrics pipeline.

The page shows containers seen in the last 15 minutes, so a container that stopped reporting
drops off rather than lingering as a stale row.

## Not included

Monoscope reads metrics, not the container runtime. There is no process list, no exec into a
container, no live `docker logs -f` tail, no image vulnerability scanning, and no manifest or
YAML history. Kubernetes objects other than containers — Deployments, Nodes, Services and the
rest — are not browsable as their own views; the shipped `kubernetes.yaml` dashboard covers
them at the cluster level instead.
