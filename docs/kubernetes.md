# Monitoring Kubernetes with Monoscope

This guide covers deploying Monoscope on Kubernetes and monitoring your cluster.

## Deploying Monoscope on Kubernetes

### Using Docker Image

Create a deployment for Monoscope:

```yaml
# monoscope-deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: monoscope
  namespace: monoscope
spec:
  replicas: 1
  selector:
    matchLabels:
      app: monoscope
  template:
    metadata:
      labels:
        app: monoscope
    spec:
      containers:
      - name: monoscope
        image: ghcr.io/monoscope-tech/monoscope:latest
        ports:
        - containerPort: 8080
          name: http
        - containerPort: 4317
          name: grpc
        env:
        - name: DATABASE_URL
          valueFrom:
            secretKeyRef:
              name: monoscope-secrets
              key: database-url
        - name: API_KEY_ENCRYPTION_SECRET_KEY
          valueFrom:
            secretKeyRef:
              name: monoscope-secrets
              key: api-key-secret
        resources:
          requests:
            memory: "2Gi"
            cpu: "1"
          limits:
            memory: "4Gi"
            cpu: "2"
---
apiVersion: v1
kind: Service
metadata:
  name: monoscope
  namespace: monoscope
spec:
  selector:
    app: monoscope
  ports:
  - name: http
    port: 8080
    targetPort: 8080
  - name: grpc
    port: 4317
    targetPort: 4317
  type: LoadBalancer
```

### Create Secrets

```bash
kubectl create namespace monoscope

kubectl create secret generic monoscope-secrets \
  --namespace=monoscope \
  --from-literal=database-url='postgresql://user:pass@postgres:5432/monoscope' \
  --from-literal=api-key-secret='your-32-char-secret-key-here'
```

### Deploy TimescaleDB

```yaml
# timescaledb-deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: timescaledb
  namespace: monoscope
spec:
  replicas: 1
  selector:
    matchLabels:
      app: timescaledb
  template:
    metadata:
      labels:
        app: timescaledb
    spec:
      containers:
      - name: timescaledb
        image: timescale/timescaledb-ha:pg16-all
        ports:
        - containerPort: 5432
        env:
        - name: POSTGRES_PASSWORD
          value: postgres
        - name: POSTGRES_DB
          value: monoscope
        volumeMounts:
        - name: postgres-storage
          mountPath: /var/lib/postgresql/data
      volumes:
      - name: postgres-storage
        persistentVolumeClaim:
          claimName: postgres-pvc
---
apiVersion: v1
kind: Service
metadata:
  name: postgres
  namespace: monoscope
spec:
  selector:
    app: timescaledb
  ports:
  - port: 5432
    targetPort: 5432
```

## Monitoring Your Kubernetes Cluster

### Deploy OpenTelemetry Collector

The OpenTelemetry Collector will gather telemetry from your cluster and send it to Monoscope:

```yaml
# otel-collector-config.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: otel-collector-config
  namespace: monoscope
data:
  config.yaml: |
    receivers:
      otlp:
        protocols:
          grpc:
            endpoint: 0.0.0.0:4317

      # Collect Kubernetes metrics
      k8s_cluster:
        auth_type: serviceAccount
        node_conditions_to_report: [Ready, MemoryPressure, DiskPressure]

      # Collect pod metrics
      kubeletstats:
        collection_interval: 30s
        auth_type: serviceAccount
        endpoint: "https://${K8S_NODE_NAME}:10250"
        insecure_skip_verify: true

    processors:
      batch:
        timeout: 10s

      k8sattributes:
        auth_type: serviceAccount
        extract:
          metadata:
            - k8s.namespace.name
            - k8s.deployment.name
            - k8s.pod.name
            - k8s.node.name

    exporters:
      otlp:
        endpoint: monoscope.monoscope.svc.cluster.local:4317
        headers:
          x-api-key: ${MONOSCOPE_API_KEY}

    service:
      pipelines:
        metrics:
          receivers: [k8s_cluster, kubeletstats]
          processors: [batch, k8sattributes]
          exporters: [otlp]
        traces:
          receivers: [otlp]
          processors: [batch, k8sattributes]
          exporters: [otlp]
        logs:
          receivers: [otlp]
          processors: [batch, k8sattributes]
          exporters: [otlp]
```

### Deploy Collector DaemonSet

```yaml
# otel-collector-daemonset.yaml
apiVersion: apps/v1
kind: DaemonSet
metadata:
  name: otel-collector
  namespace: monoscope
spec:
  selector:
    matchLabels:
      app: otel-collector
  template:
    metadata:
      labels:
        app: otel-collector
    spec:
      serviceAccountName: otel-collector
      containers:
      - name: otel-collector
        image: otel/opentelemetry-collector-k8s:latest
        env:
        - name: K8S_NODE_NAME
          valueFrom:
            fieldRef:
              fieldPath: spec.nodeName
        - name: MONOSCOPE_API_KEY
          valueFrom:
            secretKeyRef:
              name: monoscope-api
              key: api-key
        volumeMounts:
        - name: config
          mountPath: /etc/otel-collector
      volumes:
      - name: config
        configMap:
          name: otel-collector-config
```

## Auto-Instrumenting Applications

### For Java Applications

Add the OpenTelemetry Java agent:

```yaml
spec:
  containers:
  - name: my-app
    image: my-app:latest
    env:
    - name: JAVA_OPTS
      value: "-javaagent:/otel/opentelemetry-javaagent.jar"
    - name: OTEL_EXPORTER_OTLP_ENDPOINT
      value: "http://otel-collector.monoscope.svc.cluster.local:4317"
    - name: OTEL_SERVICE_NAME
      value: "my-java-app"
```

### For Python Applications

```yaml
spec:
  containers:
  - name: my-python-app
    image: my-python-app:latest
    env:
    - name: OTEL_EXPORTER_OTLP_ENDPOINT
      value: "http://otel-collector.monoscope.svc.cluster.local:4317"
    - name: OTEL_SERVICE_NAME
      value: "my-python-app"
    command: ["opentelemetry-instrument", "python", "app.py"]
```

## Kubernetes-Specific Dashboards

Once data is flowing, Monoscope will automatically create dashboards for:

- **Cluster Overview**: Node status, resource utilization
- **Pod Metrics**: CPU, memory, network by pod
- **Deployment Health**: Replica status, rollout progress
- **Service Monitoring**: Request rates, error rates, latencies

## Best Practices

1. **Use Namespaces**: Deploy Monoscope in its own namespace
2. **Resource Limits**: Set appropriate resource requests and limits
3. **Persistent Storage**: Use PVCs for database storage
4. **Service Mesh Integration**: If using Istio/Linkerd, configure telemetry export
5. **RBAC**: Create appropriate service accounts with minimal permissions

## Troubleshooting

### Collector Not Sending Data

```bash
# Check collector logs
kubectl logs -n monoscope -l app=otel-collector

# Verify API key secret
kubectl get secret -n monoscope monoscope-api -o yaml
```

### High Memory Usage

```bash
# Check resource usage
kubectl top pods -n monoscope

# Adjust batch processor settings in collector config
```

### Permission Issues

```bash
# Ensure service account has correct permissions
kubectl create clusterrolebinding otel-collector \
  --clusterrole=system:node-reader \
  --serviceaccount=monoscope:otel-collector
```