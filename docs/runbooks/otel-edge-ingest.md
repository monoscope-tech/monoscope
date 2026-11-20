# OTLP edge ingest via Cloudflare

`otel.monoscopehq.com` is the documented, Cloudflare-proxied hostname for the Frankfurt collector
(`srv-captain--otelcol`, 3 replicas → Redpanda). Clients in the US and Asia
terminate TLS at a nearby Cloudflare PoP. Cloudflare then reuses warm
connections to the origin. `otelcol.monoscope.tech` stays DNS-only, so existing
clients on `:4317` (plaintext gRPC) and `:4318` keep working. Cloudflare cannot
proxy those ports. `otel.monoscope.tech` is set up the same way and stays as an alias.

| Protocol | Endpoint |
| --- | --- |
| OTLP/HTTP | `https://otel.monoscopehq.com` (`/v1/traces`, `/v1/logs`, `/v1/metrics`) |
| OTLP/gRPC | `https://otel.monoscopehq.com:443` |

## Why no Worker, Queues or Argo

- Workers cannot terminate inbound gRPC, and gRPC is over 90% of our volume.
  A Worker → Kafka path would also skip the collector pipeline (`x-api-key`
  extraction, schema, tier-1 transforms).
- At about 1.1 TB/month into the collectors (measured in Nov 2026), Argo Smart
  Routing would cost about $110/month. The plain proxy costs $0: Cloudflare
  does not bill proxied bandwidth, and OVH does not bill ingress.

## Origin: CapRover nginx

The `otelcol` app has a custom nginx config. It is CapRover's default
`server-block-conf.ejs` with two additions:

1. After `server_name`: `set_real_ip_from` for each range in
   <https://www.cloudflare.com/ips-v4> and `/ips-v6`, plus
   `real_ip_header CF-Connecting-IP;`. The collector reads only `X-Real-IP`
   (`resource/sender` in `static/public/otelcol.yaml`). This keeps the sender IP
   (RUM geo) correct behind Cloudflare without trusting spoofable headers. When
   Cloudflare publishes new ranges, update this list.
2. Before the Let's Encrypt location: gRPC over TLS on 443.

   ```nginx
   location /opentelemetry.proto.collector. {
       set $grpc_upstream grpc://<%-s.localDomain%>:4317;
       grpc_pass $grpc_upstream;
       grpc_set_header X-Real-IP $remote_addr;
       grpc_read_timeout 120s;
       grpc_send_timeout 120s;
   }
   ```

Saving the app config does a rolling restart of the collector tasks.

## Cloudflare zones (monoscopehq.com and monoscope.tech)

Each zone has the same setup for its `otel` host:

- `otel` CNAME → `otelcol.s.past3.tech`, proxied. Add it DNS-only first and let
  CapRover issue the Let's Encrypt cert. Then turn on the proxy.
- Network → gRPC: on. This toggle is dashboard-only; there is no API setting for it.
- SSL/TLS: Full (strict) for this hostname (configuration rule, `http_config_settings` phase).
- WAF custom rule (`http_request_firewall_custom`): skip Browser Integrity
  Check, Security Level, UA block, hotlink and zone lockdown, plus the managed
  WAF and rate-limit phases, for that `otel` host. Otherwise server SDKs
  get challenged, and their exports fail with 403.
- **Bot Fight Mode must stay off.** On Free, custom rules cannot skip it.
- `PUT …/rulesets/phases/<phase>/entrypoint` replaces the whole phase. When you
  add a rule, include the existing ones.
- Request body limit is 100 MB on Free, which is above the collector's 16 MiB.
- API token: "monoscope.tech + monoscopehq.com otel edge (DNS/settings/WAF/config rules)",
  scoped to both zones, stored at `~/.cloudflare/monoscope-token` on the operator's machine.

`otelcol.monoscope.tech:443` also accepts gRPC over TLS (same nginx config). Use
it for clients that cannot change hostnames.

## Verify

```bash
curl -si -X POST https://otel.monoscopehq.com/v1/traces -H 'content-type: application/json' \
  -d '{"resourceSpans":[]}' | grep -iE '^HTTP|cf-ray'
cd proto && grpcurl -import-path . -proto opentelemetry/proto/collector/trace/v1/trace_service.proto \
  -d '{"resourceSpans":[]}' otel.monoscopehq.com:443 opentelemetry.proto.collector.trace.v1.TraceService/Export
```
