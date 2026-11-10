// Sample telemetry for app proofs, sent through the server's own OTLP/HTTP ingestion so every
// derived column, pattern and Live Tail match is real: an hour of history, then a few requests a
// second until killed.
import { createRequire } from 'node:module';
import { randomBytes } from 'node:crypto';
const protobuf = createRequire('/opt/traffic/')('protobufjs');
const root = new protobuf.Root();
root.resolvePath = (_, target) => `/opt/traffic/proto/${target}`;
root.loadSync(['opentelemetry/proto/collector/trace/v1/trace_service.proto', 'opentelemetry/proto/collector/logs/v1/logs_service.proto']);
const send = async (kind, type, body) => {
  const message = root.lookupType(type);
  const res = await fetch(`http://127.0.0.1:8080/v1/${kind}`, { method: 'POST', headers: { 'content-type': 'application/x-protobuf', 'x-api-key': 'proof' }, body: message.encode(message.fromObject(body)).finish() });
  if (!res.ok) throw new Error(`${kind}: ${res.status} ${await res.text()}`);
};
const attrs = (o) => Object.entries(o).map(([key, v]) => ({ key, value: typeof v === 'number' ? { intValue: v } : { stringValue: v } }));
const services = ['checkout', 'payments', 'inventory', 'gateway'];
const routes = [['GET', '/api/orders'], ['POST', '/api/cart'], ['POST', '/api/payments/charge'], ['GET', '/api/items/{id}']];

let n = 0;
const ns = (t) => t * 1e6;
const batch = (times) => {
  const spans = services.map(() => []), logs = services.map(() => []);
  for (const at of times) {
    const i = n++, s = i % 4, [method, route] = routes[(i >> 2) % 4];
    const status = i % 23 === 0 ? 500 : i % 13 === 0 ? 404 : 200, ms = 15 + ((i * 37) % 600);
    const traceId = randomBytes(16), spanId = randomBytes(8);
    spans[s].push({ traceId, spanId, name: `${method} ${route}`, kind: 2, startTimeUnixNano: ns(at), endTimeUnixNano: ns(at + ms), status: status === 500 ? { code: 2, message: 'upstream timeout' } : { code: 1 },
      attributes: attrs({ 'http.request.method': method, 'http.route': route, 'url.path': route.replace('{id}', String(1000 + (i % 90))), 'http.response.status_code': status }) });
    const [level, severityNumber] = status === 500 ? ['ERROR', 17] : status === 404 ? ['WARN', 13] : ['INFO', 9];
    logs[s].push({ timeUnixNano: ns(at + ms), observedTimeUnixNano: ns(at + ms), severityText: level, severityNumber, traceId, spanId,
      body: { stringValue: `${method} ${route} ${status} in ${ms}ms` }, attributes: attrs({ 'http.response.status_code': status }) });
  }
  const resource = (s) => ({ attributes: attrs({ 'service.name': services[s], 'service.version': '1.4.2', 'deployment.environment': 'production' }) });
  return Promise.all([
    send('traces', 'opentelemetry.proto.collector.trace.v1.ExportTraceServiceRequest', { resourceSpans: services.map((_, s) => ({ resource: resource(s), scopeSpans: [{ spans: spans[s] }] })) }),
    send('logs', 'opentelemetry.proto.collector.logs.v1.ExportLogsServiceRequest', { resourceLogs: services.map((_, s) => ({ resource: resource(s), scopeLogs: [{ logRecords: logs[s] }] })) }),
  ]);
};

const now = Date.now();
await batch(Array.from({ length: 600 }, (_, k) => now - 3600_000 + k * 6_000));
console.log('backfilled an hour; streaming');
setInterval(() => batch(Array(3).fill(Date.now())).catch((e) => console.error(e.message)), 1000);
