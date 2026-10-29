// Synthetic Live Tail load at the server's real shape: a drain every 250ms, chunked into
// 100-row `log` frames, bodies capped at 8000 chars. `?frames=` sets frames per drain.
import '../src/query-editor/query-editor';
import '../src/live-tail';

const params = new URLSearchParams(location.search);
const framesPerDrain = Number(params.get('frames') ?? '5');
const schemaFields = Number(params.get('fields') ?? '1500');

let seq = 0;
const word = (n: number) => 'abcdefghij'.repeat(Math.ceil(n / 10)).slice(0, n);
const jsonBody = (n: number) => {
  const obj: Record<string, unknown> = { msg: `request handled ${n}`, status: 200 + (n % 5), latency_ms: n % 997, ok: n % 3 === 0 };
  for (let i = 0; i < 25; i++) obj[`attr_${i}`] = word(40 + (i % 7) * 10);
  obj[`user_${n % 400}`] = { id: n, email: `u${n}@x.io`, tags: ['a', 'b'] }; // dynamic key names
  obj.nested = { a: { b: { c: word(200) } }, list: [1, 2, 3] };
  obj.payload = word(5500);
  return 'INFO handler ' + JSON.stringify(obj);
};
const kvBody = (n: number) => Array.from({ length: 40 }, (_, i) => `key${i}=${i % 2 ? '"' + word(30) + '"' : n + i}`).join(' ');
const row = () => {
  const n = seq++;
  const body = n % 3 === 0 ? jsonBody(n) : n % 3 === 1 ? kvBody(n) : `short message number ${n}`;
  return {
    shape: 'log',
    log: {
      id: crypto.randomUUID(),
      timestamp: new Date(Date.now() + n).toISOString(),
      level: ['info', 'warn', 'error', 'debug'][n % 4],
      service: `svc-${n % 7}`,
      trace_id: null, span_id: null, name: null,
      body: body.slice(0, 8000),
      truncated: body.length > 8000,
    },
  };
};
// Pre-serialised so the generator costs the page nothing during measurement.
const frames = Array.from({ length: 60 }, () => JSON.stringify(Array.from({ length: 100 }, row)));

class FakeEventSource {
  listeners: Record<string, ((e: any) => void)[]> = {};
  onerror: ((e: Event) => void) | null = null;
  private timer: number;
  private i = 0;
  constructor(public url: string) {
    setTimeout(() => this.emit('ready', '{}'), 10);
    this.timer = window.setInterval(() => {
      for (let f = 0; f < framesPerDrain; f++) this.emit('log', frames[this.i++ % frames.length]);
    }, 250);
  }
  addEventListener(type: string, fn: (e: any) => void) { (this.listeners[type] ??= []).push(fn); }
  emit(type: string, data: string) { for (const fn of this.listeners[type] ?? []) fn({ data }); }
  close() { clearInterval(this.timer); }
}
(window as any).EventSource = FakeEventSource;

const fields = Object.fromEntries(Array.from({ length: schemaFields }, (_, i) => [`attributes.field_${i}.name`, { type: 'string', examples: [] }]));
fields['resource.service.name'] = { type: 'string', examples: ['svc-1', 'svc-2'] };
const realFetch = window.fetch.bind(window);
window.fetch = async (input: RequestInfo | URL, init?: RequestInit) => {
  const url = String(input);
  const json = (body: unknown) => new Response(JSON.stringify(body), { headers: { 'Content-Type': 'application/json' } });
  if (url.includes('/schema')) return json({ fields });
  if (url.includes('/subscriptions') && init?.method === 'POST' && !url.includes('/renew'))
    return json({ subscription_id: crypto.randomUUID(), stream_url: '/stream', expires_at: new Date(Date.now() + 45000).toISOString() });
  if (url.includes('/validate')) return json({ ok: true });
  if (url.startsWith('/')) return json({});
  return realFetch(input, init);
};

const longTasks: number[] = [];
new PerformanceObserver((l) => l.getEntries().forEach((e) => longTasks.push(e.duration))).observe({ type: 'longtask', buffered: true });
(window as any).bench = { longTasks };

const el = document.createElement('live-tail');
el.dataset.projectId = '00000000-0000-0000-0000-000000000000';
el.dataset.leaseSecs = '45';
document.getElementById('host')!.appendChild(el);
