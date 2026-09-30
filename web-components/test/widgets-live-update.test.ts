import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { afterEach, beforeAll, expect, test, vi } from 'vitest';
import '../src/widgets';
import { graphic, color } from 'echarts';

// The RUM page re-renders whole panels on the live tick; a morph keeps the chart element,
// so a re-run of its init script must update the live instance instead of blanking it.
let htmx: any;
beforeAll(() => {
  Object.defineProperty(document, 'adoptedStyleSheets', { value: [], writable: true, configurable: true });
  CSSStyleSheet.prototype.replaceSync = () => {};
  const evaluate = XPathExpression.prototype.evaluate;
  XPathExpression.prototype.evaluate = function (node, type = 0, result = null) { return evaluate.call(this, node, type, result); };
  const source = readFileSync(join(__dirname, '../../static/public/assets/deps/htmx/htmx-4.0.0.min.js'), 'utf8');
  htmx = new Function(`${source}; return htmx;`)();
  (window as any).htmx = htmx;
  new Function(readFileSync(join(__dirname, '../../static/public/assets/deps/htmx/htmx-2-compat.js'), 'utf8'))();
});
afterEach(() => {
  document.body.replaceChildren();
  document.dispatchEvent(new CustomEvent('htmx:after:swap'));
  delete (window as any).echarts;
  vi.unstubAllGlobals();
});

const instances: any[] = [];
const fakeECharts = () => {
  instances.length = 0;
  const byEl = new Map<Element, any>();
  (window as any).echarts = {
    graphic, color,
    getInstanceByDom: (el: Element) => byEl.get(el) ?? null,
    init: (el: HTMLElement) => {
      const instance = {
        group: '', disposed: false,
        setOption: vi.fn(() => { el.innerHTML = '<canvas></canvas>'; }),
        hideLoading: vi.fn(), showLoading: vi.fn(), dispatchAction: vi.fn(), on: vi.fn(), off: vi.fn(),
        getOption: vi.fn(() => ({ series: [] })), getModel: vi.fn(),
        isDisposed: () => instance.disposed,
        dispose: vi.fn(() => { instance.disposed = true; byEl.delete(el); el.innerHTML = ''; }),
      };
      byEl.set(el, instance);
      instances.push(instance);
      return instance;
    },
  };
};
const config = (chartId: string, rows: number[][]) => ({
  chartId, chartType: 'line', widgetType: 'timeseries', pid: null, query: null, querySQL: '',
  opt: { dataset: { source: [['timestamp', 'P75'], ...rows] }, series: [{ type: 'line' }], legend: {}, yAxis: {}, xAxis: {} },
});
const panel = (rows: number[][]) => `<div id="panel" hx-get="/panel" hx-trigger="update-query from:window" hx-target="#panel" hx-select="#panel" hx-swap="outerMorph">
  <p id="label">rows:${rows.length}</p><div id="lcp" data-chart-widget hx-morph-skip></div>
  <script data-chart-init="lcp">document.dispatchEvent(new CustomEvent('test-init-chart', { detail: ${JSON.stringify(rows)} }));</script></div>`;

test('a morph-swapped panel updates its live chart in place; a replaced element initializes anew', async () => {
  fakeECharts();
  document.body.innerHTML = panel([[1, 10]]);
  (window as any).chartWidget(config('lcp', [[1, 10]]));
  const controller = new AbortController();
  document.addEventListener('test-init-chart', ((e: CustomEvent<number[][]>) => (window as any).chartWidget(config('lcp', e.detail))) as EventListener, { signal: controller.signal });
  vi.stubGlobal('fetch', vi.fn(async () => new Response(panel([[1, 10], [2, 20]]), { headers: { 'Content-Type': 'text/html' } })));
  htmx.process(document.body);
  const chartEl = document.getElementById('lcp')!;
  expect(chartEl.hasAttribute('hx-morph-skip')).toBe(true);

  window.dispatchEvent(new CustomEvent('update-query', { detail: { source: 'auto-refresh' } }));
  await vi.waitFor(() => expect(document.getElementById('label')?.textContent).toBe('rows:2'));
  await vi.waitFor(() => expect(instances[0].setOption).toHaveBeenCalledTimes(2));
  expect(instances).toHaveLength(1);
  expect(instances[0].dispose).not.toHaveBeenCalled();
  expect(document.getElementById('lcp')).toBe(chartEl); // morph kept the node, and its canvas
  expect(chartEl.querySelector('canvas')).not.toBeNull();
  const [option, notMerge] = instances[0].setOption.mock.calls[1];
  expect(option.dataset.source).toEqual([['timestamp', 'P75'], [1, 10], [2, 20]]);
  expect(notMerge).toBe(true);
  controller.abort();

  // outerHTML: the old node is gone, so the same id gets a fresh instance and the old one is torn down.
  document.getElementById('panel')!.outerHTML = panel([[3, 30]]);
  (window as any).chartWidget(config('lcp', [[3, 30]]));
  expect(instances).toHaveLength(2);
  expect(instances[0].dispose).toHaveBeenCalledTimes(1);
  expect(document.getElementById('lcp')!.hasAttribute('hx-morph-skip')).toBe(true);
});

test('an in-place update with a changed query aborts the pending fetch; an unchanged one refreshes quietly', async () => {
  fakeECharts();
  document.body.innerHTML = '<div id="lazy" data-chart-widget hx-morph-skip></div>';
  const lazy = (query: string) => ({ ...config('lazy', []), opt: { dataset: {}, series: [], legend: {}, yAxis: {} }, pid: 'p', query });
  const signals: AbortSignal[] = [];
  vi.stubGlobal('fetch', vi.fn((_url: unknown, options?: RequestInit) => new Promise<Response>((_resolve, reject) => {
    signals.push(options!.signal!);
    options!.signal!.addEventListener('abort', () => reject(options!.signal!.reason), { once: true });
  })));
  (window as any).chartWidget(lazy('a'));
  (globalThis as any).triggerIntersection();
  await vi.waitFor(() => expect(signals).toHaveLength(1));
  const draws = instances[0].setOption.mock.calls.length;

  (window as any).chartWidget(lazy('a')); // same query while the first fetch is still in flight
  expect(signals).toHaveLength(1);
  expect(instances[0].setOption).toHaveBeenCalledTimes(draws);

  (window as any).chartWidget(lazy('b'));
  await vi.waitFor(() => expect(signals).toHaveLength(2));
  expect(instances).toHaveLength(1);
  expect(signals[0].aborted).toBe(true);
  expect(new URL(String(vi.mocked(fetch).mock.calls[1][0]), location.origin).searchParams.get('query')).toMatch(/^b\b/);
});

test('a loader fetch keeps the rendered stat value until the response replaces it', async () => {
  fakeECharts();
  document.body.innerHTML = '<div id="pv" data-chart-widget></div><span id="pvValue">8.7K views</span>';
  let resolve!: (r: Response) => void;
  vi.stubGlobal('fetch', vi.fn(() => new Promise<Response>((r) => { resolve = r; })));
  (window as any).chartWidget({ ...config('pv', []), opt: { dataset: {}, series: [], legend: {}, yAxis: {} }, pid: 'p', query: 'summarize count(*)', widgetType: 'timeseries_stat', summarizeBy: 'sum' });
  (globalThis as any).triggerIntersection();
  await vi.waitFor(() => expect(fetch).toHaveBeenCalled());
  expect(document.getElementById('pvValue')!.textContent).toBe('8.7K views');
  resolve(new Response(JSON.stringify({ from: 0, to: 60000, headers: ['timestamp', 'count'], dataset: [[0, 12]], stats: { count: 1, max: 12, max_group_sum: 12, sum: 12 } }), { headers: { 'Content-Type': 'application/json' } }));
  await vi.waitFor(() => expect(document.getElementById('pvValue')!.textContent).not.toBe('8.7K views'));
  expect(document.getElementById('pvValue')!.textContent).toBe('12');
});

test('a 401 fails the chart unless a second 401 confirms it, and then reloads at most once per cooldown', async () => {
  fakeECharts();
  sessionStorage.removeItem('monoscope:session-reload');
  document.body.innerHTML = '<div id="s" data-chart-widget></div><div id="s_error" class="hidden"><span id="s_errorMsg"></span><button id="s_retry" hidden></button></div>';
  const statuses = [401, 200, 401, 401];
  vi.stubGlobal('fetch', vi.fn(async () => new Response('{}', { status: statuses.shift() ?? 200, headers: { 'Content-Type': 'application/json' } })));
  const widget = { ...config('s', []), opt: { dataset: {}, series: [], legend: {}, yAxis: {} }, pid: 'p', query: 'a' };
  (window as any).chartWidget(widget);
  (globalThis as any).triggerIntersection();
  await vi.waitFor(() => expect(document.getElementById('s_error')!.classList.contains('hidden')).toBe(false), { timeout: 5000 });
  expect(fetch).toHaveBeenCalledTimes(2); // the transient 401 was re-checked, not reloaded
  expect(sessionStorage.getItem('monoscope:session-reload')).toBeNull();
  expect(document.getElementById('s_errorMsg')!.textContent).toBe('Session expired. Retry reloads the page.');
  expect(document.getElementById('s_retry')!.hidden).toBe(false);

  (window as any).chartWidget({ ...widget, query: 'b' });
  await vi.waitFor(() => expect(fetch).toHaveBeenCalledTimes(4), { timeout: 5000 });
  await vi.waitFor(() => expect(sessionStorage.getItem('monoscope:session-reload')).not.toBeNull());
});

test('a chart whose element left with a morphed fragment is disposed once the swap settles', async () => {
  fakeECharts();
  document.body.innerHTML = panel([[1, 10]]);
  (window as any).chartWidget(config('lcp', [[1, 10]]));
  vi.stubGlobal('fetch', vi.fn(async () => new Response('<div id="panel"><p id="label">no chart</p></div>', { headers: { 'Content-Type': 'text/html' } })));
  htmx.process(document.body);
  window.dispatchEvent(new CustomEvent('update-query', { detail: { source: 'auto-refresh' } }));
  await vi.waitFor(() => expect(document.getElementById('label')?.textContent).toBe('no chart'));
  await vi.waitFor(() => expect(instances[0].dispose).toHaveBeenCalledTimes(1));
});
