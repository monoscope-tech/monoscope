import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { afterEach, beforeAll, expect, test, vi } from 'vitest';
import '../src/widgets';

let htmx: any;
const initController = new AbortController();
beforeAll(() => {
  Object.defineProperty(document, 'adoptedStyleSheets', { value: [], writable: true, configurable: true });
  CSSStyleSheet.prototype.replaceSync = () => {};
  const evaluate = XPathExpression.prototype.evaluate;
  XPathExpression.prototype.evaluate = function (node, type = 0, result = null) {
    return evaluate.call(this, node, type, result);
  };
  const source = readFileSync(join(__dirname, '../../static/public/assets/deps/htmx/htmx-4.0.0.min.js'), 'utf8');
  htmx = new Function(`${source}; return htmx;`)();
  (window as any).htmx = htmx;
  new Function(readFileSync(join(__dirname, '../../static/public/assets/deps/htmx/htmx-2-compat.js'), 'utf8'))();
  new Function(readFileSync(join(__dirname, '../../static/public/assets/js/thirdparty/_hyperscript_web0_9_93.min.js'), 'utf8')).call(window);

});

test('table sorting refetches with element-local hyperscript state and survives refresh', async () => {
  const sortBehavior = `on click
      set fetcher to the closest <[data-table-fetch]/>
      if not fetcher halt end
      set sort to '+' + my @data-sort-field
      if my @data-sort-direction is 'asc' set sort to '-' + my @data-sort-field end
      set fetcher.dataset.tableSort to sort
      trigger 'table-sort' on fetcher
    `;
  document.body.innerHTML = `<div data-table-fetch hx-get="/table" hx-trigger="table-sort, update-query from:window" hx-target="#results" hx-select="#results" hx-swap="outerHTML" hx-vals="js:{'table-sort': this.dataset.tableSort || ''}">
    <table id="results"><thead><tr><th><button data-sort-field="duration" data-sort-direction="none" _="${sortBehavior}">Duration</button></th></tr></thead><tbody><tr><td>1.2k</td></tr><tr><td>9</td></tr></tbody></table>
  </div><div data-table-fetch id="other"></div>`;
  const calls: URL[] = [];
  vi.stubGlobal('fetch', vi.fn(async (input) => {
    calls.push(new URL(String(input), location.origin));
    return new Response(`<table id="results"><thead><tr><th><button data-sort-field="duration" data-sort-direction="asc" _="${sortBehavior}">Duration</button></th></tr></thead><tbody><tr><td>server result</td></tr></tbody></table>`, {headers: {'Content-Type': 'text/html'}});
  }));
  htmx.process(document.body);
  (window as any)._hyperscript.processNode(document.body);
  (document.querySelector('button') as HTMLButtonElement).click();
  await vi.waitFor(() => expect(document.querySelector('tbody')?.textContent).toBe('server result'));
  expect(calls[0].searchParams.get('table-sort')).toBe('+duration');
  expect(document.querySelector('#other')?.hasAttribute('hx-vals')).toBe(false);
  window.dispatchEvent(new CustomEvent('update-query'));
  await vi.waitFor(() => expect(calls).toHaveLength(2));
  expect(calls[1].searchParams.get('table-sort')).toBe('+duration');
  (window as any)._hyperscript.processNode(document.body);
  (document.querySelector('button') as HTMLButtonElement).click();
  await vi.waitFor(() => expect(calls).toHaveLength(3));
  expect(calls[2].searchParams.get('table-sort')).toBe('-duration');
});
afterEach(() => {
  initController.abort();
  document.body.replaceChildren();
  document.dispatchEvent(new CustomEvent('htmx:after:swap'));
  history.replaceState({}, '', '/');
  delete (window as any).echarts;
  vi.unstubAllGlobals();
});

const config = (chartId: string) => ({
  chartId, chartType: 'bar', widgetType: 'timeseries', pid: 'p',
  query: 'summarize count(*) by bin_auto(timestamp)', querySQL: '',
  opt: { dataset: {}, series: [], legend: {}, yAxis: {} },
});
const shell = () => `<a id="explorer" href="/log_explorer">Explorer</a><main id="main-content">${['volume', 'latency'].map(id => `
  <section><div id="${id}" data-chart-widget></div>
  <script data-chart-init="${id}">document.dispatchEvent(new CustomEvent('test-init-chart', { detail: '${id}' }));</script></section>
`).join('')}</main>`;

test('Explorer morph navigation reinitializes identical widget scripts and loads the cleared query', async () => {
  const instances: any[] = [];
  // Bridge the inline script's jsdom realm to the real module initialization path.
  document.addEventListener('test-init-chart', ((event: CustomEvent<string>) => {
    (window as any).queueChartInit(() => (window as any).chartWidget(config(event.detail)), event.detail);
  }) as EventListener, { signal: initController.signal });
  (window as any).echarts = {
    getInstanceByDom: () => null,
    init: (el: HTMLElement) => {
      const instance = {
        setOption: vi.fn(() => { el.innerHTML = '<canvas></canvas>'; }),
        hideLoading: vi.fn(), showLoading: vi.fn(), dispatchAction: vi.fn(),
        isDisposed: () => false, dispose: vi.fn(), on: vi.fn(),
      };
      instances.push(instance);
      return instance;
    },
  };
  vi.stubGlobal('fetch', vi.fn(async () => new Response(JSON.stringify({
    from: 0, to: 1, headers: ['timestamp', 'count'], dataset: [[0, 1]],
  }), { headers: { 'Content-Type': 'application/json' } })));
  history.replaceState({}, '', '/log_explorer?since=14D&query=name%20%3D%3D%20%22monoscope.http%22');
  document.body.innerHTML = shell();
  for (const id of ['volume', 'latency']) (window as any).chartWidget(config(id));
  (globalThis as any).triggerIntersection();
  await vi.waitFor(() => expect(instances.every(c => c.hideLoading.mock.calls.length === 1)).toBe(true));

  for (let navigation = 0; navigation < 2; navigation++) {
    // Navigate while both outgoing charts have an unfinished refresh.
    const signals: AbortSignal[] = [];
    const pendingFetch = (_url: unknown, options?: RequestInit) => new Promise<Response>((_resolve, reject) => {
      const signal = options!.signal!;
      signals.push(signal);
      signal.addEventListener('abort', () => reject(signal.reason), { once: true });
    });
    vi.mocked(fetch).mockImplementationOnce(pendingFetch).mockImplementationOnce(pendingFetch);
    window.dispatchEvent(new CustomEvent('update-query'));
    await vi.waitFor(() => expect(signals).toHaveLength(2));
    history.replaceState({}, '', '/log_explorer');
    await htmx.swap({ text: shell(), select: '#main-content', target: '#main-content',
      sourceElement: document.getElementById('explorer'), swap: 'outerMorph' });
    await vi.waitFor(() => expect(instances).toHaveLength(4 + navigation * 2));
    (globalThis as any).triggerIntersection();
    await vi.waitFor(() => expect(instances.slice(-2).every(c => c.hideLoading.mock.calls.length === 1)).toBe(true));
    expect(document.querySelectorAll('canvas')).toHaveLength(2);
    const url = new URL(String(vi.mocked(fetch).mock.calls.at(-1)![0]), location.origin);
    expect(url.searchParams.get('query')).toBe(config('volume').query);
    expect(url.searchParams.has('since')).toBe(false);
    expect(instances.slice(0, -2).every(c => c.dispose.mock.calls.length === 1)).toBe(true);
    expect(signals.every(signal => signal.aborted)).toBe(true);
  }
});
