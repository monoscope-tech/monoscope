import { afterEach, describe, expect, test, vi } from 'vitest';
import '../src/widgets';

const frame = () => new Promise<void>((resolve) => requestAnimationFrame(() => resolve()));
const chartData = { from: 0, to: 1, headers: ['timestamp'], dataset: [[0]], rows_per_min: 0, stats: { count: 1, max: 1, max_group_sum: 1 } };

const chart = () => ({
  group: '',
  setOption: vi.fn(),
  hideLoading: vi.fn(),
  showLoading: vi.fn(),
  dispatchAction: vi.fn(),
  isDisposed: () => false,
  dispose: vi.fn(),
  resize: vi.fn(),
  getModel: vi.fn(),
  getOption: vi.fn(() => ({ series: [] })),
  on: vi.fn(),
  off: vi.fn(),
});

const widget = (chartId: string) => ({
  chartType: 'bar',
  opt: { dataset: {}, series: [], legend: {}, yAxis: {} },
  chartId,
  query: '',
  sql: '',
  querySQL: '',
  theme: 'default',
  yAxisLabel: '',
  pid: 'p',
  summarizeBy: 'sum',
  summarizeByPrefix: '',
  widgetType: 'timeseries',
  queryAST: '',
});

describe('Log Explorer chart auto-refresh', () => {
  const originalFetch = globalThis.fetch;

  afterEach(() => {
    vi.restoreAllMocks();
    globalThis.fetch = originalFetch;
    delete (window as any).echarts;
    delete (window as any).logListTable;
    document.body.innerHTML = '';
    document.dispatchEvent(new CustomEvent('htmx:after:swap'));
    history.replaceState({}, '', '/');
  });

  test('holds fetch slots until streaming bodies finish', async () => {
    const bodies: ReadableStreamDefaultController<Uint8Array>[] = [];
    document.body.innerHTML = Array.from({ length: 5 }, (_, i) => `<div id="prefetch-${i}" data-chart-widget></div>`).join('');
    globalThis.fetch = vi.fn(async () => new Response(new ReadableStream<Uint8Array>({ start(body) { bodies.push(body); } }),
      { headers: { 'Content-Type': 'application/x-ndjson' } })) as any;
    for (let i = 0; i < 5; i++) (window as any).__chartPrefetch.push(widget(`prefetch-${i}`));
    await vi.waitFor(() => expect(bodies).toHaveLength(4));
    await frame();
    expect(globalThis.fetch).toHaveBeenCalledTimes(4);
    const complete = new TextEncoder().encode(JSON.stringify({ type: 'complete', data: chartData }) + '\n');
    bodies[0].enqueue(complete);
    await vi.waitFor(() => expect(bodies).toHaveLength(5));
    for (const body of bodies.slice(1)) body.enqueue(complete);
  });

  test('renders partial points before completion and finalizes statistics afterward', async () => {
    const instance = chart();
    (window as any).echarts = { getInstanceByDom: () => null, init: () => instance };
    document.body.innerHTML = '<div id="volume" data-chart-widget></div><div id="volumeValue"></div>';
    let body!: ReadableStreamDefaultController<Uint8Array>;
    globalThis.fetch = vi.fn(async () => new Response(new ReadableStream<Uint8Array>({ start(controller) { body = controller; } }),
      { headers: { 'Content-Type': 'application/x-ndjson' } })) as any;
    (window as any).chartWidget(widget('volume'));
    (globalThis as any).triggerIntersection();
    await vi.waitFor(() => expect(globalThis.fetch).toHaveBeenCalledTimes(1));
    const send = (type: string) => body.enqueue(new TextEncoder().encode(JSON.stringify({ type, data: chartData }) + '\n'));
    send('partial');
    await vi.waitFor(() => expect(document.getElementById('volume')!.getAttribute('data-chart-partial')).toBe('true'));
    expect(instance.setOption).toHaveBeenCalled();
    expect(document.getElementById('volume')!.getAttribute('aria-busy')).toBe('true');
    send('complete');
    await vi.waitFor(() => expect(document.getElementById('volume')!.getAttribute('aria-busy')).toBe('false'));
    expect(document.getElementById('volume')!.hasAttribute('data-chart-partial')).toBe(false);
  });

  test('counts failures per widget and does not hide a failure for a changed query', async () => {
    const instances = new Map<string, ReturnType<typeof chart>>();
    (window as any).echarts = { getInstanceByDom: () => null, init: (el: HTMLElement) => {
      const instance = chart(); instances.set(el.id, instance); return instance;
    } };
    document.body.innerHTML = ['volume', 'latency'].map(id =>
      `<div id="${id}" data-chart-widget></div><div id="${id}_error" class="hidden"><span id="${id}_errorMsg"></span></div>`).join('');
    globalThis.fetch = vi.fn(async (url: string) => ({ ok: true, json: async () =>
      new URL(url, location.origin).searchParams.get('pid') === 'volume' ? { error: 'query failed' } : chartData
    })) as any;
    for (const id of ['volume', 'latency']) {
      const config = widget(id);
      (config.opt.dataset as any).source = [['timestamp', 'count'], [0, 1]];
      (window as any).chartWidget({ ...config, pid: id });
    }
    const tick = async (n: number) => {
      window.dispatchEvent(new CustomEvent('update-query', { detail: { source: 'auto-refresh' } }));
      await vi.waitFor(() => {
        for (const instance of instances.values()) expect(instance.hideLoading).toHaveBeenCalledTimes(n);
      });
    };
    await tick(1);
    await tick(2);
    expect(document.getElementById('volume_error')!.classList.contains('hidden')).toBe(true);
    await tick(3);
    expect(document.getElementById('volume_errorMsg')!.textContent).toContain('last successful data');
    expect(document.getElementById('latency_error')!.classList.contains('hidden')).toBe(true);
    history.replaceState({}, '', '/?query=level%20%3D%3D');
    await tick(4);
    expect(document.getElementById('volume_errorMsg')!.textContent).toBe('query failed');
  });

  test.each(['network', 'query'])('%s failures keep data quiet until the third consecutive background failure', async (failure) => {
    const instance = chart();
    (window as any).echarts = { getInstanceByDom: () => null, init: () => instance };
    document.body.innerHTML = '<div id="volume" data-chart-widget></div><div id="volumeValue"></div>' +
      '<div id="volume_error" class="hidden"><span id="volume_errorMsg"></span><button id="volume_retry">Retry</button></div>';
    vi.spyOn(console, 'error').mockImplementation(() => {});
    let fail = false;
    globalThis.fetch = vi.fn(async () => {
      if (fail && failure === 'network') throw new Error('offline');
      return { ok: true, json: async () => fail ? { error: 'Query execution failed' } : chartData };
    }) as any;
    (window as any).chartWidget(widget('volume'));
    (globalThis as any).triggerIntersection();
    await vi.waitFor(() => expect(instance.hideLoading).toHaveBeenCalledTimes(1));
    const value = document.getElementById('volumeValue')!.textContent;
    const banner = document.getElementById('volume_error')!;
    let completed = 1;
    const refresh = async (background = true) => {
      window.dispatchEvent(new CustomEvent('update-query', { detail: { source: background ? 'auto-refresh' : 'user' } }));
      completed++;
      await vi.waitFor(() => expect(instance.hideLoading).toHaveBeenCalledTimes(completed));
    };
    fail = true;
    await refresh();
    await refresh();
    expect(banner.classList.contains('hidden')).toBe(true);
    expect(document.getElementById('volumeValue')!.textContent).toBe(value);
    fail = false;
    await refresh(); // recovery resets the count
    fail = true;
    await refresh();
    await refresh();
    expect(banner.classList.contains('hidden')).toBe(true);
    await refresh();
    expect(banner.classList.contains('hidden')).toBe(false);
    expect(document.getElementById('volume_errorMsg')!.textContent).toContain('last successful data');
    expect(document.getElementById('volumeValue')!.textContent).toBe(value);
    fail = false;
    await refresh();
    expect(banner.classList.contains('hidden')).toBe(true);
    fail = true;
    await refresh(false); // an explicit request reports failure immediately
    expect(banner.classList.contains('hidden')).toBe(false);
  });

  test('refreshes each chart once without flashing or refetching the list', async () => {
    const charts = new Map<string, ReturnType<typeof chart>>();
    (window as any).echarts = {
      getInstanceByDom: () => null,
      init: (el: HTMLElement) => {
        const instance = chart();
        charts.set(el.id, instance);
        return instance;
      },
    };
    document.body.innerHTML = ['volume', 'latency']
      .map((id) => `<div id="${id}" data-chart-widget></div><div id="${id}_loader" class="hidden"></div><div id="${id}_bordered"></div>`)
      .join('');

    const responses: Array<(data: typeof chartData) => void> = [];
    // `ok` is not optional dressing: a real Response always carries it, and updateChartData
    // now refuses to parse a non-2xx body, so a mock without it fails every fetch.
    globalThis.fetch = vi.fn(async () => ({ ok: true, status: 200, json: () => new Promise<typeof chartData>((resolve) => responses.push(resolve)) })) as any;
    (window as any).chartWidget(widget('volume'));
    (window as any).chartWidget(widget('latency'));
    (globalThis as any).triggerIntersection();
    await vi.waitFor(() => expect(responses).toHaveLength(2));
    responses.splice(0).forEach((resolve) => resolve(chartData));
    await frame();
    await frame();

    const refetchList = vi.fn();
    (window as any).logListTable = { refetchLogs: refetchList };
    window.dispatchEvent(new CustomEvent('update-query', { detail: { source: 'auto-refresh' } }));
    await vi.waitFor(() => expect(responses).toHaveLength(2));
    await frame();

    expect(globalThis.fetch).toHaveBeenCalledTimes(4);
    expect(refetchList).not.toHaveBeenCalled();
    expect(document.querySelector('#volume_loader')?.classList.contains('hidden')).toBe(true);
    expect(document.querySelector('#latency_loader')?.classList.contains('hidden')).toBe(true);
    expect(charts.get('volume')?.setOption).toHaveBeenCalled();
    expect(charts.get('latency')?.setOption).toHaveBeenCalled();

    responses.splice(0).forEach((resolve) => resolve(chartData));
    await frame();
  });

  test('shows the loader when the time picker refreshes a chart', async () => {
    (window as any).echarts = { getInstanceByDom: () => null, init: () => chart() };
    document.body.innerHTML = '<div id="volume" data-chart-widget></div><div id="volume_loader" class="hidden"></div><div id="volume_bordered"></div>';

    const responses: Array<(data: typeof chartData) => void> = [];
    globalThis.fetch = vi.fn(async () => ({ ok: true, status: 200, json: () => new Promise<typeof chartData>((resolve) => responses.push(resolve)) })) as any;
    (window as any).chartWidget(widget('volume'));
    (globalThis as any).triggerIntersection();
    await vi.waitFor(() => expect(responses).toHaveLength(1));
    responses.splice(0).forEach((resolve) => resolve(chartData));
    await frame();
    await frame();

    // TimePicker.hs emits an unadorned update-query event after its form submits.
    window.dispatchEvent(new CustomEvent('update-query'));
    await vi.waitFor(() => expect(responses).toHaveLength(1));
    await frame();

    expect(document.querySelector('#volume_loader')?.classList.contains('hidden')).toBe(false);

    responses.splice(0).forEach((resolve) => resolve(chartData));
    await frame();
    expect(document.querySelector('#volume_loader')?.classList.contains('hidden')).toBe(true);
  });

  // Regression guard for the critical "SyntaxError: Failed to fetch new data: The string did
  // not match the expected pattern." — Safari's wording for JSON.parse on a non-JSON body.
  // updateChartData parsed every response regardless of status, so an HTML error page (or a
  // 404 from a route that moved in a deploy) surfaced as an opaque parse failure with no
  // status on it, and that is what reached the issue list.
  test('a non-2xx widget response is reported with its status, not as a parse error', async () => {
    (window as any).echarts = { getInstanceByDom: () => null, init: () => chart() };
    document.body.innerHTML =
      '<div id="volume" data-chart-widget></div><div id="volume_loader" class="hidden"></div>' +
      '<div id="volume_bordered"></div><div id="volume_error" class="hidden"><div id="volume_errorMsg"></div><button id="volume_retry" hidden>Retry</button></div>';

    const logged: string[] = [];
    const spy = vi.spyOn(console, 'error').mockImplementation((...args: unknown[]) => {
      logged.push(args.map(String).join(' '));
    });
    // A gateway returning HTML: `ok` is false and json() rejects exactly the way Safari's does.
    globalThis.fetch = vi.fn(async () => ({
      ok: false,
      status: 502,
      statusText: 'Bad Gateway',
      json: () => Promise.reject(new SyntaxError('The string did not match the expected pattern.')),
    })) as any;

    (window as any).chartWidget(widget('volume'));
    (globalThis as any).triggerIntersection();
    await vi.waitFor(() => expect(logged.length).toBeGreaterThan(0));
    spy.mockRestore();

    const retry = document.getElementById('volume_retry') as HTMLButtonElement;
    expect(retry.hidden).toBe(false);
    expect(document.getElementById('volume_error')?.classList.contains('hidden')).toBe(false);
    const refetchList = vi.fn();
    (window as any).logListTable = { refetchLogs: refetchList };
    const response = Promise.withResolvers<typeof chartData>();
    globalThis.fetch = vi.fn(async () => ({ ok: true, json: () => response.promise })) as any;
    retry.click();
    retry.click();
    expect(retry.disabled).toBe(true);
    await vi.waitFor(() => expect(globalThis.fetch).toHaveBeenCalledTimes(1));
    response.resolve(chartData);
    await vi.waitFor(() => expect(retry.disabled).toBe(false));
    expect(document.getElementById('volume_error')?.classList.contains('hidden')).toBe(true);
    expect(refetchList).not.toHaveBeenCalled();

    const all = logged.join(' ');
    expect(all).toContain('502');
    expect(all).not.toContain('did not match the expected pattern');
  });
});
