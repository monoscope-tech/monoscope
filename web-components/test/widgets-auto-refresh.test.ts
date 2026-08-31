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
  opt: { dataset: {}, series: [], legend: {} },
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
    globalThis.fetch = originalFetch;
    delete (window as any).echarts;
    delete (window as any).logListTable;
    document.body.innerHTML = '';
    document.dispatchEvent(new CustomEvent('htmx:after:swap'));
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

  // Regression guard for the critical "SyntaxError: Failed to fetch new data: The string did
  // not match the expected pattern." — Safari's wording for JSON.parse on a non-JSON body.
  // updateChartData parsed every response regardless of status, so an HTML error page (or a
  // 404 from a route that moved in a deploy) surfaced as an opaque parse failure with no
  // status on it, and that is what reached the issue list.
  test('a non-2xx widget response is reported with its status, not as a parse error', async () => {
    (window as any).echarts = { getInstanceByDom: () => null, init: () => chart() };
    document.body.innerHTML =
      '<div id="volume" data-chart-widget></div><div id="volume_loader" class="hidden"></div>' +
      '<div id="volume_bordered"></div><div id="volume_error" class="hidden"></div><div id="volume_errorMsg"></div>';

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

    const all = logged.join(' ');
    expect(all).toContain('502');
    expect(all).not.toContain('did not match the expected pattern');
    // The user still gets the friendly banner; only the logged error changes.
    expect(document.getElementById('volume_error')?.classList.contains('hidden')).toBe(false);
  });
});
