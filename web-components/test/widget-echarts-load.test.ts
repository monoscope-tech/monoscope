import { afterEach, expect, test, vi } from 'vitest';
import '../src/widgets';

afterEach(() => {
  vi.restoreAllMocks();
  document.body.replaceChildren();
  delete (window as any).echarts;
});

const chart = (id: string) =>
  `<section data-chart-config='${JSON.stringify({ chartId: id, pid: 'p', echartOpt: JSON.stringify({ dataset: { source: [[0]] }, series: [], tooltip: {} }) })}'><div id="${id}"></div></section>`;

// Regression: a failed ECharts script load was an unhandled rejection the browser SDK filed
// ("Failed to load …/echarts.min.js"), and its cached rejection meant no chart on the page retried.
test('a failed ECharts load is warned, and the next chart retries it', async () => {
  delete (window as any).echarts;
  const scripts: string[] = [];
  vi.spyOn(document.head, 'append').mockImplementation((...nodes: any[]) => {
    for (const n of nodes) { scripts.push(n.src); queueMicrotask(() => n.onerror?.()); }
  });
  const errors = vi.spyOn(console, 'error').mockImplementation(() => {});
  const warnings = vi.spyOn(console, 'warn').mockImplementation(() => {});
  const unhandled = vi.fn();
  window.addEventListener('unhandledrejection', unhandled);

  document.body.innerHTML = chart('first');
  document.dispatchEvent(new Event('htmx:after:process'));
  await vi.waitFor(() => expect(warnings).toHaveBeenCalledWith('charts could not load:', expect.anything()));

  document.body.innerHTML = chart('second');
  document.dispatchEvent(new Event('htmx:after:process'));
  await vi.waitFor(() => expect(scripts.filter((s) => s.includes('echarts'))).toHaveLength(2));
  await new Promise((r) => setTimeout(r, 0));
  expect(errors).not.toHaveBeenCalled();
  expect(unhandled).not.toHaveBeenCalled();
  window.removeEventListener('unhandledrejection', unhandled);
});
