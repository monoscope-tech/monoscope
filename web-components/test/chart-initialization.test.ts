import { describe, expect, test } from 'vitest';
import { isNearChartViewport } from '../src/chart-initialization';
import { registerChartDisposer } from '../src/widgets';

test('disposes a chart removed by an HTMX morph navigation', () => {
  const chart = document.createElement('div');
  chart.id = 'detached-chart';
  chart.dataset.chartWidget = '';
  document.body.append(chart);
  let disposals = 0;
  registerChartDisposer(chart.id, () => disposals++);

  chart.remove();
  document.dispatchEvent(new CustomEvent('htmx:after:swap'));

  expect(disposals).toBe(1);
});

describe('isNearChartViewport', () => {
  test('does not initialize charts beyond the small prefetch boundary', () => {
    expect(isNearChartViewport({ top: 1_250, bottom: 1_450 }, 1_000)).toBe(false);
  });

  test('keeps charts just below the viewport ready for scrolling', () => {
    expect(isNearChartViewport({ top: 1_149, bottom: 1_349 }, 1_000)).toBe(true);
  });
});
