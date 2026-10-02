import { describe, expect, test } from 'vitest';
import { chartLayoutForSize, isNearChartViewport } from '../src/chart-initialization';
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

test('short wide charts tighten label spacing and restore it when resized taller', () => {
  const options = {
    grid: { top: 8, bottom: 36 },
    legend: { show: true, bottom: 2, padding: [3, 6, 3, 6] },
    xAxis: { axisLabel: { margin: 8 } },
  };

  expect(chartLayoutForSize(options, 1000, 180)).toMatchObject({
    grid: { top: 4, bottom: 8 },
    legend: { bottom: 0, padding: [0, 4, 0, 4] },
    xAxis: { axisLabel: { margin: 4 } },
  });
  expect(chartLayoutForSize(options, 600, 180)).toEqual(options);
  expect(chartLayoutForSize(options, 1200, 300)).toEqual(options);
});
