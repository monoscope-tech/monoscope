import { describe, expect, test } from 'vitest';
import { chartDataUrl, hideNoDataOverlay, showChartError, showNoDataOverlay, sumTimeseriesValues } from '../src/widgets';

describe('chartDataUrl', () => {
  test('inherits the page default when the URL has no explicit time range', () => {
    window.history.replaceState({}, '', '/p/proj/infrastructure/containers');
    document.body.innerHTML = '<div data-default-window="5M"></div>';

    const url = new URL(
      chartDataUrl({ query: 'metrics', querySQL: '', pid: 'proj', chartType: 'timeseries' }),
      window.location.origin
    );

    expect(url.searchParams.get('since')).toBe('5M');
  });
});

describe('chart empty state', () => {
  test('reveals and hides the stable server-rendered guidance', () => {
    document.body.innerHTML = '<div id="latency_empty" class="chart-no-data hidden"></div>';

    showNoDataOverlay('latency');
    expect(document.querySelector('#latency_empty')?.classList.contains('hidden')).toBe(false);

    hideNoDataOverlay('latency');
    expect(document.querySelector('#latency_empty')?.classList.contains('hidden')).toBe(true);
  });

  test('replaces stale empty guidance with the fetch error', () => {
    document.body.innerHTML = `
      <div id="latency_empty" class="chart-no-data hidden"></div>
      <div id="latency_error" class="hidden"><span id="latency_errorMsg"></span></div>`;

    showNoDataOverlay('latency');
    showChartError('latency', 'Unable to load this chart.');

    expect(document.querySelector('#latency_empty')?.classList.contains('hidden')).toBe(true);
    expect(document.querySelector('#latency_error')?.classList.contains('hidden')).toBe(false);
    expect(document.querySelector('#latency_errorMsg')?.textContent).toBe('Unable to load this chart.');
  });
});

describe('sumTimeseriesValues', () => {
  test('sums every chart series while excluding timestamps and null gaps', () => {
    expect(
      sumTimeseriesValues([
        [1_000, 2, null, 3],
        [2_000, 4, 5, null],
      ])
    ).toBe(14);
  });

  test('returns zero for an empty chart and rejects a malformed dataset', () => {
    expect(sumTimeseriesValues([])).toBe(0);
    expect(sumTimeseriesValues(null)).toBeNull();
  });
});
