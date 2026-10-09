import { describe, expect, test } from 'vitest';
import { chartDataUrl, hideNoDataOverlay, showChartError, showNoDataOverlay, sumTimeseriesValues } from '../src/widgets';

describe('chartDataUrl', () => {
  test.each([
    'metrics | where metric_name == "timefusion.mem_buffer.oldest_bucket_age_seconds" | summarize max(value)',
    'metrics | where metric_name == "cpu"',
    'metrics | where metric_name == "summarize x"',
  ])('refresh_preservesSuppliedQueryWithoutAddingAnotherAggregation: %s', query => {
    const url = new URL(chartDataUrl({ query, querySQL: '', pid: 'proj', chartType: 'timeseries' }), window.location.origin);
    expect(url.searchParams.get('query')).toBe(query);
  });
  test('inherits the page default when the URL has no explicit time range', () => {
    window.history.replaceState({}, '', '/p/proj/infrastructure/containers');
    document.body.innerHTML = '<div data-default-window="5M"></div>';

    const url = new URL(
      chartDataUrl({ query: 'metrics', querySQL: '', pid: 'proj', chartType: 'timeseries' }),
      window.location.origin
    );

    expect(url.searchParams.get('since')).toBe('5M');
  });
  test('preserves a SQL widget\'s declared database source', () => {
    const url = new URL(
      chartDataUrl({ query: '', querySQL: 'SELECT 1', pid: 'proj', chartType: 'timeseries', dbSource: 'postgres' }),
      window.location.origin
    );

    expect(url.searchParams.get('db_source')).toBe('postgres');
  });

  test('sends an hourly rollup and its coverage alongside the KQL query, leaving the source to the server', () => {
    const url = (rollupFrom: string | null) => new URL(chartDataUrl({ query: 'x | summarize count(*) by bin_auto(timestamp)', querySQL: '', rollupSQL: 'SELECT 1', rollupFrom, pid: 'proj', chartType: 'timeseries' }), window.location.origin).searchParams;
    const covered = url('2026-10-01T00:00:00Z');

    expect([covered.get('rollup_sql'), covered.get('rollup_from'), covered.get('db_source'), covered.get('query_sql')]).toEqual(['SELECT 1', '2026-10-01T00:00:00Z', null, null]);
    expect(url(null).get('rollup_sql')).toBeNull();
  });

  test('marks dashboard requests so service scope comes from dashboard variables', () => {
    window.history.replaceState({}, '', '/p/proj/dashboards/overview?var-service=checkout');
    const url = new URL(
      chartDataUrl({ query: 'metrics', querySQL: '', pid: 'proj', chartType: 'timeseries', dashboardId: 'overview' }),
      window.location.origin
    );

    expect(url.searchParams.get('dashboard_id')).toBe('overview');
    expect(url.searchParams.get('var-service')).toBe('checkout');
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
