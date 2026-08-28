import { describe, expect, test } from 'vitest';
import { chartDataUrl, sumTimeseriesValues } from '../src/widgets';

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
