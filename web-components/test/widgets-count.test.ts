import { describe, expect, test } from 'vitest';
import { sumTimeseriesValues } from '../src/widgets';

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
