import { describe, test, expect } from 'vitest';
import { statScalar, formatStatValue, formatNumber } from '../src/stat-value';

// Regression for the golden-signals overview bug: the big number was a blind
// sum of every per-bin value (mislabeled "120k req/min", "1.5m" p99), instead
// of a unit-appropriate scalar. statScalar must pick rate/mean, never sum, for
// rate/latency/percentage units.
describe('statScalar', () => {
  // 1200 requests across 10 minutes (0 → 600000ms) of count() bins.
  const traffic = { min: 50, max: 200, sum: 1200, count: 10, mean: 120 };

  test('rate = sum / window-minutes (true req/min), not the raw sum', () => {
    expect(statScalar(traffic, 'rate', 0, 600000)).toBe(120); // 1200/10min
    expect(statScalar(traffic, 'rate', 0, 600000)).not.toBe(traffic.sum);
  });

  test('rate returns NaN on a missing/degenerate window (→ N/A), not a wrong-unit sum', () => {
    expect(statScalar(traffic, 'rate')).toBeNaN();
    expect(statScalar(traffic, 'rate', 100, 100)).toBeNaN();
  });

  test('mean averages per-bin values (error-rate %, p95 latency)', () => {
    // Summing per-bin percentages would give a meaningless ~ sum; mean is right.
    const errorRate = { min: 0, max: 9, sum: 230, count: 100, mean: 2.3 };
    expect(statScalar(errorRate, 'mean')).toBe(2.3);
  });

  test('default/sum keeps total for additive counts', () => {
    expect(statScalar(traffic, 'sum')).toBe(1200);
    expect(statScalar(traffic, 'whatever')).toBe(1200);
    expect(statScalar(traffic, 'max')).toBe(200);
    expect(statScalar(traffic, 'min')).toBe(50);
    expect(statScalar(traffic, 'count')).toBe(10);
  });
});

describe('formatStatValue', () => {
  test('appends non-duration units with a space', () => {
    expect(formatStatValue(120, 'req/min')).toBe('120 req/min');
    expect(formatStatValue(2.3, '%')).toBe('2.3 %');
  });

  test('ms latency formats as a duration (no millions ambiguity)', () => {
    // 45 ms must read "45.0ms", never "1.5m"/"45M".
    expect(formatStatValue(45, 'ms')).toBe('45.0ms');
    expect(formatStatValue(1500, 'ms')).toBe('1.5s');
  });

  test('unitless and bytes suffixes', () => {
    expect(formatStatValue(1500, '')).toBe('1.5K');
    expect(formatStatValue(2048, 'By')).toBe('2.0K bytes');
  });
});

describe('formatNumber NaN/null guard', () => {
  test('NaN/undefined/null render N/A, not the literal "NaN"', () => {
    expect(formatNumber(NaN)).toBe('N/A');
    expect(formatNumber(Number(undefined))).toBe('N/A');
    expect(formatNumber(null as unknown as number)).toBe('N/A');
  });
  test('finite values are unaffected', () => {
    expect(formatNumber(1500)).toBe('1.5K');
    expect(formatNumber(42)).toBe('42');
  });
  test('K/M/B branches round rather than truncate', () => {
    expect(formatNumber(1590)).toBe('1.6K');
    expect(formatNumber(1_550_000)).toBe('1.6M');
  });
  test('duration units also guard NaN (no "NaNns")', () => {
    expect(formatStatValue(NaN, 'ms')).toBe('N/A');
  });
});
