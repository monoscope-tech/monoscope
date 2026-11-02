import { describe, test, expect } from 'vitest';
import { statScalar, formatStatValue, formatNumber, formatBytes, formatDuration } from '../src/stat-value';
import { formatDuration as waterfallDuration } from '../src/charts';
import { fmtNs } from '../src/log-list';

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

  test('last is the newest bin for gauges, not a sum of levels', () => {
    expect(statScalar({ ...traffic, last: 70 }, 'last')).toBe(70);
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
    // 17 errors in 1.38M queries is 0.0012%: never round a non-zero rate down to 0.
    expect(formatStatValue(0.0012, '%')).toBe('<0.01 %');
    expect(formatStatValue(0, '%')).toBe('0 %');
  });

  test('ms latency formats as a duration (no millions ambiguity)', () => {
    // 45 ms must read "45.0ms", never "1.5m"/"45M".
    expect(formatStatValue(45, 'ms')).toBe('45.0ms');
    expect(formatStatValue(1500, 'ms')).toBe('1.5s');
  });

  test('unitless and bytes suffixes', () => {
    expect(formatStatValue(1500, '')).toBe('1.5K');
    // Was '2.0K bytes', which reads as 2000 bytes for a value that is 2 KiB. Byte units get
    // binary multiples now, matching the tables these numbers sit next to.
    expect(formatStatValue(2048, 'By')).toBe('2 KiB');
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

// A memory chart's axis used to run through formatNumber and read "1.0B" for a gigabyte —
// B for "billion" — directly above a table reading "1 GiB" for the same value, on a widget
// that had declared its unit as bytes. Binary multiples, matching Pages.Containers.formatBytes.
describe('formatBytes', () => {
  test('uses binary multiples and drops a bare .0', () => {
    expect(formatBytes(0)).toBe('0 B');
    expect(formatBytes(512)).toBe('512 B');
    expect(formatBytes(1024)).toBe('1 KiB');
    expect(formatBytes(536870912)).toBe('512 MiB');
    expect(formatBytes(1073741824)).toBe('1 GiB');
    expect(formatBytes(483183820)).toBe('460.8 MiB');
  });

  test('never renders NaN or null into the DOM', () => {
    expect(formatBytes(NaN)).toBe('N/A');
    expect(formatBytes(null)).toBe('N/A');
    expect(formatBytes(undefined)).toBe('N/A');
  });

  test('a byte-united stat tile formats as bytes, not as a decimal magnitude', () => {
    expect(formatStatValue(1073741824, 'bytes')).toBe('1 GiB');
    expect(formatStatValue(1073741824, 'By')).toBe('1 GiB');
    // Non-byte units keep the old behaviour.
    expect(formatStatValue(1073741824, '')).toBe('1.1B');
  });
});

// Pinned to the outputs of the three duration formatters before they shared scaleDuration.
test.each([
  [0, '0.0 ns', '0ns', '0ns'], [0.4, '0.4 ns', '0ns', '0ns'], [999, '999.0 ns', '999ns', '999ns'],
  [1000, '1.0 µs', '1µs', '1.0μs'], [1500, '1.5 µs', '2µs', '1.5μs'], [9999, '10.0 µs', '10µs', '10.0μs'],
  [12345, '12 µs', '12µs', '12.3μs'], [999999, '1000 µs', '1000µs', '1000.0μs'], [1.5e6, '1.5 ms', '2ms', '1.5ms'],
  [2.5e7, '25 ms', '25ms', '25.0ms'], [1.5e9, '1.5 s', '1.50s', '1.5s'], [12.6e9, '13 s', '12.60s', '12.6s'],
  [61e9, '1 m', '61.00s', '1.0m'], [3.7e12, '1 h', '3700.00s', '1.0h'],
])('duration %d ns: waterfall %s, fmtNs %s, stat %s', (ns, waterfall, short, stat) => {
  expect(waterfallDuration(ns).join(' ')).toBe(waterfall);
  expect(fmtNs(ns)).toBe(short);
  expect(formatDuration(ns)).toBe(stat);
});

// formatNumber replaced log-list's formatLargeCount; identical below a billion, which now reads as B.
test.each([[0, '0'], [999, '999'], [1000, '1.0K'], [1550, '1.6K'], [999999, '1000.0K'], [1_550_000, '1.6M'], [123456789, '123.5M'], [1.5e9, '1.5B']])(
  'formatNumber(%d) = %s', (n, out) => expect(formatNumber(n)).toBe(out));
