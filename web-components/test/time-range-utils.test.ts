import { describe, test, expect } from 'vitest';
import { expandSince, expandFromToRange, parseChartZoom, copyParams, TIME_PARAMS } from '../src/time-range-utils';

describe('time-range-utils', () => {
  describe('expandSince', () => {
    test('progresses through all tiers', () => {
      const chain = ['5M', '15M', '30M', '1H', '3H', '6H', '12H', '24H', '3D', '7D', '14D'];
      for (let i = 0; i < chain.length - 1; i++) {
        expect(expandSince(chain[i])).toBe(chain[i + 1]);
      }
    });

    test('caps at 14D for unknown values', () => {
      expect(expandSince('14D')).toBe('14D');
      expect(expandSince('99H')).toBe('14D');
    });
  });

  describe('expandFromToRange', () => {
    const to = '2026-03-17T18:00:00.000Z';

    test('doubles the range when from is provided', () => {
      // 1h range → expand by 1h earlier
      const from = '2026-03-17T17:00:00.000Z';
      const newFrom = expandFromToRange(from, to);
      expect(new Date(newFrom).getTime()).toBe(new Date('2026-03-17T16:00:00.000Z').getTime());
    });

    test('expands by at least 15 minutes even for tiny ranges', () => {
      // 1 minute range → should expand by 15 min (minimum), not 1 min
      const from = '2026-03-17T17:59:00.000Z';
      const newFrom = expandFromToRange(from, to);
      const expectedMs = new Date(from).getTime() - 15 * 60 * 1000;
      expect(new Date(newFrom).getTime()).toBe(expectedMs);
    });

    test('defaults to 1h range when from is null', () => {
      const newFrom = expandFromToRange(null, to);
      // Default range is 1h, so expansion = 1h, newFrom = (to - 1h) - 1h = to - 2h
      expect(new Date(newFrom).getTime()).toBe(new Date('2026-03-17T16:00:00.000Z').getTime());
    });

    test('moves from earlier, never changes to (prevents shrink bug)', () => {
      const from = '2026-03-17T15:00:00.000Z'; // 3h range
      const newFrom = expandFromToRange(from, to);
      expect(new Date(newFrom).getTime()).toBeLessThan(new Date(from).getTime());
    });
  });

  describe('parseChartZoom', () => {
    test('parses valid batch into ISO strings', () => {
      const result = parseChartZoom([{ startValue: '2026-03-17T17:00:00Z', endValue: '2026-03-17T18:00:00Z' }]);
      expect(result).not.toBeNull();
      expect(result!.from).toBe('2026-03-17T17:00:00.000Z');
      expect(result!.to).toBe('2026-03-17T18:00:00.000Z');
    });

    test('handles date string values from echarts', () => {
      const result = parseChartZoom([{ startValue: 'Mon Mar 17 2026 17:00:00', endValue: 'Mon Mar 17 2026 18:00:00' }]);
      expect(result).not.toBeNull();
      expect(result!.from).toMatch(/^\d{4}-\d{2}-\d{2}T/); // valid ISO string
    });

    test('returns null for missing/empty batch', () => {
      expect(parseChartZoom(undefined)).toBeNull();
      expect(parseChartZoom([])).toBeNull();
    });
  });
});

// The page's time window is carried onto request/nav URLs from four call sites; they all
// route through copyParams so none of them can quietly forget `since` (which the server
// prefers over from/to, so dropping it silently widens the window back to the default).
describe('copyParams', () => {
  test('copies the time window and leaves everything else on the target alone', () => {
    const target = new URLSearchParams({ json: 'true' });
    copyParams(new URLSearchParams({ since: '1H', from: '', query: 'x' }), target);
    expect(target.get('since')).toBe('1H');
    expect(target.get('json')).toBe('true');
    expect(target.has('query')).toBe(false);
  });

  test('an empty or absent source value never overwrites what the target already carries', () => {
    const target = new URLSearchParams({ since: '24H', to: '2024-01-01T00:00:00Z' });
    copyParams(new URLSearchParams({ since: '', from: 'x' }), target);
    expect(target.get('since')).toBe('24H');
    expect(target.get('to')).toBe('2024-01-01T00:00:00Z');
    expect(target.get('from')).toBe('x');
  });

  test('extra keys ride along when asked for', () => {
    const target = new URLSearchParams();
    copyParams(new URLSearchParams({ query: 'level==error', since: '1H' }), target, ['query', ...TIME_PARAMS]);
    expect(target.get('query')).toBe('level==error');
    expect(target.get('since')).toBe('1H');
  });
});
