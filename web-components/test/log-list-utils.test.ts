// The pure functions every rendered log row goes through.
//
// These run once per cell per row, up to 2500 rows — the most-executed code in the
// product — and none of them had a test. They are also the easiest place for a silent
// wrong answer to hide: a mis-parsed summary renders as plain text, a mis-classified
// error renders the wrong severity colour, and both look plausible.
import { describe, test, expect } from 'vitest';
import {
  parseSummaryElement,
  unescapeJsonString,
  formatTimestamp,
  formatTimestampCompact,
  lookupVecValue,
  getErrorClassification,
  calculateColumnWidth,
  calculateAutoBinWidth,
  parseUserAgent,
  dedupeById,
  cursorFromTimestamp,
  oldestRowTimestamp,
  newestRowTimestamp,
  MIN_COLUMN_WIDTH,
  classifyLevel,
  evictOldest,
  atBottom,
} from '../src/log-list-utils';

describe('parseSummaryElement', () => {
  test('splits the field;style⇒value form the server emits', () => {
    expect(parseSummaryElement('http_status;text-error⇒500')).toEqual({
      type: 'formatted',
      field: 'http_status',
      style: 'text-error',
      value: '500',
    });
  });

  test('a value containing the separator keeps everything after the first one', () => {
    expect(parseSummaryElement('msg;plain⇒a ⇒ b')).toMatchObject({ value: 'a ⇒ b' });
  });

  test('an empty style is still the formatted shape, not plain text', () => {
    expect(parseSummaryElement('field;⇒value')).toEqual({ type: 'formatted', field: 'field', style: '', value: 'value' });
  });

  test('text with no separator is plain', () => {
    expect(parseSummaryElement('just a log line')).toEqual({ type: 'plain', content: 'just a log line' });
  });

  // A message that happens to contain a semicolon after the arrow must not be
  // mistaken for a field/style prefix and have its head chopped off.
  test('a semicolon after the separator does not create a bogus field', () => {
    expect(parseSummaryElement('no prefix ⇒ then; a semicolon')).toEqual({
      type: 'plain',
      content: 'no prefix ⇒ then; a semicolon',
    });
  });

  test('the empty string is plain and lossless', () => {
    expect(parseSummaryElement('')).toEqual({ type: 'plain', content: '' });
  });
});

describe('unescapeJsonString', () => {
  test('leaves ordinary text untouched', () => {
    expect(unescapeJsonString('plain message')).toBe('plain message');
  });

  test('unescapes quotes and backslashes from a JSON-encoded payload', () => {
    expect(unescapeJsonString('{\\"a\\": 1}')).toContain('{"a"');
  });

  test('highlights JSON values rather than keys', () => {
    const out = unescapeJsonString('{"status": "ok"}');
    expect(out).toContain('<span class="text-textStrong font-medium">"ok"</span>');
    expect(out).not.toContain('<span class="text-textStrong font-medium">"status"</span>');
  });

  test('numbers, booleans and null are highlighted too', () => {
    for (const v of ['42', 'true', 'false', 'null']) {
      expect(unescapeJsonString(`{"k": ${v}}`)).toContain(`>${v}</span>`);
    }
  });

  test('converts ANSI colour codes into markup instead of leaking escape bytes', () => {
    const out = unescapeJsonString('\x1b[31mred\x1b[0m');
    expect(out).not.toContain('\x1b');
    expect(out.toLowerCase()).toContain('span');
  });

  test('is a no-op on the empty string', () => {
    expect(unescapeJsonString('')).toBe('');
  });
});

describe('timestamp formatting', () => {
  const ts = '2024-03-05T07:08:09.042Z';

  test('renders date, clock and zero-padded milliseconds', () => {
    expect(formatTimestamp(ts)).toMatch(/^Mar 05 \d{2}:\d{2}:\d{2}\.042$/);
  });

  test('the compact form drops the date and keeps the milliseconds', () => {
    expect(formatTimestampCompact(ts)).toMatch(/^\d{2}:\d{2}:\d{2}\.042$/);
  });

  test('sub-100ms values keep three digits rather than collapsing', () => {
    expect(formatTimestamp('2024-03-05T07:08:09.007Z')).toMatch(/\.007$/);
    expect(formatTimestamp('2024-03-05T07:08:09.000Z')).toMatch(/\.000$/);
  });

  // A row with a broken timestamp must render an empty cell, not "Invalid Date".
  test('an unparseable timestamp renders as empty', () => {
    for (const bad of ['', 'not-a-date', 'undefined']) {
      expect(formatTimestamp(bad)).toBe('');
      expect(formatTimestampCompact(bad)).toBe('');
    }
  });
});

describe('lookupVecValue', () => {
  const cols = { id: 0, name: 1 };

  test('reads the column the map points at', () => {
    expect(lookupVecValue(['abc', 'GET /x'], cols, 'name')).toBe('GET /x');
  });

  // The column set changes between queries; a stale key must not read a neighbouring
  // column's value into the wrong cell.
  test('an unknown column reads as empty, never as another column', () => {
    expect(lookupVecValue(['abc', 'GET /x'], cols, 'missing')).toBe('');
  });

  test('an index past the end of a short row reads as empty', () => {
    expect(lookupVecValue(['abc'], cols, 'name')).toBe('');
  });
});

describe('getErrorClassification', () => {
  const cols = { errors: 0, http_attributes: 1, status: 2 };
  const classify = (errors: any, status_code: number, status = '') => getErrorClassification([errors, { status_code }, status], cols);

  test('a span carrying errors is an error regardless of status code', () => {
    expect(classify([{ m: 'boom' }], 200).className).toContain('bg-strokeError-strong');
  });

  test('ERROR status alone is an error', () => {
    expect(classify(null, 200, 'ERROR').className).toContain('bg-strokeError-strong');
  });

  test('a 4xx/5xx without recorded errors is a warning, not an error', () => {
    for (const code of [400, 404, 500, 503]) {
      const { className } = classify(null, code);
      expect(className).toContain('bg-strokeWarning-strong');
      expect(className).not.toContain('bg-strokeError-strong');
    }
  });

  test('a successful span gets the neutral indicator', () => {
    const { className } = classify(null, 200);
    expect(className).toContain('bg-strokeBrand-weak');
    expect(className).toContain('status-indicator');
  });

  // Log records have no http_attributes at all — that must read as 0, not throw.
  test('a row with no http attributes classifies as success rather than throwing', () => {
    expect(() => getErrorClassification([null, null, ''], cols)).not.toThrow();
    expect(getErrorClassification([null, null, ''], cols).statusCode).toBe(0);
  });

  test('exactly one severity class is applied', () => {
    for (const args of [[[{}], 200], [null, 500], [null, 200]] as const) {
      const classes = classify(args[0], args[1]).className.split(' ');
      const severities = classes.filter((c) => c.startsWith('bg-stroke'));
      expect(severities).toHaveLength(1);
    }
  });
});

describe('calculateColumnWidth', () => {
  test('scales with content length', () => {
    expect(calculateColumnWidth('aaaa', 'summary')).toBeGreaterThan(calculateColumnWidth('aa', 'summary'));
  });

  test('an unknown column still gets a width from the default character size', () => {
    expect(calculateColumnWidth('abcdef', 'a_column_nobody_configured')).toBeGreaterThan(0);
  });

  test('empty content measures zero, and the caller clamps to the minimum', () => {
    expect(calculateColumnWidth('', 'summary')).toBe(0);
    expect(MIN_COLUMN_WIDTH).toBeGreaterThan(0);
  });
});

describe('calculateAutoBinWidth', () => {
  const MIN = 60_000;
  const HOUR = 60 * MIN;
  const DAY = 24 * HOUR;

  test.each([
    ['30 minutes', 30 * MIN, 30_000],
    ['1 hour', HOUR, 30_000],
    ['6 hours', 6 * HOUR, 60_000],
    ['12 hours', 12 * HOUR, 5 * 60_000],
    ['24 hours', 24 * HOUR, 10 * 60_000],
    ['3 days', 3 * DAY, 60 * 60_000],
    ['14 days', 14 * DAY, 6 * 60 * 60_000],
    ['90 days', 90 * DAY, 24 * 60 * 60_000],
  ])('%s of data bins at the expected width', (_label, duration, expected) => {
    expect(calculateAutoBinWidth(duration)).toBe(expected);
  });

  // Monotonic: a longer window must never produce narrower bins, or a chart gains
  // buckets as its range grows.
  test('bin width never shrinks as the window grows', () => {
    const widths = [1, 2, 5, 10, 24, 48, 24 * 6, 24 * 20, 24 * 60].map((h) => calculateAutoBinWidth(h * HOUR));
    expect(widths).toEqual([...widths].sort((a, b) => a - b));
  });
});

describe('parseUserAgent', () => {
  test.each([
    ['Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36', 'Chrome on macOS'],
    ['Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Safari/605.1.15', 'Safari on macOS'],
    ['Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36 Edg/120.0', 'Edge on Windows'],
    ['Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:121.0) Gecko/20100101 Firefox/121.0', 'Firefox on Windows'],
    ['Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Mobile/15E148 Safari/604.1', 'Safari on iOS'],
    ['Mozilla/5.0 (Linux; Android 14) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Mobile Safari/537.36', 'Chrome on Android'],
  ])('collapses a real browser UA into a readable label', (ua, expected) => {
    expect(parseUserAgent(ua)).toBe(expected);
  });

  // Chrome-family UAs all contain "Chrome"; the more specific brand has to win or
  // every Edge and Opera request is mislabelled.
  test('Edge and Opera are not reported as Chrome', () => {
    expect(parseUserAgent('Mozilla/5.0 Chrome/120.0 Safari/537.36 OPR/106.0')).toContain('Opera');
    expect(parseUserAgent('Mozilla/5.0 Chrome/120.0 Safari/537.36 Edg/120.0')).toContain('Edge');
  });

  test('non-browser clients keep their own name', () => {
    expect(parseUserAgent('curl/8.4.0')).toContain('curl');
    expect(parseUserAgent('PostmanRuntime/7.36.0')).toContain('Postman');
  });

  // An unrecognised agent must survive verbatim rather than disappearing from the row.
  test('an unknown agent falls back to the raw string', () => {
    expect(parseUserAgent('SomeInternalService/2.1')).toBe('SomeInternalService/2.1');
  });

  test('no user agent renders as empty', () => {
    expect(parseUserAgent('')).toBe('');
  });
});

describe('dedupeById', () => {
  test('keeps the first occurrence of a repeated id', () => {
    expect(dedupeById([{ id: 'a', n: 1 }, { id: 'b', n: 2 }, { id: 'a', n: 3 }]).map((r: any) => r.n)).toEqual([1, 2]);
  });

  // A row with no id cannot be keyed by the virtualizer or matched by the scroll
  // anchor, so it is dropped rather than rendered as an unaddressable row.
  test('rows without an id are dropped', () => {
    expect(dedupeById([{ id: null }, { id: undefined }, { id: 'a' }])).toEqual([{ id: 'a' }]);
  });
});

describe('cursor timestamps', () => {
  test('an ISO timestamp round-trips with the requested offset applied', () => {
    expect(cursorFromTimestamp('2024-03-05T07:08:09.000Z', 10)).toBe('2024-03-05T07:08:09.010Z');
  });

  // A nanosecond epoch read as milliseconds produces a year-55000 cursor, which the
  // server answers with nothing — pagination silently stalls.
  test('nanosecond and microsecond epochs are scaled, not misread as milliseconds', () => {
    const ms = Date.parse('2024-03-05T07:08:09.000Z');
    expect(cursorFromTimestamp(ms * 1e6, 0)).toBe('2024-03-05T07:08:09.000Z');
    expect(cursorFromTimestamp(ms * 1e3, 0)).toBe('2024-03-05T07:08:09.000Z');
    expect(cursorFromTimestamp(ms, 0)).toBe('2024-03-05T07:08:09.000Z');
  });
});

describe('row timestamp extremes', () => {
  const cols = { timestamp: 0 };
  const rows = [['2024-03-05T07:00:00.000Z'], ['2024-03-05T09:00:00.000Z'], ['2024-03-05T08:00:00.000Z']].map((data) => ({ data }));

  // The flattened tree appends child spans after their (older) trace root, so the
  // array endpoints are not the true min/max — these must scan.
  test('scans for the true extremes rather than trusting array order', () => {
    expect(oldestRowTimestamp(rows as any, cols)).toBe(Date.parse('2024-03-05T07:00:00.000Z'));
    expect(newestRowTimestamp(rows as any, cols)).toBe(Date.parse('2024-03-05T09:00:00.000Z'));
  });

  test('an empty list has no extreme rather than a bogus epoch', () => {
    expect(oldestRowTimestamp([] as any, cols)).toBeUndefined();
    expect(newestRowTimestamp([] as any, cols)).toBeUndefined();
  });

  // Without a timestamp column there is no cursor to page from; callers fall back to
  // nextFetchUrl rather than sending an epoch-0 cursor that returns nothing.
  test('a column set with no timestamp yields no cursor', () => {
    expect(oldestRowTimestamp(rows as any, {})).toBeUndefined();
  });

  test('rows missing a timestamp value are skipped, not read as epoch zero', () => {
    const withGaps = [{ data: [null] }, ...rows, { data: [undefined] }];
    expect(oldestRowTimestamp(withGaps as any, cols)).toBe(Date.parse('2024-03-05T07:00:00.000Z'));
  });
});

// One vocabulary for level severity. The three surfaces that colour by level (the level
// badge, the row tint, live tail) used to disagree on `critical`/`exception`/`trace`, so a
// row could be red-tinted next to a grey badge reading CRITICAL.
describe('classifyLevel', () => {
  test('the error set is the inclusive one, and case/affixes do not hide it', () => {
    for (const raw of ['error', 'ERROR', 'fatal', 'critical', 'exception', 'SEVERE_ERROR', 'Fatal'])
      expect(classifyLevel(raw)).toBe('error');
  });

  test('warn, debug and trace, and info', () => {
    expect(classifyLevel('warning')).toBe('warn');
    expect(classifyLevel('WARN')).toBe('warn');
    expect(classifyLevel('debug')).toBe('debug');
    expect(classifyLevel('trace')).toBe('debug');
    expect(classifyLevel('info')).toBe('info');
  });

  test('error outranks warn when a level names both', () => {
    expect(classifyLevel('warn_error')).toBe('error');
  });

  test('unrecognised and empty levels are null, so each surface keeps its own default', () => {
    for (const raw of ['notice', '', null, undefined]) expect(classifyLevel(raw)).toBeNull();
  });
});

describe('evictOldest', () => {
  const seed = (n: number) => new Map(Array.from({ length: n }, (_, i) => [i, i]));

  test('drops the oldest entry only once the cache is at its limit', () => {
    const under = seed(2);
    evictOldest(under, 3);
    expect(under.size).toBe(2);

    const at = seed(3);
    evictOldest(at, 3);
    expect([...at.keys()]).toEqual([1, 2]);
  });

  test('batch drops that many oldest entries in insertion order', () => {
    const cache = seed(10);
    evictOldest(cache, 10, 4);
    expect([...cache.keys()]).toEqual([4, 5, 6, 7, 8, 9]);
  });

  test('a batch larger than the cache empties it rather than throwing', () => {
    const cache = seed(2);
    evictOldest(cache, 1, 99);
    expect(cache.size).toBe(0);
  });
});

describe('atBottom', () => {
  test('1px of rounding slack by default; the slack is the caller’s to widen', () => {
    const el = { scrollTop: 900.5, clientHeight: 100, scrollHeight: 1001 };
    expect(atBottom(el)).toBe(true);
    expect(atBottom({ ...el, scrollTop: 800 })).toBe(false);
    expect(atBottom({ ...el, scrollTop: 800 }, 40)).toBe(false);
    expect(atBottom({ ...el, scrollTop: 870 }, 40)).toBe(true);
  });
});
