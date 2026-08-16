// Colour is signal, not decoration — so these functions are load-bearing.
//
// A service that changes colour between two charts breaks the only way a reader
// correlates them; a 5xx that renders in the 2xx colour is actively misleading during an
// incident. Every one of these is pure and none had a test.
import { describe, test, expect } from 'vitest';
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import {
  getStatusCodeColor,
  getPercentileColor,
  getLogLevelColor,
  getSeriesColor,
  getContrastTextColor,
  tailwindToHex,
  resolveColor,
  TAILWIND_TO_HEX,
} from '../src/colorMapping';

const isHex = (c: string) => /^#[0-9a-f]{6}$/i.test(c);

describe('status code colours', () => {
  test('every class of response gets a colour, and they differ from each other', () => {
    const byClass = ['2xx', '3xx', '4xx', '5xx'].map(getStatusCodeColor);
    byClass.forEach((c) => expect(isHex(c)).toBe(true));
    expect(new Set(byClass).size).toBe(4);
  });

  test('grouped codes are case-insensitive', () => {
    expect(getStatusCodeColor('5XX')).toBe(getStatusCodeColor('5xx'));
  });

  // Success and failure must never collide: that is the one distinction a reader
  // relies on before reading any text.
  test('success and error ranges never share a colour', () => {
    const success = [200, 201, 204].map(getStatusCodeColor);
    const failure = [400, 404, 500, 503].map(getStatusCodeColor);
    for (const s of success) for (const f of failure) expect(s).not.toBe(f);
  });

  test('a numeric code and its string form agree', () => {
    expect(getStatusCodeColor('404')).toBe(getStatusCodeColor(404));
  });

  test('a code outside any known range still gets a usable colour', () => {
    for (const odd of [0, 99, 700, -1]) expect(isHex(getStatusCodeColor(odd))).toBe(true);
  });
});

describe('percentile colours', () => {
  test('percentiles are distinguishable from one another', () => {
    const colors = ['p50', 'p75', 'p90', 'p95', 'p99'].map(getPercentileColor);
    colors.forEach((c) => expect(isHex(c)).toBe(true));
    expect(new Set(colors).size).toBeGreaterThan(1);
  });

  test('casing and surrounding whitespace do not change the colour', () => {
    expect(getPercentileColor('  P95 ')).toBe(getPercentileColor('p95'));
  });

  test('an unrecognised percentile is still stable across calls', () => {
    expect(getPercentileColor('p42')).toBe(getPercentileColor('p42'));
  });
});

describe('log level colours', () => {
  test('levels are distinguishable and stable', () => {
    for (const level of ['error', 'warn', 'info', 'debug']) {
      expect(isHex(getLogLevelColor(level))).toBe(true);
      expect(getLogLevelColor(level)).toBe(getLogLevelColor(level.toUpperCase()));
    }
    expect(getLogLevelColor('error')).not.toBe(getLogLevelColor('info'));
  });

  test('a level embedded in a longer label is still recognised', () => {
    expect(getLogLevelColor('SEVERE ERROR')).toBe(getLogLevelColor('error'));
  });
});

describe('getSeriesColor', () => {
  // The whole point: a service keeps its colour across queries, pages and sessions,
  // so the same colour means the same thing on every chart.
  test('the same value always yields the same colour', () => {
    for (const v of ['checkout-api', 'p99', '500', 'error', 'a-service']) {
      expect(getSeriesColor(v)).toBe(getSeriesColor(v));
    }
  });

  test('different services get different colours', () => {
    const services = ['auth', 'billing', 'checkout', 'search', 'notifications'];
    expect(new Set(services.map((s) => getSeriesColor(s, 'service'))).size).toBeGreaterThan(1);
  });

  test('auto-detection routes a value the same way an explicit context does', () => {
    expect(getSeriesColor('503')).toBe(getStatusCodeColor('503'));
    expect(getSeriesColor('p95')).toBe(getPercentileColor('p95'));
    expect(getSeriesColor('error')).toBe(getLogLevelColor('error'));
  });

  test('an explicit context overrides auto-detection', () => {
    expect(getSeriesColor('500', 'status')).toBe(getStatusCodeColor('500'));
  });

  // Missing data must read as absent, not as a service that happens to be grey —
  // and "unset" is OTel's explicit no-status, which is not the same as null.
  test('absent values are muted and distinguishable from each other', () => {
    const unset = getSeriesColor('unset');
    const nullish = ['null', 'undefined', 'unknown'].map(getSeriesColor);
    nullish.forEach((c) => expect(c).toBe(nullish[0]));
    expect(unset).not.toBe(nullish[0]);
  });

  test('empty and whitespace-only values fall back rather than throwing', () => {
    for (const v of ['', '   ']) expect(isHex(getSeriesColor(v))).toBe(true);
  });

  test('every result is a usable hex colour', () => {
    const samples = ['svc', '200', '404', 'p50', 'warn', 'unset', '', 'Zażółć gęślą jaźń', '🙂'];
    samples.forEach((s) => expect(isHex(getSeriesColor(s))).toBe(true));
  });
});

describe('resolveColor', () => {
  test('prefers the server-assigned colour for a known service', () => {
    const [cls, hex] = Object.entries(TAILWIND_TO_HEX)[0];
    expect(resolveColor('auth', { auth: cls })).toBe(hex);
  });

  test('a server map that already holds hex is passed through untouched', () => {
    expect(resolveColor('auth', { auth: '#123456' })).toBe('#123456');
  });

  // A service the server did not colour still has to get a stable colour, or it
  // changes appearance the moment it appears in a second query.
  test('an unmapped service still resolves to a stable hex', () => {
    const first = resolveColor('brand-new-service', {});
    expect(isHex(first)).toBe(true);
    expect(resolveColor('brand-new-service', {})).toBe(first);
  });

  test('an unknown tailwind class degrades to a valid colour', () => {
    expect(isHex(resolveColor('svc', { svc: 'bg-not-a-real-class' }))).toBe(true);
    expect(isHex(tailwindToHex('bg-not-a-real-class'))).toBe(true);
  });
});

describe('getContrastTextColor', () => {
  test('returns a light tint on dark backgrounds and a dark one on light', () => {
    const onDark = getContrastTextColor('#101010');
    const onLight = getContrastTextColor('#f5f5f5');
    expect(isHex(onDark)).toBe(true);
    expect(isHex(onLight)).toBe(true);
    // Compare rough luminance: the text over a dark chip must be the lighter of the two.
    const lum = (h: string) => parseInt(h.slice(1, 3), 16) + parseInt(h.slice(3, 5), 16) + parseInt(h.slice(5, 7), 16);
    expect(lum(onDark)).toBeGreaterThan(lum(onLight));
  });

  test('every palette colour yields a valid text colour', () => {
    Object.values(TAILWIND_TO_HEX).forEach((hex) => expect(isHex(getContrastTextColor(hex))).toBe(true));
  });
});

// The server hashes each service name into `serviceColors` (Utils.hs) and ships the
// resulting Tailwind class in the JSON. The browser then looks that class up in
// TAILWIND_TO_HEX. Nothing links the two lists, so a colour added on one side alone
// makes every service hashing to it collapse onto the same fallback hex — many
// services, one colour, and no error anywhere.
describe('server ↔ client service palette contract', () => {
  const serverPalette = (() => {
    const utils = readFileSync(resolve(process.cwd(), '../src/Utils.hs'), 'utf8');
    const block = utils.split('serviceColors ::')[1]?.split('\n\n')[0] ?? '';
    return [...block.matchAll(/"(bg-[a-z]+-\d+)"/g)].map((m) => m[1]);
  })();

  test('the server palette was located (guards this test against a refactor)', () => {
    expect(serverPalette.length).toBeGreaterThan(5);
  });

  test('every colour the server can assign has a hex on the client', () => {
    const missing = serverPalette.filter((c) => !TAILWIND_TO_HEX[c]);
    expect(missing).toEqual([]);
  });

  test('no two server palette entries collapse to the same hex', () => {
    const hexes = serverPalette.map((c) => TAILWIND_TO_HEX[c]);
    expect(new Set(hexes).size).toBe(hexes.length);
  });
});
