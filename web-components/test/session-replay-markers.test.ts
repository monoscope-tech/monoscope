// Session replay: the clock and the scrubber markers.
//
// A replay is watched by someone reconstructing an incident, so the timeline has to be
// literally true: the clock is what they quote, and the error/warning ticks and page
// markers are how they find the moment that matters. Both are derived per streamed shard
// and accumulate across a session that can be hours long — the progressive-loading spec
// covers the streaming itself, but nothing covered what those events turn into.
import { describe, test, expect, beforeEach } from 'vitest';
import { SessionReplay } from '../src/session-replay';

const format = (ms: number) => SessionReplay.formatTime(ms);

const SECOND = 1000;
const MINUTE = 60 * SECOND;
const HOUR = 60 * MINUTE;

describe('the replay clock', () => {
  test.each([
    ['the very start', 0, '00:00'],
    ['sub-second', 400, '00:00'],
    ['seconds', 7 * SECOND, '00:07'],
    ['a full minute', MINUTE, '01:00'],
    ['minutes and seconds', 9 * MINUTE + 5 * SECOND, '09:05'],
    ['past ten minutes', 42 * MINUTE + 13 * SECOND, '42:13'],
  ])('%s reads as %s', (_label, ms, expected) => {
    expect(format(ms)).toBe(expected);
  });

  // A long session must not silently wrap: 1h05m reading as "05:00" would send someone
  // to the wrong hour of an incident.
  test('grows an hours field rather than wrapping', () => {
    expect(format(HOUR)).toBe('01:00:00');
    expect(format(HOUR + 5 * MINUTE + 9 * SECOND)).toBe('01:05:09');
    expect(format(2 * HOUR + 59 * MINUTE + 59 * SECOND)).toBe('02:59:59');
  });

  test('every field is zero-padded so the timeline stays column-aligned', () => {
    expect(format(3 * MINUTE + 4 * SECOND)).toMatch(/^\d{2}:\d{2}$/);
    expect(format(HOUR + 2 * MINUTE + 3 * SECOND)).toMatch(/^\d{2}:\d{2}:\d{2}$/);
  });

  // Truncation, not rounding: the frame at 1.9s has not reached 2s yet, and the scrubber
  // label has to agree with the frame on screen.
  test('truncates toward the frame actually showing', () => {
    expect(format(1900)).toBe('00:01');
    expect(format(59_999)).toBe('00:59');
    expect(format(3_599_999)).toBe('59:59');
  });

  test('the boundary into a new unit is exact', () => {
    expect(format(60 * SECOND - 1)).toBe('00:59');
    expect(format(60 * SECOND)).toBe('01:00');
    expect(format(HOUR - 1)).toBe('59:59');
    expect(format(HOUR)).toBe('01:00:00');
  });
});

describe('scrubber markers', () => {
  const START = 1_700_000_000_000;

  // rrweb's console plugin events and Meta (navigation) events, as they arrive.
  const consoleEvent = (level: string, atMs: number) => ({
    type: 6, // EventType.Plugin
    timestamp: START + atMs,
    data: { plugin: 'rrweb/console@1', payload: { level, payload: [], trace: [] } },
  });
  const metaEvent = (atMs: number, href: string) => ({ type: 4, timestamp: START + atMs, data: { href } });

  let el: any;
  beforeEach(() => {
    el = new SessionReplay();
    el.sessionStart = START;
    el.consoleTypesCounts = { error: 0, warn: 0, info: 0 }; // the component's own default
    el.consoleEvents = [];
    el.errorTicks = [];
    el.warnTicks = [];
    el.navMarkers = [];
    el.seenFirstMeta = false;
  });

  test('errors and warnings land at their offset from the session start, not wall-clock', () => {
    el.ingestMarkers([consoleEvent('error', 5 * SECOND), consoleEvent('warn', 12 * SECOND)]);

    expect(el.errorTicks).toEqual([5 * SECOND]);
    expect(el.warnTicks).toEqual([12 * SECOND]);
  });

  test('levels are kept apart, and counted', () => {
    el.ingestMarkers([consoleEvent('error', 1), consoleEvent('error', 2), consoleEvent('warn', 3), consoleEvent('info', 4)]);

    expect(el.errorTicks).toHaveLength(2);
    expect(el.warnTicks).toHaveLength(1);
    expect(el.consoleTypesCounts.error).toBe(2);
    expect(el.consoleTypesCounts.info).toBe(1);
    expect(el.consoleEvents).toHaveLength(4); // info is listed even though it has no tick
  });

  // The first Meta event is the initial page load, not a navigation — marking it would
  // put a spurious "page change" pin at 00:00 on every replay.
  test('the opening page load is not a navigation marker', () => {
    el.ingestMarkers([metaEvent(0, 'https://app/start'), metaEvent(30 * SECOND, 'https://app/checkout')]);

    expect(el.navMarkers).toEqual([{ offset: 30 * SECOND, href: 'https://app/checkout' }]);
  });

  test('a navigation with no href still marks the moment', () => {
    el.ingestMarkers([metaEvent(0, 'https://app/start'), { type: 4, timestamp: START + SECOND, data: {} }]);

    expect(el.navMarkers).toEqual([{ offset: SECOND, href: '' }]);
  });

  // Shards stream in over the life of a long session; markers from earlier ones must
  // survive, in order, or the scrubber loses the first half of the incident.
  test('markers accumulate across streamed shards', () => {
    el.ingestMarkers([consoleEvent('error', 1 * SECOND), metaEvent(0, '/a'), metaEvent(2 * SECOND, '/b')]);
    el.ingestMarkers([consoleEvent('error', 9 * SECOND), consoleEvent('warn', 10 * SECOND), metaEvent(11 * SECOND, '/c')]);

    expect(el.errorTicks).toEqual([1 * SECOND, 9 * SECOND]);
    expect(el.warnTicks).toEqual([10 * SECOND]);
    expect(el.navMarkers.map((m: any) => m.href)).toEqual(['/b', '/c']);
  });

  // rrweb's console plugin emits log/debug/trace too, which the component does not track
  // or tick. They must not corrupt the three counts the header actually displays.
  test('an untracked console level leaves the displayed counts intact', () => {
    el.ingestMarkers([consoleEvent('error', SECOND), consoleEvent('log', 2 * SECOND), consoleEvent('debug', 3 * SECOND)]);

    expect(el.consoleTypesCounts.error).toBe(1);
    expect(el.consoleTypesCounts.warn).toBe(0);
    expect(el.consoleTypesCounts.info).toBe(0);
    // They are still listed in the console pane — dropping them would hide output the
    // reader saw in their own devtools.
    expect(el.consoleEvents).toHaveLength(3);
    expect(el.errorTicks).toEqual([SECOND]);
  });

  test('a shard with nothing worth marking changes nothing', () => {
    el.ingestMarkers([consoleEvent('error', SECOND)]);
    el.ingestMarkers([]);
    el.ingestMarkers([{ type: 3, timestamp: START + 2 * SECOND, data: {} }]); // an incremental snapshot

    expect(el.errorTicks).toEqual([SECOND]);
    expect(el.navMarkers).toEqual([]);
  });
});
