import { describe, it, expect } from 'vitest';
import { shouldReloadForStaleChunk } from '../src/stale-chunk-reload';

const fakeStorage = (initial: Record<string, string> = {}) => {
  const map = { ...initial };
  return {
    getItem: (k: string) => (k in map ? map[k] : null),
    setItem: (k: string, v: string) => {
      map[k] = v;
    },
  };
};

describe('shouldReloadForStaleChunk', () => {
  it('reloads on the first stale chunk, then not again within the cooldown', () => {
    const storage = fakeStorage();
    expect(shouldReloadForStaleChunk(1_000_000, storage)).toBe(true);
    // A page reload fires several in-flight imports; only the first may reload,
    // otherwise the tab reload-loops.
    expect(shouldReloadForStaleChunk(1_000_050, storage)).toBe(false);
    expect(shouldReloadForStaleChunk(1_059_999, storage)).toBe(false);
  });

  it('reloads again after the cooldown, so a second deploy is still covered', () => {
    // The reason the guard is a timestamp and not a boolean: we deploy several
    // times a day and a tab stays open across all of them.
    const storage = fakeStorage();
    expect(shouldReloadForStaleChunk(1_000_000, storage)).toBe(true);
    expect(shouldReloadForStaleChunk(1_060_001, storage)).toBe(true);
  });

  it('never reloads when storage is unavailable', () => {
    // Private mode / blocked storage would otherwise reload forever, since the
    // attempt can never be recorded.
    const throwing = {
      getItem: () => {
        throw new Error('denied');
      },
      setItem: () => {
        throw new Error('denied');
      },
    };
    expect(shouldReloadForStaleChunk(1_000_000, throwing)).toBe(false);

    const readOnly = {
      getItem: () => null,
      setItem: () => {
        throw new Error('denied');
      },
    };
    expect(shouldReloadForStaleChunk(1_000_000, readOnly)).toBe(false);
  });

  it('treats an unparseable stored value as no previous reload', () => {
    expect(shouldReloadForStaleChunk(1_000_000, fakeStorage({ 'monoscope:stale-chunk-reload': 'nope' }))).toBe(true);
  });
});
