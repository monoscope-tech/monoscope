import { describe, test, expect, vi, beforeEach, afterEach } from 'vitest';

// jsdom in this setup doesn't provide localStorage; the SessionReplay constructor
// reads it for the console-panel width. Provide a minimal in-memory shim before
// any element is constructed.
if (typeof (globalThis as any).localStorage?.getItem !== 'function') {
  const store: Record<string, string> = {};
  const shim = {
    getItem: (k: string) => (k in store ? store[k] : null),
    setItem: (k: string, v: string) => {
      store[k] = String(v);
    },
    removeItem: (k: string) => {
      delete store[k];
    },
    clear: () => {
      for (const k of Object.keys(store)) delete store[k];
    },
    key: () => null,
    length: 0,
  };
  (globalThis as any).localStorage = shim;
  try {
    Object.defineProperty(window, 'localStorage', { value: shim, configurable: true });
  } catch {
    /* window.localStorage may be read-only; globalThis shim suffices */
  }
}

// Capture Replayer instances so we can assert the initial (shard-1) events and
// the events streamed in later via addEvent. A fake keeps rrweb's DOM/canvas
// machinery out of jsdom while exercising the real progressive-load orchestration.
const replayers: any[] = [];
vi.mock('@rrweb/replay', () => {
  class FakeReplayer {
    added: any[] = [];
    constructor(
      public initial: any[],
      public opts: any
    ) {
      replayers.push(this);
    }
    addEvent(e: any) {
      this.added.push(e);
    }
    getMetaData() {
      return { startTime: 0, endTime: 0, totalTime: 0 };
    }
    play() {}
    pause() {}
    destroy() {}
    setConfig() {}
    get iframe() {
      return document.createElement('iframe');
    }
    get wrapper() {
      return document.createElement('div');
    }
  }
  return { Replayer: FakeReplayer };
});

import '../src/session-replay'; // registers <session-replay>

const ev = (t: number) => ({ type: 3, timestamp: t, data: {} });

async function until(pred: () => boolean, ms = 3000) {
  const start = Date.now();
  while (!pred()) {
    if (Date.now() - start > ms) throw new Error('condition timed out');
    await new Promise((r) => setTimeout(r, 5));
  }
}

async function mountPlayer(props: Record<string, any> = {}): Promise<any> {
  const el: any = document.createElement('session-replay');
  Object.assign(el, { projectId: 'proj-1', ...props });
  document.body.appendChild(el);
  await el.updateComplete;
  return el;
}

describe('session-replay progressive shard loading', () => {
  let calls: string[];
  beforeEach(() => {
    replayers.length = 0;
    calls = [];
  });
  afterEach(() => {
    document.body.innerHTML = '';
    vi.restoreAllMocks();
  });

  test('fetches manifest, plays shard 1 immediately, then streams the rest in order via addEvent', async () => {
    const manifest = {
      meta: { userEmail: 'u@example.com', userName: null, userId: null },
      errorMsg: null,
      segments: [
        { key: 'sess-1/shard-000001-10', firstTs: 10, gzipped: true },
        { key: 'sess-1/shard-000002-20', firstTs: 20, gzipped: true },
        { key: 'sess-1/shard-000003-30', firstTs: 30, gzipped: true },
      ],
    };
    const routes: Record<string, any> = {
      '/manifest': manifest,
      'shard-000001': [ev(10), ev(11)],
      'shard-000002': [ev(20), ev(21)],
      'shard-000003': [ev(30)],
    };
    (global as any).fetch = vi.fn(async (url: string) => {
      calls.push(url);
      const k = Object.keys(routes).find((r) => url.includes(r));
      return k ? { ok: true, status: 200, json: async () => routes[k] } : { ok: false, status: 404, json: async () => ({}) };
    });

    const el = await mountPlayer();
    el.fetchNewSessionData('sess-1');
    await until(() => el.loadedSegments === 3);

    // Manifest is requested first; then the three shards, strictly in order.
    expect(calls[0]).toContain('/p/proj-1/replay_session/sess-1/manifest');
    const shardCalls = calls.filter((u) => u.includes('/shard?key=')).map((u) => decodeURIComponent(u));
    expect(shardCalls.length).toBe(3);
    expect(shardCalls[0]).toContain('shard-000001');
    expect(shardCalls[1]).toContain('shard-000002');
    expect(shardCalls[2]).toContain('shard-000003');

    // Playback started on shard 1's two events; the remaining 3 events streamed in.
    expect(replayers.length).toBe(1);
    expect(replayers[0].initial.length).toBe(2);
    expect(replayers[0].added.length).toBe(3);

    // Identity surfaced from the manifest meta.
    expect(el.userEmail).toBe('u@example.com');
  });

  test('accumulates leading shards until it has ≥2 events before starting playback', async () => {
    const manifest = {
      meta: null,
      errorMsg: null,
      segments: [
        { key: 's2/shard-000001-1', firstTs: 1, gzipped: true },
        { key: 's2/shard-000002-2', firstTs: 2, gzipped: true },
      ],
    };
    const routes: Record<string, any> = {
      '/manifest': manifest,
      'shard-000001': [ev(1)], // only one event — not enough to start yet
      'shard-000002': [ev(2)],
    };
    (global as any).fetch = vi.fn(async (url: string) => {
      calls.push(url);
      const k = Object.keys(routes).find((r) => url.includes(r));
      return k ? { ok: true, status: 200, json: async () => routes[k] } : { ok: false, status: 404, json: async () => ({}) };
    });

    const el = await mountPlayer();
    el.fetchNewSessionData('s2');
    await until(() => replayers.length === 1);

    // Both shards were pulled up front to reach the 2-event minimum; the player
    // started with both events and nothing was left to stream.
    expect(replayers[0].initial.length).toBe(2);
    expect(replayers[0].added.length).toBe(0);
    expect(el.loadedSegments).toBe(2);
  });

  test('surfaces a manifest errorMsg and never starts a player', async () => {
    (global as any).fetch = vi.fn(async (url: string) =>
      url.includes('/manifest')
        ? { ok: true, status: 200, json: async () => ({ segments: [], meta: null, errorMsg: 'No recorded events found for this session.' }) }
        : { ok: false, status: 404, json: async () => ({}) }
    );
    const el = await mountPlayer();
    el.fetchNewSessionData('sess-x');
    await until(() => el.loadError === 'No recorded events found for this session.');
    expect(replayers.length).toBe(0);
  });

  test('share context (sessionUrl set) uses the full-events endpoint, not the manifest', async () => {
    (global as any).fetch = vi.fn(async (url: string) => {
      calls.push(url);
      return { ok: true, status: 200, json: async () => ({ events: [ev(1), ev(2), ev(3)], userEmail: 'shared@x' }) };
    });
    const el = await mountPlayer({ sessionUrl: '/share/r/abc/replay_session/sess-9' });
    el.fetchNewSessionData('sess-9');
    await until(() => replayers.length === 1);

    expect(calls.every((u) => !u.includes('/manifest'))).toBe(true);
    expect(calls[0]).toContain('/share/r/abc/replay_session/sess-9');
    expect(replayers[0].initial.length).toBe(3);
    expect(el.userEmail).toBe('shared@x');
  });
});
