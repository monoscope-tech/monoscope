/**
 * The client half of the live push path, shared by the Live Tail tab and the Events tab's
 * live toggle.
 *
 * Both surfaces need the identical lifecycle — register a leased subscription, hold an
 * EventSource open, renew the lease while the tab is watching, reconnect with backoff, and
 * clean up on unload — and differ only in what they do with the rows. Keeping that lifecycle
 * in one place is the point: a reconnect bug fixed for one tab is fixed for both, and the
 * lease can't drift out of step between them.
 *
 * The lease is renewed by the browser rather than the server on purpose. A subscription
 * should die when nobody is watching it, and the only component that actually knows whether
 * someone is watching is this one. See `liveTailRenewH` for the server side.
 */
// Type-only: erased at compile time, so this module stays free of the DOM-dependent
// declarations types.ts carries.
import type { ServerTraceEntry } from './types/types';

export type LiveStreamOptions = {
  projectId: string;
  /**
   * Fallback lease length in seconds, used only if the server omits `expires_at`. The real
   * schedule comes from the expiry the server returns, so an operator changing
   * LIVE_TAIL_LEASE_SECS takes effect without the client knowing the new value.
   */
  leaseSecs: number;
  /** Registration body. `allSignals` selects the Events shape; omit for Live Tail. */
  body: () => Record<string, unknown>;
  onRows: (rows: unknown[]) => void;
  onState: (state: LiveState, detail?: string) => void;
  /** Cumulative rows the server dropped for this connection. */
  onDropped?: (total: number) => void;
};

export type LiveState = 'idle' | 'connecting' | 'live' | 'reconnecting' | 'expired' | 'error';

/** Reconnect backoff, capped so a long outage doesn't become a long silence. */
const BACKOFF_MS = [1000, 2000, 5000, 10000, 15000];

export class LiveStream {
  private source: EventSource | null = null;
  private subscriptionId: string | null = null;
  private renewTimer: number | null = null;
  private reconnectTimer: number | null = null;
  private attempt = 0;
  private stopped = true;
  // Invalidates registrations that are still awaiting fetch/json when the stream is stopped
  // or replaced. EventSource.close cannot cancel a connection that has not been created yet.
  private generation = 0;

  constructor(private readonly opts: LiveStreamOptions) {}

  get isRunning(): boolean {
    return !this.stopped;
  }

  async start(): Promise<void> {
    this.stop();
    const generation = ++this.generation;
    this.stopped = false;
    this.opts.onState('connecting');

    let reg: { subscription_id: string; stream_url: string; expires_at?: string };
    try {
      const res = await fetch(`/p/${this.opts.projectId}/live_tail/subscriptions`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(this.opts.body()),
      });
      if (!res.ok) {
        if (generation !== this.generation) return;
        // The server's message names the actual cause — service gate, bad filter, limit hit.
        // Substituting our own would hide which.
        const msg = await res.json().then(j => j.error).catch(() => 'Could not start live mode.');
        if (generation !== this.generation) return;
        this.stopped = true;
        this.opts.onState('error', msg);
        return;
      }
      reg = await res.json();
    } catch {
      if (generation !== this.generation) return;
      this.stopped = true;
      this.opts.onState('error', 'Could not reach the server.');
      return;
    }

    if (generation !== this.generation || this.stopped) {
      // Registration won the network race after its owner was stopped/replaced. Release the
      // server lease without ever opening an EventSource for obsolete query results.
      void fetch(`/p/${this.opts.projectId}/live_tail/subscriptions/${reg.subscription_id}`, {
        method: 'DELETE',
        keepalive: true,
      }).catch(() => {});
      return;
    }
    this.subscriptionId = reg.subscription_id;
    this.open(reg.stream_url);
    this.scheduleRenew(reg.expires_at);
  }

  private open(url: string) {
    const source = new EventSource(url);
    this.source = source;

    source.addEventListener('ready', () => {
      this.attempt = 0;
      this.opts.onState('live');
    });

    source.addEventListener('log', e => {
      try {
        this.opts.onRows(JSON.parse((e as MessageEvent).data));
      } catch {
        // A row we can't parse is one row lost, not a reason to tear down a working stream.
      }
    });

    // The server's own voice, as opposed to `onerror` below which is the transport's. Reaching
    // here means the tail is broken in a way retrying cannot fix — a stored filter that no
    // longer compiles — so we stop rather than reconnect into a stream that will stay empty.
    // Silence is the one thing we must not answer with: it reads as "nothing is happening".
    //
    // Named `notice`, not `error`, and that is not cosmetic: EventSource dispatches a frame's
    // `event:` name directly, so `event: error` would land on `onerror` below — the transport's
    // handler — and a permanent server-side fault would be treated as a blip and retried.
    source.addEventListener('notice', e => {
      let msg = 'This live tail stopped working. Start a new one.';
      try {
        msg = JSON.parse((e as MessageEvent).data).message || msg;
      } catch {
        /* keep the fallback */
      }
      this.stop();
      this.opts.onState('error', msg);
    });

    source.addEventListener('dropped', e => {
      try {
        this.opts.onDropped?.(JSON.parse((e as MessageEvent).data).count ?? 0);
      } catch {
        /* counter only */
      }
    });

    // EventSource retries on its own, but it can't know the subscription expired — so we own
    // the retry and re-register instead of reconnecting to a lease that is gone.
    source.onerror = () => {
      if (this.stopped) return;
      source.close();
      this.source = null;
      this.scheduleReconnect();
    };
  }

  private scheduleReconnect() {
    this.opts.onState('reconnecting');
    const delay = BACKOFF_MS[Math.min(this.attempt, BACKOFF_MS.length - 1)];
    this.attempt += 1;
    this.reconnectTimer = window.setTimeout(() => void this.start(), delay);
  }

  /**
   * Renew at a third of the time actually remaining, so one failed attempt still leaves room
   * for another before the lease lapses.
   *
   * Driven by the server's `expires_at` rather than a client-side constant: the lease length is
   * configurable server-side, and a client renewing on a stale assumption would let the
   * subscription expire under it. Clamped to a floor so a clock skew or an already-short lease
   * cannot turn into a renewal storm.
   */
  private scheduleRenew(expiresAt?: string) {
    if (this.renewTimer) window.clearTimeout(this.renewTimer);
    const remainingMs = expiresAt ? Date.parse(expiresAt) - Date.now() : this.opts.leaseSecs * 1000;
    const delay = Math.max(5000, (Number.isFinite(remainingMs) ? remainingMs : this.opts.leaseSecs * 1000) / 3);
    this.renewTimer = window.setTimeout(() => void this.renew(), delay);
  }

  async renew(): Promise<void> {
    if (!this.subscriptionId || this.stopped) return;
    try {
      const res = await fetch(`/p/${this.opts.projectId}/live_tail/subscriptions/${this.subscriptionId}/renew`, {
        method: 'POST',
      });
      if (res.status === 404) {
        // The lease is gone, so rows were missed. Say so rather than silently reconnecting
        // into what would look like an unbroken stream.
        this.stop();
        this.opts.onState('expired', 'Live mode expired — restart it to resume. Events from the gap were not kept.');
        return;
      }
      const body = await res.json().catch(() => ({}));
      this.scheduleRenew(body?.expires_at);
    } catch {
      // Transient: the next scheduled renewal or the reconnect path resolves it.
      this.scheduleRenew();
    }
  }

  stop(): void {
    this.generation++;
    this.stopped = true;
    this.source?.close();
    this.source = null;
    if (this.renewTimer) window.clearTimeout(this.renewTimer);
    if (this.reconnectTimer) window.clearTimeout(this.reconnectTimer);
    this.renewTimer = this.reconnectTimer = null;
    if (this.subscriptionId) {
      // `keepalive` so this still goes out during unload — sendBeacon can't, it only POSTs.
      // Best effort regardless: the lease expiry is what actually guarantees cleanup.
      void fetch(`/p/${this.opts.projectId}/live_tail/subscriptions/${this.subscriptionId}`, {
        method: 'DELETE',
        keepalive: true,
      }).catch(() => {});
      this.subscriptionId = null;
    }
  }
}

/**
 * Turn a pushed Events row (column name → value) into the positional array the table renders.
 *
 * The server sends only what it could resolve from the in-memory record, so a column it
 * couldn't answer — an aggregate, a SQL expression — is absent here and becomes null, which
 * renders as an empty cell until the durable read replaces the row.
 */
export function tableRowToArray(cols: Record<string, unknown>, colIdxMap: Record<string, number>): unknown[] {
  const out = new Array(Object.keys(colIdxMap).length).fill(null);
  for (const [name, idx] of Object.entries(colIdxMap)) {
    if (name in cols) out[idx] = cols[name];
  }
  return out;
}


/**
 * Build the trace-adjacency entries `groupSpans` needs for a set of pushed rows.
 *
 * A fetch gets these from the server, which derives them while assembling the page. A pushed
 * row arrives without the query response's adjacency metadata, so the client synthesises it.
 * A relay frame often contains several members of the same trace; relationships within that
 * frame are retained and rows whose parent is outside it become temporary roots. Without
 * these entries `groupSpans` produces nothing — the tree is keyed off trace adjacency, not
 * off the row array.
 *
 * Rows that later turn out to belong to a larger trace are re-grouped when the durable read
 * replaces them, which is the same reconciliation a recent fetch already relies on.
 */
export function traceEntriesFor(rows: unknown[][], colIdxMap: Record<string, number>): ServerTraceEntry[] {
  const at = (r: unknown[], name: string) => (colIdxMap[name] === undefined ? null : r[colIdxMap[name]]);
  const projected = rows.map(r => {
    const rowId = String(at(r, 'id') ?? '');
    const id = String(at(r, 'kind') === 'log' ? rowId : at(r, 'latency_breakdown') || rowId);
    const traceId = String(at(r, 'trace_id') || id);
    const parentId = String(at(r, 'parent_id') ?? '');
    const ts = at(r, 'timestamp');
    const startMs = typeof ts === 'string' ? Date.parse(ts) : Number(ts ?? 0);
    return { r, id, traceId, parentId, ts, startMs };
  });

  // A relay frame can contain a whole ingest batch. Preserve the parent relationships that
  // are present inside that frame, otherwise every member of one collapsed trace is treated
  // as a standalone root and the "N new" pill counts (typically) all 200 batch rows. The
  // durable query later groups those rows and appears to insert only one, breaking the pill's
  // promise. Parents outside this frame remain roots until a durable read can reconcile them.
  const idsByTrace = new Map<string, Set<string>>();
  const childrenByTrace = new Map<string, Record<string, string[]>>();
  for (const row of projected) {
    if (!idsByTrace.has(row.traceId)) idsByTrace.set(row.traceId, new Set());
    idsByTrace.get(row.traceId)!.add(row.id);
  }
  for (const row of projected) {
    if (!row.parentId || !idsByTrace.get(row.traceId)?.has(row.parentId)) continue;
    const children = childrenByTrace.get(row.traceId) ?? {};
    (children[row.parentId] ??= []).push(row.id);
    childrenByTrace.set(row.traceId, children);
  }

  return projected
    .filter(row => !row.parentId || !idsByTrace.get(row.traceId)?.has(row.parentId))
    .map(row => ({
      trace_id: row.traceId,
      start_time: (Number.isFinite(row.startMs) ? row.startMs : 0) * 1e6,
      duration: Number(at(row.r, 'duration') ?? 0) || 0,
      trace_start_time: typeof row.ts === 'string' ? row.ts : new Date(Number.isFinite(row.startMs) ? row.startMs : 0).toISOString(),
      root: row.id,
      children: childrenByTrace.get(row.traceId) ?? {},
    }));
}
