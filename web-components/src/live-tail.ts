/**
 * Live Tail: a bounded, self-renewing view of logs arriving right now.
 *
 * The server matches on the ingest pod and pushes over SSE; this component owns everything the
 * server cannot hold on the browser's behalf — the connection state machine, the row buffer,
 * and the lease renewal that proves someone is still watching.
 *
 * Three rules shape the whole file:
 *
 * 1. **The row buffer is capped.** A tail on a busy service produces rows faster than anyone
 *    can read them; an uncapped list is an out-of-memory crash with extra steps. Rows past the
 *    cap are dropped from the top, matching the server's own drop-oldest queue.
 * 2. **Pause is a display freeze, not backpressure.** The stream keeps flowing into the same
 *    capped buffer while paused, so resuming shows the latest rows rather than a stale burst.
 *    Pausing must never make the server hold anything for us.
 * 3. **The lease is renewed by us.** If this tab dies, stops renewing, or is put to sleep, the
 *    subscription expires server-side within one lease period and ingest stops matching for it.
 *    That is the intended cleanup path, not a leak.
 */

type LiveRow = {
  id: string;
  timestamp: string;
  level: string | null;
  service: string | null;
  trace_id: string | null;
  span_id: string | null;
  name: string | null;
  body: string;
  truncated: boolean;
};

type RegisterResponse = {
  subscription_id: string;
  stream_url: string;
  expires_at: string;
};

type State = 'idle' | 'connecting' | 'live' | 'reconnecting' | 'expired' | 'error';

/** Rows kept in the DOM. Past this the oldest are dropped, mirroring the server's queue. */
const MAX_ROWS = 1000;

/** Reconnect backoff, capped so a long outage does not turn into a long silence. */
const BACKOFF_MS = [1000, 2000, 5000, 10000, 15000];

const LEVEL_CLASS: Record<string, string> = {
  error: 'text-textError',
  fatal: 'text-textError',
  warn: 'text-textWarning',
  warning: 'text-textWarning',
  info: 'text-textStrong',
  debug: 'text-textWeak',
  trace: 'text-textWeak',
};

class LiveTail extends HTMLElement {
  private projectId = '';
  private leaseSecs = 45;

  private source: EventSource | null = null;
  private subscriptionId: string | null = null;
  private renewTimer: number | null = null;
  private reconnectTimer: number | null = null;
  private attempt = 0;

  private state: State = 'idle';
  private paused = false;
  private rows: LiveRow[] = [];
  private droppedServer = 0;
  private droppedClient = 0;
  private statusMessage = '';

  connectedCallback() {
    this.projectId = this.dataset.projectId ?? '';
    this.leaseSecs = Number(this.dataset.leaseSecs ?? '45');
    this.render();
    // A tab left open overnight should not keep a lease alive against a machine that is
    // asleep, nor reconnect into a subscription the server has already expired.
    document.addEventListener('visibilitychange', this.onVisibility);
  }

  disconnectedCallback() {
    document.removeEventListener('visibilitychange', this.onVisibility);
    this.stop();
  }

  private onVisibility = () => {
    if (document.hidden || this.state !== 'live') return;
    // Coming back from a sleep the lease may already have lapsed; renewing immediately turns
    // that into a visible "expired" rather than a stream that silently stopped producing.
    void this.renew();
  };

  // ---------------------------------------------------------------------------------------
  // Lifecycle
  // ---------------------------------------------------------------------------------------

  private async start() {
    const service = this.q<HTMLSelectElement>('[data-service]')?.value?.trim() ?? '';
    if (!service) {
      this.fail('Select a service before starting the tail.');
      return;
    }
    this.stop();
    this.setState('connecting');

    const body = {
      service,
      environment: this.q<HTMLSelectElement>('[data-environment]')?.value || null,
      query: this.q<HTMLInputElement>('[data-query]')?.value || null,
    };

    let reg: RegisterResponse;
    try {
      const res = await fetch(`/p/${this.projectId}/live_tail/subscriptions`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(body),
      });
      if (!res.ok) {
        // The server's message is the actionable one (service gate, bad filter, limit hit);
        // inventing our own here would hide which of those it was.
        const msg = await res.json().then(j => j.error).catch(() => 'Could not start the tail.');
        this.fail(msg);
        return;
      }
      reg = await res.json();
    } catch {
      this.fail('Could not reach the server.');
      return;
    }

    this.subscriptionId = reg.subscription_id;
    this.openStream(reg.stream_url);
    this.scheduleRenew();
  }

  private openStream(url: string) {
    const source = new EventSource(url);
    this.source = source;

    source.addEventListener('ready', () => {
      this.attempt = 0;
      this.setState('live');
    });

    source.addEventListener('log', e => {
      const batch = JSON.parse((e as MessageEvent).data) as LiveRow[];
      this.appendRows(batch);
    });

    source.addEventListener('dropped', e => {
      this.droppedServer = JSON.parse((e as MessageEvent).data).count ?? 0;
      this.renderStatus();
    });

    // EventSource reconnects on its own, but it cannot know the subscription expired — so we
    // take over the retry and re-register when the lease is gone.
    source.onerror = () => {
      if (this.state === 'expired') return;
      source.close();
      this.source = null;
      this.scheduleReconnect();
    };
  }

  private scheduleReconnect() {
    this.setState('reconnecting');
    const delay = BACKOFF_MS[Math.min(this.attempt, BACKOFF_MS.length - 1)];
    this.attempt += 1;
    this.reconnectTimer = window.setTimeout(() => void this.start(), delay);
  }

  /**
   * Renew a little before the halfway point of the lease, so a single failed renewal still
   * leaves time for another attempt before the subscription actually lapses.
   */
  private scheduleRenew() {
    if (this.renewTimer) window.clearTimeout(this.renewTimer);
    this.renewTimer = window.setTimeout(() => void this.renew(), (this.leaseSecs * 1000) / 3);
  }

  private async renew() {
    if (!this.subscriptionId) return;
    try {
      const res = await fetch(
        `/p/${this.projectId}/live_tail/subscriptions/${this.subscriptionId}/renew`,
        { method: 'POST' },
      );
      if (res.status === 404) {
        // The lease is gone, so rows have been missed. Say so rather than reconnecting
        // silently into what would look like an unbroken stream.
        this.stop();
        this.setState('expired');
        this.statusMessage = 'This tail expired. Start it again to resume — logs from the gap were not kept.';
        this.render();
        return;
      }
      this.scheduleRenew();
    } catch {
      // Transient: the next scheduled renewal, or the reconnect path, will resolve it.
      this.scheduleRenew();
    }
  }

  private stop() {
    this.source?.close();
    this.source = null;
    if (this.renewTimer) window.clearTimeout(this.renewTimer);
    if (this.reconnectTimer) window.clearTimeout(this.reconnectTimer);
    this.renewTimer = this.reconnectTimer = null;
    if (this.subscriptionId) {
      // `keepalive` so the request still goes out when this fires during unload — sendBeacon
      // cannot, because it only issues POSTs. Best effort either way: the lease expiry is
      // what actually guarantees cleanup, so a dropped delete costs nothing but a few
      // seconds of a subscription nobody is reading.
      void fetch(`/p/${this.projectId}/live_tail/subscriptions/${this.subscriptionId}`, {
        method: 'DELETE',
        keepalive: true,
      }).catch(() => {});
      this.subscriptionId = null;
    }
  }

  // ---------------------------------------------------------------------------------------
  // Rows
  // ---------------------------------------------------------------------------------------

  private appendRows(batch: LiveRow[]) {
    this.rows.push(...batch);
    if (this.rows.length > MAX_ROWS) {
      this.droppedClient += this.rows.length - MAX_ROWS;
      this.rows = this.rows.slice(-MAX_ROWS);
    }
    // Paused freezes the view, not the stream: the buffer keeps filling so resuming lands on
    // what is happening now rather than replaying a stale burst.
    if (!this.paused) this.renderRows();
    this.renderStatus();
  }

  private clear() {
    this.rows = [];
    this.droppedServer = 0;
    this.droppedClient = 0;
    this.renderRows();
    this.renderStatus();
  }

  // ---------------------------------------------------------------------------------------
  // Rendering
  // ---------------------------------------------------------------------------------------

  private setState(s: State) {
    this.state = s;
    if (s !== 'error' && s !== 'expired') this.statusMessage = '';
    this.render();
  }

  private fail(msg: string) {
    this.statusMessage = msg;
    this.setState('error');
  }

  private q<T extends Element>(sel: string): T | null {
    return this.querySelector<T>(sel);
  }

  private render() {
    if (!this.querySelector('[data-rows]')) this.renderShell();
    this.renderControls();
    this.renderRows();
    this.renderStatus();
  }

  private renderShell() {
    this.innerHTML = `
      <div class="w-full h-full flex flex-col">
        <div class="flex flex-wrap items-center gap-2 px-4 py-3 border-b border-strokeWeak" data-controls></div>
        <div class="px-4 py-1.5 text-xs border-b border-strokeWeak" data-status></div>
        <div class="flex-1 overflow-y-auto c-scroll font-mono text-xs" data-rows></div>
      </div>`;
    this.q('[data-controls]')!.addEventListener('click', e => {
      const action = (e.target as HTMLElement).closest<HTMLElement>('[data-action]')?.dataset.action;
      if (action === 'start') void this.start();
      if (action === 'stop') { this.stop(); this.setState('idle'); }
      if (action === 'pause') { this.paused = !this.paused; this.render(); }
      if (action === 'clear') this.clear();
    });
  }

  /**
   * Build the controls once, then only ever mutate their state.
   *
   * Re-rendering this markup on every state change would wipe the service the user picked and
   * the filter they typed — and it changes state on connect, on reconnect, and on every
   * incoming batch. The user's input has to outlive all of that, so the DOM here is created
   * once and afterwards only its `disabled` flags and button labels move.
   */
  private renderControls() {
    const el = this.q('[data-controls]');
    if (!el) return;
    if (!el.children.length) {
      el.innerHTML = `
        <select data-service class="select select-sm w-48" aria-label="Service">
          <option value="">Select a service…</option>
        </select>
        <select data-environment class="select select-sm w-40" aria-label="Environment">
          <option value="">All environments</option>
        </select>
        <input data-query class="input input-sm flex-1 min-w-64" placeholder="Filter, e.g. level == &quot;error&quot;"
               aria-label="Filter query" />
        <button data-action="toggle" class="btn btn-sm btn-primary"></button>
        <button data-action="pause" class="btn btn-sm"></button>
        <button data-action="clear" class="btn btn-sm btn-ghost">Clear</button>`;
      void this.hydrateSelectors();
    }

    const running = this.state === 'live' || this.state === 'connecting' || this.state === 'reconnecting';
    // Locked while running: a filter change mid-tail would mean the rows above and below it
    // came from different queries, with nothing in the list saying so.
    for (const sel of ['[data-service]', '[data-environment]', '[data-query]']) {
      const input = this.q<HTMLInputElement>(sel);
      if (input) input.disabled = running;
    }
    const toggle = this.q<HTMLButtonElement>('[data-action="toggle"]');
    if (toggle) toggle.textContent = running ? 'Stop' : 'Start tail';
    const pause = this.q<HTMLButtonElement>('[data-action="pause"]');
    if (pause) {
      pause.textContent = this.paused ? 'Resume' : 'Pause';
      pause.disabled = !running;
    }
  }

  /**
   * Fill the service and environment pickers from the project's own facets, so the options
   * are the values that actually exist rather than free text a user has to guess at.
   */
  private async hydrateSelectors() {
    const service = this.q<HTMLSelectElement>('[data-service]');
    if (!service || service.dataset.hydrated) return;
    service.dataset.hydrated = '1';
    try {
      const res = await fetch(`/p/${this.projectId}/log_explorer/schema`);
      const schema = await res.json();
      const services: string[] = schema?.services ?? [];
      for (const s of services) service.add(new Option(s, s));
    } catch {
      // Leave the picker with just its placeholder; the tail simply cannot start until the
      // user has a service, which the status line already says.
    }
  }

  private renderStatus() {
    const el = this.q('[data-status]');
    if (!el) return;
    const label: Record<State, string> = {
      idle: 'Not connected',
      connecting: 'Connecting…',
      live: this.paused ? 'Live — display paused' : 'Live',
      reconnecting: 'Reconnecting…',
      expired: 'Expired',
      error: 'Error',
    };
    const dot = this.state === 'live' && !this.paused ? 'bg-fillSuccess' : this.state === 'error' || this.state === 'expired' ? 'bg-fillError' : 'bg-fillWarning';
    const dropped = this.droppedServer + this.droppedClient;
    el.innerHTML = `
      <span class="inline-flex items-center gap-1.5 text-textWeak">
        <span class="inline-block w-1.5 h-1.5 rounded-full ${dot}"></span>
        <span>${label[this.state]}</span>
        <span>· ${this.rows.length} shown</span>
        ${dropped > 0 ? `<span class="text-textWarning">· ${dropped.toLocaleString()} dropped — narrow your filter</span>` : ''}
      </span>
      ${this.statusMessage ? `<span class="ml-2 text-textError">${escapeHtml(this.statusMessage)}</span>` : ''}`;
  }

  private renderRows() {
    const el = this.q('[data-rows]');
    if (!el) return;
    if (!this.rows.length) {
      el.innerHTML = `<div class="p-8 text-center text-textWeak font-sans text-sm">
        ${this.state === 'live' ? 'Waiting for matching logs…' : 'Select a service and start the tail.'}
      </div>`;
      return;
    }
    // Newest last, scrolled to the bottom — a terminal tail, which is the mental model the
    // name sets and the one people already have from `kubectl logs -f`.
    const wasAtBottom = el.scrollTop + el.clientHeight >= el.scrollHeight - 40;
    el.innerHTML = this.rows.map(rowHtml).join('');
    if (wasAtBottom) el.scrollTop = el.scrollHeight;
  }
}

function rowHtml(r: LiveRow): string {
  const level = (r.level ?? '').toLowerCase();
  return `<div class="flex gap-3 px-4 py-1 border-b border-strokeWeak/40 hover:bg-fillWeaker">
    <span class="text-textWeak shrink-0 tabular-nums">${escapeHtml(r.timestamp.slice(11, 23))}</span>
    <span class="shrink-0 w-14 uppercase ${LEVEL_CLASS[level] ?? 'text-textWeak'}">${escapeHtml(r.level ?? '')}</span>
    <span class="shrink-0 w-32 truncate text-textWeak">${escapeHtml(r.service ?? '')}</span>
    <span class="flex-1 whitespace-pre-wrap break-all">${escapeHtml(r.body)}${r.truncated ? '<span class="text-textWeak"> …truncated</span>' : ''}</span>
  </div>`;
}

function escapeHtml(s: string): string {
  const d = document.createElement('div');
  d.textContent = s;
  return d.innerHTML;
}

if (!customElements.get('live-tail')) customElements.define('live-tail', LiveTail);

export {};
