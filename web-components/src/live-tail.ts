/**
 * Live Tail: a bounded view of logs arriving right now.
 *
 * The server matches on the ingest pod and pushes over SSE; `LiveStream` owns the connection
 * lifecycle. This component owns only what is left — the row buffer and how it renders.
 *
 * Four rules shape it:
 *
 * 1. **The row buffer is capped.** A tail on a busy service produces rows faster than anyone
 *    can read them; an uncapped list is an out-of-memory crash with extra steps. Rows past the
 *    cap are dropped from the top, matching the server's own drop-oldest queue.
 * 2. **Pause is a display freeze, not backpressure.** The stream keeps flowing into the same
 *    capped buffer while paused, so resuming shows the latest rows rather than a stale burst.
 *    Pausing must never make the server hold anything for us.
 * 3. **State drives CSS, not imperative writes.** `data-state` / `data-paused` on the host let
 *    the button labels and the status dot switch in stylesheet rules, the same way the
 *    Explorer's own live toggle works (`logExplorerActions_`). The only thing CSS genuinely
 *    cannot do — setting `disabled`, which has to be real for assistive tech — stays here.
 * 4. **The selection is the control.** There is no start button. The page opens tailing every
 *    service's logs, and changing service, environment, kind or filter re-registers on the
 *    spot: a button would only add a step between a choice and its consequence, and a stale
 *    tail — still running under the previous filter — for as long as it went unpressed.
 *    Everything selected lives in the URL, so a tail pasted into an incident channel opens on
 *    the same stream the sender was watching.
 */

import { LitElement, html, nothing, TemplateResult } from 'lit';
import { customElement, state } from 'lit/decorators.js';
import { repeat } from 'lit/directives/repeat.js';
import { LiveStream, LiveState } from './live-stream';

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

/** Rows kept in the DOM. Past this the oldest are dropped, mirroring the server's queue. */
const MAX_ROWS = 1000;

/**
 * Level → text colour for a log line.
 *
 * Substring matching rather than exact keys, because real-world levels are messy
 * ("SEVERE_ERROR", "warning"). The classification mirrors `Utils.levelFillColor`: that returns
 * *fill* classes for a badge and this *text* classes for a line, so the outputs legitimately
 * differ — but the two must agree on which levels count as errors. Change them together.
 */
const levelClass = (raw: string | null): string => {
  const v = (raw ?? '').toLowerCase();
  if (v.includes('error') || v.includes('fatal')) return 'text-textError';
  if (v.includes('warn')) return 'text-textWarning';
  if (v.includes('debug') || v.includes('trace')) return 'text-textWeak';
  if (v.includes('info')) return 'text-textStrong';
  return 'text-textWeak';
};

const STATE_LABEL: Record<LiveState, string> = {
  idle: 'Not connected',
  connecting: 'Connecting…',
  live: 'Live',
  reconnecting: 'Reconnecting…',
  expired: 'Expired',
  error: 'Error',
};

/** Signal-kind modes, mirroring `Pkg.LiveTail.SignalKind` — the values are its derived spelling. */
const KINDS: Array<[string, string]> = [
  ['logs', 'Logs'],
  ['spans', 'Spans'],
  ['any', 'Logs & spans'],
];

@customElement('live-tail')
export class LiveTail extends LitElement {
  @state() private streamState: LiveState = 'idle';
  @state() private paused = false;
  @state() private rows: LiveRow[] = [];
  @state() private droppedServer = 0;
  @state() private droppedClient = 0;
  @state() private statusMessage = '';
  @state() private services: string[] = [];
  @state() private environments: string[] = [];

  // What the tail is registered for. Held here rather than read off the controls at start
  // time, because there is no start: every change to one of these re-registers immediately.
  private service = '';
  private environment = '';
  private kind = 'logs';
  private query = '';

  /**
   * The buffer keeps filling while paused; `rows` is the frozen view of it.
   *
   * Two fields rather than one because pause must not become backpressure: the server keeps
   * sending, this keeps accepting, and only the render is held. Resuming then shows what is
   * happening now instead of replaying the burst that arrived while the reader was reading.
   */
  private buffer: LiveRow[] = [];
  private stream: LiveStream | null = null;
  private projectId = '';
  private leaseSecs = 45;
  private stickToBottom = true;

  // Light DOM so the app's Tailwind applies — same as log-list.
  createRenderRoot() {
    return this;
  }

  connectedCallback() {
    super.connectedCallback();
    this.projectId = this.dataset.projectId ?? '';
    this.leaseSecs = Number(this.dataset.leaseSecs ?? '45');
    // The URL is the source of truth for what to tail, so a pasted link opens the same stream.
    // `query` is the same parameter the query editor maintains, and the Events tab uses.
    const params = new URLSearchParams(location.search);
    this.service = params.get('service') ?? '';
    this.environment = params.get('env') ?? '';
    const kind = params.get('kind') ?? '';
    this.kind = KINDS.some(([v]) => v === kind) ? kind : 'logs';
    this.query = params.get('query') ?? '';
    this.classList.add('group/lt');
    document.addEventListener('visibilitychange', this.onVisibility);
  }

  disconnectedCallback() {
    document.removeEventListener('visibilitychange', this.onVisibility);
    this.teardown();
    super.disconnectedCallback();
  }

  firstUpdated() {
    // The tail starts itself: the page's whole job is to show what is arriving, and a screen
    // that waits to be told to do that is a step between the user and the only thing here.
    this.restart();
    void this.loadFacets();
    // Monaco is lazy-loaded by a scan of the server-rendered document (see index.ts), which ran
    // before this element rendered its editor. Ask for a re-scan so this editor gets armed the
    // same way — importing Monaco here instead would pull ~1MB eagerly on every page that
    // renders a <live-tail>, including the log explorer's hidden one.
    this.dispatchEvent(new CustomEvent('arm-deferred-components', { bubbles: true }));
  }

  updated() {
    // The only imperative state→DOM write left. Everything keyed off these two attributes is a
    // stylesheet rule, so relabelling on connect/reconnect costs no template work.
    this.dataset.state = this.streamState;
    this.dataset.paused = String(this.paused);
    if (this.stickToBottom) {
      const list = this.querySelector('[data-rows]');
      if (list) list.scrollTop = list.scrollHeight;
    }
  }

  private onVisibility = () => {
    if (document.hidden || this.streamState !== 'live') return;
    // Back from sleep the lease may already have lapsed; renewing now turns that into a
    // visible "expired" rather than a stream that silently stopped producing.
    void this.stream?.renew();
  };

  private get running(): boolean {
    return this.streamState === 'live' || this.streamState === 'connecting' || this.streamState === 'reconnecting';
  }

  /**
   * Offer the services and environments the project actually has, so narrowing a tail is a
   * pick rather than a guess.
   *
   * Both come from the schema endpoint's facet values, which land under
   * `fields[<dotted key>].examples` — the same enrichment the query editor's autocomplete
   * reads. There is no top-level `services` list to ask for.
   *
   * A failure here is not fatal: the tail already runs across every service, and the dropdown
   * is only how you narrow it. So the stream is left alone and the dropdown says why it is
   * empty rather than the page claiming to be broken.
   */
  private async loadFacets() {
    try {
      const res = await fetch(`/p/${this.projectId}/log_explorer/schema`);
      const fields = (await res.json())?.fields ?? {};
      const examples = (key: string): string[] => fields[key]?.examples ?? [];
      this.services = examples('resource.service.name');
      this.environments = examples('resource.deployment.environment.name');
    } catch {
      this.services = [];
      this.environments = [];
    }
  }

  /**
   * (Re)register the tail for the current selection.
   *
   * Everything the server matches on is fixed at registration, so any change to it is a new
   * subscription — and the rows already on screen came from the old one. Clearing them is the
   * honest answer: a list mixing two filters is one nobody can read a conclusion out of.
   */
  private restart = () => {
    this.teardown();
    this.buffer = [];
    this.rows = [];
    this.droppedServer = 0;
    this.droppedClient = 0;
    // Only what differs from the default is written, so a plain /live_tail link stays plain.
    const url = new URL(location.href);
    const inUrl: Array<[string, string]> = [
      ['service', this.service],
      ['env', this.environment],
      ['kind', this.kind === 'logs' ? '' : this.kind],
    ];
    inUrl.forEach(([k, v]) => (v ? url.searchParams.set(k, v) : url.searchParams.delete(k)));
    history.replaceState({}, '', url);
    this.stream = new LiveStream({
      projectId: this.projectId,
      leaseSecs: this.leaseSecs,
      body: () => ({
        service: this.service || null,
        environment: this.environment || null,
        kind: this.kind,
        query: this.query || null,
      }),
      onRows: rows => this.appendRows(rows.map(r => (r as any).log as LiveRow).filter(Boolean)),
      onDropped: total => (this.droppedServer = total),
      onState: (s, detail) => {
        this.streamState = s;
        this.statusMessage = detail ?? '';
      },
    });
    void this.stream.start();
  };

  private teardown() {
    this.stream?.stop();
    this.stream = null;
  }

  private appendRows(batch: LiveRow[]) {
    if (!batch.length) return;
    this.buffer = [...this.buffer, ...batch];
    if (this.buffer.length > MAX_ROWS) {
      this.droppedClient += this.buffer.length - MAX_ROWS;
      this.buffer.splice(0, this.buffer.length - MAX_ROWS);
    }
    if (!this.paused) this.rows = this.buffer;
  }

  private togglePause() {
    this.paused = !this.paused;
    // Resuming jumps to the live edge rather than replaying what was missed.
    if (!this.paused) this.rows = this.buffer;
  }

  render(): TemplateResult {
    const dropped = this.droppedServer + this.droppedClient;
    return html`
      <div class="w-full h-full flex flex-col">
        <div class="flex flex-wrap items-center gap-2 px-4 py-3 border-b border-strokeWeak">
          <select
            class="select select-sm w-48"
            aria-label="Service"
            @change=${(e: Event) => {
              this.service = (e.target as HTMLSelectElement).value;
              this.restart();
            }}
          >
            <option value="" ?selected=${!this.service}>All services</option>
            ${this.services.map(s => html`<option value=${s} ?selected=${s === this.service}>${s}</option>`)}
          </select>
          <select
            class="select select-sm w-40"
            aria-label="Environment"
            @change=${(e: Event) => {
              this.environment = (e.target as HTMLSelectElement).value;
              this.restart();
            }}
          >
            <option value="" ?selected=${!this.environment}>All environments</option>
            ${this.environments.map(e => html`<option value=${e} ?selected=${e === this.environment}>${e}</option>`)}
          </select>
          <!-- Kind is a control, not a hidden kind == "log" inside the filter: it decides what
               the page is showing, and a mode the user cannot see is one they cannot undo. -->
          <select
            class="select select-sm w-36"
            aria-label="Signal kind"
            @change=${(e: Event) => {
              this.kind = (e.target as HTMLSelectElement).value;
              this.restart();
            }}
          >
            ${KINDS.map(([v, label]) => html`<option value=${v} ?selected=${v === this.kind}>${label}</option>`)}
          </select>
          <!-- The same editor the Events tab uses, so a filter written in one is valid in the
               other: KQL autocomplete against this project's schema, and server-side
               validation as you type. It emits update-query once the query settles. -->
          <query-editor
            class="flex-1 min-w-64 flex items-center min-h-[38px]"
            project-id=${this.projectId}
            default-value=${this.query}
            @update-query=${(e: CustomEvent<{ value: string }>) => {
              const next = (e.detail?.value ?? '').trim();
              if (next === this.query) return;
              this.query = next;
              this.restart();
            }}
          ></query-editor>
          <button class="btn btn-sm" ?disabled=${!this.running} @click=${() => this.togglePause()}>
            <span class="group-data-[paused=true]/lt:hidden">Pause</span>
            <span class="hidden group-data-[paused=true]/lt:inline">Resume</span>
          </button>
          <button
            class="btn btn-sm btn-ghost"
            @click=${() => {
              this.buffer = [];
              this.rows = [];
              this.droppedServer = 0;
              this.droppedClient = 0;
            }}
          >
            Clear
          </button>
        </div>

        <div class="px-4 py-1.5 text-xs border-b border-strokeWeak" role="status" aria-live="polite">
          <span class="inline-flex items-center gap-1.5 text-textWeak">
            <span
              aria-hidden="true"
              class="inline-block w-1.5 h-1.5 rounded-full bg-fillWarning-strong group-data-[state=idle]/lt:bg-fillWeak group-data-[state=live]/lt:bg-fillSuccess-strong group-data-[state=error]/lt:bg-fillError-strong group-data-[state=expired]/lt:bg-fillError-strong"
            ></span>
            <span>${STATE_LABEL[this.streamState]}${this.paused && this.streamState === 'live' ? ' — display paused' : ''}</span>
            <span>· ${this.rows.length} shown</span>
            ${dropped > 0
              ? html`<span class="text-textWarning">· ${dropped.toLocaleString()} dropped — narrow your filter</span>`
              : nothing}
          </span>
          ${this.statusMessage ? html`<span class="ml-2 text-textError">${this.statusMessage}</span>` : nothing}
        </div>

        <div
          class="flex-1 overflow-y-auto c-scroll font-mono text-xs"
          data-rows
          @scroll=${(e: Event) => {
            // Following the tail only while the reader is at the bottom; scrolling up to read
            // must not be yanked back by the next batch.
            const el = e.target as HTMLElement;
            this.stickToBottom = el.scrollTop + el.clientHeight >= el.scrollHeight - 40;
          }}
        >
          ${this.rows.length
            ? repeat(
                this.rows,
                r => r.id,
                r => this.rowTemplate(r)
              )
            : html`<div class="p-8 text-center text-textWeak font-sans text-sm">
                ${this.streamState === 'live' ? 'Waiting for matching records…' : STATE_LABEL[this.streamState]}
              </div>`}
        </div>
      </div>
    `;
  }

  // Newest last, scrolled to the bottom — the `kubectl logs -f` model the name sets up.
  private rowTemplate(r: LiveRow): TemplateResult {
    return html`<div class="flex gap-3 px-4 py-1 border-b border-strokeWeak/40 hover:bg-fillWeaker">
      <span class="text-textWeak shrink-0 tabular-nums">${r.timestamp.slice(11, 23)}</span>
      <span class="shrink-0 w-14 uppercase ${levelClass(r.level)}">${r.level ?? ''}</span>
      <span class="shrink-0 w-32 truncate text-textWeak">${r.service ?? ''}</span>
      <!-- A span has no body; its name is the line. Without the fallback a spans tail renders
           a column of blank rows and reads as a broken stream rather than a working one. -->
      <span class="flex-1 whitespace-pre-wrap break-all"
        >${r.body || r.name || ''}${r.truncated ? html`<span class="text-textWeak"> …truncated</span>` : nothing}</span
      >
    </div>`;
  }
}
