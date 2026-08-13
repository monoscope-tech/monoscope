/**
 * Live Tail: a bounded view of logs arriving right now.
 *
 * The server matches on the ingest pod and pushes over SSE; `LiveStream` owns the connection
 * lifecycle. This component owns only what is left — the row buffer and how it renders.
 *
 * Three rules shape it:
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
    this.classList.add('group/lt');
    document.addEventListener('visibilitychange', this.onVisibility);
  }

  disconnectedCallback() {
    document.removeEventListener('visibilitychange', this.onVisibility);
    this.teardown();
    super.disconnectedCallback();
  }

  firstUpdated() {
    void this.loadFacets();
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
   * Offer the services and environments the project actually has, so the required service
   * selection is a pick rather than a guess.
   *
   * Both come from the schema endpoint's facet values, which land under
   * `fields[<dotted key>].examples` — the same enrichment the query editor's autocomplete
   * reads. There is no top-level `services` list to ask for.
   *
   * On failure the placeholders stand alone. That is a dead end for Live Tail, since a service
   * is mandatory, so it says so rather than leaving an empty dropdown to explain itself.
   */
  private async loadFacets() {
    try {
      const res = await fetch(`/p/${this.projectId}/log_explorer/schema`);
      const fields = (await res.json())?.fields ?? {};
      const examples = (key: string): string[] => fields[key]?.examples ?? [];
      this.services = examples('resource.service.name');
      this.environments = examples('resource.deployment.environment.name');
      if (!this.services.length) {
        this.streamState = 'error';
        this.statusMessage = 'No services found for this project yet — send some telemetry first.';
      }
    } catch {
      this.services = [];
      this.environments = [];
      this.streamState = 'error';
      this.statusMessage = 'Could not load the service list.';
    }
  }

  private start = () => {
    const service = this.querySelector<HTMLSelectElement>('[data-service]')?.value?.trim() ?? '';
    if (!service) {
      this.streamState = 'error';
      this.statusMessage = 'Select a service before starting the tail.';
      return;
    }
    this.droppedServer = 0;
    this.droppedClient = 0;
    this.stream = new LiveStream({
      projectId: this.projectId,
      leaseSecs: this.leaseSecs,
      body: () => ({
        service,
        environment: this.querySelector<HTMLSelectElement>('[data-environment]')?.value || null,
        query: this.querySelector<HTMLInputElement>('[data-query]')?.value || null,
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

  private stopStream = () => {
    this.teardown();
    // An expired or failed tail keeps its state so the status line can explain itself; a
    // deliberate stop goes back to idle.
    if (this.streamState !== 'expired' && this.streamState !== 'error') this.streamState = 'idle';
  };

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
          <select data-service class="select select-sm w-48" aria-label="Service" ?disabled=${this.running}>
            <option value="">Select a service…</option>
            ${this.services.map(s => html`<option value=${s}>${s}</option>`)}
          </select>
          <select data-environment class="select select-sm w-40" aria-label="Environment" ?disabled=${this.running}>
            <option value="">All environments</option>
            ${this.environments.map(e => html`<option value=${e}>${e}</option>`)}
          </select>
          <input
            data-query
            class="input input-sm flex-1 min-w-64"
            placeholder=${'Filter, e.g. level == "error"'}
            aria-label="Filter query"
            ?disabled=${this.running}
          />
          <!-- One button, two labels: which one shows is a stylesheet rule keyed off the
               host's data-state, so connecting/reconnecting relabel without a re-render. -->
          <button class="btn btn-sm btn-primary" @click=${() => (this.running ? this.stopStream() : this.start())}>
            <span class="group-data-[state=live]/lt:hidden group-data-[state=connecting]/lt:hidden group-data-[state=reconnecting]/lt:hidden">Start tail</span>
            <span class="hidden group-data-[state=live]/lt:inline group-data-[state=connecting]/lt:inline group-data-[state=reconnecting]/lt:inline">Stop</span>
          </button>
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
            <span class="inline-block w-1.5 h-1.5 rounded-full bg-fillWarning group-data-[state=idle]/lt:bg-fillWeak group-data-[state=live]/lt:bg-fillSuccess group-data-[state=error]/lt:bg-fillError group-data-[state=expired]/lt:bg-fillError"></span>
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
                ${this.streamState === 'live' ? 'Waiting for matching logs…' : 'Select a service and start the tail.'}
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
      <span class="flex-1 whitespace-pre-wrap break-all"
        >${r.body}${r.truncated ? html`<span class="text-textWeak"> …truncated</span>` : nothing}</span
      >
    </div>`;
  }
}
