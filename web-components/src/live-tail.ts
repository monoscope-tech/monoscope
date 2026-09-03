/**
 * Live Tail: a bounded, readable view of logs arriving right now.
 *
 * The server matches on the ingest pod and pushes over SSE; `LiveStream` owns the connection
 * lifecycle. This component owns the bounded row buffer, field projection and detail drawer.
 * Rows pin service and time; chosen fields always flow as key=value pairs in one message lane,
 * never as more columns. Pause freezes display rather than applying backpressure.
 */

import '@lit-labs/virtualizer';
import { LitElement, html, nothing, PropertyValues, TemplateResult } from 'lit';
import { customElement, state } from 'lit/decorators.js';
import { faSprite_ } from './assets';
import { LiveStream, LiveState } from './live-stream';
import { atBottom, classifyLevel, getStyleClass, parseSummaryElement, type LevelSeverity } from './log-list-utils';

type JsonValue = string | number | boolean | null | JsonValue[] | { [key: string]: JsonValue };
type FieldValue = JsonValue | undefined;

type LiveRow = {
  id: string;
  timestamp: string;
  level: string | null;
  service: string | null;
  trace_id: string | null;
  span_id: string | null;
  name: string | null;
  body: string;
  fields?: Record<string, JsonValue>;
  truncated: boolean;
};

type PreparedRow = LiveRow & {
  displayFields: Record<string, FieldValue>;
  parsedBody: JsonValue | null;
  bodyPrefix: string;
  bodyFields: Array<[string, FieldValue]>;
};

const MAX_ROWS = 1000;
const MAX_MOUNTED_ROWS = 100;
const DEFAULT_FIELDS: Record<string, string[]> = {
  logs: ['level', 'body'],
  spans: ['summary'],
  any: ['level', 'summary', 'body'],
};
const PINNED_FIELDS = ['service', 'timestamp'];
const BASE_FIELDS = ['timestamp', 'level', 'service', 'trace_id', 'span_id', 'name', 'body'];
const KINDS: Array<[string, string]> = [
  ['logs', 'Logs'],
  ['spans', 'Spans'],
  ['any', 'Logs & spans'],
];

const STATE_LABEL: Record<LiveState, string> = {
  idle: 'Not connected',
  connecting: 'Connecting…',
  live: 'Live',
  reconnecting: 'Reconnecting…',
  expired: 'Expired',
  error: 'Error',
};

const displayFieldName = (name: string): string => name.replaceAll('___', '.');

const formatValue = (value: FieldValue): string => {
  if (value === undefined) return '—';
  if (typeof value === 'string') return JSON.stringify(value);
  return JSON.stringify(value) ?? String(value);
};

const findJson = (body: string): { value: JsonValue | null; prefix: string } => {
  const trimmed = body.trim();
  const starts = [...new Set([0, trimmed.indexOf('{'), trimmed.indexOf('[')])].filter((start) => start >= 0).sort((a, b) => a - b);
  for (const start of starts) {
    try {
      return { value: JSON.parse(trimmed.slice(start)) as JsonValue, prefix: trimmed.slice(0, start).trim() };
    } catch {
      // A log prefix often precedes the JSON. Try its first object and array boundary.
    }
  }
  return { value: null, prefix: '' };
};

const flattenJson = (value: JsonValue, prefix: string, out: Record<string, FieldValue>, depth = 0): Record<string, FieldValue> => {
  if (Object.keys(out).length >= 100) return out;
  if (value !== null && !Array.isArray(value) && typeof value === 'object' && depth < 5) {
    for (const [key, child] of Object.entries(value)) flattenJson(child, prefix ? `${prefix}.${key}` : key, out, depth + 1);
  } else {
    out[prefix] = value;
  }
  return out;
};

const prepareRow = (row: LiveRow): PreparedRow => {
  const { value: parsedBody, prefix: bodyPrefix } = findJson(row.body);
  const displayFields: Record<string, FieldValue> = {
    timestamp: row.timestamp,
    level: row.level ?? undefined,
    service: row.service ?? undefined,
    trace_id: row.trace_id ?? undefined,
    span_id: row.span_id ?? undefined,
    name: row.name ?? undefined,
    body: row.body,
    ...(row.fields ?? {}),
  };
  if (parsedBody !== null) flattenJson(parsedBody, 'body', displayFields);
  const bodyFields =
    parsedBody !== null && !Array.isArray(parsedBody) && typeof parsedBody === 'object'
      ? Object.entries(flattenJson(parsedBody, '', {}))
      : [];
  return { ...row, displayFields, parsedBody, bodyPrefix, bodyFields };
};

// Severity vocabulary is shared (classifyLevel); only the class names are this surface's.
// `null` (a level this vocabulary does not recognise) keeps live tail's informational look.
const LEVEL_TEXT: Record<LevelSeverity, string> = {
  error: 'text-textError',
  warn: 'text-textWarning',
  debug: 'text-textWeak',
  info: 'text-textInformation',
};
const LEVEL_CHIP: Record<LevelSeverity, string> = {
  error: 'bg-fillError-weak text-textError border-strokeError-weak',
  warn: 'bg-fillWarning-weak text-textWarning border-strokeWarning-weak',
  debug: 'bg-fillWeak text-textWeak border-strokeWeak',
  info: 'bg-fillInformation-weak text-textInformation border-strokeInformation-weak',
};

const levelTextClass = (raw: string | null): string => LEVEL_TEXT[classifyLevel(raw) ?? 'info'];
const levelClasses = (raw: string | null): string => LEVEL_CHIP[classifyLevel(raw) ?? 'info'];

const scalarTemplate = (value: FieldValue): TemplateResult => {
  const text = formatValue(value);
  const classes =
    value === undefined || value === null
      ? 'text-textWeak'
      : typeof value === 'string'
        ? 'text-textSuccess'
        : typeof value === 'number'
          ? 'text-textWarning'
          : typeof value === 'boolean'
            ? 'text-textStrong font-semibold'
            : 'text-textWeak';
  return html`<span class=${classes}>${text}</span>`;
};

const jsonTemplate = (json: string): TemplateResult => {
  const token = /("(?:\\.|[^"\\])*")(?=\s*:)|"(?:\\.|[^"\\])*"|-?\d+(?:\.\d+)?(?:e[+-]?\d+)?|\b(?:true|false|null)\b|[{}\[\],:]/gi;
  const parts: TemplateResult[] = [];
  let cursor = 0;
  for (const match of json.matchAll(token)) {
    const index = match.index ?? 0;
    if (index > cursor) parts.push(html`${json.slice(cursor, index)}`);
    const value = match[0];
    const rest = json.slice(index + value.length);
    const classes = value.startsWith('"')
      ? /^\s*:/.test(rest)
        ? 'text-textInformation'
        : 'text-textSuccess'
      : /^-?\d/.test(value)
        ? 'text-textWarning'
        : /^(true|false)$/i.test(value)
          ? 'text-textStrong font-semibold'
          : 'text-textWeak';
    parts.push(html`<span class=${classes}>${value}</span>`);
    cursor = index + value.length;
  }
  if (cursor < json.length) parts.push(html`${json.slice(cursor)}`);
  return html`${parts}`;
};

const logBodyTemplate = (row: PreparedRow): TemplateResult => {
  const truncated = row.truncated ? html`<span class="text-textWeak">…truncated</span>` : nothing;
  if (row.parsedBody !== null && !Array.isArray(row.parsedBody) && typeof row.parsedBody === 'object') {
    return html`${row.bodyPrefix ? html`<span class="text-textWeak">${row.bodyPrefix}</span>` : nothing}${row.bodyFields.map(
      ([field, value]) =>
        html`<span class="min-w-0 break-words"><span class="text-textWeak">${field}=</span>${scalarTemplate(value)}</span>`
    )}${truncated}`;
  }
  if (row.parsedBody !== null) {
    return html`<span class="min-w-0 whitespace-pre-wrap break-words text-textStrong">${jsonTemplate(JSON.stringify(row.parsedBody))}</span
      >${truncated}`;
  }
  const token = /([\w.@/-]+)=("(?:\\.|[^"\\])*"|[^\s]+)/g;
  const parts: TemplateResult[] = [];
  let cursor = 0;
  for (const match of row.body.matchAll(token)) {
    const index = match.index ?? 0;
    if (index > cursor) parts.push(html`${row.body.slice(cursor, index)}`);
    parts.push(html`<span class="text-textInformation">${match[1]}</span><span class="text-textWeak">=</span>${jsonTemplate(match[2])}`);
    cursor = index + match[0].length;
  }
  if (cursor < row.body.length) parts.push(html`${row.body.slice(cursor)}`);
  return html`<span class="min-w-0 whitespace-pre-wrap break-words text-textStrong">${parts}</span>${truncated}`;
};

@customElement('live-tail')
export class LiveTail extends LitElement {
  @state() private streamState: LiveState = 'idle';
  @state() private paused = false;
  @state() private rows: PreparedRow[] = [];
  @state() private droppedServer = 0;
  @state() private droppedClient = 0;
  @state() private statusMessage = '';
  @state() private services: string[] = [];
  @state() private environments: string[] = [];
  @state() private schemaFields: string[] = [];
  @state() private selectedFields: string[] = DEFAULT_FIELDS.logs;
  @state() private fieldSearch = '';
  @state() private selectedRow: PreparedRow | null = null;
  @state() private fullRecord: Record<string, JsonValue> | null = null;
  @state() private detailLoading = false;
  @state() private detailError = '';
  @state() private copied = false;
  @state() private stickToBottom = true;
  @state() private aiSearchOpen = false;
  @state() private aiPrompt = '';
  @state() private aiLoading = false;
  @state() private aiError = '';

  private service = '';
  private environment = '';
  private kind = 'logs';
  private query = '';
  private buffer: PreparedRow[] = [];
  private fieldCounts = new Map<string, number>();
  private stream: LiveStream | null = null;
  private projectId = '';
  private leaseSecs = 45;
  private detailRequestSeq = 0;
  private followFrame: number | null = null;
  private followPending = false;
  private followPinned = false;
  private rowFrame: number | null = null;

  createRenderRoot() {
    return this;
  }

  connectedCallback() {
    super.connectedCallback();
    this.projectId = this.dataset.projectId ?? '';
    this.leaseSecs = Number(this.dataset.leaseSecs ?? '45');
    const params = new URLSearchParams(location.search);
    this.service = params.get('service') ?? '';
    this.environment = params.get('env') ?? '';
    const kind = params.get('kind') ?? '';
    this.kind = KINDS.some(([value]) => value === kind) ? kind : 'logs';
    this.query = params.get('query') ?? '';
    this.selectedFields = this.fieldsForKind();
    this.classList.add('group/lt');
    document.addEventListener('visibilitychange', this.onVisibility);
  }

  disconnectedCallback() {
    document.removeEventListener('visibilitychange', this.onVisibility);
    this.teardown();
    if (this.rowFrame !== null) cancelAnimationFrame(this.rowFrame);
    this.rowFrame = null;
    super.disconnectedCallback();
  }

  firstUpdated() {
    this.restart();
    void this.loadFacets();
    this.dispatchEvent(new CustomEvent('arm-deferred-components', { bubbles: true }));
  }

  updated(changed: PropertyValues) {
    this.dataset.state = this.streamState;
    this.dataset.paused = String(this.paused);
    const list = this.querySelector<HTMLElement>('[data-rows]');
    if (list?.tagName === 'LIT-VIRTUALIZER') {
      const follow = this.followPending || this.stickToBottom;
      this.followPending = false;
      if (follow) this.scheduleFollow();
    } else if (this.stickToBottom && list) list.scrollTop = list.scrollHeight;
    if (changed.has('selectedRow') && this.selectedRow) {
      const dialog = this.querySelector<HTMLDialogElement>('[data-details]');
      if (dialog && !dialog.open) {
        if (typeof dialog.showModal === 'function') dialog.showModal();
        else dialog.setAttribute('open', '');
      }
    }
  }

  private get fieldStorageKey(): string {
    return `live-tail-fields:${this.projectId}:${this.kind}`;
  }

  private fieldsForKind(): string[] {
    try {
      const saved = JSON.parse(localStorage.getItem(this.fieldStorageKey) ?? 'null');
      if (Array.isArray(saved) && saved.every((field) => typeof field === 'string')) {
        return saved.filter((field) => !PINNED_FIELDS.includes(field));
      }
    } catch {
      // Preferences are optional; private browsing can make storage unavailable.
    }
    return [...(DEFAULT_FIELDS[this.kind] ?? DEFAULT_FIELDS.logs)];
  }

  private get running(): boolean {
    return this.streamState === 'live' || this.streamState === 'connecting' || this.streamState === 'reconnecting';
  }

  private get availableFields(): string[] {
    return [...new Set([...BASE_FIELDS, ...this.selectedFields, ...this.schemaFields, ...this.fieldCounts.keys()])]
      .filter((field) => !PINNED_FIELDS.includes(field))
      .sort((a, b) => {
        const ai = this.selectedFields.indexOf(a);
        const bi = this.selectedFields.indexOf(b);
        if (ai >= 0 || bi >= 0) return ai >= 0 && bi >= 0 ? ai - bi : ai >= 0 ? -1 : 1;
        return displayFieldName(a).localeCompare(displayFieldName(b));
      });
  }

  private onVisibility = () => {
    if (!document.hidden && this.streamState === 'live') void this.stream?.renew();
  };

  private async loadFacets() {
    try {
      const response = await fetch(`/p/${this.projectId}/log_explorer/schema`);
      const fields = (await response.json())?.fields ?? {};
      const examples = (key: string): string[] => fields[key]?.examples ?? [];
      this.services = examples('resource.service.name');
      this.environments = examples('resource.deployment.environment.name');
      this.schemaFields = Object.keys(fields);
    } catch {
      this.services = [];
      this.environments = [];
      this.schemaFields = [];
    }
  }

  private projectedFields(): string[] {
    return this.selectedFields.filter((field) => !BASE_FIELDS.includes(field) && !field.startsWith('body.'));
  }

  private restart = (clearRows = true) => {
    this.teardown();
    if (clearRows) {
      this.buffer = [];
      this.rows = [];
      this.fieldCounts.clear();
      this.droppedClient = 0;
      this.selectedRow = null;
    }
    this.droppedServer = 0;
    const url = new URL(location.href);
    const inUrl: Array<[string, string]> = [
      ['service', this.service],
      ['env', this.environment],
      ['kind', this.kind === 'logs' ? '' : this.kind],
      ['query', this.query],
    ];
    inUrl.forEach(([key, value]) => (value ? url.searchParams.set(key, value) : url.searchParams.delete(key)));
    history.replaceState({}, '', url);
    this.stream = new LiveStream({
      projectId: this.projectId,
      leaseSecs: this.leaseSecs,
      body: () => ({
        service: this.service || null,
        environment: this.environment || null,
        kind: this.kind,
        query: this.query || null,
        columns: this.projectedFields(),
      }),
      onRows: (rows) => this.appendRows(rows.map((row) => (row as any).log as LiveRow).filter(Boolean)),
      onDropped: (total) => (this.droppedServer = total),
      onState: (state, detail) => {
        this.streamState = state;
        this.statusMessage = detail ?? '';
      },
    });
    void this.stream.start();
  };

  private teardown() {
    this.stream?.stop();
    this.stream = null;
    if (this.followFrame !== null) cancelAnimationFrame(this.followFrame);
    this.followFrame = null;
    this.followPending = false;
    this.followPinned = false;
  }

  private appendRows(batch: LiveRow[]) {
    if (!batch.length) return;
    const prepared = batch.map(prepareRow);
    prepared.forEach((row) =>
      Object.keys(row.displayFields).forEach((field) => this.fieldCounts.set(field, (this.fieldCounts.get(field) ?? 0) + 1))
    );
    this.buffer.push(...prepared);
    if (this.buffer.length > MAX_ROWS) {
      const evicted = this.buffer.splice(0, this.buffer.length - MAX_ROWS);
      this.droppedClient += evicted.length;
      evicted.forEach((row) =>
        Object.keys(row.displayFields).forEach((field) => {
          const count = (this.fieldCounts.get(field) ?? 1) - 1;
          if (count) this.fieldCounts.set(field, count);
          else this.fieldCounts.delete(field);
        })
      );
    }
    if (this.paused) return;
    if (this.buffer.length < MAX_MOUNTED_ROWS) this.rows = [...this.buffer];
    else if (this.rowFrame === null) {
      this.rowFrame = requestAnimationFrame(() => {
        this.rowFrame = null;
        if (!this.paused) {
          this.followPending = this.stickToBottom;
          this.rows = [...this.buffer];
        }
      });
    }
  }

  private togglePause() {
    this.paused = !this.paused;
    if (!this.paused) {
      this.rows = [...this.buffer];
      this.stickToBottom = true;
    }
  }

  private openAiSearch = async () => {
    this.aiError = '';
    this.aiSearchOpen = true;
    await this.updateComplete;
    this.querySelector<HTMLInputElement>('[aria-label="AI search prompt"]')?.focus();
  };

  private closeAiSearch = () => {
    if (this.aiLoading) return;
    this.aiSearchOpen = false;
    this.aiError = '';
  };

  private submitAiSearch = async () => {
    const input = this.aiPrompt.trim();
    if (!input || this.aiLoading) return;
    this.aiLoading = true;
    this.aiError = '';
    try {
      const response = await fetch(`/p/${this.projectId}/log_explorer/ai_search`, {
        method: 'POST',
        headers: { Accept: 'application/json', 'Content-Type': 'application/json' },
        body: JSON.stringify({ input, timezone: Intl.DateTimeFormat().resolvedOptions().timeZone }),
      });
      const result = (await response.json()) as { query?: string; error?: string };
      const query = result.query?.trim();
      if (!response.ok || !query) throw new Error(result.error || 'AI search did not return a query.');

      this.aiSearchOpen = false;
      await this.updateComplete;
      const editor = this.querySelector<HTMLElement & { handleAddQuery?: (query: string, replace: boolean) => void }>('query-editor');
      if (editor?.handleAddQuery) editor.handleAddQuery(query, true);
      else {
        this.query = query;
        this.restart();
      }
    } catch (error) {
      this.aiError = error instanceof Error ? error.message : 'AI search failed. Try again.';
    } finally {
      this.aiLoading = false;
    }
  };

  private toggleField(field: string, shown: boolean) {
    if (PINNED_FIELDS.includes(field)) return;
    this.selectedFields = shown
      ? [...this.selectedFields.filter((selected) => selected !== field), field]
      : this.selectedFields.filter((selected) => selected !== field);
    try {
      localStorage.setItem(this.fieldStorageKey, JSON.stringify(this.selectedFields));
    } catch {
      // Rendering still changes even when the preference cannot persist.
    }
    this.restart(false);
  }

  private openDetails(row: PreparedRow) {
    this.copied = false;
    this.fullRecord = null;
    this.detailError = '';
    this.detailLoading = true;
    this.selectedRow = row;
    void this.loadFullRecord(row, ++this.detailRequestSeq);
  }

  private async loadFullRecord(row: PreparedRow, seq: number) {
    if (!/^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i.test(row.id)) {
      this.detailLoading = false;
      return;
    }
    for (const delay of [0, 400]) {
      if (delay) await new Promise((resolve) => window.setTimeout(resolve, delay));
      if (seq !== this.detailRequestSeq) return;
      const controller = new AbortController();
      const timeout = window.setTimeout(() => controller.abort(), 4000);
      try {
        const response = await fetch(
          `/p/${this.projectId}/live_tail/records/${encodeURIComponent(row.id)}/${encodeURIComponent(row.timestamp)}`,
          { headers: { Accept: 'application/json' }, signal: controller.signal }
        );
        if (response.ok) {
          const record = (await response.json()) as Record<string, JsonValue>;
          if (seq === this.detailRequestSeq) {
            this.fullRecord = record;
            this.detailLoading = false;
          }
          return;
        }
        if (response.status !== 404) break;
      } catch {
        if (delay === 400) break;
      } finally {
        window.clearTimeout(timeout);
      }
    }
    if (seq === this.detailRequestSeq) {
      this.detailLoading = false;
      this.detailError = 'The complete stored record is not available yet. Showing the streamed preview.';
    }
  }

  private closeDetails() {
    ++this.detailRequestSeq;
    const dialog = this.querySelector<HTMLDialogElement>('[data-details]');
    if (dialog?.open && typeof dialog.close === 'function') dialog.close();
    else dialog?.removeAttribute('open');
    this.selectedRow = null;
    this.fullRecord = null;
    this.detailLoading = false;
    this.detailError = '';
  }

  private scheduleFollow() {
    this.followPinned = true;
    if (this.followFrame !== null) return;
    this.followFrame = requestAnimationFrame(async () => {
      const frame = this.followFrame;
      const list = this.querySelector<HTMLElement & { layoutComplete?: Promise<void> }>('[data-rows]');
      if (list?.layoutComplete) {
        await Promise.race([list.layoutComplete, new Promise<void>((resolve) => requestAnimationFrame(() => resolve()))]);
      }
      if (this.followFrame !== frame) return;
      if (this.followPinned && list?.isConnected) list.scrollTop = list.scrollHeight;
      this.followFrame = null;
      this.followPinned = false;
    });
  }

  private trackScrollPosition = (event: Event) => {
    const list = event.currentTarget as HTMLElement;
    if (list.tagName === 'LIT-VIRTUALIZER' && this.followFrame !== null) return;
    this.stickToBottom = atBottom(list, 40);
  };

  private jumpToLive() {
    this.stickToBottom = true;
    const list = this.querySelector<HTMLElement>('[data-rows]');
    if (list) list.scrollTop = list.scrollHeight;
  }

  private async copyRecord() {
    if (!this.selectedRow) return;
    try {
      await navigator.clipboard.writeText(JSON.stringify(this.rawRecord(this.selectedRow), null, 2));
      this.copied = true;
      window.setTimeout(() => (this.copied = false), 1200);
    } catch {
      this.copied = false;
    }
  }

  private rawRecord(row: PreparedRow): Record<string, unknown> {
    return (
      this.fullRecord ?? {
        id: row.id,
        timestamp: row.timestamp,
        level: row.level,
        service: row.service,
        trace_id: row.trace_id,
        span_id: row.span_id,
        name: row.name,
        body: row.parsedBody ?? row.body,
        fields: row.fields ?? {},
      }
    );
  }

  render(): TemplateResult {
    const dropped = this.droppedServer + this.droppedClient;
    return html`
      <div class="w-full h-full min-h-0 flex flex-col bg-bgBase text-textStrong">
        ${this.toolbarTemplate()}
        <div class="flex items-center gap-2 min-h-9 px-4 py-1.5 text-xs border-b border-strokeWeak bg-bgSunken">
          <span class="inline-flex items-center gap-1.5 text-textWeak" role="status" aria-live="polite">
            <span
              aria-hidden="true"
              class="inline-block w-1.5 h-1.5 rounded-full bg-fillWarning-strong group-data-[state=idle]/lt:bg-fillWeak group-data-[state=live]/lt:bg-fillSuccess-strong group-data-[state=error]/lt:bg-fillError-strong group-data-[state=expired]/lt:bg-fillError-strong"
            ></span>
            ${STATE_LABEL[this.streamState]}${this.paused && this.streamState === 'live' ? ' — display paused' : ''}
          </span>
          <span class="text-textWeak tabular-nums">${this.rows.length.toLocaleString()} shown</span>
          ${dropped > 0 ? html`<span class="text-textWarning">${dropped.toLocaleString()} dropped — narrow your filter</span>` : nothing}
          ${this.statusMessage ? html`<span class="text-textError">${this.statusMessage}</span>` : nothing}
          ${!this.stickToBottom
            ? html`<button
                type="button"
                class="ml-auto inline-flex items-center gap-1.5 min-h-7 px-2 rounded-field bg-bgRaised border border-strokeWeak text-textBrand hover:bg-fillWeaker focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-strokeBrand-strong"
                @click=${this.jumpToLive}
              >
                ${faSprite_('arrow-down', 'regular', 'w-3 h-3')} Jump to live
              </button>`
            : nothing}
        </div>

        ${this.rows.length >= MAX_MOUNTED_ROWS
          ? html`<lit-virtualizer
              class="flex-1 min-h-0 overflow-y-auto c-scroll font-mono text-xs"
              data-rows
              .items=${this.rows}
              .keyFunction=${(row: PreparedRow) => `${row.id}:${row.timestamp}`}
              .renderItem=${(row: PreparedRow, index: number) => this.rowTemplate(row, index)}
              .scroller=${true}
              @scroll=${this.trackScrollPosition}
            ></lit-virtualizer>`
          : html`<div class="flex-1 min-h-0 overflow-y-auto c-scroll font-mono text-xs" data-rows @scroll=${this.trackScrollPosition}>
              ${this.rows.length
                ? this.rows.map((row, index) => this.rowTemplate(row, index))
                : html`<div class="min-h-48 flex flex-col items-center justify-center gap-1 px-6 text-center font-sans">
                    <span class="text-sm font-medium text-textStrong">
                      ${this.streamState === 'live' ? 'Waiting for matching records' : STATE_LABEL[this.streamState]}
                    </span>
                    <span class="text-xs text-textWeak">
                      ${this.streamState === 'live'
                        ? 'New records will appear here as they arrive. Adjust the filters above to narrow the stream.'
                        : 'The connection state will update automatically.'}
                    </span>
                  </div>`}
            </div>`}
      </div>
      ${this.detailsTemplate()}
    `;
  }

  private toolbarTemplate(): TemplateResult {
    const query = this.fieldSearch.trim().toLowerCase();
    const fields = this.availableFields.filter((field) => displayFieldName(field).toLowerCase().includes(query));
    return html`
      <div class="flex flex-wrap items-center gap-2 px-4 py-1 border-b border-strokeWeak bg-bgRaised">
        <select
          class="select select-sm w-48 max-md:flex-1 cursor-pointer"
          aria-label="Service"
          @change=${(event: Event) => {
            this.service = (event.target as HTMLSelectElement).value;
            this.restart();
          }}
        >
          <option value="" ?selected=${!this.service}>All services</option>
          ${this.services.map((service) => html`<option value=${service} ?selected=${service === this.service}>${service}</option>`)}
        </select>
        <select
          class="select select-sm w-40 max-md:flex-1 cursor-pointer"
          aria-label="Environment"
          @change=${(event: Event) => {
            this.environment = (event.target as HTMLSelectElement).value;
            this.restart();
          }}
        >
          <option value="" ?selected=${!this.environment}>All environments</option>
          ${this.environments.map(
            (environment) => html`<option value=${environment} ?selected=${environment === this.environment}>${environment}</option>`
          )}
        </select>
        <select
          class="select select-sm w-36 max-md:flex-1 cursor-pointer"
          aria-label="Signal kind"
          @change=${(event: Event) => {
            this.kind = (event.target as HTMLSelectElement).value;
            this.selectedFields = this.fieldsForKind();
            this.restart();
          }}
        >
          ${KINDS.map(([value, label]) => html`<option value=${value} ?selected=${value === this.kind}>${label}</option>`)}
        </select>
        <div class="flex-1 basis-72 min-w-64 max-md:order-last max-md:basis-full">
          <query-editor
            class=${`${this.aiSearchOpen ? 'hidden' : 'flex'} w-full items-center min-h-[38px]`}
            project-id=${this.projectId}
            default-value=${this.query}
            standalone-ai-search
            @open-ai-search=${this.openAiSearch}
            @update-query=${(event: CustomEvent<{ value: string }>) => {
              const next = (event.detail?.value ?? '').trim();
              if (next === this.query) return;
              this.query = next;
              this.restart();
            }}
          >
            <div
              class="w-full h-8 flex items-center justify-between gap-3 px-2.5 rounded-field border border-strokeWeak bg-bgBase text-xs text-textWeak"
            >
              <span>${this.query || 'Filter the live stream with KQL'}</span><kbd class="font-sans text-2xs">/</kbd>
            </div>
          </query-editor>
          ${this.aiSearchOpen
            ? html`<div
                  class="flex min-h-[38px] items-center gap-2 rounded-lg border-2 border-strokeBrand-strong bg-fillWeaker px-2 shadow-xs"
                >
                  <span class="shrink-0 rounded bg-fillBrand-weak px-1.5 py-0.5 text-2xs font-semibold text-textBrand">AI</span>
                  <input
                    class="min-w-0 flex-1 border-0 bg-transparent p-1 text-sm no-focus-ring"
                    aria-label="AI search prompt"
                    autocomplete="new-password"
                    placeholder="Ask in plain English — e.g. errors in payment service"
                    .value=${this.aiPrompt}
                    @input=${(event: Event) => (this.aiPrompt = (event.target as HTMLInputElement).value)}
                    @keydown=${(event: KeyboardEvent) => {
                      if (event.key === 'Escape') this.closeAiSearch();
                      if (event.key === 'Enter') {
                        event.preventDefault();
                        void this.submitAiSearch();
                      }
                    }}
                  />
                  ${this.aiLoading ? html`<span role="status">${faSprite_('spinner', 'regular', 'h-4 w-4 animate-spin')}</span>` : nothing}
                  <button
                    type="button"
                    aria-label="Submit AI search"
                    class="inline-flex min-h-7 cursor-pointer items-center gap-1.5 rounded-sm border border-strokeBrand-strong px-2 text-xs font-medium text-textBrand hover:bg-fillBrand-weak disabled:cursor-not-allowed disabled:opacity-50"
                    ?disabled=${!this.aiPrompt.trim() || this.aiLoading}
                    @click=${this.submitAiSearch}
                  >
                    ${faSprite_('arrow-right', 'regular', 'h-3.5 w-3.5')} Submit
                  </button>
                  <button
                    type="button"
                    aria-label="Close AI search"
                    class="inline-flex h-7 w-7 cursor-pointer items-center justify-center rounded-sm text-textWeak hover:bg-fillWeaker hover:text-textStrong disabled:cursor-not-allowed disabled:opacity-50 focus-visible:outline focus-visible:outline-2 focus-visible:outline-strokeBrand-strong"
                    ?disabled=${this.aiLoading}
                    @click=${this.closeAiSearch}
                  >
                    ${faSprite_('xmark', 'solid', 'h-3.5 w-3.5')}
                  </button>
                </div>
                ${this.aiError ? html`<p class="mt-1 text-xs text-textError" role="alert">${this.aiError}</p>` : nothing}`
            : nothing}
        </div>

        <details class="relative group/fields">
          <summary
            class="list-none cursor-pointer inline-flex items-center gap-1.5 min-h-8 px-2.5 rounded-field border border-strokeWeak bg-bgBase text-xs font-medium hover:bg-fillWeaker active:scale-[0.96] transition-transform focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-strokeBrand-strong"
          >
            ${faSprite_('table-columns', 'regular', 'w-3.5 h-3.5')} Fields
            <span class="tabular-nums text-textWeak">${this.selectedFields.length}</span>
          </summary>
          <div
            class="absolute z-40 right-0 top-full mt-1.5 w-80 max-w-[calc(100vw-2rem)] max-h-[min(30rem,70vh)] hidden group-open/fields:flex flex-col rounded-md border border-strokeWeak bg-bgOverlay shadow-md"
          >
            <div class="p-2 border-b border-strokeWeak">
              <label class="sr-only" for="live-tail-field-search">Find a field</label>
              <input
                id="live-tail-field-search"
                class="input input-sm w-full"
                type="search"
                placeholder="Find a field"
                .value=${this.fieldSearch}
                @input=${(event: Event) => (this.fieldSearch = (event.target as HTMLInputElement).value)}
              />
            </div>
            <div class="overflow-y-auto c-scroll p-1" aria-label="Visible live-tail fields">
              ${fields.length
                ? fields.map((field) => this.fieldToggleTemplate(field))
                : html`<p class="px-2 py-4 text-center text-xs text-textWeak">No fields match “${this.fieldSearch}”.</p>`}
            </div>
          </div>
        </details>

        <button
          type="button"
          class="btn btn-sm inline-flex items-center gap-1.5 active:scale-[0.96] transition-transform"
          ?disabled=${!this.running}
          aria-pressed=${this.paused}
          @click=${this.togglePause}
        >
          ${faSprite_(this.paused ? 'play' : 'pause', 'solid', 'w-3 h-3')} ${this.paused ? 'Resume' : 'Pause'}
        </button>
        <button
          type="button"
          class="btn btn-sm btn-ghost active:scale-[0.96] transition-transform"
          @click=${() => {
            this.buffer = [];
            this.rows = [];
            this.fieldCounts.clear();
            this.droppedServer = 0;
            this.droppedClient = 0;
          }}
        >
          Clear
        </button>
      </div>
    `;
  }

  private fieldToggleTemplate(field: string): TemplateResult {
    const shown = this.selectedFields.includes(field);
    return html`<label
      class="flex items-center gap-2 min-h-9 px-2 rounded-sm cursor-pointer hover:bg-fillWeaker text-xs"
      title=${displayFieldName(field)}
    >
      <input
        type="checkbox"
        class="checkbox checkbox-sm"
        .checked=${shown}
        aria-label=${`${shown ? 'Hide' : 'Show'} ${displayFieldName(field)} in each row`}
        @change=${(event: Event) => this.toggleField(field, (event.target as HTMLInputElement).checked)}
      />
      <span class="font-mono truncate ${shown ? 'text-textStrong' : 'text-textWeak'}">${displayFieldName(field)}</span>
    </label>`;
  }

  private rowTemplate(row: PreparedRow, index: number): TemplateResult {
    return html`<button
      type="button"
      data-row
      aria-label=${`Inspect record from ${row.service ?? 'unknown service'} at ${row.timestamp}`}
      aria-haspopup="dialog"
      class="group/row flex w-full cursor-pointer items-start gap-1.5 min-h-8 px-2 py-1.5 text-left border-b border-strokeWeak/50 ${index %
        2 ===
      0
        ? 'bg-bgBase'
        : 'bg-bgAlternate/50'} hover:bg-fillWeaker focus-visible:outline focus-visible:outline-2 focus-visible:-outline-offset-2 focus-visible:outline-strokeBrand-strong"
      @click=${() => this.openDetails(row)}
    >
      <span
        aria-hidden="true"
        class="shrink-0 inline-flex items-center justify-center w-7 h-7 max-md:w-9 max-md:h-9 -my-0.5 rounded-sm text-iconNeutral opacity-60 max-md:opacity-100 group-hover/row:opacity-100 group-hover/row:text-textBrand"
      >
        ${faSprite_('circle-info', 'regular', 'w-4 h-4 stroke-current')}
      </span>
      <div
        class="flex-1 min-w-0 grid grid-cols-[minmax(0,1fr)_auto] md:grid-cols-[10rem_7.5rem_minmax(0,1fr)] items-baseline gap-x-3 gap-y-1 font-mono text-xs leading-5"
      >
        <span
          data-service
          class="min-w-0 truncate font-sans text-xs font-semibold ${row.service ? 'text-textStrong' : 'text-textWeak'}"
          title=${row.service ?? 'Unknown service'}
          >${row.service ?? 'Unknown service'}</span
        >
        <time data-time class="shrink-0 tabular-nums text-textWeak" datetime=${row.timestamp} title=${row.timestamp}
          >${row.timestamp.slice(11, 23)}</time
        >
        <div data-message class="col-span-2 md:col-span-1 min-w-0 flex flex-wrap items-baseline gap-x-3 gap-y-0.5">
          ${this.selectedFields.length
            ? this.selectedFields.map((field) => this.rowFieldTemplate(row, field))
            : html`<span class="font-sans text-textWeak">No message fields selected. Use Fields to add one.</span>`}
        </div>
      </div>
    </button>`;
  }

  private rowFieldTemplate(row: PreparedRow, field: string): TemplateResult | typeof nothing {
    const value = row.displayFields[field];
    if (value === undefined) return nothing;
    if (field === 'level') {
      return html`<span class="min-w-0 break-words"
        ><span class="text-textWeak">level=</span><span class=${levelTextClass(row.level)}>${formatValue(value)}</span></span
      >`;
    }
    if (field === 'body') return logBodyTemplate(row);
    if (field === 'summary' && Array.isArray(value)) {
      return html`<span class="basis-full min-w-0 inline-flex flex-wrap items-center gap-1">
        ${value.map((element) => {
          const parsed = parseSummaryElement(typeof element === 'string' ? element : JSON.stringify(element));
          if (parsed.type === 'plain') {
            return html`<span class="min-w-0 text-textStrong" title=${parsed.content}>${parsed.content}</span>`;
          }
          if (parsed.style.startsWith('text-')) {
            return html`<span
              class=${`${parsed.style === 'text-textStrong' ? 'text-textStrong font-medium' : 'text-textWeak'}`}
              title=${`${parsed.field}: ${parsed.value}`}
              >${parsed.value}</span
            >`;
          }
          return html`<span class=${`cbadge-sm ${getStyleClass(parsed.style)} shrink-0`} title=${parsed.field}>${parsed.value}</span>`;
        })}
      </span>`;
    }
    return html`<span class="min-w-0 whitespace-pre-wrap break-words" title=${`${displayFieldName(field)}=${formatValue(value)}`}
      ><span class="text-textWeak">${displayFieldName(field)}=</span>${scalarTemplate(value)}</span
    >`;
  }

  private detailsTemplate(): TemplateResult {
    const row = this.selectedRow;
    const detailFields: Record<string, FieldValue> = row
      ? this.fullRecord
        ? { ...row.displayFields, ...flattenJson(this.fullRecord, '', {}) }
        : row.displayFields
      : {};
    const fields = Object.entries(detailFields).sort(([a], [b]) => {
      const ai = BASE_FIELDS.indexOf(a);
      const bi = BASE_FIELDS.indexOf(b);
      return ai >= 0 || bi >= 0 ? (ai < 0 ? 1 : bi < 0 ? -1 : ai - bi) : displayFieldName(a).localeCompare(displayFieldName(b));
    });
    return html`<dialog
      data-details
      aria-labelledby="live-tail-detail-title"
      class="fixed inset-y-0 right-0 left-auto m-0 h-dvh w-[min(46rem,100vw)] max-h-none max-w-none overflow-hidden rounded-none border-0 border-l border-strokeWeak p-0 bg-bgBase text-textStrong shadow-lg backdrop:bg-black/35 backdrop:backdrop-blur-[1px] max-md:w-full"
      style="left:auto;right:0;height:100dvh;max-height:100dvh"
      @close=${() => (this.selectedRow = null)}
      @click=${(event: MouseEvent) => {
        if (event.target === event.currentTarget) this.closeDetails();
      }}
    >
      ${row
        ? html`<div class="h-full min-h-0 flex flex-col">
            <header class="shrink-0 flex items-start gap-3 px-5 py-4 border-b border-strokeWeak bg-bgRaised">
              <div class="min-w-0 flex-1">
                <div class="flex flex-wrap items-center gap-2 mb-1">
                  ${row.level
                    ? html`<span
                        class="inline-flex items-center h-5 px-1.5 rounded-sm border text-2xs font-semibold uppercase ${levelClasses(
                          row.level
                        )}"
                        >${row.level}</span
                      >`
                    : nothing}
                  <time class="font-mono text-xs tabular-nums text-textWeak" datetime=${row.timestamp}>${row.timestamp}</time>
                </div>
                <h2 id="live-tail-detail-title" class="text-lg font-semibold text-textStrong">Record details</h2>
                <p class="text-xs text-textWeak truncate">${row.service ?? 'Unknown service'}${row.name ? ` · ${row.name}` : ''}</p>
              </div>
              <button
                type="button"
                class="btn btn-sm btn-ghost inline-flex items-center gap-1.5"
                aria-label="Copy raw record"
                @click=${this.copyRecord}
              >
                ${faSprite_(this.copied ? 'check' : 'copy', 'regular', 'w-3.5 h-3.5')} ${this.copied ? 'Copied' : 'Copy'}
              </button>
              <button
                type="button"
                class="inline-flex items-center justify-center w-8 h-8 rounded-field text-textWeak hover:text-textStrong hover:bg-fillWeaker focus-visible:outline focus-visible:outline-2 focus-visible:outline-strokeBrand-strong"
                aria-label="Close record details"
                @click=${this.closeDetails}
              >
                ${faSprite_('xmark', 'solid', 'w-3.5 h-3.5')}
              </button>
            </header>

            <div class="flex-1 min-h-0 overflow-y-auto c-scroll px-5 py-5 space-y-6">
              <section aria-labelledby="live-tail-raw-title">
                <div class="flex items-baseline justify-between gap-3 mb-2">
                  <h3 id="live-tail-raw-title" class="text-sm font-semibold">Raw record</h3>
                  <span class="text-2xs text-textWeak">${this.fullRecord ? 'Complete stored JSON' : 'Streamed JSON preview'}</span>
                </div>
                ${this.detailLoading
                  ? html`<p class="mb-2 text-xs text-textWeak" role="status">Loading the complete stored record…</p>`
                  : this.detailError
                    ? html`<p class="mb-2 text-xs text-textWarning" role="status">${this.detailError}</p>`
                    : nothing}
                <pre
                  class="max-h-80 overflow-auto c-scroll rounded-md bg-bgSunken border border-strokeWeak p-3 font-mono text-xs leading-5 whitespace-pre-wrap break-words"
                ><code>${jsonTemplate(JSON.stringify(this.rawRecord(row), null, 2))}</code></pre>
              </section>

              <section aria-labelledby="live-tail-fields-title">
                <h3 id="live-tail-fields-title" class="text-sm font-semibold">Fields</h3>
                <p class="mt-0.5 mb-2 text-xs text-textWeak">Choose which fields appear in every live row.</p>
                <div class="border-y border-strokeWeak divide-y divide-strokeWeak">
                  ${fields.map(([field, value]) => {
                    const pinned = PINNED_FIELDS.includes(field);
                    const shown = pinned || this.selectedFields.includes(field);
                    return html`<label
                      class="grid grid-cols-12 items-center gap-3 min-h-11 px-2 py-2 ${pinned ? '' : 'hover:bg-fillWeaker cursor-pointer'}"
                    >
                      <span class="col-span-4 max-sm:col-span-11 font-mono text-xs font-medium break-all">${displayFieldName(field)}</span>
                      <span
                        class="col-span-7 max-sm:col-span-11 max-sm:col-start-1 max-sm:row-start-2 font-mono text-xs break-all ${value ===
                        undefined
                          ? 'text-textWeak'
                          : 'text-textStrong'}"
                        >${formatValue(value)}</span
                      >
                      ${pinned
                        ? html`<span class="col-span-1 justify-self-end text-2xs text-textWeak">Pinned</span>`
                        : html`<input
                            type="checkbox"
                            class="col-span-1 max-sm:col-start-12 max-sm:row-start-1 justify-self-end checkbox checkbox-sm"
                            .checked=${shown}
                            aria-label=${`${shown ? 'Hide' : 'Show'} ${displayFieldName(field)} in each row`}
                            @change=${(event: Event) => this.toggleField(field, (event.target as HTMLInputElement).checked)}
                          />`}
                    </label>`;
                  })}
                </div>
              </section>
            </div>
          </div>`
        : nothing}
    </dialog>`;
  }
}
