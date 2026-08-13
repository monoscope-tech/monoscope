'use strict';
import '@lit-labs/virtualizer';
import { FlowLayout } from '@lit-labs/virtualizer/layouts/flow.js';
import { LitElement, html, css, TemplateResult, nothing } from 'lit';
import { customElement, state, query, property } from 'lit/decorators.js';
import { ref, createRef, RefOrCallback } from 'lit/directives/ref.js';
import { APTEvent, ChildrenForLatency, ColIdxMap, EventLine, ServerTraceEntry, Trace, TraceDataMap } from './types/types';
import debounce from 'lodash/debounce';
import { LiveStream, tableRowToArray, traceEntriesFor } from './live-stream';
import { includes, startsWith, map, forEach, compact, chunk, chain, lt } from 'lodash';
// Import worker as URL instead of worker instance
import LogWorkerUrl from './log-worker?worker&url';
import { groupSpans } from './log-worker-functions';
import { spriteUrl } from './assets';
import clsx from 'clsx';
import {
  formatTimestamp,
  lookupVecValue,
  getErrorClassification,
  faSprite,
  renderBadge,
  renderIconWithTooltip,
  getColumnWidth,
  getStyleClass,
  CHAR_WIDTHS,
  MIN_COLUMN_WIDTH,
  parseSummaryElement,
  unescapeJsonString,
  calculateAutoBinWidth,
  createCachedIconRenderer,
  WEAK_TEXT_STYLES,
  RIGHT_PREFIX_REGEX,
  formatLargeCount,
  highlightPlaceholders,
  generateId,
  dedupeById,
  shouldBufferRecent,
  oldestRowTimestamp,
  newestRowTimestamp,
  renderSparkline,
  parseUserAgent,
  isBotUserAgent,
  deviceIconName,
  middleTruncatePath,
} from './log-list-utils';
import { expandSince, expandFromToRange, parseChartZoom } from './time-range-utils';
import { toEChartsColor } from './widgets';
import { unsafeHTML } from 'lit/directives/unsafe-html.js';
import { keyed } from 'lit/directives/keyed.js';

// Convert CSS token to hex for ECharts (which can't parse oklch)
const cssTokenToHex = (token: string): string => toEChartsColor(getComputedStyle(document.body).getPropertyValue(token).trim());

// TypeScript declarations for global functions
declare global {
  interface Window {
    updateUrlState: (key: string, value: string, action?: 'set' | 'delete') => void;
  }
}

// Ensure all badge classes are included in the final CSS build
// prettier-ignore
const _ensureBadgeClasses = html`
  <span class="badge-postgres badge-mysql badge-redis badge-mongo badge-mongodb badge-elastic badge-elasticsearch badge-cassandra badge-dynamodb badge-memcached badge-sqlite badge-clickhouse"></span>
  <span class="badge-2xx badge-3xx badge-4xx badge-5xx badge-error badge-success badge-warning badge-info badge-fatal badge-neutral"></span>
  <span class="badge-GET badge-POST badge-PUT badge-DELETE badge-PATCH"></span>
  <span class="cbadge cbadge-sm"></span>
  <span class="bg-fillBrand-strong bg-fillWarning-strong bg-fillError-strong bg-fillSuccess-strong bg-fillWarning-strong bg-fillInformation-strong bg-fillBrand-strong bg-fillBrand-strong bg-fillStrong bg-fillWarning-strong"></span>
  <span class="bg-amber-100 text-amber-800 dark:bg-amber-900/30 dark:text-amber-300"></span>
`;

const noopRef: RefOrCallback = () => {};

// Special item types for virtual list
type VirtualListItem = EventLine | { type: 'fetchRecent' } | { type: 'loadMore' } | { type: 'aggregateChildren'; parentKey: string };
type ScrollAnchor = { id: string; offset: number };

/**
 * Identity of a virtual row. Without it the virtualizer keys rows by index, so a live-tail
 * batch prepended at the top re-renders every visible row's cells — the whole list repaints
 * and re-measures on each tick instead of the existing rows simply moving down.
 */
// `'id' in item` and not `'type' in item`: an EventLine carries a `type` of its own ('log' | 'span').
export const virtualItemKey = (item: VirtualListItem) =>
  'id' in item ? item.id : item.type === 'aggregateChildren' ? `aggregateChildren:${item.parentKey}` : item.type;

const MAX_RETAINED_ROWS = 5000;

// FlowLayout starts at 100px until it observes rows. Log rows are 28px, so the
// initial estimate inflated the virtual scroll range roughly 3.5×.
export class DenseRowFlowLayout extends FlowLayout {
  constructor(...args: ConstructorParameters<typeof FlowLayout>) {
    super(...args);
    this._itemSize.height = 28;
    // The 1000px default renders ~36 offscreen rows per side. Dense fixed-height
    // rows need only a short runway, keeping style/paint work near the viewport.
    this._overhang = 200;
  }
}

/**
 * The dimension the latency bar attributes time to. `service` answers "which service ate the
 * request"; `kind` answers "was this my code or something it called". Both are projected on
 * every row already, so neither costs a query.
 */
export type LatencyDim = 'service' | 'kind';

/**
 * jsdom in this setup provides no `localStorage` at all, and a browser in private mode can
 * throw on access. A reading preference falling back to its default is the correct outcome;
 * it must never be the thing that stops the list from mounting.
 */
const latencyDimPref = {
  read: (): LatencyDim => {
    try {
      return localStorage.getItem('latencyDim') === 'kind' ? 'kind' : 'service';
    } catch {
      return 'service';
    }
  },
  write: (dim: LatencyDim) => {
    try {
      localStorage.setItem('latencyDim', dim);
    } catch {
      /* preference simply does not persist */
    }
  },
};

@customElement('log-list')
export class LogList extends LitElement {
  @property({ type: String }) projectId: string = '';
  @property({ type: String }) initialFetchUrl: string = '';
  @property({ type: String }) mode: 'logs' | 'patterns' | 'sessions' = 'logs';

  @state() private expandedTraces: Record<string, boolean> = {};
  // Session ids whose children are currently being fetched — drives the inline
  // row spinner so an expanded-but-empty session reads as "loading", not broken.
  @state() private loadingSessions: Record<string, boolean> = {};
  @state() private flipDirection: boolean = false;
  @state() private spanListTree: EventLine[] = [];
  // Ids currently in spanListTree, kept so paginated/live appends dedupe the
  // incoming page in O(page) instead of re-scanning the whole merged list each
  // call (which was O(pages²·rows) over a long scroll). Rebuilt on full
  // fetch/refresh; extended on child-expand splices and tree merges.
  private seenIds = new Set<string>();
  @state() private recentDataToBeAdded: EventLine[] = [];
  @state() private view: 'tree' | 'list' = 'tree';
  @state() private shouldScrollToBottom: boolean = false;
  @state() private logsColumns: string[] = [];
  @state() private wrapLines: boolean = false;
  @state() private hasMore: boolean = true;
  @state() private hasNewer: boolean = false;
  @state() private expandTimeRange: boolean = true;
  @state() private loadedCount: number = 0;
  @state() private totalCount: number = 0;
  @state() private totalPatterns: number = 0;
  @state() private totalSessions: number = 0;
  @state() private isLiveStreaming: boolean = false;
  @state() private isLoading: boolean = false;
  @state() private isFetchingRecent: boolean = false;
  @state() private isLoadingMore: boolean = false;
  @state() private fetchError: string | null = null;
  @state() private fetchedNew: boolean = false;
  @state() private visibleItems: EventLine[] = [];
  @state() private virtualListItems: VirtualListItem[] = [];
  // Inline-expand state keyed by aggregate row key (session_id or pattern_hash).
  @state() private expandedAggregates: Record<
    string,
    { rows: any[][]; cols: string[]; colIdxMap: ColIdxMap; hasMore: boolean; loading: boolean; skip: number; eventLines?: EventLine[] }
  > = {};
  @state() private fixedColumnWidths: Record<string, number> = {};
  // Which dimension the latency bar attributes time to. Persisted like the other column
  // preferences, because "am I looking at services or at span kinds" is a reading mode a
  // user settles into rather than a per-page choice.
  @state() private latencyDim: LatencyDim = latencyDimPref.read();

  // Refs for DOM elements
  @query('#logs_list_container_inner') private logsContainer?: HTMLElement;
  @query('#loader') private loaderElement?: HTMLElement;
  @query('#log_details_container') private logDetailsContainer?: HTMLElement;
  @query('#resizer-details_width-wrapper') private resizerWrapper?: HTMLElement;
  @query('#details_indicator') private detailsIndicator?: HTMLElement;

  private cachedServerTraces: ServerTraceEntry[] = [];
  // Non-reactive overrides used during renderAggregateChildren to avoid triggering Lit re-renders
  private _renderOverrides: { colIdxMap: ColIdxMap; logsColumns: string[]; mode: string } | null = null;
  private resizeTarget: string | null = null;
  private mouseState: { x: number } = { x: 0 };
  private colIdxMap: ColIdxMap = {};
  private serviceColors: Record<string, string> = {};
  private columnMaxWidthMap: ColIdxMap = {};
  private recentFetchUrl: string = '';
  // Live mode is a server push, not a poll. Polling could never be fast here: a row has to
  // clear its ingest batch and land in TimeFusion before a query can return it, so the
  // interval was bounded below by write-visibility latency no matter how short it got. The
  // push path matches on the ingest pod *before* the write, which is the only way to beat it.
  //
  // The consequence, accepted deliberately: pushed rows are provisional. A row whose write
  // later fails would show here and vanish on the next durable read. And under load the
  // server drops the oldest queued rows rather than buffering without bound — Events has no
  // service gate to bound it up front, so it is bounded here instead, with a visible count.
  private liveStream: LiveStream | null = null;
  @state() private liveDropped = 0;
  private barChart: any = null;
  private lineChart: any = null;
  private initChartsTimer: ReturnType<typeof setTimeout> | null = null;
  private _loadMoreObserver: IntersectionObserver | null = null;
  private _loadNewerObserver: IntersectionObserver | null = null;
  private _visibilityObserver: IntersectionObserver | null = null;
  private updateBatchTimer: NodeJS.Timeout | null = null;
  private pendingUpdates: Set<string> = new Set();
  private handleMouseUp: (() => void) | null = null;
  private sessionPlayerWrapper: HTMLElement | null = null;
  private containerRef = createRef<HTMLDivElement>();
  private nextFetchUrl = '';
  private fetchGeneration = 0;
  private isNewResetTimer: ReturnType<typeof setTimeout> | null = null;

  // Debounced functions
  private debouncedFetchData: any;
  private debouncedUpdateChartMarkArea: ReturnType<typeof debounce>;

  // Bound functions for event listeners
  private boundHandleResize: any;
  private handleFormSubmit = (e: Event) => {
    if ((e.target as HTMLElement)?.id === 'log_explorer_form') {
      e.preventDefault();
      this.debouncedRefetchLogs();
    }
  };
  private handleUpdateQuery = (e: Event) => {
    const source = (e as CustomEvent).detail?.source || 'default';
    if (source === 'expand-timerange') return;
    this.debouncedRefetchLogs();
  };
  private liveBtn: HTMLInputElement | null = null;
  // Named (not anonymous) so they can be removed on disconnect — see setupEventListeners.
  private handleLiveToggle = (e: Event) => {
    if ((e.target as HTMLInputElement).checked) {
      this.isLiveStreaming = true;
      if (!this.liveStream?.isRunning) void this.startLiveStream();
    } else {
      this.stopLiveStream();
    }
    this.requestUpdate();
  };
  // Tear down live-tail polling and reflect it in the toggle; optional toast when
  // stopped automatically (e.g. the live window reached the range's upper bound).
  private stopLiveStream(message?: string) {
    this.liveStream?.stop();
    this.liveStream = null;
    this.isLiveStreaming = false;
    if (this.liveBtn) this.liveBtn.checked = false;
    if (message) this.showErrorToast(message);
    this.requestUpdate();
  }
  private handlePageHide = () => {
    // Drops the lease too, so a closed tab stops matching on the ingest pods rather than
    // waiting out its expiry.
    this.liveStream?.stop();
    this.liveStream = null;
  };
  private isCalculatingWidths: boolean = false;
  private lastVisibilityRange: { first: number; last: number } | null = null;
  private isScrolling = false;
  private scrollEndTimer: ReturnType<typeof setTimeout> | null = null;
  private worker: Worker | null = null;
  private workerReqId = 0;
  private workerCallbacks = new Map<number, { resolve: Function; reject: Function }>();
  // Seam for tests: the fetch+group step. Defaults to the worker-backed path;
  // tests override it to feed canned {tree, meta} responses deterministically.
  transport: (url: string) => Promise<{ tree: any[]; meta: any }> = (url) => this.workerFetch(url);

  constructor() {
    super();
    this.debouncedFetchData = debounce(this.fetchData.bind(this), 300);
    this.debouncedUpdateChartMarkArea = debounce(this.updateChartMarkArea.bind(this), 100);
    // Bind resize handler for immediate feedback
    this.boundHandleResize = this.handleResize.bind(this);
    this.expandTrace = this.expandTrace.bind(this);
  }

  // Worker + listeners are set up on connect (not in the constructor) so they are
  // symmetric with disconnectedCallback teardown and survive disconnect→reconnect.
  private initWorker() {
    if (this.worker) return;
    this.worker = new Worker(LogWorkerUrl, { type: 'module' });
    this.worker.onmessage = (e) => this.handleWorkerMsg(e);
    this.worker.onerror = (e: ErrorEvent) => console.error('[Worker] Error:', e.message, e.filename, e.lineno);
  }

  private handleWorkerMsg(e: MessageEvent) {
    const { type, tree, meta, error, queryError, id } = e.data;
    const cb = this.workerCallbacks.get(id);
    if (!cb) {
      console.warn('[Worker] No callback found for message id:', id);
      return;
    }
    this.workerCallbacks.delete(id);
    if (type !== 'success' && queryError) this.reportQueryError(error);
    type === 'success' ? cb.resolve({ tree, meta }) : cb.reject(new Error(error));
  }

  // A server-reported query error (bad field, unsupported query) belongs under
  // the query box next to the client-side squiggles, not only in the row area.
  // Same `showParseError` channel the server's HX-Trigger uses, so the page has
  // one listener rather than a second global entry point. Returns the message so
  // call sites can throw it in one expression.
  private reportQueryError(msg: string): string {
    document.body.dispatchEvent(new CustomEvent('showParseError', { detail: msg, bubbles: true, composed: true }));
    return msg;
  }

  private async workerFetch(url: string): Promise<{ tree: any[]; meta: any }> {
    // Patterns / sessions: fetch directly, no span grouping needed
    if (this.isAggregate || this.mode === 'sessions') {
      // Consume the <head> preload (server points it at the matching viz endpoint)
      // so the initial aggregate load overlaps shell render instead of starting
      // only now. Subsequent refetches fall through to a direct fetch.
      const early = (window as any).logDataPromise;
      let data: any;
      if (early) {
        (window as any).logDataPromise = null;
        data = await early;
      } else {
        const resp = await fetch(url, { headers: { Accept: 'application/json' }, credentials: 'include' });
        if (!resp.ok) throw new Error(resp.status === 401 ? 'Session expired, please refresh' : `Server error (${resp.status})`);
        data = await resp.json();
      }
      if (data.error) throw new Error(this.reportQueryError(data.error));
      const colIdxMap = data.colIdxMap || {};
      const isSessions = this.mode === 'sessions';
      // The sessions summary is computed in the same scan as the rows and shipped
      // as pre-rendered HTML; inject it into the (skeleton) summary region. The
      // header is script-free (bucket-filter handler lives in the page init), and
      // tippy tooltips are globally delegated, so innerHTML injection is safe.
      if (typeof data.summaryHtml === 'string') {
        const region = document.getElementById('page-summary-region');
        if (region) {
          region.innerHTML = data.summaryHtml;
          region.removeAttribute('aria-busy');
          // Idempotent; rAF retry guards the load-time race where the formatter
          // (defined in the page init script) isn't ready at first injection.
          const applyFmt = () => (window as any).formatSummaryChart?.(region);
          applyFmt();
          requestAnimationFrame(applyFmt);
        }
      }
      const tree = (data.logsData || []).map((row: any[]) => {
        const sessionId = isSessions ? (row[colIdxMap['trace_id']] as string) || '' : '';
        const eventCount = isSessions ? (row[colIdxMap['event_count']] as number) || 0 : 0;
        // Sessions ship their error tally in the `errors` column (errorCount). Wire it
        // into hasErrors so the play button, expand chip, and summary all redden — the
        // left status indicator derives its own red via getErrorClassification.
        const errorCount = isSessions ? Number(row[colIdxMap['errors']]) || 0 : 0;
        return {
          id: sessionId || generateId(),
          data: row,
          depth: 0,
          children: eventCount || (isSessions ? 1 : 0),
          traceId: sessionId,
          parentIds: [],
          show: true,
          expanded: false,
          isLastChild: true,
          siblingsArr: [],
          childErrors: false,
          hasErrors: errorCount > 0,
          isNew: false,
          startNs: 0,
          duration: 0,
          traceStart: 0,
          traceEnd: 0,
          childrenTimeSpans: [],
          type: 'log' as const,
        };
      });
      return {
        tree,
        meta: {
          serviceColors: {},
          nextUrl: '',
          cols: data.cols || [],
          colIdxMap,
          count: data.count || 0,
          totalPatterns: data.totalPatterns ?? 0,
          totalSessions: data.totalSessions ?? 0,
          traces: [],
          hasMore: data.hasMore ?? (data.logsData || []).length > 0,
          queryResultCount: data.queryResultCount ?? 0,
        },
      };
    }

    // Use early fetch promise if available (set by server-rendered script in head)
    const earlyPromise = (window as any).logDataPromise;
    if (earlyPromise) {
      (window as any).logDataPromise = null;
      const data = await earlyPromise;
      // Propagate server errors instead of silently falling through to the worker —
      // otherwise the user waits 2 min for "Worker timeout" masking the real cause.
      if (data.error) throw new Error(this.reportQueryError(data.error));
      const { logsData, serviceColors, nextUrl, recentUrl, cols, colIdxMap, count, traces } = data;
      const tree = logsData?.length ? groupSpans(logsData, colIdxMap, this.expandedTraces, this.flipDirection, traces || []) : [];
      return {
        tree,
        meta: {
          serviceColors,
          nextUrl,
          recentUrl,
          cols,
          colIdxMap,
          count,
          traces: traces || [],
          hasMore: data.hasMore ?? logsData?.length > 0,
          queryResultCount: data.queryResultCount ?? logsData?.length ?? 0,
        },
      };
    }
    // Fallback to worker
    if (!this.worker) throw new Error('Worker not initialized');
    const id = ++this.workerReqId;
    return new Promise((resolve, reject) => {
      this.workerCallbacks.set(id, { resolve, reject });
      this.worker!.postMessage({
        type: 'fetch',
        url,
        colIdxMap: this.colIdxMap,
        expandedTraces: this.expandedTraces,
        flipDirection: this.flipDirection,
        id,
      });
      setTimeout(() => {
        if (this.workerCallbacks.has(id)) {
          this.workerCallbacks.delete(id);
          reject(new Error('Worker timeout'));
        }
      }, 120000);
    });
  }

  updateChartDataZoom(start: number, end: number) {
    // Chart data zoom functionality - currently disabled
  }

  private get isAggregate() {
    return this.mode === 'patterns';
  }

  private setupEventListeners() {
    // Live streaming button — disabled for aggregate views (patterns/sessions).
    // Handler is bound + stored so it can be removed in disconnectedCallback.
    this.liveBtn = document.querySelector('#streamLiveData') as HTMLInputElement | null;
    if (this.liveBtn && !this.isAggregate) this.liveBtn.addEventListener('change', this.handleLiveToggle);

    // Global event listeners
    ['submit', 'add-query'].forEach((ev) => window.addEventListener(ev, this.debouncedRefetchLogs));

    // Form submit listener
    document.addEventListener('submit', this.handleFormSubmit);

    // Filter element update listener
    document.addEventListener('update-query', this.handleUpdateQuery);

    // Window lifecycle events
    window.addEventListener('pagehide', this.handlePageHide);

    // Pointer events for resizing (supports mouse + touch)
    this.handleMouseUp = () => {
      this.resizeTarget = null;
      document.body.style.userSelect = 'auto';
    };
    window.addEventListener('pointerup', this.handleMouseUp);
    window.addEventListener('pointermove', this.boundHandleResize);

    // Chart initialization - use polling instead of 'load' event which
    // never re-fires on HTMX morph navigation.
    let chartRetries = 0;
    const initCharts = () => {
      this.barChart = (window as any).barChart;
      this.lineChart = (window as any).lineChart;
      if (!this.barChart && !this.lineChart) {
        if (++chartRetries < 25) this.initChartsTimer = setTimeout(initCharts, 200);
        return;
      }
      this.initChartsTimer = null;
      if (this.barChart) {
        this.barChart.dispatchAction({
          type: 'takeGlobalCursor',
          key: 'dataZoomSelect',
          dataZoomSelectActive: true,
        });
        this.barChart.on('datazoom', this.handleChartZoom);
      }
      if (this.lineChart) {
        this.lineChart.on('datazoom', this.handleChartZoom);
      }
    };
    initCharts();
  }

  // Log rows/aggregates come from dedicated endpoints, not the page shell:
  // /log_explorer/data (logs), /log_explorer/patterns, /log_explorer/sessions.
  // Derived from projectId (element attribute) so it never depends on the current
  // browser pathname's shape — the path segment is the single source of truth for mode.
  private dataSubPath(): string {
    const seg = this.mode === 'patterns' ? 'patterns' : this.mode === 'sessions' ? 'sessions' : 'data';
    return `/p/${this.projectId}/log_explorer/${seg}`;
  }

  private buildJsonUrl(): string {
    // Preserve all existing query parameters and add json=true
    if (this.initialFetchUrl) {
      const url = new URL(this.initialFetchUrl, window.location.origin);
      url.searchParams.set('json', 'true');
      // Merge time params from page URL so dashboard time picker changes apply
      const pageParams = new URLSearchParams(window.location.search);
      for (const key of ['since', 'from', 'to']) {
        if (pageParams.has(key)) url.searchParams.set(key, pageParams.get(key)!);
      }
      return url.toString();
    } else {
      const p = new URLSearchParams(window.location.search);
      p.set('json', 'true');
      // Mode is encoded by the path segment (dataSubPath), so viz_type is redundant
      // on the fetch URL — drop it. It stays in the browser URL to drive the CSS/tabs.
      p.delete('viz_type');
      return `${window.location.origin}${this.dataSubPath()}?${p.toString()}`;
    }
  }

  /**
   * Open the push stream for the query currently in the box.
   *
   * The subscription carries this table's column list because the server cannot derive it —
   * a query's final columns may be SQL expressions only the database can evaluate — so ingest
   * resolves each name it can against the in-memory record and omits the rest.
   */
  private async startLiveStream() {
    const url = new URL(window.location.href);
    this.liveDropped = 0;
    this.liveStream = new LiveStream({
      projectId: this.projectId,
      leaseSecs: 45,
      body: () => ({
        // No service gate on Events: it streams whatever the query says, bounded by the
        // server's per-connection queue rather than refused up front.
        all_signals: true,
        query: url.searchParams.get('query') || null,
        columns: Object.keys(this.colIdxMap ?? {}),
      }),
      onRows: rows => this.handleLiveRows(rows),
      onDropped: total => {
        this.liveDropped = total;
        this.requestUpdate();
      },
      onState: (state, detail) => {
        if (state === 'expired' || state === 'error') this.stopLiveStream(detail);
      },
    });
    await this.liveStream.start();
  }

  /**
   * Merge pushed rows through the same path a recent fetch uses.
   *
   * Reusing `groupSpans` and `mergeIntoTree` is what keeps trace grouping, the new-row
   * highlight and the scroll-anchoring identical between pushed and fetched rows — a second
   * merge path would drift from this one on the first change to either.
   */
  private handleLiveRows(rows: unknown[]) {
    if (!rows.length || !this.colIdxMap) return;
    const positional = rows
      .map((r: any) => (r?.shape === 'table' ? tableRowToArray(r.cols ?? {}, this.colIdxMap as any) : null))
      .filter((r): r is unknown[] => r !== null);
    if (!positional.length) return;

    // Trace adjacency has to be synthesised: a fetch receives it from the server, but a pushed
    // row arrives alone, and groupSpans keys the tree off adjacency rather than off the rows.
    const traces = traceEntriesFor(positional as any, this.colIdxMap as any);
    const tree = groupSpans(positional as any, this.colIdxMap, this.expandedTraces, this.flipDirection, traces as any);
    if (!tree.length) return;
    tree.forEach(t => (t.isNew = true));
    this.fetchedNew = true;

    // The container decides *where* the row goes, never *whether* it arrives. Returning early
    // when it is missing (before first paint, or while the list is detached) would drop pushed
    // rows on the floor — and unlike a fetch, there is no cursor to re-request them with.
    const container = this.logsContainer;
    const scrollTop = container?.scrollTop ?? 0;
    const scrolledToBottom = container ? scrollTop + container.clientHeight >= container.scrollHeight - 1 : true;
    if (scrolledToBottom) this.shouldScrollToBottom = true;
    // Same rule as a recent fetch: a user who has scrolled away gets a "N new" pill rather
    // than having the viewport yanked out from under them mid-read.
    if (container && shouldBufferRecent(this.isLiveStreaming, scrollTop, scrolledToBottom, this.flipDirection)) {
      this.recentDataToBeAdded = this.addWithFlipDirection(this.recentDataToBeAdded, tree, true);
    } else {
      // Reaching here means the viewport is parked at the edge the rows arrive at (that is
      // what shouldBufferRecent decided), so the row under the user's eye is the one being
      // pushed down on purpose. Anchoring would scroll the previous top row back into view
      // every tick — a visible bounce that also hides the rows just streamed in.
      const anchor = container && !this.atNewRowEdge(scrollTop, scrolledToBottom) ? this.captureScrollAnchor() : null;
      this.spanListTree = this.mergeIntoTree(tree, true);
      this.updateVisibleItems();
      if (anchor) void this.restoreScrollAnchor(anchor);
    }
    this.requestUpdate();
  }

  private buildRecentFetchUrl(): string {
    // Always build from current browser URL to ensure we have latest query params
    const url = new URL(window.location.href);
    url.searchParams.set('json', 'true');

    // Forward cursor = newest retained row + 10ms (skip the inclusive boundary).
    // The server scans ascending for this request, then returns canonical newest-first rows.
    const cursor = this.edgeCursor(newestRowTimestamp, 10);
    if (cursor != null) {
      url.searchParams.set('cursor', cursor);
      url.searchParams.set('direction', 'newer');
      url.searchParams.delete('from');
      url.searchParams.delete('since');
      const to = url.searchParams.get('to');
      const toMs = to ? Date.parse(to) : NaN;
      if (!isNaN(toMs) && Date.parse(cursor) >= toMs) this.stopLiveStream('Reached the end of the selected time range — live tail paused.');
    }

    url.pathname = this.dataSubPath();
    return url.toString();
  }

  // Cursor at an extreme of everything loaded, ± offset (offset skips the
  // inclusive boundary row). Scanned, not positional — the flattened trace tree
  // appends child spans after their (older) trace root, so the array endpoints
  // aren't the true min/max. null when empty. oldest → page earlier/load-more;
  // newest → live-tail lower bound.
  private edgeCursor(by: typeof oldestRowTimestamp, offsetMs: number): string | null {
    const ms = by(this.spanListTree, this.colIdxMap);
    return ms == null ? null : new Date(ms + offsetMs).toISOString();
  }

  private buildLoadMoreUrl(): string {
    // Patterns / sessions: increment aggregate_skip based on loaded count
    if (this.isAggregate || this.mode === 'sessions') {
      // sessions also use aggregate skip for top-level pagination
      const url = new URL(window.location.href);
      url.searchParams.set('json', 'true');
      url.searchParams.set('aggregate_skip', String(this.spanListTree.length));
      url.pathname = this.dataSubPath();
      return url.toString();
    }

    // If we have no data, use base URL
    if (this.spanListTree.length === 0) {
      console.warn('[LoadMore] No data in spanListTree, using buildJsonUrl');
      return this.buildJsonUrl();
    }

    // Cursor from the oldest row, -10ms buffer to skip the inclusive boundary.
    const cursor = this.edgeCursor(oldestRowTimestamp, -10);

    if (!cursor) {
      console.warn('[LoadMore] No timestamp found, falling back to nextFetchUrl or buildJsonUrl', {
        flipDirection: this.flipDirection,
        treeLength: this.spanListTree.length,
        hasNextUrl: !!this.nextFetchUrl,
      });
      return this.nextFetchUrl || this.buildJsonUrl();
    }

    // Start with nextFetchUrl if available, otherwise current URL
    const baseUrl = this.nextFetchUrl || window.location.href;
    const url = new URL(baseUrl, window.location.origin + window.location.pathname);
    url.searchParams.set('json', 'true');
    url.searchParams.set('cursor', cursor); // preserves from/to/since filters already on the base URL
    url.searchParams.set('direction', 'older');

    url.pathname = this.dataSubPath();
    return url.toString();
  }

  private expandTimeRangeUrl(): string {
    const baseUrl = this.nextFetchUrl ? new URL(this.nextFetchUrl, window.location.origin) : new URL(window.location.href);

    const url = new URL(baseUrl);
    const since = url.searchParams.get('since');
    const to = url.searchParams.get('to');

    let target = '1H';

    if (since) {
      target = expandSince(since);
      url.searchParams.set('since', target);
      // Sync the picker dropdown label to the widened range; expandTimeRangeUrl
      // owns the URL itself, so skipSetParams (the to-branch already does this).
      window.updateTimePicker({ since: target }, { skipSetParams: true });
    } else if (to) {
      const newFrom = expandFromToRange(url.searchParams.get('from'), to);
      url.searchParams.set('from', newFrom);
      target = window.updateTimePicker({ from: newFrom, to }, { skipSetParams: true });
    } else {
      target = '3H';
      url.searchParams.set('since', target);
    }

    // Cursor from the oldest loaded row so we fetch strictly older logs on expand.
    const cursor = this.edgeCursor(oldestRowTimestamp, 0);
    if (cursor) url.searchParams.set('cursor', cursor);

    // Ensure json=true and layout=loadmore for the API request
    url.searchParams.set('json', 'true');
    url.searchParams.set('layout', 'loadmore');

    // Save URL with cursor, json, and layout for the fetch (data endpoint;
    // the browser URL below keeps the plain /log_explorer pathname).
    this.nextFetchUrl = this.dataSubPath() + url.search;

    // Remove cursor, json, and layout from browser URL (cleaner for user)
    url.searchParams.delete('cursor');
    url.searchParams.delete('json');
    url.searchParams.delete('layout');
    const newUrl = url.pathname + url.search;
    this.updateUrlStateAndQuery(newUrl, url.searchParams.get('queryAST') || '', target, 'expand-timerange');
    return this.nextFetchUrl;
  }

  async fetchInitialData() {
    const vizType = new URLSearchParams(window.location.search).get('viz_type');
    this.mode = vizType === 'patterns' ? 'patterns' : vizType === 'sessions' ? 'sessions' : 'logs';
    this.fetchData(this.buildJsonUrl(), false);
  }

  async refetchLogs() {
    this.fetchData(this.buildJsonUrl(), true);
  }

  debouncedRefetchLogs = debounce(async () => {
    this.refetchLogs();
  }, 50);

  // Is `col` currently shown? logsColumns mirrors the server-rendered set.
  isColumnOnTable = (col: string) => this.logsColumns.includes(col);

  // Edits the `cols` URL param, which the server reads as a *delta* over its default set: a bare
  // token `foo` adds a column, `-foo` hides a default. Each edit is the exact inverse of the prior
  // one (re-adding drops the `-`, re-removing drops the bare token), so the param stays minimal,
  // reversible, and safe to share — no transient client state can collapse the table.
  syncColsUrlParam = (removed: string[], added: string[]) => {
    const p = new URLSearchParams(window.location.search);
    const toks = new Set((p.get('cols') || '').split(',').filter(Boolean));
    for (const col of removed) {
      if (toks.has(col))
        toks.delete(col); // drop an explicit add
      else toks.add(`-${col}`); // hide a default
    }
    for (const col of added) {
      if (toks.has(`-${col}`))
        toks.delete(`-${col}`); // un-hide a default
      else toks.add(col); // add a column
    }
    if (toks.size) p.set('cols', [...toks].join(','));
    else p.delete('cols');
    const qs = p.toString();
    window.history.replaceState({}, '', `${window.location.pathname}${qs ? '?' + qs : ''}${window.location.hash}`);
  };

  // Toggle a single column's visibility. Returns the column's new visibility.
  toggleColumnOnTable = (col: string): boolean => {
    const has = this.isColumnOnTable(col);
    this.syncColsUrlParam(has ? [col] : [], has ? [] : [col]);
    this.fetchData(this.buildJsonUrl(), true);
    return !has;
  };

  handleChartZoom = (params: { batch?: { startValue: string; endValue: string }[] }) => {
    const range = parseChartZoom(params.batch);
    if (!range) return;

    const label = window.updateTimePicker({ from: range.from, to: range.to }, { skipSetParams: true });

    const p = new URLSearchParams(window.location.search);
    p.set('from', range.from);
    p.set('to', range.to);
    p.delete('since');
    const newUrl = `${window.location.pathname}?${p.toString()}${window.location.hash}`;
    this.updateUrlStateAndQuery(newUrl, p.get('queryAST') || '', label, 'chart-zoom');

    this.debouncedRefetchLogs();
  };

  private updateUrlStateAndQuery(newUrl: string, q: string, timeRange: string, source: string = 'default') {
    window.history.replaceState({}, '', newUrl);

    this.dispatchEvent(
      new CustomEvent('update-query', {
        bubbles: true,
        detail: {
          ast: q,
          source: source,
          timeRange: timeRange,
        },
      })
    );
  }

  // updateTableData method is no longer needed as we fetch data directly

  toggleWrapLines = () => {
    this.wrapLines = !this.wrapLines;
    this.requestUpdate();
  };

  changeView = (view: 'tree' | 'list') => {
    this.view = view;
    this.updateVisibleItems();
    this.requestUpdate();
  };

  connectedCallback() {
    super.connectedCallback();
    this.initWorker();
    this.setupEventListeners();
    // Initialize empty state
    this.logsColumns = [];
    this.colIdxMap = {};
    this.serviceColors = {};
    this.spanListTree = [];
    this.seenIds.clear();
    this.visibleItems = [];
    this.hasMore = false;

    // Initialize fixed column widths
    this.initializeFixedColumnWidths();

    // Project ID is now passed as a property from the server

    // Fetch initial data from the JSON endpoint. Embedded lists (issue-page tabs,
    // dashboards) mount inside panels that are display:none until their tab is opened, so
    // fetching on connect spends a full query on a panel the user may never look at — on the
    // issue page that was the single biggest cost of the page load. Defer those to first
    // visibility; the log explorer itself (no initialFetchUrl) stays eager.
    // IntersectionObserver fires on the next frame for already-visible elements, so the
    // visible case costs a frame rather than a branch that has to guess at layout.
    if (this.initialFetchUrl) {
      this._visibilityObserver = new IntersectionObserver((entries) => {
        if (!entries.some((e) => e.isIntersecting)) return;
        this._visibilityObserver?.disconnect();
        this._visibilityObserver = null;
        this.fetchInitialData();
      });
      this._visibilityObserver.observe(this);
    } else {
      this.fetchInitialData();
    }
  }

  private initializeFixedColumnWidths() {
    // Set fixed widths for all columns to avoid dynamic calculations during scroll
    this.fixedColumnWidths = {
      id: 24,
      pattern_count: 115,
      volume: 165,
      level: 70,
      timestamp: 155,
      created_at: 155,
      status_code: 102,
      method: 102,
      raw_url: 212,
      url_path: 212,
      service: 136,
      summary: 3600,
      latency_breakdown: 120,
    };
  }

  private rowCountEls: (HTMLElement | null)[] | null = null;
  private getRowCountEls() {
    return (this.rowCountEls ??= ['row-count-display', 'row-count-suffix', 'row-count-display-mobile', 'row-count-suffix-mobile'].map(
      (id) => document.getElementById(id)
    ));
  }

  private updateRowCountDisplay() {
    const [countEl, suffixEl, mobileCount, mobileSuffix] = this.getRowCountEls();
    if (!countEl) return;
    let countText: string, suffixText: string;
    if (this.mode === 'patterns') {
      countText = `${formatLargeCount(this.totalPatterns)} patterns`;
      suffixText = ` found (based on ${formatLargeCount(this.totalCount)} logs)`;
    } else if (this.mode === 'sessions') {
      countText = `${formatLargeCount(this.totalSessions)} sessions`;
      suffixText = this.totalCount ? ` (${formatLargeCount(this.totalCount)} events)` : '';
    } else {
      countText = formatLargeCount(this.loadedCount);
      suffixText = this.loadedCount < this.totalCount ? ` of ${formatLargeCount(this.totalCount)} rows` : ' rows';
    }
    countEl.textContent = countText;
    if (suffixEl) suffixEl.textContent = suffixText;
    if (mobileCount) mobileCount.textContent = countText;
    if (mobileSuffix) mobileSuffix.textContent = suffixText;
  }

  private showLoadingSpinner(show: boolean) {
    // Find or create the spinner element next to row count
    const countElement = document.getElementById('row-count-display');
    if (!countElement) return;

    const spinnerId = 'log-list-loading-spinner';
    let spinner = document.getElementById(spinnerId);

    if (show && !spinner) {
      // Create spinner if it doesn't exist
      spinner = document.createElement('span');
      spinner.id = spinnerId;
      spinner.className = 'ml-2 inline-block';
      spinner.innerHTML = `<svg class="inline-block icon w-4 h-4 animate-spin text-textBrand"><use href="${spriteUrl('regular')}#spinner"></use></svg>`;
      countElement.parentElement?.appendChild(spinner);
    } else if (!show && spinner) {
      // Remove spinner
      spinner.remove();
    }
  }

  firstUpdated() {
    // Initialization handled by lit-virtualizer
  }

  // Runs BEFORE render (unlike updated), so clearing here means the first frame
  // after a viz-mode switch already shows the loading skeleton — not the previous
  // mode's rows. Switching logs↔patterns↔sessions replaces the result set with a
  // differently-shaped one via a fetch that can take seconds; the rendered list
  // reads virtualListItems, so that (and the count display) must be wiped too, or
  // the stale rows keep painting until the new data lands. Guard on a defined old
  // value so the initial undefined→mode set doesn't wipe server-seeded rows.
  willUpdate(changedProperties: Map<string, any>) {
    if (changedProperties.has('mode') && changedProperties.get('mode') !== undefined) {
      this.spanListTree = [];
      this.seenIds.clear();
      this.virtualListItems = [];
      this.visibleItems = [];
      this.loadedCount = 0;
      this.totalCount = 0;
      this.totalPatterns = 0;
      this.totalSessions = 0;
      this.updateRowCountDisplay();
    }
  }

  updated(changedProperties: Map<string, any>) {
    // Stop live streaming when switching to an aggregate view
    if (changedProperties.has('mode') && this.isAggregate && this.liveStream) {
      this.liveStream.stop();
      this.liveStream = null; // else handleLiveToggle's isRunning guard skips restart on switch-back
      this.isLiveStreaming = false;
    }

    if (this.shouldScrollToBottom && this.flipDirection) {
      requestAnimationFrame(() => this.scrollToBottom());
    }

    // Reset isNew flag after animation. Keyed on new rows actually being PRESENT in
    // spanListTree (not on fetchedNew), so rows that arrive via the buffer→"N new"
    // concatenation path still get cleared — fetchedNew may already be false by then.
    if (changedProperties.has('spanListTree') && this.spanListTree.some((s) => s.isNew)) {
      if (this.isNewResetTimer) clearTimeout(this.isNewResetTimer);
      this.isNewResetTimer = setTimeout(() => {
        this.spanListTree.forEach((span) => {
          span.isNew = false;
        });
        this.fetchedNew = false;
        this.isNewResetTimer = null;
        this.requestUpdate();
      }, 4000); // Match the animation duration
    }
  }

  scrollToBottom() {
    // Use ref instead of DOM query
    if (this.logsContainer) {
      // Batch all DOM operations in a single animation frame
      requestAnimationFrame(() => {
        if (this.logsContainer) {
          // Direct assignment without reading first - browser handles this efficiently
          this.logsContainer.scrollTop = this.logsContainer.scrollHeight;
        }
      });
    }
  }

  disconnectedCallback() {
    if (this.worker) {
      this.worker.terminate();
      this.worker = null;
    }
    // Drop (don't reject) in-flight worker callbacks: the worker is gone, and a
    // late reject would touch a torn-down component. The pending 120s timeouts
    // see an empty map and no-op.
    this.workerCallbacks.clear();

    // Clean up all observers and timers
    if (this._loadMoreObserver) {
      this._loadMoreObserver.disconnect();
      this._loadMoreObserver = null;
    }
    if (this._loadNewerObserver) {
      this._loadNewerObserver.disconnect();
      this._loadNewerObserver = null;
    }
    if (this._visibilityObserver) {
      this._visibilityObserver.disconnect();
      this._visibilityObserver = null;
    }
    if (this.updateBatchTimer) {
      clearTimeout(this.updateBatchTimer);
      this.updateBatchTimer = null;
    }
    if (this.liveStream) {
      this.liveStream.stop();
      this.liveStream = null;
    }
    if (this.scrollEndTimer) {
      clearTimeout(this.scrollEndTimer);
      this.scrollEndTimer = null;
    }
    if (this.initChartsTimer) {
      clearTimeout(this.initChartsTimer);
      this.initChartsTimer = null;
    }

    // Clean up event listeners
    window.removeEventListener('pointermove', this.boundHandleResize);
    if (this.handleMouseUp) {
      window.removeEventListener('pointerup', this.handleMouseUp);
    }
    ['submit', 'add-query'].forEach((ev) => window.removeEventListener(ev, this.debouncedRefetchLogs));
    document.removeEventListener('submit', this.handleFormSubmit);
    document.removeEventListener('update-query', this.handleUpdateQuery);
    this.liveBtn?.removeEventListener('change', this.handleLiveToggle);
    this.liveBtn = null;
    window.removeEventListener('pagehide', this.handlePageHide);

    // Clean up chart event handlers
    if (this.barChart) {
      this.barChart.off('datazoom', this.handleChartZoom);
    }
    if (this.lineChart) {
      this.lineChart.off('datazoom', this.handleChartZoom);
    }

    // Note: Caches in renderSummaryElements closure will be garbage collected
    // when the component is destroyed

    super.disconnectedCallback();
  }

  private handleResize(event: MouseEvent) {
    if (this.resizeTarget === null) return;
    const diff = event.clientX - this.mouseState.x;
    // Seed from the column's actual current width (custom → fixed → min), not a
    // hardcoded 16 that snapped fixed-width columns to ~100px on first drag.
    const start = this.columnMaxWidthMap[this.resizeTarget] ?? this.fixedColumnWidths[this.resizeTarget] ?? MIN_COLUMN_WIDTH;
    this.columnMaxWidthMap[this.resizeTarget] = Math.max(MIN_COLUMN_WIDTH, start + diff);
    this.requestUpdate();
    this.mouseState = { x: event.clientX };
  }

  private batchRequestUpdate(source: string) {
    this.pendingUpdates.add(source);
    if (this.updateBatchTimer) {
      clearTimeout(this.updateBatchTimer);
    }
    // Use requestAnimationFrame for better performance
    this.updateBatchTimer = setTimeout(() => {
      this.updateBatchTimer = null;
      requestAnimationFrame(() => {
        this.pendingUpdates.clear();
        this.requestUpdate();
      });
    }, 16); // ~60fps
  }

  buildSpanListTree(logs: any[][]) {
    return groupSpans(logs, this.colIdxMap, this.expandedTraces, this.flipDirection, this.cachedServerTraces);
  }

  // Rows of a tree that the list actually renders: in tree view a collapsed trace contributes
  // its root only. The buffer holds whole traces, so counting it raw promised a "72 new" that
  // inserted a fraction of that many rows.
  private renderableRows(tree: EventLine[]): EventLine[] {
    return !this.isAggregate && (this.view === 'tree' || this.mode === 'sessions') ? tree.filter((e) => e.show) : tree;
  }

  // Count behind the "N new" pill — what clicking it will actually put on screen.
  private get recentCount(): number {
    return this.renderableRows(this.recentDataToBeAdded).length;
  }

  private updateVisibleItems() {
    const items = this.renderableRows(this.spanListTree);
    this.visibleItems = items;

    // Build virtual list with special items
    const virtualItems: VirtualListItem[] = [];

    if (this.isAggregate) {
      // Splice inline expanded children after each expanded aggregate row.
      for (const row of items) {
        virtualItems.push(row);
        const key = this.aggregateRowKey(row);
        if (key && this.expandedAggregates[key]) {
          virtualItems.push({ type: 'aggregateChildren', parentKey: key });
        }
      }
      if (this.hasMore || items.length > 0) virtualItems.push({ type: 'loadMore' });
    } else {
      const isEmbedded = !!this.initialFetchUrl;
      // Add fetch recent button at the start (for non-flipped) or end (for flipped)
      if (!isEmbedded && !this.flipDirection && items.length > 0) {
        virtualItems.push({ type: 'fetchRecent' });
      }

      // Add all data items
      virtualItems.push(...items);

      // Add load more button at the end (for non-flipped) or start (for flipped)
      if (!this.flipDirection && (this.hasMore || items.length > 0)) {
        virtualItems.push({ type: 'loadMore' });
      } else if (this.flipDirection) {
        // For flipped direction, add buttons in reverse order
        if (!isEmbedded && items.length > 0) {
          virtualItems.push({ type: 'fetchRecent' });
        }
        if (this.hasMore || items.length > 0) {
          virtualItems.unshift({ type: 'loadMore' });
        }
      }
    }

    this.virtualListItems = virtualItems;

    // Trigger initial chart mark area update after virtual items are set
    if (items.length > 0 && !this.lastVisibilityRange) {
      // Set initial visibility range to show first items
      const startIdx = this.flipDirection ? Math.max(0, virtualItems.length - 20) : 0;
      const endIdx = this.flipDirection ? virtualItems.length - 1 : Math.min(19, virtualItems.length - 1);
      this.lastVisibilityRange = { first: startIdx, last: endIdx };
      // Defer chart update to allow chart to be ready
      setTimeout(() => this.debouncedUpdateChartMarkArea(), 500);
    }
  }

  // Derive a stable key for an aggregate row (patterns).
  private aggregateRowKey(rowData: EventLine): string | null {
    if (this.mode === 'patterns') {
      // Patterns rows expose either a stable pattern hash or the template summary.
      const hash = lookupVecValue<string>(rowData.data, this.colIdxMap, 'pattern_hash');
      if (hash) return hash;
      const summary = rowData.data?.[this.colIdxMap['summary']];
      if (Array.isArray(summary)) return summary.join('\x1e');
      return typeof summary === 'string' ? summary : null;
    }
    return null;
  }

  // Toggle inline expansion of a patterns/sessions row and fetch child events on first open.
  toggleAggregateRow = async (rowData: EventLine) => {
    const key = this.aggregateRowKey(rowData);
    if (!key) {
      this.showErrorToast('Unable to expand: missing session identifier');
      return;
    }
    if (this.expandedAggregates[key]) {
      const { [key]: _dropped, ...rest } = this.expandedAggregates;
      this.expandedAggregates = rest;
      rowData.expanded = false;
      this.updateVisibleItems();
      this.requestUpdate();
      return;
    }
    // First open — seed loading entry and fetch.
    this.expandedAggregates = {
      ...this.expandedAggregates,
      [key]: { rows: [], cols: [], colIdxMap: {}, hasMore: false, loading: true, skip: 0 },
    };
    rowData.expanded = true;
    this.updateVisibleItems();
    this.requestUpdate();
    await this.fetchAggregateChildren(key, 0);
  };

  private buildExpandUrl(key: string, skip: number): string {
    const pageParams = new URLSearchParams(window.location.search);
    const url = new URL(window.location.origin + window.location.pathname.split('/log_explorer')[0] + '/log_explorer/expand');
    url.searchParams.set('kind', this.mode === 'sessions' ? 'session' : 'pattern');
    url.searchParams.set('key', key);
    url.searchParams.set('skip', String(skip));
    for (const p of ['query', 'since', 'from', 'to']) {
      const v = pageParams.get(p);
      if (v) url.searchParams.set(p, v);
    }
    return url.toString();
  }

  private async fetchAggregateChildren(key: string, skip: number) {
    // Paging (skip > 0) reuses this path; toggleAggregateRow only seeds loading on
    // first open, so without this the "Load more" button gave no feedback at all.
    const pending = this.expandedAggregates[key];
    if (pending && !pending.loading) {
      this.expandedAggregates = { ...this.expandedAggregates, [key]: { ...pending, loading: true } };
      this.requestUpdate();
    }
    try {
      const resp = await fetch(this.buildExpandUrl(key, skip), { headers: { Accept: 'application/json' }, credentials: 'include' });
      if (!resp.ok) throw new Error(resp.status === 401 ? 'Session expired, please refresh' : `Server error (${resp.status})`);
      const data = await resp.json();
      const cols: string[] = data.cols || [];
      const childIdxMap: ColIdxMap = data.colIdxMap || {};
      if (!Object.keys(childIdxMap).length) cols.forEach((c, i) => (childIdxMap[c] = i));
      const existing = this.expandedAggregates[key];
      if (!existing) return;
      const newRows: any[][] = data.rows || [];
      const mergedRows = skip === 0 ? newRows : [...existing.rows, ...newRows];
      // Build trace tree from rows + server traces for full tree rendering
      const traces = data.traces || [];
      const eventLines = mergedRows.length ? dedupeById(groupSpans(mergedRows, childIdxMap, {}, false, traces)) : [];
      eventLines.forEach((el) => {
        el.show = true;
        el.expanded = true;
      });
      // skip is a CUMULATIVE offset for the next page. queryResultCount is this
      // page's count, so advance by it — storing it directly pinned skip to the
      // page size and refetched (and duplicated) the same window every load-more.
      const nextSkip = skip + (data.queryResultCount ?? newRows.length);
      this.expandedAggregates = {
        ...this.expandedAggregates,
        [key]: {
          rows: mergedRows,
          cols,
          colIdxMap: childIdxMap,
          hasMore: !!data.hasMore && nextSkip < 500,
          loading: false,
          skip: nextSkip,
          eventLines,
        },
      };
      this.requestUpdate();
    } catch (e) {
      console.error('fetchAggregateChildren failed:', e);
      const existing = this.expandedAggregates[key];
      if (existing) {
        this.expandedAggregates = { ...this.expandedAggregates, [key]: { ...existing, loading: false } };
        this.requestUpdate();
      }
      this.showErrorToast((e as Error).message || 'Failed to load events');
    }
  }

  private renderAggregateChildren(parentKey: string) {
    const state = this.expandedAggregates[parentKey];
    if (!state) return nothing;
    const eventLines = state.eventLines || [];

    // Use non-reactive overrides so logItemRow renders with child columns without triggering Lit re-renders
    this._renderOverrides = { colIdxMap: state.colIdxMap, logsColumns: state.cols, mode: 'logs' };
    const rows = eventLines.filter((el) => el.show).map((el) => this.logItemRow(el));
    this._renderOverrides = null;

    return html`<tr class="item-row flex w-full">
      <td class="w-full bg-fillWeaker/40 border-l-2 border-strokeBrand-weak py-1">
        ${state.loading && eventLines.length === 0
          ? html`<div class="text-xs text-textWeak px-2 py-1">Loading events…</div>`
          : eventLines.length === 0
            ? html`<div class="text-xs text-textWeak px-2 py-1">No events.</div>`
            : html`<div class="flex flex-col">${rows}</div>`}
        ${state.hasMore
          ? html`<button
              class="mt-1 text-xs px-2 py-1 relative"
              @click=${(e: any) => {
                e.stopPropagation();
                this.fetchAggregateChildren(parentKey, state.skip);
              }}
              ?disabled=${state.loading}
              aria-busy=${state.loading}
            >
              <span class=${clsx('text-textBrand underline', state.loading && 'invisible')}>Load more</span>
              <span
                class=${clsx('absolute left-2 top-1.5 loading loading-dots loading-sm', !state.loading && 'invisible')}
                aria-label="Loading"
              ></span>
            </button>`
          : nothing}
      </td>
    </tr>`;
  }

  expandTrace = (tracId: string, spanId: string) => {
    this.shouldScrollToBottom = false;
    // Sessions: fetch/toggle children on demand
    if (this.mode === 'sessions') {
      this.expandSessionTrace(spanId);
      return;
    }
    const expanded = !this.expandedTraces[spanId];
    const nextExpanded: Record<string, boolean> = { ...this.expandedTraces, [spanId]: expanded };
    const toggleSpans = (spans: EventLine[]) => {
      for (const span of spans) {
        if (span.traceId !== tracId) continue;
        if (span.id === spanId) {
          span.expanded = expanded;
          span.show = true;
        } else if (span.parentIds?.includes(spanId)) {
          span.expanded = expanded;
          span.show = expanded;
          nextExpanded[span.id] = expanded;
        }
      }
    };
    toggleSpans(this.spanListTree);
    // Also toggle inside any expanded aggregate-children groups so collapse
    // affects spans rendered there (they live on expandedAggregates[*].eventLines,
    // not on spanListTree).
    for (const key of Object.keys(this.expandedAggregates)) {
      const group = this.expandedAggregates[key];
      if (group?.eventLines?.length) toggleSpans(group.eventLines);
    }
    this.expandedTraces = nextExpanded;
    this.updateVisibleItems();
    this.requestUpdate();
  };

  private async expandSessionTrace(sessionId: string) {
    const parentIdx = this.spanListTree.findIndex((e) => e.id === sessionId);
    if (parentIdx < 0) {
      console.warn('expandSessionTrace: session not found in tree', sessionId);
      return;
    }
    const parent = this.spanListTree[parentIdx];
    const wasExpanded = this.expandedTraces[sessionId];
    const nextExpanded: Record<string, boolean> = { ...this.expandedTraces, [sessionId]: !wasExpanded };
    parent.expanded = !wasExpanded;

    if (wasExpanded) {
      // Collapse: hide children
      for (const span of this.spanListTree) {
        if (span.parentIds?.includes(sessionId)) {
          span.show = false;
          span.expanded = false;
          nextExpanded[span.id] = false;
        }
      }
      this.expandedTraces = nextExpanded;
      this.updateVisibleItems();
      this.requestUpdate();
      return;
    }
    this.expandedTraces = nextExpanded;

    // Check if children already loaded
    const hasChildren = this.spanListTree.some((e) => e.parentIds?.includes(sessionId));
    if (hasChildren) {
      for (const span of this.spanListTree) {
        if (span.parentIds?.includes(sessionId) && span.parentIds.length === 1) {
          span.show = true;
        }
      }
      this.updateVisibleItems();
      this.requestUpdate();
      return;
    }

    // Fetch children
    this.loadingSessions = { ...this.loadingSessions, [sessionId]: true };
    this.requestUpdate();
    try {
      const url = this.buildExpandUrl(sessionId, 0);
      const resp = await fetch(url, { headers: { Accept: 'application/json' }, credentials: 'include' });
      if (!resp.ok) throw new Error(`Server error (${resp.status})`);
      const data = await resp.json();
      const childIdxMap: ColIdxMap = data.colIdxMap || {};
      const rows = data.rows || [];
      const traces = data.traces || [];
      const eventLines = rows.length ? groupSpans(rows, childIdxMap, {}, false, traces) : [];
      // Set parent relationship and depth relative to session parent
      for (const el of eventLines) {
        el.parentIds = [sessionId, ...el.parentIds];
        el.depth = el.depth + 1;
        el.show = true;
        el.expanded = false;
        el.traceId = el.traceId || sessionId;
      }
      // Insert children after parent and update children count to actual loaded count
      this.spanListTree.splice(parentIdx + 1, 0, ...eventLines);
      eventLines.forEach((el) => this.seenIds.add(el.id));
      parent.children = eventLines.filter((el) => el.depth === 1).length;
      this.updateVisibleItems();
      this.requestUpdate();
    } catch (e) {
      console.error('Failed to load session children:', e);
      // Roll back optimistic expansion so the next click retries the fetch
      // instead of going down the (no-op) collapse branch.
      parent.expanded = false;
      const { [sessionId]: _dropped, ...rest } = this.expandedTraces;
      this.expandedTraces = rest;
      this.updateVisibleItems();
      this.requestUpdate();
      this.showErrorToast((e as Error).message || 'Failed to load events');
    } finally {
      const { [sessionId]: _done, ...rest } = this.loadingSessions;
      this.loadingSessions = rest;
      this.requestUpdate();
    }
  }

  fetchData = async (url: string, isRefresh = false, isRecentFetch = false, isLoadMore = false, revealRecent = false) => {
    if (isRecentFetch && this.isFetchingRecent) return;
    if (isLoadMore && this.isLoadingMore) return;

    const loadMoreAnchor = isLoadMore ? this.captureScrollAnchor() : null;
    if (isRecentFetch) this.isFetchingRecent = true;
    else if (isLoadMore) this.isLoadingMore = true;
    else this.isLoading = true;

    // A refresh or initial load replaces the whole result set; load-more/recent
    // append to it. Several decisions below hinge on that distinction.
    const isFullFetch = !isLoadMore && !isRecentFetch;

    // A full fetch bumps the generation; a load-more/recent captures it and bails
    // on resolve if a refresh has since replaced the data — otherwise its rows
    // (from the OLD query) get merged into the NEW query's results.
    if (isFullFetch) this.fetchGeneration++;
    const gen = this.fetchGeneration;

    this.showLoadingSpinner(true);

    try {
      const { tree, meta } = await this.transport(url);
      // Query-editor and time-picker initialization can issue a newer full fetch
      // while the head-preloaded request is still running. Full fetches therefore
      // use latest-request-wins too: never let an obsolete empty response replace
      // rows for the URL the charts have already adopted.
      if (gen !== this.fetchGeneration) return;
      this.fetchError = null;

      // Handle results
      if (tree.length === 0) {
        // An empty pagination page exhausts that edge. Initial/refresh fetches
        // keep trusting meta; a quiet live-tail tick simply stays at the newest edge.
        if (isLoadMore) this.hasMore = false;
        else if (isRecentFetch) this.hasNewer = false;
        else this.hasMore = meta.hasMore || false;
        // A quiet live-tail tick (no new rows) isn't "history exhausted" — don't flash
        // the "Show earlier events" button on every empty 5s recent fetch.
        if (!isRecentFetch) this.expandTimeRange = !this.hasMore;
        // A full fetch (new query, filter or time-range change) that returns
        // nothing is the FULL result set — clear stale rows so the empty state
        // shows, instead of leaving the previous query's results on screen.
        if (isFullFetch) {
          this.spanListTree = [];
          this.seenIds.clear();
          this.loadedCount = 0;
          this.hasNewer = false;
          if (meta.count !== undefined) this.totalCount = meta.count;
          this.updateVisibleItems();
          this.updateRowCountDisplay();
        }
        return;
      }

      this.hasMore = meta.hasMore !== false;
      if (!this.hasMore) this.expandTimeRange = true;
      if (isLoadMore || isRefresh || !this.spanListTree.length) this.nextFetchUrl = meta.nextUrl;
      if (isRecentFetch || !this.spanListTree.length) this.recentFetchUrl = meta.recentUrl;
      if (meta.count !== undefined && !isLoadMore) this.totalCount = meta.count;
      if (meta.totalPatterns !== undefined && !isLoadMore) this.totalPatterns = meta.totalPatterns;
      if (meta.totalSessions !== undefined && !isLoadMore) this.totalSessions = meta.totalSessions;
      if (meta.serviceColors) Object.assign(this.serviceColors, meta.serviceColors);
      // Only a new query / refresh redefines the column set. Load-more pages and
      // 5s live-stream ticks return the same server cols, so adopting them here
      // would silently undo a user's hideColumn / reorder on every tick.
      if (isFullFetch) this.logsColumns = meta.cols;
      this.colIdxMap = meta.colIdxMap;
      // Cache one adjacency entry per trace for expand/collapse and direction flips.
      // Inclusive cursor pages repeat the boundary trace; keeping those duplicates
      // made every rebuild emit duplicate rows and retain duplicate metadata.
      if (meta.traces) {
        const traces = new Map<string, ServerTraceEntry>();
        const source = isLoadMore || isRecentFetch ? [...this.cachedServerTraces, ...meta.traces] : meta.traces;
        source.forEach((trace: ServerTraceEntry) => {
          traces.delete(trace.trace_id);
          traces.set(trace.trace_id, trace);
        });
        this.cachedServerTraces = [...traces.values()];
      }

      if (isRefresh) {
        // New query/filter/time-range: drop inline-expanded aggregate children so
        // they don't render stale rows from the previous query under a surviving key.
        this.expandedAggregates = {};
        this.hasNewer = false;
        this.spanListTree = dedupeById(tree);
        this.seenIds = new Set(this.spanListTree.map((r) => r.id));
        this.updateVisibleItems();
        if (tree.length > 0) {
          requestAnimationFrame(() => {
            const container = this.logsContainer || document.querySelector('#logs_list_container_inner');
            if (container) container.scrollTop = this.flipDirection ? container.scrollHeight : 0;
          });
        }
      } else if (isRecentFetch) {
        this.fetchedNew = true;
        tree.forEach((t) => (t.isNew = true));
        const container = this.logsContainer;
        if (container) {
          const scrollTop = container.scrollTop;
          const clientHeight = container.clientHeight;
          const scrollHeight = container.scrollHeight;
          const scrolledToBottom = scrollTop + clientHeight >= scrollHeight - 1;
          if (scrolledToBottom) this.shouldScrollToBottom = true;
          if (shouldBufferRecent(this.isLiveStreaming, scrollTop, scrolledToBottom, this.flipDirection)) {
            this.recentDataToBeAdded = this.addWithFlipDirection(this.recentDataToBeAdded, tree, isRecentFetch);
          } else {
            const anchor = revealRecent || this.atNewRowEdge(scrollTop, scrolledToBottom) ? null : this.captureScrollAnchor();
            this.spanListTree = this.mergeIntoTree(tree, isRecentFetch);
            this.updateVisibleItems();
            if (anchor) void this.restoreScrollAnchor(anchor);
            else if (revealRecent) requestAnimationFrame(() => (container.scrollTop = this.flipDirection ? container.scrollHeight : 0));
          }
        }
      } else {
        const anchor = this.captureScrollAnchor() ?? loadMoreAnchor;
        this.spanListTree = this.mergeIntoTree(tree, isRecentFetch);
        this.updateVisibleItems();
        if (anchor) void this.restoreScrollAnchor(anchor);
      }
      // Count what's actually visible. queryResultCount over-counts because the
      // dedup-dropped boundary row is re-reported on every paginated page.
      this.loadedCount = this.spanListTree.length;
      this.updateRowCountDisplay();

      // Defer column width calculation
      if ('requestIdleCallback' in window) {
        (window as any).requestIdleCallback(
          () => {
            this.updateColumnMaxWidthMap(tree.map((t) => t.data).filter(Boolean));
          },
          { timeout: 2000 }
        );
      } else {
        setTimeout(() => this.updateColumnMaxWidthMap(tree.map((t) => t.data).filter(Boolean)), 100);
      }
    } catch (error) {
      // A newer full fetch owns the UI now. Do not surface an error from the
      // obsolete request or replace the newer request's loading state.
      if (gen !== this.fetchGeneration) return;
      console.error(error);
      const msg = error instanceof Error ? error.message : 'Network error';
      // Show inline error when initial load fails (no data yet), toast otherwise
      if (this.spanListTree.length === 0) {
        this.fetchError = msg;
      } else {
        this.showErrorToast(msg);
      }
    } finally {
      // Reset only THIS fetch's guard — the three kinds run concurrently, so
      // clearing all of them would let an in-flight load-more re-fire when an
      // unrelated recent/refresh finishes first.
      if (isRecentFetch) this.isFetchingRecent = false;
      else if (isLoadMore) this.isLoadingMore = false;
      else if (gen === this.fetchGeneration) this.isLoading = false;
      if (!isFullFetch || gen === this.fetchGeneration) this.showLoadingSpinner(false);
      this.requestUpdate();
    }
  };

  private showErrorToast(message: string) {
    document.body.dispatchEvent(
      new CustomEvent('errorToast', {
        detail: { value: [message] },
        bubbles: true,
        composed: true,
      })
    );
  }

  hideColumn(column: string) {
    this.syncColsUrlParam([column], []);
    this.logsColumns = this.logsColumns.filter((col) => col !== column);
    delete this.columnMaxWidthMap[column]; // don't leak a stale width for a removed column
    this.requestUpdate();
  }
  setLatencyDim(dim: LatencyDim) {
    this.latencyDim = dim;
    latencyDimPref.write(dim);
    // Every cached bar is keyed on the old dimension; the cache check reads `dim`, so the
    // rows repaint on the next render without a refetch.
    this.requestUpdate();
  }

  handleColumnsChanged(e: { detail: string[] }) {
    const next = e.detail;
    const nextSet = new Set(next);
    const prevSet = new Set(this.logsColumns);
    this.syncColsUrlParam(
      this.logsColumns.filter((c) => !nextSet.has(c)),
      next.filter((c) => !prevSet.has(c))
    );
    for (const c of Object.keys(this.columnMaxWidthMap)) if (!nextSet.has(c)) delete this.columnMaxWidthMap[c];
    this.logsColumns = next;
    this.requestUpdate();
  }
  updateColumnMaxWidthMap = (recVecs: any[][]) => {
    if (this.isCalculatingWidths) return;
    this.isCalculatingWidths = true;

    // Use fixed widths primarily, only calculate for custom columns
    requestAnimationFrame(() => {
      try {
        // Use fixed widths for standard columns
        Object.entries(this.fixedColumnWidths).forEach(([key, width]) => {
          if (!this.columnMaxWidthMap[key]) {
            this.columnMaxWidthMap[key] = width;
          }
        });

        // Only calculate widths for non-standard columns
        const customColumns = Object.keys(this.colIdxMap).filter((key) => !this.fixedColumnWidths[key] && key !== 'id');

        if (customColumns.length > 0) {
          // Process only first 10 rows for custom columns
          const sampleRows = recVecs.slice(0, 10);
          customColumns.forEach((key) => {
            const value = this.colIdxMap[key];
            let maxWidth = MIN_COLUMN_WIDTH * CHAR_WIDTHS.default;

            sampleRows.forEach((vec) => {
              const content = String(vec[value] || '');
              const target = content.length * CHAR_WIDTHS.default;
              maxWidth = Math.max(maxWidth, target);
            });

            this.columnMaxWidthMap[key] = Math.min(maxWidth, 400); // Cap at 400px
          });
        }
        this.batchRequestUpdate('columnWidths');
      } finally {
        this.isCalculatingWidths = false;
      }
    });
  };
  toggleLogRow = (event: any, targetInfo: [string, string, string], pid: string) => {
    // Use refs when available, fallback to querySelector
    const sideView = this.logDetailsContainer || (document.querySelector('#log_details_container')! as HTMLElement);
    const resizerWrapper = this.resizerWrapper || document.querySelector('#resizer-details_width-wrapper');

    // Batch DOM reads and writes
    requestAnimationFrame(() => {
      const width = sideView.offsetWidth;
      this.shouldScrollToBottom = false;

      if (width < 50) {
        sideView.style.width = `550px`;
        updateUrlState('details_width', '550');
      }

      // Always show the resizer when a log row is clicked
      if (resizerWrapper) {
        resizerWrapper.classList.remove('hidden', 'opacity-0', 'pointer-events-none');
      }
    });

    // Use event delegation instead of querying all rows
    const prevActive = event.currentTarget.parentElement?.querySelector('.bg-fillBrand-strong');
    if (prevActive) {
      prevActive.classList.remove('bg-fillBrand-strong');
    }
    event.currentTarget.classList.add('bg-fillBrand-strong');
    const indicator = this.detailsIndicator || document.querySelector('#details_indicator');
    if (indicator) {
      indicator.classList.add('htmx-request');
    }

    const [rdId, rdCreatedAt, source] = targetInfo;
    const url = `/p/${pid}/log_explorer/${rdId}/${rdCreatedAt}/detailed?source=${source}`;
    updateUrlState('target_event', `${rdId}/${rdCreatedAt}/detailed?source=${source}`);
    // innerHTML, not morph: measured, the swap is ~7ms of a ~200ms click, and idiomorph's
    // in-place mutation means hyperscript never installs FieldMenuDelegate on the new
    // content, which silently kills the field context menu.
    (window as any).htmx.ajax('GET', url, { target: '#log_details_container', swap: 'innerHTML', indicator: '#details_indicator' });
  };

  moveColumn(column: string, direction: number) {
    const index = this.logsColumns.indexOf(column);
    if (index === -1) return;
    const newIndex = index + direction;
    if (newIndex < 0 || newIndex >= this.logsColumns.length) return;
    this.logsColumns[index] = this.logsColumns[newIndex];
    this.logsColumns[newIndex] = column;
    this.requestUpdate();
  }

  // New rows enter at the top for recent/live fetches and the bottom for load-more,
  // flipped when oldest-first is on.
  private orderMerge(current: any[], newData: any[], isRecentFetch: boolean) {
    return this.flipDirection
      ? isRecentFetch
        ? [...current, ...newData]
        : [...newData, ...current]
      : isRecentFetch
        ? [...newData, ...current]
        : [...current, ...newData];
  }

  // Buffer accumulation ("N new" pill) is bounded too: a user can leave live
  // tail paused for hours while inspecting an older row.
  private addWithFlipDirection(current: any[], newData: any[], isRecentFetch: boolean) {
    // Drop rows already on screen here rather than at merge time: mergeIntoTree filters them
    // anyway, so buffering them only inflates the "N new" pill above what it will insert.
    const fresh = newData.filter((r) => !this.seenIds.has(r.id));
    const merged = dedupeById(this.orderMerge(current, fresh, isRecentFetch));
    return this.flipDirection ? merged.slice(-MAX_RETAINED_ROWS) : merged.slice(0, MAX_RETAINED_ROWS);
  }

  // Merge a freshly fetched page into spanListTree, dropping ids already present
  // (boundary row recurs across inclusive-cursor pages) using the persistent
  // seenIds set so the cost is O(page), not O(whole tree).
  private mergeIntoTree(newData: EventLine[], isRecentFetch: boolean) {
    const fresh = newData.filter((r) => {
      if (this.seenIds.has(r.id)) return false;
      this.seenIds.add(r.id);
      return true;
    });
    const merged = this.orderMerge(this.spanListTree, fresh, isRecentFetch);
    if (this.mode !== 'logs' || merged.length <= MAX_RETAINED_ROWS) return merged;

    // Evict from the edge opposite the fetch. Move the cut past a trace boundary
    // so a root and its children are never split across retained/evicted state.
    const dropStart = this.flipDirection === isRecentFetch;
    const boundedCut = dropStart ? merged.length - MAX_RETAINED_ROWS : MAX_RETAINED_ROWS;
    let cut = boundedCut;
    if (dropStart) {
      while (cut < merged.length && cut > 0 && merged[cut].traceId === merged[cut - 1].traceId) cut++;
    } else {
      while (cut > 0 && cut < merged.length && merged[cut].traceId === merged[cut - 1].traceId) cut--;
    }
    // A single trace can exceed the entire window. Preserve a useful hard-bounded
    // prefix/suffix rather than moving the trace-aware cut to an empty edge.
    if (cut === 0 || cut === merged.length) cut = boundedCut;
    const kept = dropStart ? merged.slice(cut) : merged.slice(0, cut);
    const dropped = dropStart ? merged.slice(0, cut) : merged.slice(cut);
    dropped.forEach((r) => this.seenIds.delete(r.id));

    const retainedIds = new Set(kept.map((r) => r.id));
    const retainedTraces = new Set(kept.map((r) => r.traceId));
    this.cachedServerTraces = this.cachedServerTraces.filter((t) => retainedTraces.has(t.trace_id));
    this.expandedTraces = Object.fromEntries(Object.entries(this.expandedTraces).filter(([id]) => retainedTraces.has(id)));
    this.loadingSessions = Object.fromEntries(Object.entries(this.loadingSessions).filter(([id]) => retainedIds.has(id)));
    if (isRecentFetch) this.hasMore = true;
    else this.hasNewer = true;
    return kept;
  }

  // At the edge new rows are inserted at — top for newest-first, bottom when flipped.
  private atNewRowEdge(scrollTop: number, scrolledToBottom: boolean): boolean {
    return this.flipDirection ? scrolledToBottom : scrollTop <= 0;
  }

  private captureScrollAnchor(): ScrollAnchor | null {
    const container = this.logsContainer;
    if (!container || this.mode !== 'logs') return null;
    const top = container.getBoundingClientRect().top;
    const row = [...container.querySelectorAll<HTMLElement>('[data-row-id]')].find((el) => el.getBoundingClientRect().bottom > top);
    if (row) return { id: row.dataset.rowId!, offset: row.getBoundingClientRect().top - top };

    const range = this.lastVisibilityRange;
    const item = range && this.virtualListItems.slice(range.first, range.last + 1).find((entry) => 'id' in entry);
    return item ? { id: item.id, offset: 0 } : null;
  }

  private async restoreScrollAnchor(anchor: ScrollAnchor) {
    await this.updateComplete;
    const index = this.virtualListItems.findIndex((item) => 'id' in item && item.id === anchor.id);
    const virtualizer = this.querySelector('lit-virtualizer');
    if (index < 0 || !virtualizer) return;
    virtualizer.element(index)?.scrollIntoView({ block: 'start' });
    try {
      await virtualizer.layoutComplete;
    } catch (error) {
      if (this.isConnected) throw error;
      return;
    }
    requestAnimationFrame(() => {
      const container = this.logsContainer;
      const row = [...(container?.querySelectorAll<HTMLElement>('[data-row-id]') || [])].find((el) => el.dataset.rowId === anchor.id);
      if (container && row) container.scrollTop += row.getBoundingClientRect().top - container.getBoundingClientRect().top - anchor.offset;
    });
  }

  handleRecentClick() {
    const container = document.querySelector('#logs_list_container_inner');
    if (container) {
      container.scrollTop = 0;
    }
    this.handleRecentConcatenation();
  }

  handleRecentConcatenation() {
    if (this.recentDataToBeAdded.length === 0) return;
    this.spanListTree = this.mergeIntoTree(this.recentDataToBeAdded, true);
    this.recentDataToBeAdded = [];
    this.updateVisibleItems();
    this.batchRequestUpdate('recentConcatenation');
  }

  handleVisibilityChange = (e: any) => {
    const first = e.first;
    const last = e.last;
    if (!first || !last) return;

    // Store visibility range for deferred chart update
    this.lastVisibilityRange = { first, last };

    // Mark as scrolling
    this.isScrolling = true;

    // Clear existing timer
    if (this.scrollEndTimer) {
      clearTimeout(this.scrollEndTimer);
    }

    // Set timer to detect scroll end
    this.scrollEndTimer = setTimeout(() => {
      this.isScrolling = false;
    }, 50);

    // Debounced chart update (runs at most every 100ms)
    this.debouncedUpdateChartMarkArea();
  };

  private updateChartMarkArea() {
    if (!this.lastVisibilityRange || !this.barChart) return;

    const { first, last } = this.lastVisibilityRange;

    // Use requestIdleCallback for non-critical chart updates
    const updateChart = () => {
      let fTarget = this.virtualListItems[first];
      let lTarget = this.virtualListItems[last];

      if (!fTarget || !lTarget) return;

      fTarget = fTarget.type === 'fetchRecent' || fTarget.type === 'loadMore' ? (this.virtualListItems[first + 1] as EventLine) : fTarget;
      lTarget = lTarget.type === 'fetchRecent' || lTarget.type === 'loadMore' ? (this.virtualListItems[last - 1] as EventLine) : lTarget;

      if (!fTarget || !lTarget || !('data' in fTarget) || !('data' in lTarget)) return;

      const endTime = lookupVecValue(fTarget.data, this.colIdxMap, 'timestamp');
      const startTimeRaw = lookupVecValue(lTarget.data, this.colIdxMap, 'timestamp');

      // Convert to numbers (timestamps in ms)
      let startTime = new Date(startTimeRaw).getTime();
      let end = new Date(endTime).getTime();

      if (this.flipDirection) {
        const v = startTime;
        startTime = end;
        end = v;
      }

      // Get time range from chart to calculate appropriate bin width
      let MIN_RANGE = 30 * 1000; // Default 30s
      try {
        const xAxis = this.barChart.getModel().getComponent('xAxis', 0);
        const xAxisData = xAxis.axis.scale;
        const minValue = xAxisData.getExtent()[0];
        const maxValue = xAxisData.getExtent()[1];
        const timDiff = maxValue - minValue;
        MIN_RANGE = calculateAutoBinWidth(timDiff);
      } catch (e) {
        // Fall back to default if chart access fails
      }

      if (end - startTime < MIN_RANGE) {
        startTime = end - MIN_RANGE;
      }

      if (this.barChart) {
        this.barChart.setOption({
          series: [
            {
              markArea: {
                itemStyle: {
                  color: (window as any).echarts.color.modifyAlpha(cssTokenToHex('--color-fillBrand-strong'), 0.2),
                  borderColor: cssTokenToHex('--color-fillBrand-strong'),
                  borderWidth: 1,
                  borderType: 'dashed',
                },
                data: [[{ xAxis: endTime }, { xAxis: startTimeRaw }]],
                z: 999,
                zlevel: 999,
              },
            },
          ],
        });
      }
    };

    // Skip chart updates during active scrolling
    if (this.isScrolling) return;

    // Use requestIdleCallback for non-critical chart updates
    if ('requestIdleCallback' in window) {
      (window as any).requestIdleCallback(updateChart, { timeout: 500 });
    } else {
      setTimeout(updateChart, 250);
    }
  }

  // Comment to allow classes be rendered.
  render() {
    // Check if we're in initial loading state
    const isInitialLoading = this.isLoading && this.spanListTree.length === 0;
    const isPatterns = this.mode === 'patterns';
    const isAggregate = isPatterns;

    return html`
      <style>
        @keyframes fadeBg {
          0% {
            background-color: var(--color-strokeBrand-weak);
          }
          100% {
            background-color: transparent;
          }
        }

        .animate-fadeBg {
          animation: fadeBg 1.5s ease-out;
          will-change: background-color;
        }

        @keyframes pulseIndicator {
          0%,
          90% {
            background-color: oklch(48% 0.205 265);
          }
          100% {
            background-color: transparent;
          }
        }

        .animate-fadeBg .status-indicator {
          animation: pulseIndicator 4s ease-out forwards;
          will-change: background-color;
        }

        /* Performance optimizations that can't be done with Tailwind */
        .contain-layout-style-paint {
          contain: layout style paint;
        }

        /* Fixed table layout for performance */
        table {
          table-layout: fixed;
        }

        /* Prevent clicks on closed popovers */
        [popover]:not(:popover-open) {
          pointer-events: none;
        }

        /* Column width styles - dynamically generated for all known columns */
        ${unsafeHTML(
          [...new Set([...this.logsColumns, ...Object.keys(this.columnMaxWidthMap)])]
            .map((col) => {
              if (col === 'summary')
                return `.col-summary.break-all { width: var(--col-summary-width); min-width: var(--col-summary-width); }
.col-summary:not(.break-all) { width: var(--col-summary-width); min-width: var(--col-summary-width); max-width: var(--col-summary-width); }`;
              return `.col-${col} { width: var(--col-${col}-width); min-width: var(--col-${col}-width); max-width: var(--col-${col}-width); }`;
            })
            .join('\n')
        )}
      </style>
      ${this.options()}
      <div
        ${ref(this.containerRef)}
        class=${clsx(
          'relative group-hash-full shrink-1 min-w-0 pb-32 m-0 surface-raised rounded-t-2xl w-full h-full c-scroll overflow-y-auto contain-strict',
          isInitialLoading && 'overflow-hidden'
        )}
        id="logs_list_container_inner"
        style="min-height: 500px; overflow-anchor: none;"
      >
        ${this.liveDropped > 0
          ? html`<div class="sticky top-0 z-50 flex justify-center" role="status" aria-live="polite">
              <span
                class="cbadge-sm badge-neutral bg-fillWarning-strong text-textInverse-strong shadow rounded-lg text-sm"
                title="Live mode drops the oldest events when they arrive faster than the browser can take them. Narrow the query to see every event."
              >
                ${this.liveDropped.toLocaleString()} dropped — narrow your query
              </span>
            </div>`
          : nothing}
        ${!isAggregate && this.recentCount > 0 && !this.flipDirection
          ? html` <div class="sticky top-[30px] z-50 flex justify-center" role="status" aria-live="polite">
              <button
                class="cbadge-sm badge-neutral cursor-pointer bg-fillBrand-strong text-textInverse-strong shadow rounded-lg text-sm"
                @pointerdown=${this.handleRecentClick}
                aria-label="${this.recentCount} new events, click to load"
              >
                ${this.recentCount} new
              </button>
            </div>`
          : nothing}
        <table
          role="grid"
          aria-label="${isPatterns ? 'Log patterns' : this.mode === 'sessions' ? 'Sessions' : 'Log events'}"
          aria-rowcount=${this.totalCount || -1}
          class="table-fixed ${isAggregate || this.wrapLines ? 'w-full' : 'w-max'} relative ctable table-pin-rows table-pin-cols text-sm"
          style=${Object.entries(
            this.logsColumns.reduce(
              (acc, column) => {
                const width = this.columnMaxWidthMap[column] || this.fixedColumnWidths[column];
                if (width) {
                  acc[`--col-${column}-width`] = `${width}px`;
                }
                return acc;
              },
              {} as Record<string, string>
            )
          )
            .map(([k, v]) => `${k}: ${v}`)
            .join('; ')}
        >
          <thead class="z-10 sticky top-0 isolate">
            <tr class="text-textWeak border-b flex min-w-0 relative font-medium isolate">
              ${isInitialLoading
                ? skeletonColumns(this.logsColumns).map((column, idx) => {
                    // Mirror skeletonCell's per-column widths so header pills sit
                    // directly above their row cells (id is the narrow stripe,
                    // latency_breakdown pins right).
                    const isId = column === 'id';
                    const widthClass = isId
                      ? 'w-3'
                      : column === 'latency_breakdown'
                        ? 'sticky right-0 max-md:static z-10'
                        : getColumnWidth(column);
                    return html`
                      <td
                        class=${`p-0 m-0 whitespace-nowrap relative flex justify-between items-center pl-2.5 pr-2 text-sm font-normal bg-bgBase ${widthClass}`}
                      >
                        ${isId
                          ? nothing
                          : html`<div class="relative overflow-hidden">
                              <div class="h-4 rounded skeleton-shimmer w-16" style="animation-delay: ${idx * 0.1}s"></div>
                            </div>`}
                      </td>
                    `;
                  })
                : html`
                    ${this.logsColumns.filter((v) => v !== 'latency_breakdown').map((column) => this.logTableHeading(column))}
                    ${this.logsColumns.includes('latency_breakdown') && !isAggregate ? this.logTableHeading('latency_breakdown') : nothing}
                  `}
            </tr>
          </thead>
          ${isInitialLoading
            ? loadingSkeleton(this.logsColumns)
            : html`
                <tbody class="min-w-0 text-xs">
                  ${keyed(
                    this.isAggregate || this.wrapLines ? 'measured' : 'dense',
                    html`<lit-virtualizer
                      .items=${this.virtualListItems}
                      .keyFunction=${virtualItemKey}
                      .renderItem=${this.renderVirtualItem}
                      @visibilityChanged=${this.handleVisibilityChange}
                      .layout=${this.isAggregate || this.wrapLines ? {} : { type: DenseRowFlowLayout }}
                    ></lit-virtualizer>`
                  )}
                </tbody>
              `}
        </table>
        ${!isInitialLoading && this.virtualListItems.length === 0
          ? html`<div class="flex flex-col items-center justify-center py-12 px-4 text-center gap-2">
              ${faSprite('inbox-full', 'regular', 'w-6 h-6 text-iconNeutral')}
              <span class="text-sm text-textWeak">No events match in the selected time range.</span>
              <span class="text-xs text-textWeak">Try expanding the time picker above.</span>
            </div>`
          : nothing}
        ${!isAggregate && !this.shouldScrollToBottom && this.flipDirection
          ? html` <div style="position: sticky;bottom: 0px;overflow-anchor: none;">
              <button
                @pointerdown=${() => {
                  this.shouldScrollToBottom = true;
                  this.scrollToBottom();
                  this.handleRecentConcatenation();
                }}
                data-tip="Scroll to bottom"
                aria-label=${this.recentCount > 0 ? `Scroll to bottom (${this.recentCount} new events)` : 'Scroll to bottom'}
                class=${clsx(
                  'absolute tooltip tooltip-left right-8 bottom-2 group z-50 text-textInverse-strong flex justify-center items-center rounded-full shadow-lg h-10 w-10 transition-all duration-300 hover:shadow-xl hover:scale-110',
                  this.recentCount > 0
                    ? 'bg-gradient-to-br from-fillBrand-strong to-fillBrand-weak animate-pulse'
                    : 'bg-gradient-to-br from-fillStrong to-fillWeak'
                )}
              >
                ${this.recentCount > 0
                  ? html`<span class="absolute inset-0 rounded-full bg-fillBrand-strong opacity-30 blur animate-ping"></span>`
                  : nothing}
                <span class="relative">
                  ${faSprite('arrow-down', 'regular', 'h-6 w-6 fill-textInverse-strong stroke-textInverse-strong')}
                </span>
              </button>
            </div>`
          : nothing}
      </div>
    `;
  }
  createRenderRoot() {
    return this;
  }

  private parseSummaryData(dataArr: any[]): string[] {
    const cim = this._renderOverrides?.colIdxMap ?? this.colIdxMap;
    const summary = lookupVecValue<string[] | string>(dataArr, cim, 'summary');
    // Coerce non-string elements (e.g. a TF to_jsonb that re-parsed JSON-looking
    // text) so one bad element can't throw and blank the whole row.
    if (Array.isArray(summary)) return summary.map((e) => (typeof e === 'string' ? e : (JSON.stringify(e) ?? '')));
    try {
      return typeof summary === 'string' ? JSON.parse(summary) : [];
    } catch (err) {
      // Silent [] made whole rows render blank with no signal. Log + surface a
      // visible sentinel so support engineers can spot malformed payloads.
      console.error('parseSummaryData: malformed summary payload', { summary, err });
      return ['\u26A0 malformed summary'];
    }
  }

  // Ultra-optimized renderSummaryElements using closure for caching
  renderSummaryElements = (() => {
    // Private cache with fast hashing
    const cache = new Map<number, TemplateResult[]>();
    const parseCache = new WeakMap<string[], any[]>();
    const unescapeCache = new Map<string, string>();

    // FNV-1a hash for ultra-fast cache keys
    const hashArray = (arr: string[], wrap: boolean): number => {
      let hash = 0x811c9dc5; // FNV offset basis
      for (let i = 0; i < arr.length; i++) {
        const str = arr[i];
        for (let j = 0; j < str.length; j++) {
          hash ^= str.charCodeAt(j);
          hash = Math.imul(hash, 0x01000193); // FNV prime
        }
      }
      return (hash >>> 0) | (wrap ? 0x80000000 : 0);
    };

    // Cached unescaping with bounded cache
    const getCachedUnescape = (str: string): string => {
      let unescaped = unescapeCache.get(str);
      if (unescaped === undefined) {
        unescaped = unescapeJsonString(str);
        // Bounded cache - evict oldest when limit reached
        if (unescapeCache.size >= 1024) {
          const firstKey = unescapeCache.keys().next().value!;
          unescapeCache.delete(firstKey);
        }
        unescapeCache.set(str, unescaped);
      }
      return unescaped;
    };

    // Create cached icon renderer instance
    const renderIcon = createCachedIconRenderer();

    // The main render function
    return function (this: LogList, summaryArray: string[], wrapLines: boolean): TemplateResult[] {
      if (!summaryArray?.length) return [];

      // Check main render cache first
      const cacheKey = hashArray(summaryArray, wrapLines);
      const cached = cache.get(cacheKey);
      if (cached) return cached;

      // Get or parse elements
      let parsed = parseCache.get(summaryArray);
      if (!parsed) {
        parsed = summaryArray.map((el) => parseSummaryElement(el));
        parseCache.set(summaryArray, parsed);
      }

      const wrapClass = wrapLines ? 'whitespace-break-spaces' : 'whitespace-nowrap';
      // Plain body text preserves leading whitespace so multiline logs (where each
      // indented line arrives as its own LogRecord) align visually in the list.
      const plainWrapClass = wrapLines ? 'whitespace-pre-wrap' : 'whitespace-pre';
      const result: TemplateResult[] = [];

      // Optimized single pass with early continues
      for (let i = 0; i < parsed.length; i++) {
        const p = parsed[i];

        // Skip right-aligned elements
        if (p.type !== 'plain' && RIGHT_PREFIX_REGEX.test(p.style)) continue;

        if (p.type === 'plain') {
          if (this.mode === 'patterns') {
            result.push(html`<span class=${`fill-textStrong ${plainWrapClass}`}>${highlightPlaceholders(p.content)}</span>`);
          } else {
            result.push(html`<span class=${`fill-textStrong ${plainWrapClass}`}>${unsafeHTML(getCachedUnescape(p.content))}</span>`);
          }
          continue;
        }

        const { field, style, value } = p;

        // Skip rendering 'kind=database' as text since db.system icon will be shown
        if (field === 'kind' && value === 'database') {
          continue;
        }

        // Check for icon fields first
        if (field === 'request_type' || field === 'kind' || field === 'db.system') {
          const icon = renderIcon(field, value);
          if (icon) {
            result.push(icon);
            continue;
          }
        }

        // Direct style checks with early returns
        if (style === 'text-textStrong') {
          result.push(
            this.mode === 'patterns'
              ? html`<span class="text-textStrong">${highlightPlaceholders(value)}</span>`
              : html`<span class="text-textStrong">${value}</span>`
          );
        } else if (WEAK_TEXT_STYLES.has(style)) {
          result.push(
            this.mode === 'patterns'
              ? html`<span class="text-textWeak">${highlightPlaceholders(value)}</span>`
              : html`<span class="text-textWeak">${unsafeHTML(getCachedUnescape(value))}</span>`
          );
        } else {
          // Top-level session rows are rendered via renderSessionSummary in
          // the summary case; this fallback handles regular log rows and
          // also expanded span children inside a session.
          result.push(renderBadge(clsx('cbadge-sm', this.getStyleClass(style), wrapClass), value));
        }
      }

      // Bounded main cache with bulk eviction
      if (cache.size >= 512) {
        // Remove oldest 256 entries
        const entries = Array.from(cache.keys()).slice(0, 256);
        entries.forEach((k) => cache.delete(k));
      }

      cache.set(cacheKey, result);
      return result;
    };
  })();

  getStyleClass(style: string): string {
    return getStyleClass(style);
  }

  logItemCol = (rowData: EventLine, key: string): any => {
    const { data: dataArr, depth, children, traceId, childErrors, hasErrors, expanded, type, id, isLastChild, siblingsArr } = rowData;
    const wrapClass = this.wrapLines ? 'whitespace-break-spaces' : 'whitespace-nowrap';
    // When rendering inside aggregate children, use overridden colIdxMap
    const colIdxMap = this._renderOverrides?.colIdxMap ?? this.colIdxMap;
    // Detect once, reused by latency + summary cases.
    const isSyntheticRow = isSyntheticRowId(lookupVecValue<string>(dataArr, colIdxMap, 'id'));

    switch (key) {
      case 'pattern_count':
        const count = lookupVecValue<number>(dataArr, colIdxMap, key);
        const mergedCount = lookupVecValue<number>(dataArr, colIdxMap, 'merged_count') || 0;
        const maxCount =
          this.mode === 'patterns' && this.visibleItems.length ? lookupVecValue<number>(this.visibleItems[0].data, colIdxMap, key) || 1 : 1;
        const pct = (count / maxCount) * 100;
        return html`<div
          class="flex items-center gap-1.5 w-full min-w-0"
          title="${pct.toFixed(1)}% of total${mergedCount > 0 ? ` (${mergedCount} merged)` : ''}"
        >
          <span class="text-sm tabular-nums text-textStrong w-10 shrink-0 text-right">${formatLargeCount(count)}</span>
          ${mergedCount > 0
            ? html`<span class="text-2xs tabular-nums text-textWeak shrink-0" title="${mergedCount} similar patterns merged"
                >+${mergedCount}</span
              >`
            : ''}
          <div class="w-12 shrink-0 h-2 bg-strokeWeak rounded-sm overflow-hidden">
            <div class="h-full bg-fillBrand-strong rounded-sm" style="width:${pct}%"></div>
          </div>
        </div>`;
      case 'volume':
        const volBuckets = lookupVecValue<number[]>(dataArr, colIdxMap, key);
        return html`<div class="flex items-center w-full">${renderSparkline(volBuckets)}</div>`;
      case 'level':
        const lv = lookupVecValue<string>(dataArr, colIdxMap, key);
        if (!lv) return html`<span class="text-textWeak text-xs text-center w-full inline-block">-</span>`;
        const lvColors: Record<string, string> = {
          error: 'badge-error',
          warn: 'badge-warning',
          warning: 'badge-warning',
          info: 'badge-info',
          debug: 'badge-neutral',
        };
        return renderBadge(`cbadge-sm ${lvColors[lv.toLowerCase()] || 'badge-neutral'}`, lv);
      case 'id':
        if (!this._renderOverrides && this.isAggregate) {
          return html`<div class="flex items-center justify-between w-3">
            <span class="col-span-1 h-5 rounded-sm flex w-1 bg-strokeBrand-weak"></span>
          </div>`;
        }
        const { statusCode: status, hasErrors: errCount, className: errClass } = getErrorClassification(dataArr, colIdxMap);
        const isExpanded = expanded || rowData.parentIds?.some((pid: string) => this.expandedTraces[pid]);
        // Session roots get a wider bar so a red "this session errored" signal is
        // scannable down the rail rather than a 1px hairline you must hover to read.
        const indicatorClass = (isExpanded ? errClass.replace('-weak', '-strong') : errClass).replace(
          'w-1',
          this.mode === 'sessions' && depth === 0 ? 'w-1.5' : 'w-1'
        );
        const errTip =
          this.mode === 'sessions'
            ? `${errCount || 0} error${errCount === 1 ? '' : 's'} in this session`
            : `${errCount} errors attached; status ${status}`;
        return html`
          <div class="flex items-center justify-between w-3">
            <span class="col-span-1 h-5 rounded-sm flex"> ${renderIconWithTooltip(indicatorClass, errTip, html``)} </span>
          </div>
        `;
      case 'created_at':
      case 'timestamp': {
        const timestamp = lookupVecValue<string>(dataArr, colIdxMap, key);
        const rowTraceId = traceId || lookupVecValue<string>(dataArr, colIdxMap, 'trace_id');
        const rowKind = lookupVecValue<string>(dataArr, colIdxMap, 'kind');
        return html`<div class="relative">
          <time class=${`monospace text-xs text-textWeak tooltip tooltip-right ${wrapClass}`} data-tip="timestamp" datetime=${timestamp}
            >${formatTimestamp(timestamp)}</time
          >
          ${rowTraceId && rowKind !== 'log' && this.mode !== 'sessions'
            ? html`<button
                class="absolute inset-y-0 left-0 z-30 hidden group-hover:flex items-center cursor-pointer group/btn"
                data-tippy-content="Open trace fullscreen"
                @pointerdown=${(e: Event) => e.stopPropagation()}
                @click=${(e: Event) => {
                  e.stopPropagation();
                  window.dispatchEvent(new CustomEvent('openTraceFullscreen', { detail: { traceId: rowTraceId, timestamp } }));
                }}
              >
                <span
                  class="flex items-center justify-center w-5 h-5 rounded border border-strokeMedium bg-bgBase text-iconNeutral group-hover/btn:border-strokeBrand group-hover/btn:text-textBrand group-hover/btn:bg-fillBrand/10 transition-colors"
                >
                  ${faSprite('up-right-and-down-left-from-center', 'regular', 'w-2.5 h-2.5')}
                </span>
              </button>`
            : nothing}
        </div>`;
      }
      case 'latency_breakdown':
        // Cache rendered latency breakdown
        const currentWidth = this.columnMaxWidthMap['latency_breakdown'] || this.fixedColumnWidths['latency_breakdown'] || 120;
        if (
          !rowData._latencyCache ||
          rowData._latencyCache.width !== currentWidth ||
          rowData._latencyCache.expanded !== expanded ||
          rowData._latencyCache.dim !== this.latencyDim
        ) {
          const { traceStart, traceEnd, startNs, duration, childrenTimeSpans } = rowData;
          // Colour keyed on the chosen dimension, not on `span_name`. Keying on the span name
          // made this a per-operation palette wearing the name "service colors": two spans in
          // one service got two colours, the same operation in two services got one, and any
          // name missing the palette fell back to grey.
          const dimOf = (arr: any) => lookupVecValue<string>(arr, colIdxMap, this.latencyDim) || '';
          const colorOf = (value: string) =>
            this.latencyDim === 'kind'
              ? KIND_COLORS[value] ?? 'bg-fillStrong'
              : this.serviceColors[value] || 'bg-fillStrong';
          const color = isSyntheticRow ? 'bg-transparent border border-dashed border-strokeWeak' : colorOf(dimOf(dataArr));
          const chil = childrenTimeSpans.map(({ startNs, duration, data, depth }: ChildrenForLatency) => {
            const label = dimOf(data) || 'unknown';
            return { startNs, duration, depth, label, color: colorOf(label) };
          });
          // The title always describes the row itself, whichever axis the bar is drawn on, and
          // always exclusively — nested spans would otherwise bill the same time to every
          // ancestor's service and make the parts sum past the total.
          const ownSegments = exclusiveSegments({ startNs, duration }, chil);
          const { track, segments, frame } = latencyBar(
            !!expanded,
            { startNs, duration, traceStart, traceEnd, label: dimOf(dataArr) || 'unknown', color },
            chil
          );

          // Extract right-aligned badges from summary array
          const summaryArr = this.parseSummaryData(dataArr);
          const rightAlignedBadges: TemplateResult[] = [];

          // Use optimized parsing for right-aligned badges
          let userEmail = '',
            userName = '',
            userId = '',
            userBadgeStyle = '';
          for (let i = 0; i < summaryArr.length; i++) {
            const element = summaryArr[i];
            const sepIdx = element.indexOf('⇒');
            if (sepIdx === -1) continue;

            const semiIdx = element.indexOf(';');
            if (semiIdx === -1 || semiIdx > sepIdx) continue;

            const style = element.substring(semiIdx + 1, sepIdx);
            if (!RIGHT_PREFIX_REGEX.test(style)) continue;

            const field = element.substring(0, semiIdx);
            const value = element.substring(sepIdx + 1);
            const badgeStyle = this.getStyleClass(style);

            if (field === 'session') {
              // In tree mode, the play button only renders on tree roots
              // (depth 0 or rows with children). Leaf children inherit the
              // session from their parent — repeating the button on every
              // resource row turns the whole right rail into noise.
              if (depth === 0 || (children && children > 0)) {
                rightAlignedBadges.push(this.createSessionButton(value, !!hasErrors));
              }
            } else if (field === 'user email') {
              userEmail = value;
              userBadgeStyle = badgeStyle;
            } else if (field === 'user name') {
              userName = value;
              if (!userBadgeStyle) userBadgeStyle = badgeStyle;
            } else if (field === 'user id') {
              userId = value;
              if (!userBadgeStyle) userBadgeStyle = badgeStyle;
            } else {
              rightAlignedBadges.push(renderBadge(`cbadge-sm ${badgeStyle} bg-opacity-100`, value));
            }
          }
          // One identity pill per row, whichever identifiers the span carries. The server
          // emits email, name and id under their own labels; folding them here keeps the row
          // to a single badge while the tooltip still names every identifier, and the full
          // session/user/tenant set lives in the detail panel.
          if (userEmail || userName || userId) {
            const full = userEmail || userName || userId;
            const display = full.length > 20 ? full.substring(0, 18) + '…' : full;
            const tip = [userName, userEmail, userId && `id ${userId}`].filter(Boolean).join(' — ');
            rightAlignedBadges.push(renderBadge(`cbadge-sm ${userBadgeStyle} bg-opacity-100`, display, tip));
          }

          // Session roots have no trace waterfall. Duration (data) now lives in the
          // summary line; this trailing column is a pure *actions* column holding
          // only the replay button, so the header stays unlabeled (actions columns
          // don't get a data label).
          const isSessionRoot = this.mode === 'sessions' && depth === 0;
          let latencyHtml;
          if (isSessionRoot) {
            latencyHtml = html`
              <div class="flex justify-end items-center gap-1 pl-2 bg-bgBase group-hover:bg-fillWeaker" style="min-width:${currentWidth}px">
                ${rightAlignedBadges}
                <span class="w-1"></span>
              </div>
            `;
          } else {
            latencyHtml = html`
              <div class="flex justify-end items-center gap-1 text-textWeak pl-1 rounded-lg bg-bgBase" style="min-width:${currentWidth}px">
                ${rightAlignedBadges}
                ${spanLatencyBreakdown({
                  track,
                  segments,
                  frame,
                  title: latencyTitle(this.latencyDim, { startNs, duration, traceStart }, ownSegments),
                  barWidth: currentWidth - 12,
                })}
                <span class="w-1"></span>
              </div>
            `;
          }

          rowData._latencyCache = {
            content: latencyHtml,
            width: currentWidth,
            expanded: expanded,
            dim: this.latencyDim,
          };
        }
        return rowData._latencyCache.content;
      case 'summary':
        const isSessionTopLevel = this.mode === 'sessions' && depth === 0;
        if (!rowData._summaryCache || rowData._summaryCache.wrapLines !== this.wrapLines) {
          const summaryArray = this.parseSummaryData(dataArr);
          // Session top-level rows use the two-line identity/context layout;
          // everything else (logs, patterns, expanded session children) uses
          // the flat element list.
          rowData._summaryCache = {
            content: isSessionTopLevel
              ? [this.renderSessionSummary(summaryArray)]
              : this.renderSummaryElements(summaryArray, this.wrapLines),
            wrapLines: this.wrapLines,
          };
        }
        // Synthetic-orphan rows append a click-to-copy chip showing the full
        // upstream parent id (carried in the latency_breakdown column).
        const synthParentId = isSyntheticRow ? (lookupVecValue<string>(dataArr, colIdxMap, 'latency_breakdown') ?? '') : '';
        if (this.mode === 'patterns') {
          const patIsError = lookupVecValue<boolean>(dataArr, colIdxMap, 'is_error') === true;
          // Flex lays the drain-template tokens out horizontally and drops the
          // whitespace text nodes lit emits between spans. The old
          // break-all/break-spaces preserved those newlines and inflated every
          // row to ~4 blank lines (88px) regardless of pattern length.
          return html`<div class="flex items-center gap-1 min-w-0 ${this.wrapLines ? 'flex-wrap' : 'whitespace-nowrap overflow-hidden'}">
            ${patIsError
              ? html`<span class="cbadge-sm badge-error shrink-0 align-middle" title="Pattern includes error-level events">error</span>`
              : nothing}
            ${rowData._summaryCache.content}
          </div>`;
        }
        const errClas = hasErrors
          ? 'bg-fillError-strong text-textInverse-strong fill-textInverse-strong stroke-strokeError-strong'
          : childErrors
            ? 'border border-strokeError-strong bg-fillWeak text-textWeak fill-textWeak'
            : 'border border-strokeWeak bg-fillWeak text-textWeak fill-textWeak';
        const summaryContent = rowData._summaryCache.content;
        return html`<div
          class=${clsx('flex w-full gap-1 min-w-0', isSessionTopLevel ? 'items-center' : this.wrapLines ? 'items-start' : 'items-center')}
        >
          ${this.view === 'tree' || this.mode === 'sessions'
            ? html`
                <div class="flex items-center shrink-0">
                  ${map(
                    Array(Math.max(0, depth - 1)),
                    (_, i) =>
                      html`<div class="w-8 h-5 shrink-0 flex items-center justify-center">
                        ${siblingsArr[i] ? faSprite('tree-straight', 'regular', 'w-8 h-5 text-iconNeutral') : nothing}
                      </div>`
                  )}
                  ${depth > 0
                    ? html`<div class="w-8 h-5 shrink-0 flex items-center justify-center">
                        ${faSprite(isLastChild ? 'tree-angle' : 'tree-tee', 'regular', 'w-8 h-5 text-iconNeutral')}
                      </div>`
                    : nothing}
                  ${children > 0
                    ? html`<button
                        @click=${(e: any) => {
                          e.stopPropagation();
                          e.preventDefault();
                        }}
                        @pointerdown=${(e: any) => {
                          e.stopPropagation();
                          e.preventDefault();
                          this.expandTrace(traceId, id);
                        }}
                        aria-expanded=${expanded}
                        aria-busy=${!!this.loadingSessions[id]}
                        aria-label="${this.loadingSessions[id]
                          ? 'Loading'
                          : expanded
                            ? 'Collapse'
                            : 'Expand'} trace (${children} ${children === 1 ? 'span' : 'spans'})"
                        class=${`hover:border-strokeBrand-strong rounded-sm ml-1 cursor-pointer shrink-0 w-8 px-1 flex justify-center gap-[2px] text-xs items-center h-5 ${errClas}`}
                      >
                        ${this.loadingSessions[id]
                          ? faSprite('spinner', 'regular', 'w-3 h-3 shrink-0 animate-spin')
                          : expanded
                            ? faSprite('minus', 'regular', 'w-3 h-1 shrink-0')
                            : faSprite('plus', 'regular', 'w-3 h-3 shrink-0')}
                        ${children}
                      </button>`
                    : depth === 0
                      ? nothing
                      : html`<div class=${`rounded-sm ml-1 shrink-0 w-3 h-5 ${errClas}`}></div>`}
                </div>
              `
            : nothing}
          <div
            class=${clsx(
              'flex gap-1 min-w-0',
              isSessionTopLevel
                ? 'flex-1 items-center'
                : this.wrapLines
                  ? 'items-center break-all flex-wrap'
                  : 'items-center overflow-hidden'
            )}
          >
            ${summaryContent}${synthParentId ? renderCopyIdChip(synthParentId) : nothing}
          </div>
        </div>`;
      case 'service':
        let serviceData = lookupVecValue<string>(dataArr, colIdxMap, key);
        // Session roots carry a space-joined service list that is often empty for
        // browser/RUM sessions. Render one badge per service when present; render
        // nothing when empty so the cell doesn't read as a stuck loading skeleton.
        if (this.mode === 'sessions' && depth === 0) {
          const svcs = (serviceData || '').split(/\s+/).filter(Boolean);
          // Device (user-agent) is packed into the summary payload. Surface it here
          // in this column (relabeled "client" for sessions) as a device-class glyph
          // + label, instead of leaving the summary body to carry it. Service badges
          // still render alongside when a session actually has service data.
          let device = '';
          for (const el of this.parseSummaryData(dataArr)) {
            const p = parseSummaryElement(el);
            if (p.type === 'formatted' && p.field === 'device') {
              device = p.value;
              break;
            }
          }
          if (!device && !svcs.length) return nothing;
          const icon = device ? deviceIconName(device) : '';
          const isBot = device ? isBotUserAgent(device) : false;
          return html`<div class=${clsx('flex items-center gap-1.5 min-w-0 overflow-hidden', isBot && 'opacity-60')}>
            ${device
              ? html`<span class="inline-flex items-center gap-1 min-w-0 text-xs text-textWeak tooltip tooltip-left" data-tip=${device}>
                  ${icon ? faSprite(icon, 'solid', 'w-3 h-3 shrink-0 fill-iconNeutral') : nothing}
                  <span class="truncate">${parseUserAgent(device)}</span>
                </span>`
              : nothing}
            ${svcs.map((s) => renderBadge('cbadge-sm badge-neutral shrink-0', s))}
          </div>`;
        }
        return renderBadge('cbadge-sm badge-neutral ' + wrapClass, serviceData, key);
      default:
        let v = lookupVecValue<string>(dataArr, colIdxMap, key);
        return html`<span class=${wrapClass} title=${key}>${v}</span>`;
    }
  };

  // Label and spinner both stay mounted and swap via `invisible`; the spinner is
  // overlaid absolutely so the label alone sizes the box. Conditionally rendering
  // one OR the other left a frame where the row was empty (longer once the
  // virtualizer re-measured the changed row) and resized it, reflowing the list.
  createLoadingRow = (id: string | null, label: string | TemplateResult, loading: boolean, onClick: () => void, rowRef?: RefOrCallback) => html`
    <tr
      class="w-full flex relative h-[28px] cursor-pointer hover:bg-fillWeaker"
      id=${id || nothing}
      aria-busy=${loading}
      @click=${onClick}
      ${ref(rowRef ?? noopRef)}
    >
      <td colspan=${String(this.logsColumns.length)} class="relative pl-[calc(40vw-10ch)]">
        <div class="h-7 relative flex items-center justify-center">
          <span class=${clsx('text-textBrand underline font-semibold', loading && 'invisible')}>${label}</span>
          <div class=${clsx('absolute top-1 loading loading-dots loading-md h-5', !loading && 'invisible')} role="status" aria-label="Loading"></div>
        </div>
      </td>
    </tr>
  `;

  renderExpandTimeRangeButton = () =>
    this.createLoadingRow(null, 'Show earlier events', this.isLoading || this.isLoadingMore, () => {
      this.fetchData(this.expandTimeRangeUrl(), false, false, true);
      this.expandTimeRange = false;
    });

  renderLoadMoreButton = () => {
    if (this.fetchError && this.spanListTree.length === 0) {
      return errorState(this.logsColumns.length, this.fetchError, () => {
        this.fetchError = null;
        this.fetchData(this.buildJsonUrl(), true);
      });
    }
    if (this.spanListTree.length === 0 && !this.isLoading && !this.hasMore && !this.flipDirection) {
      return emptyState(this.logsColumns.length);
    }
    if (this.expandTimeRange && !this.hasMore && !!this.spanListTree.length) return this.renderExpandTimeRangeButton();
    if (!this.hasMore || !this.spanListTree.length) return html`<tr></tr>`;

    // Use a ref to observe when this element comes into view
    const loadMoreRef = createRef<HTMLTableRowElement>();

    // Set up observer after render
    requestAnimationFrame(() => {
      if (loadMoreRef.value && !this.isLoadingMore && !this.isLoading) {
        const observer = new IntersectionObserver(
          ([entry]) => {
            if (entry.isIntersecting && !this.isLoadingMore && !this.isLoading) {
              this.debouncedFetchData(this.buildLoadMoreUrl(), false, false, true);
              observer.disconnect();
            }
          },
          {
            root: this.logsContainer,
            rootMargin: '100px',
            threshold: 0.1,
          }
        );
        observer.observe(loadMoreRef.value);

        // Store observer for cleanup
        if (this._loadMoreObserver) {
          this._loadMoreObserver.disconnect();
        }
        this._loadMoreObserver = observer;
      }
    });

    return this.createLoadingRow(
      null,
      'Load more',
      this.isLoading || this.isLoadingMore,
      () => this.fetchData(this.buildLoadMoreUrl(), false, false, true),
      loadMoreRef
    );
  };

  renderFetchRecentButton = () => {
    if (this.spanListTree.length === 0 && !this.isLoading && !this.hasMore && this.flipDirection) {
      return emptyState(this.logsColumns.length);
    }

    if (!this.spanListTree.length) return html`<tr></tr>`;

    // Aggregate views (patterns) don't support live streaming or loading newer events
    if (this.isAggregate) return html`<tr></tr>`;

    const fetchRecentRef = createRef<HTMLTableRowElement>();
    if (this.hasNewer) {
      requestAnimationFrame(() => {
        if (!fetchRecentRef.value || this.isFetchingRecent || this.isLoading) return;
        const observer = new IntersectionObserver(
          ([entry]) => {
            if (entry.isIntersecting && !this.isFetchingRecent && !this.isLoading) {
              this.fetchData(this.buildRecentFetchUrl(), false, true, false, true);
              observer.disconnect();
            }
          },
          { root: this.logsContainer, rootMargin: '100px', threshold: 0.1 }
        );
        observer.observe(fetchRecentRef.value);
        this._loadNewerObserver?.disconnect();
        this._loadNewerObserver = observer;
      });
    }

    return this.createLoadingRow(
      'recent-logs',
      this.isLiveStreaming ? html`<span class="font-normal no-underline text-textWeak">Live streaming latest data...</span>` : 'Load newer events',
      this.isFetchingRecent,
      () => {
        if (this.isLiveStreaming) return;
        this.fetchData(this.buildRecentFetchUrl(), false, true, false, true);
      },
      fetchRecentRef
    );
  };

  renderLoadMore() {
    return this.renderLoadMoreButton();
  }

  fetchRecent() {
    return this.renderFetchRecentButton();
  }

  logTableHeading(column: string) {
    if (column === 'id') return html`<td class="p-0 m-0 whitespace-nowrap col-id pl-2.5"></td>`;

    const width = this.columnMaxWidthMap[column] || this.fixedColumnWidths[column];
    // Tailwind safelist: class="max-md:static"
    const config = {
      pattern_count: { title: 'count', classes: 'shrink-0' },
      volume: { title: '~volume', classes: 'shrink-0' },
      level: { title: 'status', classes: 'shrink-0' },
      timestamp: { title: 'timestamp', classes: 'shrink-0' },
      created_at: { title: 'timestamp', classes: 'shrink-0' },
      // Sessions use this trailing column purely for the replay action (duration
      // lives in the summary line), so leave its header blank — actions columns
      // aren't data and don't get a label.
      latency_breakdown: { title: this.mode === 'sessions' ? '' : 'latency', classes: 'sticky right-0 max-md:static shrink-0' },
      status_code: { title: 'status', classes: 'shrink-0' },
      method: { title: 'method', classes: 'shrink-0' },
      raw_url: { title: column, classes: 'shrink-0' },
      url_path: { title: column, classes: 'shrink-0' },
      // Sessions repurpose this column for the client/device on root rows (service
      // badges still show on expanded child spans), so label it "client" there.
      service: { title: this.mode === 'sessions' ? 'client' : 'service', classes: 'shrink-0' },
      summary: { title: 'summary', classes: 'shrink-1' },
    };

    const { title = column, classes = 'shrink-0' } = config[column] || {};
    return this.tableHeadingWrapper(title, column, classes, width);
  }

  renderVirtualItem = (item: VirtualListItem) => {
    // Handle special item types efficiently
    if ('type' in item && item.type === 'fetchRecent') {
      return this.renderFetchRecentButton();
    }
    if ('type' in item && item.type === 'loadMore') {
      return this.renderLoadMoreButton();
    }
    if ('type' in item && item.type === 'aggregateChildren') {
      return this.renderAggregateChildren(item.parentKey);
    }
    // Regular event line item
    return this.logItemRow(item as EventLine);
  };

  logItemRow = (rowData: EventLine) => {
    try {
      const ov = this._renderOverrides;
      const effectiveMode = ov?.mode ?? this.mode;
      const effectiveColIdxMap = ov?.colIdxMap ?? this.colIdxMap;
      const effectiveLogsColumns = ov?.logsColumns ?? this.logsColumns;
      const isPatterns = effectiveMode === 'patterns';
      const isAggregate = isPatterns;
      const s = rowData.type === 'log' ? 'logs' : 'spans';
      const targetInfo = isAggregate ? '' : requestDumpLogItemUrlPath(rowData.data, effectiveColIdxMap, s);
      const isNew = rowData.isNew;

      // Pre-calculate CSS custom properties for widths
      const columnStyles = effectiveLogsColumns.reduce(
        (acc, column) => {
          const width = this.columnMaxWidthMap[column] || this.fixedColumnWidths[column];
          if (width) {
            acc[`--col-${column}-width`] = `${width}px`;
          }
          return acc;
        },
        {} as Record<string, string>
      );

      const isSessionTopLevelRow = effectiveMode === 'sessions' && rowData.depth === 0;
      // Error sessions escalate visually: the row takes on a soft red tint
      // so a broken session reads as the urgent thing on the page without
      // shouting. The existing red left-stripe (rendered in the id column
      // via getErrorClassification) anchors the severity; this tint makes
      // the whole row belong to that signal.
      const isErrorSessionRow = isSessionTopLevelRow && !!rowData.hasErrors;
      const cellBg = isErrorSessionRow ? 'bg-fillError-weak' : 'bg-bgBase';
      const rowHoverBg = isErrorSessionRow ? 'hover:bg-fillError-weak' : 'hover:bg-fillWeaker';
      // Synthetic placeholder rows (server tags id="synthetic-<parent_id>")
      // get muted styling so they don't compete with real spans.
      const isSynthetic = isSyntheticRowId(lookupVecValue<string>(rowData.data, this.colIdxMap, 'id'));
      // When rendered as an aggregate child (ov is set), the parent is a <div>
      // inside a <td>, so emitting <tr>/<td> here produces invalid nesting and
      // browsers reparent the orphan <tr>s. Use <div role="row|cell"> instead.
      const rowClass = clsx(
        'item-row relative p-0 flex group whitespace-nowrap isolate cursor-pointer',
        rowHoverBg,
        !ov && 'contain-layout-style-paint',
        isPatterns && (this.wrapLines ? 'items-start' : 'items-center'),
        // All non-wrapping, non-aggregate rows (including sessions) use the
        // dense 28px log row height for a consistent rhythm.
        !this.wrapLines && !isAggregate && 'h-[28px] items-center',
        isSynthetic && 'italic text-textWeak border-l-2 border-dashed border-strokeWeak',
        isNew && 'animate-fadeBg'
      );
      const rowStyle = Object.entries(columnStyles)
        .map(([k, v]) => `${k}: ${v}`)
        .join('; ');
      const rowClick = isAggregate
        ? (event: any) => {
            event.stopPropagation();
            this.toggleAggregateRow(rowData);
          }
        : effectiveMode === 'sessions' && rowData.depth === 0
          ? (event: any) => {
              event.stopPropagation();
              this.expandTrace(rowData.traceId, rowData.id);
            }
          : (event: any) => this.toggleLogRow(event, targetInfo, this.projectId);
      const cells = effectiveLogsColumns
        .filter((v) => v !== 'latency_breakdown')
        .map((column) => {
          const hasWidth = this.columnMaxWidthMap[column] || this.fixedColumnWidths[column];
          // In aggregate child rows (ov), skip fixed summary width so it flexes to fill remaining space
          const skipFixedWidth = ov && column === 'summary';
          const cellClass = `${this.wrapLines ? 'break-all whitespace-break-spaces' : ''} ${cellBg} group-hover:bg-inherit relative pl-2 ${
            column === 'summary'
              ? `flex-1 min-w-0 ${ov ? 'overflow-hidden' : ''}`
              : 'flex-shrink-0 overflow-hidden hover:overflow-visible hover:z-30'
          } ${hasWidth && !(isAggregate && column === 'summary') && !skipFixedWidth ? `col-${column}` : ''}`;
          return ov
            ? html`<div role="cell" class=${cellClass}>${this.logItemCol(rowData, column)}</div>`
            : html`<td class=${cellClass}>${this.logItemCol(rowData, column)}</td>`;
        });
      const latencyCell = effectiveLogsColumns.includes('latency_breakdown')
        ? ov
          ? html`<div role="cell" class=${`${cellBg} group-hover:bg-inherit pl-2 shrink-0 col-latency_breakdown`}>
              ${this.logItemCol(rowData, 'latency_breakdown')}
            </div>`
          : html`<td class=${`sticky right-0 max-md:static z-10 ${cellBg} group-hover:bg-inherit pl-2 shrink-0`}>
              ${this.logItemCol(rowData, 'latency_breakdown')}
            </td>`
        : nothing;
      const rowHtml = ov
        ? html`<div role="row" data-row-id=${rowData.id} class=${rowClass} style=${rowStyle} @click=${rowClick}>${cells}${latencyCell}</div>`
        : html`<tr data-row-id=${rowData.id} class=${rowClass} style=${rowStyle} @click=${rowClick}>
            ${cells}${latencyCell}
          </tr>`;
      return rowHtml;
    } catch (error) {
      console.error('logItemRow error:', error);
      return html`<tr>
        <td>Error rendering row: ${(error as Error).message}</td>
      </tr>`;
    }
  };

  tableHeadingWrapper(title: string, column: string, classes: string, width?: number) {
    const finalWidth = width || this.columnMaxWidthMap[column] || this.fixedColumnWidths[column];
    if (!finalWidth && column === 'latency_breakdown') {
      this.columnMaxWidthMap[column] = 120;
    }

    return html`
      <td
        class=${`cursor-pointer p-0 m-0 whitespace-nowrap relative flex justify-between items-center pl-2.5 pr-2 text-sm font-normal bg-bgBase ${classes} ${
          finalWidth ? `col-${column}` : ''
        }`}
      >
        <button
          class="font-medium text-base py-1 cursor-pointer"
          data-tippy-content=${title}
          aria-label="${title.split('•').reverse()[0]} column options"
          aria-haspopup="true"
          popovertarget=${`col-dropdown-${column}`}
          style=${`anchor-name: --col-dropdown-${column}`}
        >
          ${title.split('•').reverse()[0]}
          <span class="ml-1 p-0.5 border border-strokeWeak rounded-sm inline-flex">
            ${faSprite('chevron-down', 'regular', 'w-3 h-3')}
          </span>
        </button>
        <ul
          popover
          id=${`col-dropdown-${column}`}
          style=${`position-anchor: --col-dropdown-${column}`}
          class="dropdown menu flex flex-col font-normal bg-bgBase border w-64 border-strokeWeak p-2 text-sm rounded shadow"
        >
          <li class="px-1 cursor-pointer hover:bg-fillWeak">
            <button class="cursor-pointer py-0.5" @pointerdown=${() => this.hideColumn(column)}>Hide column</button>
          </li>
          <li class="px-1 cursor-pointer hover:bg-fillWeak">
            <button class="cursor-pointer py-0.5" @pointerdown=${() => this.moveColumn(column, -1)}>Move column left</button>
          </li>
          <li class="px-1 cursor-pointer hover:bg-fillWeak">
            <button class="cursor-pointer py-0.5" @pointerdown=${() => this.moveColumn(column, 1)}>Move column right</button>
          </li>
          ${column === 'latency_breakdown'
            ? (['service', 'kind'] as LatencyDim[]).map(
                dim => html`<li class="px-1 cursor-pointer hover:bg-fillWeak">
                  <button
                    class="cursor-pointer py-0.5"
                    aria-pressed=${this.latencyDim === dim}
                    @pointerdown=${() => this.setLatencyDim(dim)}
                  >
                    ${this.latencyDim === dim ? '✓ ' : ''}Break down by ${dim}
                  </button>
                </li>`
              )
            : nothing}
        </ul>
        <div
          @pointerdown=${(event: any) => {
            this.resizeTarget = column;
            this.mouseState = { x: event.clientX };
            document.body.style.userSelect = 'none';
            (event.target as HTMLElement).setPointerCapture?.(event.pointerId);
          }}
          class="w-3 text-textWeak text-right select-none hover:text-textBrand overflow-hidden font-bold absolute right-0 top-1/2 -translate-y-1/2 h-4 cursor-ew-resize"
        >
          |
        </div>
      </td>
    `;
  }

  // Single-line session row: user · URL · errors · events · duration · device · error
  // matches the dense log-row rhythm (28px). The user is the anchor (strongest
  // weight); the error badge keeps its color; everything else reads secondary.
  private renderSessionSummary(summaryArray: string[]): TemplateResult {
    const fields: Record<string, string> = {};
    for (const el of summaryArray) {
      const parsed = parseSummaryElement(el);
      if (parsed.type === 'formatted') {
        if (RIGHT_PREFIX_REGEX.test(parsed.style)) continue;
        fields[parsed.field] = parsed.value;
      } else {
        // Tolerate bare `field⇒value` (no style / no semicolon) — shouldn't
        // happen after the Haskell fix, but keeps the UI resilient if an
        // older emitter or cached payload sneaks through.
        const sep = parsed.content.indexOf('\u21d2');
        if (sep > 0) fields[parsed.content.slice(0, sep)] = parsed.content.slice(sep + 1);
      }
    }

    const { user, url, device, events, errors, duration, error: errorText } = fields;
    const sep = html`<span class="text-textWeak/40 select-none shrink-0" aria-hidden="true">·</span>`;
    // Bot traffic (curl, PostmanRuntime, HeadlessChrome, *bot*) is real but
    // rarely what a support engineer is scanning for — dim the whole row so
    // human sessions read first.
    const isBot = device ? isBotUserAgent(device) : false;

    const parts: TemplateResult[] = [];
    const add = (content: TemplateResult) => {
      if (parts.length) parts.push(sep);
      parts.push(content);
    };

    if (user) add(html`<span class="text-sm font-semibold text-textStrong shrink-0 truncate max-w-[24ch]">${user}</span>`);
    if (url) {
      // Middle-truncate: keep the last path segment visible since it usually
      // identifies the page ("/checkout/cart" is more useful than "/api/v2/…").
      const [head, tail] = middleTruncatePath(url);
      add(
        html`<span class="text-xs font-mono text-textStrong inline-flex items-center min-w-0" title=${url}
          >${head ? html`<span class="truncate min-w-0">${head}</span>` : nothing}<span class="shrink-0">${tail}</span></span
        >`
      );
    }
    if (errors) {
      add(
        html`<span class="cbadge-sm badge-error tabular-nums shrink-0" title="${errors} error${errors === '1' ? '' : 's'} in this session"
          >${errors} ${errors === '1' ? 'error' : 'errors'}</span
        >`
      );
    }
    if (events) add(html`<span class="text-xs text-textWeak tabular-nums shrink-0">${events} events</span>`);
    if (duration) add(html`<span class="text-xs text-textWeak tabular-nums shrink-0" title="session duration">${duration}</span>`);
    // duration moved to the latency column; device moved to the "client" column — both
    // pulled out of the summary body to declutter the identity line.
    // Cap error excerpt at a readable width — stack traces blown into the
    // row kill the single-line rhythm; the full text stays in the tooltip.
    if (errorText) add(html`<span class="text-xs text-textError truncate min-w-0 max-w-[60ch]" title=${errorText}>${errorText}</span>`);

    return html` <div class=${clsx('flex items-center gap-1.5 min-w-0 w-full overflow-hidden', isBot && 'opacity-60')}>${parts}</div> `;
  }

  // Error sessions reuse the neutral pill but swap the border to strokeError
  // as a quiet severity signal — no filled CTA, no scale change.
  createSessionButton = (sessionId: string, hasErrors: boolean = false) => html`
    <button
      class=${clsx(
        'inline-flex items-center justify-center shrink-0 self-center rounded-md cursor-pointer tooltip tooltip-left',
        'h-6 px-2 gap-1 shadow-sm transition-transform duration-150 ease-out hover:scale-105 active:scale-100',
        'motion-reduce:transition-none motion-reduce:hover:scale-100 text-textInverse-strong fill-textInverse-strong hover:brightness-110',
        // Primary action: solid fill, always visible, so replay reads as the thing to do.
        // One verb for both states; the error fill + tooltip carry the "this one broke" signal
        // so the action label stays a stable, non-ambiguous constant.
        hasErrors ? 'bg-fillError-strong' : 'bg-fillBrand-strong'
      )}
      data-tip=${hasErrors ? 'Replay — errors in this session' : 'Replay recording'}
      aria-label=${hasErrors ? 'Replay session with errors' : 'Replay session recording'}
      @click=${(e: any) => {
        e.stopPropagation();
        e.preventDefault();
      }}
      @pointerdown=${(e: any) => {
        e.stopPropagation();
        e.preventDefault();
        window.dispatchEvent(new CustomEvent('loadSessionReplay', { detail: { sessionId }, bubbles: true, cancelable: false }));
        let wrapper = this.sessionPlayerWrapper;
        if (!wrapper) {
          wrapper = document.querySelector('#sessionPlayerWrapper');
          this.sessionPlayerWrapper = wrapper;
        }
        if (wrapper) wrapper.classList.remove('hidden');
      }}
    >
      ${faSprite('play', 'solid', 'w-3 h-3')}
      <span class="text-xs font-medium">Replay</span>
    </button>
  `;

  renderCheckbox = (label: string, icon: string, checked: boolean, onChange: (checked: boolean) => void) => html`
    <label class="flex items-center cursor-pointer w-full gap-1 px-2 py-1 text-sm rounded text-textWeak hover:bg-fillWeaker">
      <input
        type="checkbox"
        class="checkbox checkbox-xs checkbox-primary mr-1"
        .checked=${checked}
        @change=${(e: any) => onChange(e.target.checked)}
      />
      ${faSprite(icon, 'regular', 'h-4 w-4')}
      <span class="sm:inline hidden">${label}</span>
    </label>
  `;

  options() {
    const viewButton = (view: 'tree' | 'list', icon: string, label: string) =>
      html` <button
        @pointerdown=${() => this.changeView(view)}
        aria-pressed=${this.view === view}
        aria-label="${label} view"
        class=${`flex items-center cursor-pointer justify-center gap-1 px-2 py-1 text-xs rounded ${
          this.view === view ? 'bg-fillWeak text-textStrong' : 'text-textWeak hover:bg-fillWeaker'
        }`}
      >
        ${faSprite(icon, 'regular', 'h-4 w-4')}
        <span class="sm:inline hidden">${label}</span>
      </button>`;

    if (this.mode === 'patterns') return html`<div class="border-b" style="border-color: var(--color-strokeWeak)"></div>`;

    return html`
      <div class="w-full flex justify-end px-2 gap-3">
        <div class="tabs tabs-box tabs-md p-0 tabs-outline items-center border">
          ${viewButton('tree', 'tree', 'Tree')} ${viewButton('list', 'list-view', 'List')}
        </div>

        <div class="relative dropdown dropdown-end">
          <button
            tabindex="0"
            role="button"
            aria-label="Log display options"
            aria-haspopup="true"
            class=${`flex cursor-pointer items-center justify-center gap-1 px-2 py-1 text-xs rounded text-textWeak hover:text-textStrong focus:bg-fillBrand-strong focus:text-white focus:fill-white`}
          >
            ${faSprite('gear', 'regular', `h-3 w-3`)}
            <span class="sm:inline hidden">Options</span>
          </button>
          <div tabindex="0" class="dropdown-content space-y-2 bg-bgBase border w-64 border-strokeWeak p-2 text-sm rounded shadow">
            ${this.renderCheckbox('Flip direction', 'flip-vertical', this.flipDirection, (checked) => {
              this.flipDirection = checked;
              // Just reverse the existing trees without rebuilding
              // Just reversing the existing trees causes trace tree to be upside down (when direction is flipped)
              this.spanListTree = this.buildSpanListTree(this.spanListTree.map((sp) => sp.data));
              this.recentDataToBeAdded = this.buildSpanListTree(this.recentDataToBeAdded.map((sp) => sp.data));
              if (this.recentDataToBeAdded.length > 0) {
                this.spanListTree = this.mergeIntoTree(this.recentDataToBeAdded, true);
                this.recentDataToBeAdded = [];
              }
              this.requestUpdate();
            })}
            ${this.renderCheckbox('Wrap lines', 'wrap-text', this.wrapLines, (checked) => {
              this.wrapLines = checked;
              if (this.wrapLines) {
                requestAnimationFrame(() => {
                  const container = this.logsContainer;
                  if (container) {
                    let availableWidth = container.offsetWidth;
                    // Subtract widths of all non-summary columns
                    this.logsColumns.forEach((col) => {
                      if (col !== 'summary') {
                        const width = this.columnMaxWidthMap[col] || this.fixedColumnWidths[col] || 0;
                        availableWidth -= width + 8; // 8px for padding
                      }
                    });
                    // Set a reasonable max width for summary column
                    this.columnMaxWidthMap['summary'] = Math.max(300, availableWidth - 40); // Min 300px, with 40px buffer
                    this.requestUpdate();
                  }
                });
              } else {
                // Reset to default wide width when wrap is disabled
                this.columnMaxWidthMap['summary'] = this.fixedColumnWidths['summary'];
                this.requestUpdate();
              }
            })}

            <columns-settings .columns=${this.logsColumns} @columns-changed=${this.handleColumnsChanged}></columns-settings>
          </div>
        </div>
      </div>
    `;
  }
}

@customElement('columns-settings')
class ColumnsSettings extends LitElement {
  @state() private showModal: boolean = false;
  @state() private searchTerm: string = '';
  @state() private showSearchResults: boolean = false;
  @state() private dragOverIndex: number | null = null;
  @state() private columns: string[] = [];

  private defaultColumns = [
    'trace_id',
    'severity_text',
    'parent_id',
    'errors',
    'kind',
    'span_name',
    'status',
    'start_time',
    'end_time',
    'duration',
    'timestamp',
    'service',
    'summary',
    'latency_breakdown',
  ];

  private dragIndex: number | null = null;

  createRenderRoot() {
    return this;
  }
  updated(changedProperties: Map<string, any>) {
    if (changedProperties.has('columns')) {
      const currentNames = new Set(this.defaultColumns);
      const merged = [...this.defaultColumns, ...this.columns.filter((c) => !currentNames.has(c))];
      this.defaultColumns = merged;
    }
  }

  render() {
    return html`
      <div tabindex="0" class="bg-bgBase w-full border-t border-t-strokeWeak p-2 pt-4 text-sm mt-4">
        <div class="relative mb-4">
          <span class="block mb-1 text-sm text-textStrong font-medium">Add column</span>
          <input
            type="text"
            placeholder="Search columns..."
            class="input input-xs w-full max-w-xs focus:outline-none focus:border-textBrand focus:ring-0"
            .value=${this.searchTerm || ''}
            @input=${(e: any) => {
              this.searchTerm = e.target.value;
            }}
          />

          ${this.searchTerm && this.searchTerm.length > 0
            ? html`
                <ul class="mt-1 w-full text-sm max-h-48 overflow-y-auto">
                  ${this.defaultColumns
                    .concat(this.columns)
                    .filter((col) => !this.columns.some((c) => c === col) && col.toLowerCase().includes(this.searchTerm.toLowerCase()))
                    .map(
                      (col) => html`
                        <li
                          class="px-1 py-0.5 hover:bg-fillWeak cursor-pointer"
                          @pointerdown=${() => {
                            let summaryIndex = this.columns.indexOf('summary');
                            if (summaryIndex === -1 || col === 'latency_breakdown') {
                              this.columns.push(col);
                            } else {
                              this.columns.splice(summaryIndex, 0, col);
                            }
                            this.searchTerm = '';
                            this._emitChanges();
                          }}
                        >
                          ${col}
                        </li>
                      `
                    )}
                  ${this.defaultColumns.filter(
                    (col) => !this.columns.some((c) => c === col) && col.toLowerCase().includes(this.searchTerm.toLowerCase())
                  ).length === 0
                    ? html`<li class="px-3 py-2 text-textWeak">No results</li>`
                    : ''}
                </ul>
              `
            : nothing}
        </div>

        <ul class="flex flex-col gap-1 py-2">
          ${this.columns.map(
            (col, index) => html`
              <li
                class=${`flex items-center group justify-between  px-1 py-0.5 rounded ${
                  col === 'latency_breakdown' ? 'cursor-default select-none' : 'cursor-move hover:bg-fillWeak'
                } ${this.dragOverIndex === index ? 'border border-strokeBrand-strong' : ''}`}
                draggable=${col === 'latency_breakdown' ? 'false' : 'true'}
                @dragstart=${(e: any) => this._onDragStart(e, index)}
                @dragover=${(e: any) => this._onDragOver(e, index)}
                @drop=${(e: any) => this._onDrop(e, index)}
              >
                <span class="text-textStrong">${col}</span>
                <div class="flex items-center gap-2">
                  <button class="hidden group-hover:inline-block cursor-pointer" @pointerdown=${() => this._removeColumn(index)}>
                    ${faSprite('trash-can', 'regular', 'h-3 w-3 text-iconNeutral fill-iconError')}
                  </button>
                  ${faSprite('grip-dots-vertical', 'regular', 'h-4 w-4 text-iconNeutral')}
                </div>
              </li>
            `
          )}
        </ul>
      </div>
    `;
  }

  _onDragOver(e: any, index: number) {
    e.preventDefault();
    if (this.columns[index] === 'latency_breakdown' || index === this.dragIndex) {
      this.dragOverIndex = null;
      return;
    }

    this.dragOverIndex = index;
  }

  _removeColumn(index: number) {
    this.columns.splice(index, 1);
    this._emitChanges();
  }

  _onDragStart(e: any, index: number) {
    this.dragIndex = index;
  }

  _onDrop(e: any, index: number) {
    if (index === this.dragIndex || !this.dragIndex) return;
    if (index === this.columns.length - 1 && this.columns[index] === 'latency_breakdown') return;
    const dragged = this.columns[this.dragIndex];
    if (dragged === 'latency_breakdown') return;
    this.columns.splice(this.dragIndex, 1);
    this.columns.splice(index, 0, dragged);
    this.dragIndex = null;
    this.dragOverIndex = null;
    this._emitChanges();
  }

  _emitChanges() {
    this.dispatchEvent(
      new CustomEvent('columns-changed', {
        detail: this.columns,
        bubbles: true,
        composed: true,
      })
    );
  }
}

const isSyntheticRowId = (id: unknown): id is string => typeof id === 'string' && id.startsWith('synthetic-');

// Click-to-copy chip for synthetic-row parent ids. Briefly swaps the copy
// icon for a checkmark on success — confirmation without a toast.
function renderCopyIdChip(fullId: string) {
  const onClick = async (e: MouseEvent) => {
    e.stopPropagation();
    e.preventDefault();
    const btn = e.currentTarget as HTMLButtonElement;
    try {
      await navigator.clipboard.writeText(fullId);
      btn.classList.add('copied');
      setTimeout(() => btn.classList.remove('copied'), 1200);
    } catch (err) {
      console.warn('clipboard.writeText failed for parent id:', err);
    }
  };
  const short = fullId.length > 10 ? fullId.slice(0, 8) + '…' : fullId;
  return html`<button
    type="button"
    class="group/copy inline-flex items-center gap-1 px-1.5 py-px ml-1 rounded border border-strokeWeak text-xs font-mono text-textWeak hover:text-textStrong hover:border-strokeStrong cursor-copy transition-colors [&.copied_.copy-icon]:hidden [&.copied_.check-icon]:inline-flex"
    title="Copy full parent id: ${fullId}"
    aria-label="Copy parent id ${fullId}"
    @click=${onClick}
  >
    <span class="truncate max-w-[10ch]">${short}</span>
    <span class="copy-icon opacity-50 group-hover/copy:opacity-100" aria-hidden="true">${faSprite('copy', 'regular', 'w-3 h-3')}</span>
    <span class="check-icon hidden text-textSuccess" aria-hidden="true">${faSprite('check', 'regular', 'w-3 h-3')}</span>
  </button>`;
}

/**
 * Kind colours carry meaning rather than identity, so they are fixed rather than hashed:
 * work we ran ourselves reads as one family, work we waited on as another.
 */
const KIND_COLORS: Record<string, string> = {
  server: 'bg-fillBrand-strong',
  internal: 'bg-fillInformation-strong',
  client: 'bg-fillWarning-strong',
  producer: 'bg-fillWarning-strong',
  consumer: 'bg-fillSuccess-strong',
  log: 'bg-fillStrong',
};

export type LatencySegment = { leftPct: number; widthPct: number; color: string; label: string; ns: number };

/**
 * Children laid out inside a window, as a percentage of it.
 *
 * The window is the row's own duration for a collapsed row and the whole trace for an
 * expanded one — see `latencyBar`. Children are intersected with the window: a child whose
 * clock skewed outside it must not paint outside the bar, and a child that ends after its
 * parent is still time the parent waited on.
 */
export function latencySegments(
  row: { startNs: number; duration: number },
  children: { startNs: number; duration: number; label: string; color: string }[]
): LatencySegment[] {
  if (!(row.duration > 0)) return [];
  const rowEnd = row.startNs + row.duration;
  const segments: LatencySegment[] = [];
  for (const c of children) {
    // Interval intersection, not an offset clamp: a child whose clock skewed it entirely
    // outside its parent contributes nothing to the parent's window, and must not be pinned
    // to the start of the bar as though it happened there.
    const from = Math.max(row.startNs, c.startNs);
    const to = Math.min(rowEnd, c.startNs + Math.max(0, c.duration));
    if (to <= from) continue;
    segments.push({
      leftPct: ((from - row.startNs) / row.duration) * 100,
      widthPct: ((to - from) / row.duration) * 100,
      color: c.color,
      label: c.label,
      ns: to - from,
    });
  }
  return segments;
}

type Descendant = { startNs: number; duration: number; label: string; color: string; depth?: number };

/**
 * Where a row's time actually went, attributed exclusively.
 *
 * `latencySegments` paints each descendant in its own right, so nested spans overlap and the
 * same nanosecond is claimed by every ancestor of the span that spent it. For a summary that
 * is wrong twice over: the colours stack (the deepest, most specific span is painted under its
 * parents rather than over them) and the totals sum past the row. Here each instant belongs to
 * exactly one span — the deepest one covering it, i.e. the service actually doing the work
 * rather than the one waiting on it — and the leftovers are the row's own self time, which the
 * track shows through. Segments come out disjoint, ordered, and merged across equal neighbours,
 * so `latencyTitle` can sum them per label and get real per-service time.
 */
export function exclusiveSegments(row: { startNs: number; duration: number }, descendants: Descendant[]): LatencySegment[] {
  if (!(row.duration > 0)) return [];
  const rowEnd = row.startNs + row.duration;
  const spans = descendants
    .map(d => ({ from: Math.max(row.startNs, d.startNs), to: Math.min(rowEnd, d.startNs + Math.max(0, d.duration)), d }))
    .filter(s => s.to > s.from)
    .sort((a, b) => a.from - b.from);
  if (!spans.length) return [];

  const bounds = [...new Set(spans.flatMap(s => [s.from, s.to]))].sort((a, b) => a - b);
  const parts: { from: number; to: number; label: string; color: string }[] = [];
  let next = 0;
  let active: typeof spans = [];
  for (let i = 0; i < bounds.length - 1; i++) {
    const [from, to] = [bounds[i], bounds[i + 1]];
    while (next < spans.length && spans[next].from <= from) active.push(spans[next++]);
    active = active.filter(s => s.to > from);
    let win: (typeof spans)[number] | undefined;
    for (const s of active) if (!win || (s.d.depth ?? 1) > (win.d.depth ?? 1)) win = s;
    if (!win) continue;
    const prev = parts[parts.length - 1];
    if (prev && prev.to === from && prev.label === win.d.label && prev.color === win.d.color) prev.to = to;
    else parts.push({ from, to, label: win.d.label, color: win.d.color });
  }
  return parts.map(p => ({
    leftPct: ((p.from - row.startNs) / row.duration) * 100,
    widthPct: ((p.to - p.from) / row.duration) * 100,
    color: p.color,
    label: p.label,
    ns: p.to - p.from,
  }));
}

/**
 * The bar for one row: which track it sits on, and what paints over it.
 *
 * Expanding a trace turns the column back into a waterfall, which is the whole point of
 * expanding it — the child rows are a breakdown of one request, and a breakdown needs a
 * shared axis, so every row of an expanded trace draws its own span positioned in the trace
 * with the trace as the empty track — and only its direct children inside it, since every
 * deeper span is drawn by its own row just below. A collapsed row has no siblings to line up
 * with and no rows below to defer to, so it stays its own axis: full-width track (its self
 * time) under an exclusive breakdown of the whole subtree, which is the only place the time
 * spent in each service is visible at all.
 *
 * `frame` is the |---[]---| rule the trace axis needs and the row axis doesn't: it marks
 * where the trace begins and ends, which is what makes a short span read as short and
 * placed rather than just small. On the row axis the bar already fills that range, so the
 * same three lines would only restate the bar's own bounds.
 */
export function latencyBar(
  expanded: boolean,
  row: { startNs: number; duration: number; traceStart: number; traceEnd: number; label: string; color: string },
  descendants: Descendant[]
): { track: string; segments: LatencySegment[]; frame: boolean } {
  if (!(expanded && row.traceEnd > 0)) return { track: row.color, segments: exclusiveSegments(row, descendants), frame: false };
  const axis = { startNs: row.traceStart, duration: row.traceEnd };
  const direct = descendants.filter(c => (c.depth ?? 1) === 1);
  return { track: 'bg-fillWeak', segments: [rowMarker(axis, row), ...latencySegments(axis, direct)], frame: true };
}

/**
 * The row's own span as a mark on the trace axis.
 *
 * Unlike `latencySegments` this never drops the row. A log has a place in the trace but no
 * extent, and a row the axis no longer covers — the axis is sized by spans, so a log seconds
 * after the last one sits past its end — still happened. A row that renders nothing at all is
 * how "everything ignores its timing and stays at the beginning" reads. Position is clamped
 * into the window, so a late log pins to the end rather than escaping the bar.
 */
function rowMarker(
  axis: { startNs: number; duration: number },
  row: { startNs: number; duration: number; label: string; color: string }
): LatencySegment {
  const pct = (ns: number) => Math.min(100, Math.max(0, ((ns - axis.startNs) / axis.duration) * 100));
  const leftPct = pct(row.startNs);
  const ns = Math.max(0, row.duration);
  return { leftPct, widthPct: pct(row.startNs + ns) - leftPct, color: row.color, label: row.label, ns };
}

/** Human-readable nanoseconds, matching the duration badge's vocabulary. */
export const fmtNs = (ns: number): string =>
  ns >= 1e9 ? `${(ns / 1e9).toFixed(2)}s` : ns >= 1e6 ? `${Math.round(ns / 1e6)}ms` : ns >= 1e3 ? `${Math.round(ns / 1e3)}\u00b5s` : `${Math.round(ns)}ns`;

/**
 * What the bar is saying, in words — for the tooltip and for the screen reader, neither of
 * which can read a colour. Carries the trace offset the bar no longer encodes positionally.
 */
export function latencyTitle(dim: LatencyDim, row: { startNs: number; duration: number; traceStart: number }, segments: LatencySegment[]): string {
  const byLabel = new Map<string, number>();
  for (const s of segments) byLabel.set(s.label, (byLabel.get(s.label) ?? 0) + s.ns);
  const accounted = segments.reduce((a, s) => a + s.ns, 0);
  const self = Math.max(0, row.duration - accounted);
  const parts = [...byLabel.entries()].sort((a, b) => b[1] - a[1]).map(([label, ns]) => `${label} ${fmtNs(ns)}`);
  return [
    `${fmtNs(row.duration)} total`,
    `+${fmtNs(Math.max(0, row.startNs - row.traceStart))} into the trace`,
    `self ${fmtNs(self)}`,
    ...(parts.length ? [`by ${dim}: ${parts.join(', ')}`] : []),
  ].join(' \u00b7 ');
}

function spanLatencyBreakdown({
  track,
  segments,
  title,
  barWidth,
  frame,
}: {
  track: string;
  segments: LatencySegment[];
  title: string;
  barWidth: number;
  frame: boolean;
}) {
  // On the row axis the track IS the row's self time, so a gap between two children is time
  // the row spent on its own rather than an absence of information; on the trace axis it is
  // the rest of the trace, and the row's own span is the first segment painted on it.
  //
  // The floor is 3px rather than a percentage: a percentage floor of a short span is still
  // sub-pixel on a 120px column, which renders as nothing at all. Pushing left back by the
  // floored width keeps a mark at the far end inside the bar instead of clipped away.
  const minPct = (3 / Math.max(barWidth, 1)) * 100;
  return html`<div class="-mt-1 shrink-0" title=${title} aria-label=${title}>
    <div class=${`flex h-5 relative rounded-sm overflow-hidden ${track}`} style=${`width:${barWidth}px`}>
      ${segments.map(s => {
        const width = Math.max(s.widthPct, minPct);
        return html`<div
          class=${`h-full absolute top-0 rounded-sm ${s.color}`}
          style=${`left:${Math.min(s.leftPct, 100 - width)}%; width:${width}%`}
        ></div>`;
      })}
      <!-- |---[]---| : the trace's own start and end, and the timeline between them. Without it
           a span two thirds of the way into a trace is just a small block somewhere. -->
      ${frame
        ? html`<div class="absolute inset-0 pointer-events-none">
            <div class="absolute top-0 left-0 h-full border-l-2 border-strokeBrand-strong shadow-[0_0_4px_var(--color-strokeBrand-weak)]"></div>
            <div class="absolute top-0 right-0 h-full border-r-2 border-strokeBrand-strong shadow-[0_0_4px_var(--color-strokeBrand-weak)]"></div>
            <div class="absolute top-1/2 -translate-y-1/2 left-0 w-full h-px bg-strokeBrand-strong shadow-[0_0_2px_var(--color-strokeBrand-weak)]"></div>
          </div>`
        : nothing}
    </div>
  </div>`;
}

// Fallback column set used only when logsColumns hasn't loaded yet, so the
// skeleton still resembles the real table (narrow id stripe → fields → latency).
const SKELETON_FALLBACK_COLUMNS = ['id', 'timestamp', 'service', 'summary', 'latency_breakdown'];

const skeletonColumns = (columns: string[]) => (columns.length ? columns : SKELETON_FALLBACK_COLUMNS);

// A skeleton cell mirrors the real column layout by name (col-id is the narrow
// stripe, latency_breakdown is sticky-right) so header pills line up over rows.
const skeletonCell = (column: string) => {
  const isId = column === 'id';
  const isLatency = column === 'latency_breakdown';
  const classes = clsx('bg-bgBase relative pl-2', isId ? 'w-3' : getColumnWidth(column), isLatency && 'z-10');

  if (isId) {
    return html`<td class=${classes}>
      <div class="w-1 h-5 bg-fillBrand-strong opacity-20 rounded-full skeleton-glow"></div>
    </td>`;
  }

  return html`<td class=${classes}>
    <div class="relative overflow-hidden">
      <div class="h-4 rounded skeleton-shimmer skeleton-wave ${isLatency ? 'w-24' : 'w-3/4'}"></div>
      ${isLatency ? html`<div class="absolute right-0 top-0 h-full w-16 bg-gradient-to-r from-transparent to-bgBase"></div>` : nothing}
    </div>
  </td>`;
};

const skeletonRow = (rowIdx: number, columns: string[]) => html`
  <tr class="item-row relative p-0 flex items-center group whitespace-nowrap" style="--row-index: ${rowIdx}">
    ${columns.map((c) => skeletonCell(c))}
  </tr>
`;

function loadingSkeleton(columns: string[]) {
  const cols = skeletonColumns(columns);
  return html`
    <tbody class="min-w-0 text-xs">
      <tr class="w-full flex justify-center">
        <td colspan=${String(cols.length)} class="w-full">
          <p class="text-sm text-textWeak text-center py-3">Loading events...</p>
        </td>
      </tr>
      ${map(Array(10), (_, rowIdx) => skeletonRow(rowIdx, cols))}
    </tbody>
  `;
}

function errorState(cols: number, message: string, onRetry: () => void) {
  return html`
    <tr class="w-full flex justify-center">
      <td colspan=${String(cols)} class="w-full mx-auto">
        <div class="max-w-full mx-auto my-8 text-center p-5 sm:py-10 sm:px-24 flex flex-col gap-3 items-center">
          ${faSprite('circle-exclamation', 'regular', 'h-10 w-10 stroke-strokeError fill-fillError-strong opacity-70')}
          <h2 class="text-lg text-textStrong font-semibold">Failed to load events</h2>
          <p class="text-sm text-textWeak max-w-sm">${message}</p>
          <button class="btn btn-sm btn-ghost border border-strokeWeak mt-1" @click=${onRetry}>
            ${faSprite('arrow-rotate-right', 'regular', 'h-3.5 w-3.5')} Retry
          </button>
        </div>
      </td>
    </tr>
  `;
}

function emptyState(cols: number) {
  let title = `No events found`;
  let subText = `No results matched your query, or this project hasn't received any events yet.`;
  return html`
    <tr class="w-full flex justify-center">
      <td colspan=${String(cols)} class="w-full mx-auto">
        <div class="max-w-full mx-auto my-8 text-center p-5 sm:py-14 sm:px-24 flex flex-col gap-4">
          <div class="relative">
            <div class="absolute inset-0 -m-8">
              <div class="w-full h-full rounded-full bg-gradient-to-b from-fillBrand-weak to-transparent opacity-20 blur-xl"></div>
            </div>
            <div class="relative">
              ${faSprite('empty', 'regular', 'h-24 w-24 mx-auto stroke-strokeBrand-strong fill-fillBrand-strong opacity-80')}
            </div>
          </div>
          <div class="flex flex-col gap-3">
            <h2 class="text-2xl text-textStrong font-bold">${title}</h2>
            <p class="text-sm max-w-md font-medium text-textWeak leading-relaxed">${subText}</p>
            <a href="https://monoscope.tech/docs/sdks/" target="_BLANK" class="btn text-sm w-max mx-auto btn-primary border-0">
              Read integration guides
            </a>
          </div>
        </div>
      </td>
    </tr>
  `;
}
function requestDumpLogItemUrlPath(rd: any[], colIdxMap: ColIdxMap, source: string): [string, string, string] {
  const rdId = lookupVecValue<string>(rd, colIdxMap, 'id');
  const rdCreatedAt = lookupVecValue<string>(rd, colIdxMap, 'created_at') || lookupVecValue<string>(rd, colIdxMap, 'timestamp');
  return [rdId, rdCreatedAt, source]; // Source parameter is preserved for future use
}
