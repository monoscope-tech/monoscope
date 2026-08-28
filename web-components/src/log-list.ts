'use strict';
import '@lit-labs/virtualizer';
import { FlowLayout } from '@lit-labs/virtualizer/layouts/flow.js';
import { LitElement, html, css, TemplateResult, nothing, render as renderLit } from 'lit';
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
  formatTimestampCompact,
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
  atInsertionEdge,
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

// Convert CSS token to hex for ECharts (which can't parse oklch).
//
// Memoised because getComputedStyle forces a style recalc and this is called from the chart
// mark-area update, which runs on every visibility change while the list is scrolling — it was
// measurably the second-largest source of forced layout on the page. Token values only change
// with the theme, so that (an attribute read, no recalc) is the cache key.
const cssHexCache = new Map<string, string>();
const cssTokenToHex = (token: string): string => {
  const key = `${document.body.getAttribute('data-theme') ?? ''}:${token}`;
  let hex = cssHexCache.get(key);
  if (hex === undefined) {
    hex = toEChartsColor(getComputedStyle(document.body).getPropertyValue(token).trim());
    cssHexCache.set(key, hex);
  }
  return hex;
};

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
// `index` is the row's position in virtualListItems, so a reader whose anchor row is cut
// by the retention window can still be put back near where they were.
type ScrollAnchor = { id: string; offset: number; index: number; scrollTop: number };
type RecentDelivery = 'manual' | 'auto-refresh';

/**
 * Identity of a virtual row. Without it the virtualizer keys rows by index, so a live-tail
 * batch prepended at the top re-renders every visible row's cells — the whole list repaints
 * and re-measures on each tick instead of the existing rows simply moving down.
 */
// `'id' in item` and not `'type' in item`: an EventLine carries a `type` of its own ('log' | 'span').
export const virtualItemKey = (item: VirtualListItem) =>
  'id' in item ? item.id : item.type === 'aggregateChildren' ? `aggregateChildren:${item.parentKey}` : item.type;

// Retained rows are the scroll cost: measured at 4x CPU throttle, an identical scroll workload
// blocks the main thread 4767ms at 5000 rows and 3660ms at 2500 (400 rows only reaches 2915ms,
// so the rest is per-scroll work that shrinking the window cannot buy back). Evicting reopens
// the edge — mergeIntoTree sets hasMore/hasNewer — so a smaller window costs a refetch when you
// scroll back past it, not access to the rows.
export const MAX_RETAINED_ROWS = 2500;
// Matches Tailwind's `md` breakpoint, so the JS-side column switch and the CSS-side
// `max-md:` rules flip at the same width instead of disagreeing in a 1px band.
const NARROW_VIEWPORT = '(max-width: 767px)';
const NARROW_COLUMNS = ['id', 'timestamp', 'created_at', 'service', 'summary'];
// Start one history request before the virtualizer mounts its load-more sentinel. Forty dense
// rows are ~1120px: enough runway to hide ordinary network latency without fetching pages the
// user has not approached. This is a range comparison only; it performs no layout reads.
export const HISTORY_PREFETCH_ROWS = 40;

// Who can ask the list to refetch. Two targets because the dispatchers disagree: the time
// picker triggers on document, the dashboard auto-refresh and variable fallback on window.
const UPDATE_QUERY_TARGETS: EventTarget[] = [window, document];

// FlowLayout starts at 100px until it observes rows. Log rows are 28px, so the
// initial estimate inflated the virtual scroll range roughly 3.5×.
const DENSE_ROW_HEIGHT = 28;
export class DenseRowFlowLayout extends FlowLayout {
  constructor(...args: ConstructorParameters<typeof FlowLayout>) {
    super(...args);
    this._itemSize.height = DENSE_ROW_HEIGHT;
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
  private resumeBufferedRecentAtEdge = false;
  @state() private view: 'tree' | 'list' = 'tree';
  @state() private shouldScrollToBottom: boolean = false;
  @state() private logsColumns: string[] = [];
  @state() private wrapLines: boolean = false;
  @state() private hasMore: boolean = true;
  @state() private hasNewer: boolean = false;
  /**
   * Bumped whenever the retention window evicts rows, to remount the virtualizer.
   *
   * Shrinking `.items` leaves lit-virtualizer's layout holding a scroll size for the old,
   * longer list (measured: 79989px against a true 73437px). Scroll positions near the end then
   * map past the last item, the computed range comes back empty, and the list renders *nothing*
   * — permanently, since every later scroll lands in the same dead zone. It looked to users like
   * the log explorer went blank and only a page reload brought it back.
   *
   * Remounting is what resyncs it; the merge paths already capture a scroll anchor and restore
   * it immediately after, so the user keeps their place across the swap.
   */
  @state() private virtualizerEpoch = 0;
  /**
   * Number of reasons the list's scroll position is still in motion.
   *
   * A retention eviction remounts the virtualizer, and a freshly mounted one reports a
   * zero-height scroll range until it lays out. For that frame the browser clamps
   * scrollTop to 0 and *both* edge sentinels sit inside the viewport at once — so the
   * "Load newer events" row at the top fired while the reader was paging history at the
   * bottom, and its reveal threw them back to the top mid-read.
   *
   * Every automatic fetch (the two sentinel observers and the proximity prefetch) is
   * therefore suspended from the remount until the layout and any scroll restoration have
   * landed. Explicit clicks are never suspended: the reader asked.
   */
  private scrollSettling = 0;
  // Which end the retention window last cut, and how many rows went with it, so a reader
  // whose anchor row was among them can still be put back. null/0 when the last merge
  // retained everything.
  private evictedEdge: 'start' | 'end' | null = null;
  private evictedCount = 0;
  private get isRepositioning() {
    return this.scrollSettling > 0;
  }
  private holdRepositioningForRemount() {
    this.scrollSettling++;
    // Two frames: one for the keyed remount to render, one for the new virtualizer to lay out.
    requestAnimationFrame(() => requestAnimationFrame(() => this.scrollSettling--));
  }
  @state() private expandTimeRange: boolean = true;
  @state() private loadedCount: number = 0;
  @state() private totalCount: number = 0;
  @state() private hasChartCount: boolean = false;
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
  // Monotonic id for detail-panel loads, so a superseded request can tell it lost the race.
  private detailRequestSeq = 0;
  private readonly serviceTimePopoverId = `service-time-breakdown-${generateId()}`;

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
  // Fingerprint of the query/columns the active stream was registered for. A full fetch can
  // change either while the toggle stays on; rows from the old subscription must never leak
  // into the replacement result set.
  private liveStreamKey: string | null = null;
  @state() private liveDropped = 0;
  // Phone-width layout is a different table, not a scaled one — see displayColumns.
  @state() private isNarrow = window.matchMedia(NARROW_VIEWPORT).matches;
  private narrowQuery = window.matchMedia(NARROW_VIEWPORT);
  private onNarrowChange = (e: MediaQueryListEvent) => (this.isNarrow = e.matches);
  // The active row of the grid keyboard model. Focus stays on the <table> and this drives
  // aria-activedescendant plus the active outline: lit-virtualizer recycles row nodes, so
  // DOM focus placed on a row dies with the node as soon as the list scrolls.
  @state() private focusedRowId: string | null = null;
  // The row whose detail panel is open. Rendered from state rather than by adding a class
  // in the click handler: the virtualizer destroys a row's element when it scrolls out of
  // the runway, so an imperative class was lost the moment the reader scrolled away and
  // back — during an incident, losing track of which row you are reading.
  @state() private openRowId: string | null = null;
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
  private debouncedLoadMore: ReturnType<typeof debounce>;
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
    if (source === 'auto-refresh' && !this.initialFetchUrl && !this.isAggregate) {
      void this.fetchData(this.buildRecentFetchUrl(), false, true, false, false, 'auto-refresh');
      return;
    }
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
    this.liveStreamKey = null;
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
    this.liveStreamKey = null;
  };
  private isCalculatingWidths: boolean = false;
  private lastVisibilityRange: { first: number; last: number } | null = null;
  /**
   * Previous visible range, used only to tell a scroll toward history from a merge.
   *
   * Kept apart from lastVisibilityRange (which drives the chart mark area and is
   * seeded synthetically) because a merge renumbers rows: prepending newer rows
   * raises `last` while the reader sits still, and reading that as movement made the
   * prefetch cascade through pages nobody had scrolled to. updateVisibleItems clears
   * it whenever the row count changes, so movement is only ever measured between two
   * events that describe the same list.
   */
  private prefetchBaseline: { first: number; last: number } | null = null;
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
    // The range prefetch and the sentinel can observe the same scroll. Delay the sentinel as
    // a fallback, but discard it if the history edge changed while it waited — otherwise a
    // fast range response completes before this fires and one gesture consumes two pages.
    this.debouncedLoadMore = debounce((historyWindow: string) => {
      if (
        historyWindow === this.historyWindowKey() &&
        this.hasMore &&
        !this.isLoading &&
        !this.isLoadingMore &&
        !this.isRepositioning &&
        this.isConnected
      ) {
        void this.fetchData(this.buildLoadMoreUrl(), false, false, true);
      }
    }, 300);
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

    // Both targets: the time picker triggers on `document`, while the dashboard
    // auto-refresh timer and the variable fallback dispatch on `window` — and a
    // window-dispatched event never reaches a document listener, so an embedded logs
    // widget sat frozen while every chart around it refreshed. An event that reaches
    // both fires the handler twice; refetchLogs is debounced, so that collapses to one.
    UPDATE_QUERY_TARGETS.forEach((t) => t.addEventListener('update-query', this.handleUpdateQuery));

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
  private liveSubscriptionBody() {
    const url = new URL(window.location.href);
    return {
      // No service gate on Events: it streams whatever the query says, bounded by the
      // server's per-connection queue rather than refused up front.
      all_signals: true,
      query: url.searchParams.get('query') || null,
      columns: Object.keys(this.colIdxMap ?? {}),
    };
  }

  private liveSubscriptionKey() {
    return JSON.stringify(this.liveSubscriptionBody());
  }

  private async startLiveStream() {
    const body = this.liveSubscriptionBody();
    const key = JSON.stringify(body);
    this.liveStream?.stop();
    this.liveDropped = 0;
    let stream: LiveStream;
    stream = new LiveStream({
      projectId: this.projectId,
      leaseSecs: 45,
      body: () => body,
      onRows: (rows) => {
        if (this.liveStream === stream) this.handleLiveRows(rows);
      },
      onDropped: (total) => {
        if (this.liveStream !== stream) return;
        this.liveDropped = total;
        this.requestUpdate();
      },
      onState: (state, detail) => {
        if (this.liveStream === stream && (state === 'expired' || state === 'error')) this.stopLiveStream(detail);
      },
    });
    this.liveStream = stream;
    this.liveStreamKey = key;
    await stream.start();
    // A query change can replace this stream while registration is in flight. The old
    // registration owns no UI and must not reopen beside its replacement.
    if (this.liveStream !== stream) stream.stop();
  }

  private syncLiveSubscription() {
    if (!this.isLiveStreaming || this.isAggregate || this.liveSubscriptionKey() === this.liveStreamKey) return;
    this.liveStream?.stop();
    this.liveStream = null;
    this.liveStreamKey = null;
    void this.startLiveStream();
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
    tree.forEach((t) => (t.isNew = true));
    this.fetchedNew = true;

    // The container decides *where* the row goes, never *whether* it arrives. Returning early
    // when it is missing (before first paint, or while the list is detached) would drop pushed
    // rows on the floor — and unlike a fetch, there is no cursor to re-request them with.
    const container = this.logsContainer;
    const scrollTop = container?.scrollTop ?? 0;
    const scrolledToBottom = container ? scrollTop + container.clientHeight >= container.scrollHeight - 1 : true;
    // This flag only drives oldest-first's "jump to newest" affordance. Setting it while
    // newest-first pagination is parked at the bottom creates an irrelevant reactive update
    // when a row click clears it, and that full rerender can empty a deep virtualizer runway.
    if (this.flipDirection && scrolledToBottom) this.shouldScrollToBottom = true;
    // Same rule as a recent fetch: a user who has scrolled away gets a "N new" pill rather
    // than having the viewport yanked out from under them mid-read.
    if (container && shouldBufferRecent(this.isLiveStreaming, scrollTop, scrolledToBottom, this.flipDirection)) {
      this.recentDataToBeAdded = this.addWithFlipDirection(this.recentDataToBeAdded, tree, true);
    } else {
      // Reaching here means the viewport is parked at the edge the rows arrive at (that is
      // what shouldBufferRecent decided), so the row under the user's eye is the one being
      // pushed down on purpose. Anchoring would scroll the previous top row back into view
      // every tick — a visible bounce that also hides the rows just streamed in.
      const anchor = container && !atInsertionEdge(scrollTop, scrolledToBottom, this.flipDirection) ? this.captureScrollAnchor() : null;
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

  private historyWindowKey(): string {
    return `${this.fetchGeneration}:${this.spanListTree.length}:${this.spanListTree[0]?.id ?? ''}:${this.spanListTree.at(-1)?.id ?? ''}`;
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
    this.isNarrow = this.narrowQuery.matches;
    this.narrowQuery.addEventListener('change', this.onNarrowChange);
    window.addEventListener('chart-updated', this.handleChartCountUpdate);
    // Watchdog for the stuck-blank virtualizer. It has to be a timer: the failure renders
    // nothing, so no update and no scroll event follows it to hang a check on. One
    // querySelector every two seconds is far cheaper than leaving a user staring at an empty
    // list that only a page reload will fix.
    this.blankWatchdog = setInterval(() => this.healBlankVirtualizer(), 2000);
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
      countText = formatLargeCount(this.hasChartCount ? this.totalCount : this.loadedCount);
      suffixText = !this.hasChartCount && this.hasMore ? '+ rows' : ' rows';
    }
    countEl.textContent = countText;
    if (suffixEl) suffixEl.textContent = suffixText;
    if (mobileCount) mobileCount.textContent = countText;
    if (mobileSuffix) mobileSuffix.textContent = suffixText;
  }

  private handleChartCountUpdate = (event: Event) => {
    const { chartId, total } = (event as CustomEvent<{ chartId?: string; total?: number }>).detail ?? {};
    if (this.mode !== 'logs' || chartId !== 'log-explorer-all-traces' || typeof total !== 'number' || !Number.isFinite(total)) return;
    this.totalCount = total;
    this.hasChartCount = true;
    this.updateRowCountDisplay();
  };

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

  /**
   * Last-resort recovery for a virtualizer that has stopped rendering entirely.
   *
   * Reachable once the retention window starts evicting: lit-virtualizer settles into a state
   * where it renders no rows at all while `items` is non-empty, and it never comes back on its
   * own — every subsequent scroll lands in the same dead range. To a user the log list simply
   * goes blank and stays blank until they reload the page, which is exactly what was reported.
   *
   * Resetting the scroll origin is what forces the layout to re-measure; the user is then
   * returned to the row they were reading. Bounded per eviction so a persistent failure degrades
   * to "list is at the top" rather than a scroll fight.
   */
  private blankWatchdog: ReturnType<typeof setInterval> | null = null;
  private blankHealAttempts = 0;
  private blankHealEpoch = -1;
  private blankHealAt = 0;
  private detailResizeHealTimer: ReturnType<typeof setTimeout> | null = null;
  private healBlankVirtualizer() {
    // Driven by a 2s watchdog interval, so it can fire against a list that has since been
    // removed (tab switch, HTMX swap). A detached list has no viewport to heal, and the
    // scroll nudge below would be measuring and mutating a node nobody is looking at.
    if (!this.isConnected) return;
    const virtualizer = this.querySelector('lit-virtualizer');
    const container = this.logsContainer;
    if (!virtualizer || !container || this.isLoading || this.virtualListItems.length === 0) return;

    // A stale layout can leave rows mounted but position every one outside the viewport. The
    // old `querySelector('tr')` check called that healthy even though the user saw an empty
    // list; scrolling upward happened to bring those misplaced rows back. Require a real data
    // row intersecting the viewport instead. Sentinel rows do not prove log content is visible.
    const viewport = container.getBoundingClientRect();
    const hasVisibleDataRow = [...virtualizer.querySelectorAll<HTMLElement>('[data-row-id]')].some((row) => {
      const rect = row.getBoundingClientRect();
      return rect.bottom > viewport.top && rect.top < viewport.bottom;
    });
    if (hasVisibleDataRow) {
      this.blankHealAttempts = 0;
      return;
    }
    // Budget is per eviction, not for the life of the component: the empty frames that occur
    // *during* an eviction would otherwise spend it before the user ever sees the stuck state.
    if (this.blankHealEpoch !== this.virtualizerEpoch) {
      this.blankHealEpoch = this.virtualizerEpoch;
      this.blankHealAttempts = 0;
    }
    const now = performance.now();
    if (this.blankHealAttempts >= 3 || now - this.blankHealAt < 500) return;
    this.blankHealAttempts++;
    this.blankHealAt = now;
    // A real one-pixel scroll reliably makes lit-virtualizer recompute its runway. Preserve
    // the user's deep position: resetting to zero made the list reappear at the newest edge
    // and could leave the intended range blank until the user scrolled manually.
    const originalScrollTop = container.scrollTop;
    const nudgedScrollTop = originalScrollTop > 0 ? originalScrollTop - 1 : originalScrollTop + 1;
    container.scrollTop = nudgedScrollTop;
    requestAnimationFrame(() => {
      if (this.isConnected) container.scrollTop = originalScrollTop;
    });
  }

  updated(changedProperties: Map<string, any>) {
    // Deferred twice: once for the virtualizer to render off this update, once more so a
    // legitimate mid-update empty frame is not mistaken for the stuck state.
    requestAnimationFrame(() => requestAnimationFrame(() => this.healBlankVirtualizer()));
    // An eviction is the one moment the virtualizer is known to get stuck, and once stuck it
    // renders nothing, so nothing changes and no further update ever arrives to notice. Give it
    // time to settle, then check on a timer rather than waiting for a render that never comes.
    if (changedProperties.has('virtualizerEpoch')) setTimeout(() => this.healBlankVirtualizer(), 400);

    // Stop live streaming when switching to an aggregate view
    if (changedProperties.has('mode') && this.isAggregate && this.liveStream) {
      this.liveStream.stop();
      this.liveStream = null; // else handleLiveToggle's isRunning guard skips restart on switch-back
      this.liveStreamKey = null;
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
    // Deferred a frame to batch the write — so the pin is re-read *here*, not at the call
    // site. A render while the list was pinned queued this; by the time it ran the reader
    // could have scrolled up to page history, and the stale frame dragged them back down.
    requestAnimationFrame(() => {
      if (this.logsContainer && this.shouldScrollToBottom) this.logsContainer.scrollTop = this.logsContainer.scrollHeight;
    });
  }

  disconnectedCallback() {
    if (this.blankWatchdog) {
      clearInterval(this.blankWatchdog);
      this.blankWatchdog = null;
    }
    if (this.detailResizeHealTimer) {
      clearTimeout(this.detailResizeHealTimer);
      this.detailResizeHealTimer = null;
    }
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
      this.liveStreamKey = null;
    }
    if (this.scrollEndTimer) {
      clearTimeout(this.scrollEndTimer);
      this.scrollEndTimer = null;
    }
    if (this.initChartsTimer) {
      clearTimeout(this.initChartsTimer);
      this.initChartsTimer = null;
    }
    this.debouncedLoadMore.cancel();

    // Clean up event listeners
    window.removeEventListener('pointermove', this.boundHandleResize);
    if (this.handleMouseUp) {
      window.removeEventListener('pointerup', this.handleMouseUp);
    }
    ['submit', 'add-query'].forEach((ev) => window.removeEventListener(ev, this.debouncedRefetchLogs));
    document.removeEventListener('submit', this.handleFormSubmit);
    UPDATE_QUERY_TARGETS.forEach((t) => t.removeEventListener('update-query', this.handleUpdateQuery));
    this.liveBtn?.removeEventListener('change', this.handleLiveToggle);
    this.liveBtn = null;
    window.removeEventListener('pagehide', this.handlePageHide);
    window.removeEventListener('chart-updated', this.handleChartCountUpdate);

    // Clean up chart event handlers
    if (this.barChart) {
      this.barChart.off('datazoom', this.handleChartZoom);
    }
    if (this.lineChart) {
      this.lineChart.off('datazoom', this.handleChartZoom);
    }

    // Note: Caches in renderSummaryElements closure will be garbage collected
    // when the component is destroyed
    this.narrowQuery.removeEventListener('change', this.onNarrowChange);

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

    if (virtualItems.length !== this.virtualListItems.length) this.prefetchBaseline = null;
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

  fetchData = async (
    url: string,
    isRefresh = false,
    isRecentFetch = false,
    isLoadMore = false,
    revealRecent = false,
    recentDelivery: RecentDelivery = 'manual'
  ) => {
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

    // The URL changes before its replacement fetch starts. Stop the old subscription now,
    // rather than letting old-query rows arrive during the request and briefly contaminate
    // the list. `finally` starts the correctly scoped stream once metadata/columns settle.
    const resubscribeLive =
      isFullFetch && this.isLiveStreaming && this.liveStreamKey !== null && this.liveStreamKey !== this.liveSubscriptionKey();
    if (resubscribeLive) {
      this.liveStream?.stop();
      this.liveStream = null;
      this.liveStreamKey = null;
    }

    if (isFullFetch) {
      this.hasChartCount = false;
    }

    this.showLoadingSpinner(true);

    try {
      const { tree, meta } = await this.transport(url);
      // Query-editor and time-picker initialization can issue a newer full fetch
      // while the head-preloaded request is still running. Full fetches therefore
      // use latest-request-wins too: never let an obsolete empty response replace
      // rows for the URL the charts have already adopted.
      if (gen !== this.fetchGeneration) return;
      this.fetchError = null;

      // A replacement query owns all query-scoped state even when it returns no rows. Doing
      // this only in the non-empty branch left the old live buffer/pill available on an empty
      // result page, ready to inject rows that did not match the new query.
      if (isRefresh) {
        this.expandedAggregates = {};
        this.hasNewer = false;
        this.recentDataToBeAdded = [];
        this.resumeBufferedRecentAtEdge = false;
        this.liveDropped = 0;
        this.cachedServerTraces = [];
      }

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
          if (meta.cols) this.logsColumns = meta.cols;
          if (meta.colIdxMap) this.colIdxMap = meta.colIdxMap;
          if (meta.count !== undefined) this.totalCount = meta.count;
          this.updateVisibleItems();
          this.updateRowCountDisplay();
        }
        return;
      }

      // A newer-direction response's hasMore describes its newest edge, not the history
      // edge rendered by the load-more row. Preserve history pagination across refresh ticks.
      if (!isRecentFetch) {
        this.hasMore = meta.hasMore !== false;
        if (!this.hasMore) this.expandTimeRange = true;
      }
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
          if (this.flipDirection && scrolledToBottom) this.shouldScrollToBottom = true;
          const bufferWhenAway = !revealRecent && (this.isLiveStreaming || recentDelivery === 'auto-refresh');
          if (shouldBufferRecent(bufferWhenAway, scrollTop, scrolledToBottom, this.flipDirection)) {
            this.recentDataToBeAdded = this.addWithFlipDirection(this.recentDataToBeAdded, tree, isRecentFetch);
            if (recentDelivery === 'auto-refresh') this.resumeBufferedRecentAtEdge = true;
          } else {
            const anchor =
              revealRecent || atInsertionEdge(scrollTop, scrolledToBottom, this.flipDirection) ? null : this.captureScrollAnchor();
            this.spanListTree = this.mergeIntoTree(tree, isRecentFetch);
            this.updateVisibleItems();
            if (anchor) void this.restoreScrollAnchor(anchor);
            else if (revealRecent) requestAnimationFrame(() => (container.scrollTop = this.flipDirection ? container.scrollHeight : 0));
          }
        }
      } else {
        const anchor = this.captureScrollAnchor() ?? loadMoreAnchor;
        const epochBeforeMerge = this.virtualizerEpoch;
        this.spanListTree = this.mergeIntoTree(tree, isRecentFetch);
        this.updateVisibleItems();
        // Newest-first load-more appends after the visible rows, so their geometry does not
        // move and forcing an anchor correction only makes the virtualizer fight the user's
        // downward scroll. We need to restore only when older rows are prepended (flipped
        // direction) or retention-window eviction remounts the virtualizer.
        const insertedBeforeVisibleRows = this.flipDirection;
        const remountedAfterEviction = this.virtualizerEpoch !== epochBeforeMerge;
        if (anchor && (insertedBeforeVisibleRows || remountedAfterEviction)) void this.restoreScrollAnchor(anchor);
        // No anchor and the retention window remounted the virtualizer: the reader is sitting
        // on the top the remount clamped them to, with nothing else on the way to move them
        // off it. The last rendered range is where they were — restore through the same
        // anchor-lost path, which shifts it by what the cut dropped.
        else if (remountedAfterEviction) {
          void this.restoreScrollAnchor({
            id: '',
            offset: 0,
            index: this.lastVisibilityRange?.first ?? 0,
            scrollTop: this.logsContainer?.scrollTop ?? 0,
          });
        }
      }
      // Count what's actually visible. queryResultCount over-counts because the
      // dedup-dropped boundary row is re-reported on every paginated page.
      this.loadedCount = this.spanListTree.length;
      this.updateRowCountDisplay();

      // Defer column width calculation. The isConnected guard matters because this runs
      // up to 2s later: a list removed in between (tab switch, HTMX swap) would otherwise
      // measure and lay out a detached element it no longer owns.
      const measure = () => {
        if (this.isConnected) this.updateColumnMaxWidthMap(tree.map((t) => t.data).filter(Boolean));
      };
      if ('requestIdleCallback' in window) (window as any).requestIdleCallback(measure, { timeout: 2000 });
      else setTimeout(measure, 100);
    } catch (error) {
      // A newer full fetch owns the UI now. Do not surface an error from the
      // obsolete request or replace the newer request's loading state.
      if (gen !== this.fetchGeneration) return;
      console.error(error);
      // An Error with an empty message (a bare `new Error()`, some DOM exceptions) took the
      // first branch and produced a blank toast — a failure the reader is shown nothing about.
      const msg = (error instanceof Error && error.message) || 'Network error';
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
      if (isFullFetch && gen === this.fetchGeneration && (resubscribeLive || this.liveStream !== null)) this.syncLiveSubscription();
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
    const listWasDeepScrolled = (this.logsContainer?.scrollTop ?? 0) > 0;

    // Batch DOM reads and writes
    requestAnimationFrame(() => {
      const width = sideView.offsetWidth;
      // Newest-first never consumes this flag. Mutating it there caused a full host rerender
      // on row click after load-more, which is the direct trigger for the blank runway.
      if (this.flipDirection) this.shouldScrollToBottom = false;

      if (width < 50) {
        sideView.style.width = `550px`;
        updateUrlState('details_width', '550');
      }

      // Always show the resizer when a log row is clicked
      if (resizerWrapper) {
        resizerWrapper.classList.remove('hidden', 'opacity-0', 'pointer-events-none');
      }

      // Opening the details pane changes the list's width. At a deep virtual scroll offset,
      // lit-virtualizer can briefly resolve that resize to an empty rendered range. A click
      // does not otherwise schedule a Lit update, so the ordinary post-update health check
      // never runs and the user sees a blank flash until the watchdog notices. Check on the
      // next frame and once more after ResizeObserver/layout work has settled. The latter is
      // necessary because a deep stale range can still contain off-viewport <tr> nodes during
      // the first check and become visibly blank only after the virtualizer processes the width.
      if (width < 50 && listWasDeepScrolled) {
        requestAnimationFrame(() => this.healBlankVirtualizer());
        if (this.detailResizeHealTimer) clearTimeout(this.detailResizeHealTimer);
        this.detailResizeHealTimer = setTimeout(() => {
          this.detailResizeHealTimer = null;
          this.healBlankVirtualizer();
        }, 100);
      }
    });

    // One assignment, and the template does the rest. The previous approach searched the
    // DOM for the last marked row, which also matched the latency bar *inside* a row (same
    // class) — stripping its colour while leaving the old row marked — and could not
    // survive the virtualizer recycling the row's element.
    // Re-query rather than reuse a cached ref: the indicator is rendered *inside* the container
    // this request innerHTML-swaps, so every response replaces the node. A ref captured on an
    // earlier click points at a detached element, and clearing that leaves the live one spinning.
    const showIndicator = (on: boolean) => document.querySelector('#details_indicator')?.classList.toggle('htmx-request', on);
    showIndicator(true);

    const [rdId, rdCreatedAt, source] = targetInfo;
    this.openRowId = rdId;
    const url = `/p/${pid}/log_explorer/${rdId}/${rdCreatedAt}/detailed?source=${source}`;
    updateUrlState('target_event', `${rdId}/${rdCreatedAt}/detailed?source=${source}`);
    // Only the newest click owns the indicator: #log_details_container carries
    // hx-sync="this:replace", so clicking again aborts this request, and the loser settling
    // must not clear the loader out from under the winner still in flight.
    const seq = ++this.detailRequestSeq;
    // innerHTML, not morph: measured, the swap is ~7ms of a ~200ms click, and idiomorph's
    // in-place mutation means hyperscript never installs FieldMenuDelegate on the new
    // content, which silently kills the field context menu.
    void Promise.resolve(
      (window as any).htmx.ajax('GET', url, { target: '#log_details_container', swap: 'innerHTML', indicator: '#details_indicator' })
    )
      .catch(() => {})
      // A dropped, aborted or failed request still has to hand the loader back — that omission
      // is what stranded the panel on a frozen three-dot loader until a reload.
      .finally(() => seq === this.detailRequestSeq && showIndicator(false));
  };

  private preserveGridFocusOnPointerRowClick = (event: MouseEvent) => {
    // The table is the keyboard tab stop. Chrome's default action for mousedown on a plain
    // cell focuses that table and scrolls the enormous virtual table itself into view. Near
    // the pagination edge this clamps the scroll surface to its maximum before `click`, and
    // lit-virtualizer briefly resolves that offset to an empty range. A pointer click does not
    // need to move keyboard focus; genuine controls inside a row still retain normal focus.
    const target = event.target as HTMLElement;
    if (event.button !== 0 || target.closest('button, a, input, textarea, select, [contenteditable="true"]')) return;
    event.preventDefault();
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

  /**
   * Merge independently grouped pages by trace time.
   *
   * Ingest delivery is not timestamp delivery: an SDK retry or a slow service can send an
   * older event after newer rows are already visible. Concatenating every live batch at the
   * latest edge puts that row in the wrong place. Sorting individual rows would split expanded
   * traces, so contiguous trace groups move as one unit, keyed by the trace start used by the
   * worker's own ordering.
   */
  private mergeInTimeOrder(current: EventLine[], newData: EventLine[], isRecentFetch: boolean): EventLine[] {
    const groups: { rows: EventLine[]; time: number; position: number }[] = [];
    for (const row of this.orderMerge(current, newData, isRecentFetch)) {
      const previous = groups.at(-1);
      if (previous && previous.rows[0].traceId === row.traceId) previous.rows.push(row);
      else groups.push({ rows: [row], time: row.traceStart || row.startNs || 0, position: groups.length });
    }
    groups.sort((a, b) => {
      if (!a.time || !b.time || a.time === b.time) return a.position - b.position;
      return this.flipDirection ? a.time - b.time : b.time - a.time;
    });
    return groups.flatMap((group) => group.rows);
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
    // Cursor pagination is guaranteed to return the next history edge and can append without
    // touching visible geometry. Recent/live delivery is not timestamp-ordered, so only that
    // path pays to place delayed trace groups chronologically.
    const merged = isRecentFetch
      ? this.mergeInTimeOrder(this.spanListTree, fresh, true)
      : this.orderMerge(this.spanListTree, fresh, false);
    if (this.mode !== 'logs' || merged.length <= MAX_RETAINED_ROWS) {
      this.evictedEdge = null;
      this.evictedCount = 0;
      return merged;
    }

    // Evict from the edge opposite the fetch. Move the cut past a trace boundary
    // so a root and its children are never split across retained/evicted state.
    const dropStart = this.flipDirection === isRecentFetch;
    this.evictedEdge = dropStart ? 'start' : 'end';
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
    this.evictedCount = dropped.length;
    dropped.forEach((r) => this.seenIds.delete(r.id));

    const retainedIds = new Set(kept.map((r) => r.id));
    const retainedTraces = new Set(kept.map((r) => r.traceId));
    this.cachedServerTraces = this.cachedServerTraces.filter((t) => retainedTraces.has(t.trace_id));
    this.expandedTraces = Object.fromEntries(Object.entries(this.expandedTraces).filter(([id]) => retainedTraces.has(id)));
    this.loadingSessions = Object.fromEntries(Object.entries(this.loadingSessions).filter(([id]) => retainedIds.has(id)));
    if (isRecentFetch) this.hasMore = true;
    else this.hasNewer = true;
    this.virtualizerEpoch++;
    this.holdRepositioningForRemount();
    return kept;
  }

  private captureScrollAnchor(): ScrollAnchor | null {
    const container = this.logsContainer;
    if (!container || this.mode !== 'logs') return null;
    const top = container.getBoundingClientRect().top;
    const row = [...container.querySelectorAll<HTMLElement>('[data-row-id]')].find((el) => el.getBoundingClientRect().bottom > top);
    if (row) {
      const id = row.dataset.rowId!;
      return {
        id,
        offset: row.getBoundingClientRect().top - top,
        index: this.virtualListItems.findIndex((i) => 'id' in i && i.id === id),
        scrollTop: container.scrollTop,
      };
    }

    const range = this.lastVisibilityRange;
    const index = range ? this.virtualListItems.slice(range.first, range.last + 1).findIndex((entry) => 'id' in entry) : -1;
    if (index < 0) return null;
    const item = this.virtualListItems[range!.first + index] as EventLine;
    return {
      id: item.id,
      offset: 0,
      index: range!.first + index,
      scrollTop: container.scrollTop,
    };
  }

  // Holds the auto-fetch suspension for its whole duration: until the anchor row is back
  // under the reader's eyes, every sentinel in the viewport is an artefact of the merge.
  private async restoreScrollAnchor(anchor: ScrollAnchor) {
    this.scrollSettling++;
    try {
      await this.updateComplete;
      // keyed() updates the child after the host update can already have resolved. Let that
      // replacement render and lay out before writing scrollTop; writing earlier is simply
      // clamped back to zero when the new virtualizer installs its initially empty runway.
      if (this.evictedEdge !== null) {
        await new Promise((resolve) => requestAnimationFrame(() => requestAnimationFrame(resolve)));
      }
      const virtualizer = this.querySelector('lit-virtualizer');
      const index = this.virtualListItems.findIndex((item) => 'id' in item && item.id === anchor.id);
      // The anchor row is gone, which means the retention cut took it. Which end the reader
      // belongs on is decided by which end was cut — and neither answer is the top the
      // remount clamped them to. Leaving them there is what threw a reader paging history
      // back to the newest edge, where the (now armed) load-newer sentinel fired on arrival.
      if (index < 0) {
        // 'end': live tail trimmed history out from under a reader deep in it — leave them
        // on the oldest retained row. 'start': a load-more dropped the newest rows, so the
        // reader's place is their pre-merge index less what was dropped. Rows, not virtual
        // items, so an expanded trace inside the cut lands them a few rows off — close, and
        // still their place, where the top is neither.
        const container = this.logsContainer;
        if (!container) return;
        if (this.evictedEdge === 'end') {
          container.scrollTop = container.scrollHeight;
        } else if (this.evictedEdge === 'start') {
          const target = Math.max(0, anchor.index - this.evictedCount);
          container.scrollTop = this.wrapsLines
            ? (target * container.scrollHeight) / Math.max(1, this.virtualListItems.length)
            : target * DENSE_ROW_HEIGHT;
        }
        await this.afterLayout(virtualizer);
        return;
      }
      if (!virtualizer) return;

      // The normal pagination case keeps the anchor inside the virtualizer's runway. Correct
      // that row in place: scrollToIndex('start') would first snap it to the top, then the
      // offset correction below would move it back a frame later — the visible load-more jump.
      // After an eviction, however, Lit can leave the old row mounted for this host update;
      // trusting that stale rectangle returns just before the keyed remount blanks the runway.
      if (this.evictedEdge === null && this.alignAnchor(anchor)) return;

      // Move the external scroller to the target's estimated runway immediately. For an
      // ordinary update scrollToIndex helps; after a keyed remount it can remain pending or
      // replay a stale pin, so an eviction uses the deterministic old/new index delta instead.
      if (this.evictedEdge === null) virtualizer.scrollToIndex(index, 'start');
      const container = this.logsContainer;
      if (!container) return;
      container.scrollTop = this.wrapsLines
        ? (index * container.scrollHeight) / Math.max(1, this.virtualListItems.length)
        : anchor.scrollTop + (index - anchor.index) * DENSE_ROW_HEIGHT;
      await new Promise((resolve) => requestAnimationFrame(resolve));
      if (this.evictedEdge === null && this.alignAnchor(anchor)) return;
      if (await this.afterLayout(virtualizer)) this.alignAnchor(anchor, virtualizer);
    } catch (error) {
      // Every caller is fire-and-forget (`void this.restoreScrollAnchor(...)`), so a throw
      // here would escape as an unhandled rejection rather than as anything actionable —
      // and trip error reporting for what costs the reader their scroll position, not their
      // data. Report it and let the list carry on.
      console.error('[log-list] scroll restore failed', error);
    } finally {
      this.scrollSettling--;
    }
  }

  // Settle the virtualizer's layout and paint one frame on top of it, so the geometry read
  // afterwards is the geometry the reader sees. False once the component is gone.
  private async afterLayout(virtualizer: { layoutComplete?: Promise<void> } | null): Promise<boolean> {
    try {
      const layoutComplete = virtualizer?.layoutComplete;
      if (layoutComplete) {
        let timeout: ReturnType<typeof setTimeout> | null = null;
        try {
          // lit-virtualizer can leave this promise pending after an external-scroller remount.
          // Never let one lost layout disable both pagination edges for the rest of the page.
          await Promise.race([layoutComplete, new Promise<void>((resolve) => (timeout = setTimeout(resolve, 250)))]);
        } finally {
          if (timeout) clearTimeout(timeout);
        }
      }
    } catch (error) {
      if (this.isConnected) throw error;
      return false;
    }
    await new Promise((resolve) => requestAnimationFrame(resolve));
    return this.isConnected;
  }

  // Put the anchor row back at its captured offset. False when it is not mounted.
  // Targets the row by id rather than materialising every rendered row to scan it.
  private alignAnchor(anchor: ScrollAnchor, scope?: ParentNode): boolean {
    const container = this.logsContainer;
    const row = (scope ?? container)?.querySelector<HTMLElement>(`[data-row-id="${CSS.escape(anchor.id)}"]`);
    if (!container || !row) return false;
    container.scrollTop += row.getBoundingClientRect().top - container.getBoundingClientRect().top - anchor.offset;
    return true;
  }

  // The <table> is the tab stop, so entering it with nothing active starts at the top.
  private activateFirstRow = () => {
    if (!this.focusedRowId) void this.moveRowFocus(0);
  };

  // Rows are virtualized, so the target may not be rendered yet. Move the marker first,
  // then ask the virtualizer to bring it into view; focus itself never leaves the table.
  private async moveRowFocus(to: number | 'first' | 'last') {
    const rowIndexes = this.virtualListItems.reduce<number[]>((acc, item, i) => ('id' in item ? (acc.push(i), acc) : acc), []);
    if (!rowIndexes.length) return;
    const current = rowIndexes.findIndex((i) => (this.virtualListItems[i] as EventLine).id === this.focusedRowId);
    const target =
      to === 'first'
        ? 0
        : to === 'last'
          ? rowIndexes.length - 1
          : // With nothing focused yet, the first move selects the first row rather than
            // counting from it: `0 + 1` skipped straight past row one.
            current < 0
            ? 0
            : Math.max(0, Math.min(rowIndexes.length - 1, current + to));
    const index = rowIndexes[target];
    const id = (this.virtualListItems[index] as EventLine).id;
    if (id === this.focusedRowId && current >= 0) return;
    this.focusedRowId = id;
    await this.updateComplete;
    // scrollToIndex, not element(index).scrollIntoView: element() only resolves rows that are
    // currently rendered, so a jump to Home/End past the window would silently not scroll.
    // Home/End pin to the edge; 'nearest' leaves them a no-op when the target is far off-screen.
    this.querySelector('lit-virtualizer')?.scrollToIndex(index, to === 'first' ? 'start' : to === 'last' ? 'end' : 'nearest');
  }

  private handleGridKeydown = (event: KeyboardEvent) => {
    // Controls inside a cell own their own keys — a button's Enter must not also
    // open the row behind it, and typing in a header filter must not scroll the list.
    if ((event.target as HTMLElement).closest('button, a, input, textarea, select, [contenteditable="true"]')) return;
    const moves: Record<string, number | 'first' | 'last'> = {
      ArrowDown: 1,
      ArrowUp: -1,
      PageDown: 10,
      PageUp: -10,
      Home: 'first',
      End: 'last',
    };
    if (event.key in moves) {
      event.preventDefault();
      void this.moveRowFocus(moves[event.key]);
      return;
    }
    if ((event.key === 'Enter' || event.key === ' ') && this.focusedRowId) {
      event.preventDefault();
      void this.activateFocusedRow();
    }
  };

  // A jump (Home/End/PageUp/PageDown) scrolls before the virtualizer mounts the target,
  // so activating straight away would find no element and silently do nothing.
  private async activateFocusedRow() {
    const id = this.focusedRowId;
    if (!id) return;
    for (let attempt = 0; attempt < 10; attempt++) {
      const row = this.querySelector<HTMLElement>(`[data-row-id="${CSS.escape(id)}"]`);
      if (row) return row.click();
      await new Promise((resolve) => requestAnimationFrame(resolve));
    }
  }

  handleRecentClick = () => {
    // This list's own container, not the first one on the page: dashboards embed several.
    if (this.logsContainer) this.logsContainer.scrollTop = 0;
    this.handleRecentConcatenation();
  };

  /**
   * Colour for one value of the breakdown dimension.
   *
   * Keyed on the dimension, not on `span_name`. Keying on the span name made this a
   * per-operation palette wearing the name "service colors": two spans in one service got two
   * colours, the same operation in two services got one, and any name missing the palette fell
   * back to grey. Kind is a fixed semantic palette; service is hashed, so a service keeps its
   * colour across queries, sessions and pages — which is what makes the legend worth reading.
   */
  private dimColor(value: string): string {
    return this.latencyDim === 'kind' ? (KIND_COLORS[value] ?? 'bg-fillStrong') : this.serviceColors[value] || 'bg-fillStrong';
  }

  /**
   * The legend: where the whole result set spent its time, not one row.
   *
   * A card answers "what is this row waiting on". Nothing answered "what dominates these
   * results", and the colours in the bars were write-only until something named them — the
   * mapping is stable, so reading it once here teaches it everywhere.
   *
   * Memoised on the tree's identity and length rather than recomputed per render: it walks
   * every loaded row, and the list holds up to 5000.
   */
  private get dimLegend(): { label: string; ns: number; pct: number; color: string }[] {
    const key = `${this.spanListTree.length}:${this.latencyDim}:${this.spanListTree[0]?.id ?? ''}`;
    if (this._legendCache?.key !== key) {
      const totals = dimTotals(this.spanListTree, this.colIdxMap, this.latencyDim);
      const sum = totals.reduce((a, t) => a + t.ns, 0);
      this._legendCache = {
        key,
        rows: totals.map((t) => ({ ...t, pct: sum > 0 ? (t.ns / sum) * 100 : 0, color: this.dimColor(t.label) })),
      };
    }
    return this._legendCache.rows;
  }
  private _legendCache: { key: string; rows: { label: string; ns: number; pct: number; color: string }[] } | null = null;

  /**
   * Oldest-first pins the viewport to the newest edge, and `updated` re-asserts that pin on
   * every render. Nothing ever cleared it on scroll, so a reader who scrolled up to page
   * history was thrown back to the bottom by the very render their load-more caused — the
   * same complaint as the newest-first jump-to-top, from the opposite end of the list.
   *
   * Skipped while the list is repositioning: a remounted virtualizer reports a zero-height
   * scroll range, which reads as "at the bottom" for exactly the frame it is not.
   */
  private syncBottomPin() {
    const container = this.logsContainer;
    if (!container || !this.flipDirection || this.isRepositioning) return;
    this.shouldScrollToBottom = container.scrollTop + container.clientHeight >= container.scrollHeight - 1;
  }

  // Flush buffered background rows if the viewport returns to their insertion edge.
  // Public so the visibility handler and any future scroll source share one rule.
  resumeLiveTailAtEdge() {
    const container = this.logsContainer;
    if (!container || (!this.isLiveStreaming && !this.resumeBufferedRecentAtEdge) || this.recentDataToBeAdded.length === 0) return;
    const scrolledToBottom = container.scrollTop + container.clientHeight >= container.scrollHeight - 1;
    if (atInsertionEdge(container.scrollTop, scrolledToBottom, this.flipDirection)) this.handleRecentConcatenation();
  }

  handleRecentConcatenation() {
    if (this.recentDataToBeAdded.length === 0) return;
    this.spanListTree = this.mergeIntoTree(this.recentDataToBeAdded, true);
    this.recentDataToBeAdded = [];
    this.resumeBufferedRecentAtEdge = false;
    this.updateVisibleItems();
    this.batchRequestUpdate('recentConcatenation');
  }

  /**
   * Keep each virtual row's paint invalidation inside that row while the viewport moves.
   * Paint containment is released shortly after scrolling so overflowing badge/tooltips are
   * not clipped during normal interaction. Mutating the scroll-surface class directly avoids
   * scheduling a Lit render on every scroll event.
   */
  private markScrolling() {
    this.isScrolling = true;
    this.logsContainer?.classList.add('is-scrolling');
    if (this.scrollEndTimer) clearTimeout(this.scrollEndTimer);
    this.scrollEndTimer = setTimeout(() => {
      this.isScrolling = false;
      this.logsContainer?.classList.remove('is-scrolling');
      this.scrollEndTimer = null;
    }, 80);
  }

  private handleListScroll = () => {
    this.markScrolling();
    this.syncBottomPin();
    this.resumeLiveTailAtEdge();
    // Also checked here, not only after an update: once the virtualizer is stuck it renders
    // nothing, so nothing changes, so Lit never updates again — `updated` would never run and
    // the list would stay blank forever. Scrolling is what the user does when they see it.
    this.healBlankVirtualizer();
  };

  handleVisibilityChange = (e: any) => {
    const first = e.first;
    const last = e.last;
    if (!Number.isInteger(first) || !Number.isInteger(last)) return;

    const previousRange = this.prefetchBaseline;
    this.prefetchBaseline = { first, last };
    // Store visibility range for deferred chart update
    this.lastVisibilityRange = { first, last };

    this.markScrolling();

    // Also resume here, not only on the container's scroll event: this fires as rows enter and
    // leave, which covers a jump straight to the top that never crosses the intermediate
    // scroll positions. The other direction is why the scroll binding exists too — already
    // showing the first row, nudging the last few pixels to 0 changes no row's visibility.
    this.resumeLiveTailAtEdge();

    // IntersectionObserver cannot prefetch early in a virtual list: the sentinel itself is not
    // mounted until it enters the virtualizer's short render runway. The range is already known
    // here, so a pair of integer comparisons starts one page early with no scroll/layout reads.
    // Require movement toward that edge: visibilityChanged also fires after an items merge, and
    // proximity alone could otherwise cascade through several pages while the user is stationary.
    // fetchData sets isLoadingMore synchronously, making repeated movement events single-flight.
    const nearHistoryEdge = this.flipDirection
      ? first <= HISTORY_PREFETCH_ROWS
      : last >= this.virtualListItems.length - 1 - HISTORY_PREFETCH_ROWS;
    const movingTowardHistory = previousRange !== null && (this.flipDirection ? first < previousRange.first : last > previousRange.last);
    if (nearHistoryEdge && movingTowardHistory && this.hasMore && !this.isLoading && !this.isLoadingMore && !this.isRepositioning) {
      void this.fetchData(this.buildLoadMoreUrl(), false, false, true);
    }

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
            background-color: var(--color-fillBrand-strong);
          }
          100% {
            background-color: transparent;
          }
        }

        .animate-fadeBg .status-indicator {
          animation: pulseIndicator 4s ease-out forwards;
          will-change: background-color;
        }

        /*
          Layout and style containment, deliberately without paint.

          Paint containment clips every descendant to the row's own box, and a log row is 28px.
          Measured, that left a device tooltip carrying a full user-agent string 61px tall with
          46 percent of it visible — and it clips fixed positioning too, so nothing positions
          its way out. The tooltips inside a row are labels on spans, and the one declarative
          way into the top layer (interestfor) only invokes from a link or a button, so keeping
          paint containment would mean turning labels into controls.

          Little is given up: the virtualiser already removes off-screen rows from the DOM, so
          paint containment was not saving their paint — layout and style containment are what
          keep one row's contents from affecting its siblings. If row scrolling ever regresses,
          this is the line to revisit.
        */
        .contain-layout-style {
          contain: layout style;
        }

        /*
          The virtualizer removes off-screen rows, but rows entering/leaving the runway still
          repaint. Bound that work to one 28px row only during active scrolling. Once scrolling
          stops, paint containment is removed so ordinary overflowing row tooltips remain usable.
          Top-layer latency cards are unaffected by this temporary clip.
        */
        .is-scrolling .contain-layout-style {
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

        .results-toolbar {
          container-type: inline-size;
        }
        .service-time-region {
          margin-left: 16rem;
        }
        @container (max-width: 1100px) {
          .service-time-region {
            margin-left: 13rem;
          }
        }
        @container (max-width: 880px) {
          .service-time-items {
            display: none;
          }
          .service-time-summary {
            display: inline-flex;
          }
        }
        @media (pointer: coarse) {
          .service-time-trigger {
            min-width: 44px;
            min-height: 44px;
            padding-inline: 8px;
            font-size: 14px;
          }
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
        @mousedown=${this.preserveGridFocusOnPointerRowClick}
        @scroll=${{ handleEvent: this.handleListScroll, passive: true }}
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
          ? html` <div class="pointer-events-none sticky top-[30px] z-50 flex h-0 justify-center" role="status" aria-live="polite">
              <button
                class="cbadge-sm pointer-events-auto cursor-pointer border border-strokeStrong bg-bgRaised text-textStrong shadow-sm rounded-full text-sm hover:bg-fillWeak focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-strokeBrand-strong"
                @click=${this.handleRecentClick}
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
          tabindex="0"
          aria-activedescendant=${this.focusedRowId ? `logrow-${this.focusedRowId}` : nothing}
          @focus=${this.activateFirstRow}
          @keydown=${this.handleGridKeydown}
          class="table-fixed ${isAggregate || this.wrapsLines || this.isNarrow
            ? 'w-full'
            : 'w-max'} relative ctable table-pin-rows table-pin-cols text-sm"
          style=${Object.entries(
            this.logsColumns.reduce(
              (acc, column) => {
                const width = this.columnWidth(column);
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
          <!-- Column headers label columns; a stacked phone row has none, so they are noise there. -->
          <thead class=${clsx('z-10 sticky top-0 isolate', this.isNarrow && 'hidden')}>
            <tr class="text-textWeak border-b flex min-w-0 relative font-medium isolate">
              ${isInitialLoading
                ? skeletonColumns(this.displayColumns).map((column, idx) => {
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
                    ${this.displayColumns.filter((v) => v !== 'latency_breakdown').map((column) => this.logTableHeading(column))}
                    ${this.displayColumns.includes('latency_breakdown') && !isAggregate
                      ? this.logTableHeading('latency_breakdown')
                      : nothing}
                  `}
            </tr>
          </thead>
          ${isInitialLoading
            ? loadingSkeleton(this.displayColumns)
            : html`
                <tbody class="min-w-0 text-xs">
                  ${keyed(
                    `${this.isAggregate || this.wrapsLines ? 'measured' : 'dense'}:${this.virtualizerEpoch}`,
                    html`<lit-virtualizer
                      .items=${this.virtualListItems}
                      .keyFunction=${virtualItemKey}
                      .renderItem=${this.renderVirtualItem}
                      @visibilityChanged=${this.handleVisibilityChange}
                      .layout=${this.isAggregate || this.wrapsLines ? {} : { type: DenseRowFlowLayout }}
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
                @click=${() => {
                  this.shouldScrollToBottom = true;
                  this.scrollToBottom();
                  this.handleRecentConcatenation();
                }}
                data-tip="Scroll to bottom"
                aria-label=${this.recentCount > 0 ? `Scroll to bottom (${this.recentCount} new events)` : 'Scroll to bottom'}
                class=${clsx(
                  'absolute tooltip tooltip-left right-8 bottom-2 group z-50 text-textInverse-strong flex justify-center items-center rounded-full shadow-lg h-10 w-10 transition-colors duration-150 hover:brightness-110 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-strokeBrand-strong',
                  this.recentCount > 0 ? 'bg-fillBrand-strong' : 'bg-fillStrong'
                )}
              >
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
    const wrapClass = this.wrapsLines ? 'whitespace-break-spaces' : 'whitespace-nowrap';
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
            >${this.isNarrow ? formatTimestampCompact(timestamp) : formatTimestamp(timestamp)}</time
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
                  class="flex items-center justify-center w-5 h-5 rounded border border-strokeStrong bg-bgBase text-iconNeutral group-hover/btn:border-strokeBrand-strong group-hover/btn:text-textBrand group-hover/btn:bg-bgRaised transition-colors"
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
          const colorOf = (value: string) => this.dimColor(value);
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
                  // The id is both an element id and half of a `--dashed-ident` anchor name, so
                  // it has to survive as a CSS identifier — span ids are hex but log ids are not.
                  card: ownSegments.length
                    ? {
                        id: `lat-${String(rowData.id ?? '').replace(/[^\w-]/g, '_')}`,
                        // Most rows are only passed while scrolling. Defer the aggregation,
                        // sorting and hidden tooltip DOM until somebody asks to see this card.
                        body: () => latencyTooltip({ startNs, duration, traceStart, color }, ownSegments),
                      }
                    : null,
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
        if (!rowData._summaryCache || rowData._summaryCache.wrapLines !== this.wrapsLines) {
          const summaryArray = this.parseSummaryData(dataArr);
          // Session top-level rows use the two-line identity/context layout;
          // everything else (logs, patterns, expanded session children) uses
          // the flat element list.
          rowData._summaryCache = {
            content: isSessionTopLevel
              ? [this.renderSessionSummary(summaryArray)]
              : this.renderSummaryElements(summaryArray, this.wrapsLines),
            wrapLines: this.wrapsLines,
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
          return html`<div class="flex items-center gap-1 min-w-0 ${this.wrapsLines ? 'flex-wrap' : 'whitespace-nowrap overflow-hidden'}">
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
          class=${clsx('flex w-full gap-1 min-w-0', isSessionTopLevel ? 'items-center' : this.wrapsLines ? 'items-start' : 'items-center')}
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
                : this.wrapsLines
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
  createLoadingRow = (
    id: string | null,
    label: string | TemplateResult,
    loading: boolean,
    onClick: () => void,
    rowRef?: RefOrCallback
  ) => html`
    <tr
      class="w-full flex relative h-[28px] cursor-pointer hover:bg-fillWeaker"
      id=${id || nothing}
      aria-busy=${loading}
      @click=${() => loading || onClick()}
      ${ref(rowRef ?? noopRef)}
    >
      <td colspan=${String(this.displayColumns.length)} class="relative pl-[calc(40vw-10ch)]">
        <div class="h-7 relative flex items-center justify-center">
          <span class=${clsx('text-textBrand underline font-semibold', loading && 'invisible')}>${label}</span>
          <div
            class=${clsx('absolute top-1 loading loading-dots loading-md h-5', !loading && 'invisible')}
            role="status"
            aria-label="Loading"
          ></div>
        </div>
      </td>
    </tr>
  `;

  // expandTimeRange is left alone on click: it is what keeps this row — and the spinner
  // the reader is watching — mounted while the page is in flight. Clearing it here swapped
  // the row for an empty <tr> the moment it was clicked, so the click read as doing nothing
  // and the list shifted by the row's height under the pointer. fetchData resolves the flag
  // from the response: a page that arrives sets hasMore and this row becomes "Load more".
  renderExpandTimeRangeButton = () =>
    this.createLoadingRow(null, 'Show earlier events', this.isLoading || this.isLoadingMore, () =>
      this.fetchData(this.expandTimeRangeUrl(), false, false, true)
    );

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
            if (entry.isIntersecting && !this.isLoadingMore && !this.isLoading && !this.isRepositioning) {
              this.debouncedLoadMore(this.historyWindowKey());
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
            if (entry.isIntersecting && !this.isFetchingRecent && !this.isLoading && !this.isRepositioning) {
              // No revealRecent: the observer fired because the reader is already at this edge,
              // so the rows arrive in view on their own. Forcing scrollTop to 0 here is what
              // teleported a reader who was paging history at the other end of the list.
              this.fetchData(this.buildRecentFetchUrl(), false, true, false, false);
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
      this.isLiveStreaming
        ? html`<span class="font-normal no-underline text-textWeak">Live streaming latest data...</span>`
        : 'Load newer events',
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

  // On a phone the desktop table is ~4000px wide, so summary — the column that says
  // what actually happened — starts off-screen behind two columns of chrome. Narrow
  // viewports render the same rows with only the identifying columns, and summary
  // flexes to fill instead of taking its fixed 3600px. The user's own column choices
  // are untouched; this filters what is drawn, not what is configured.
  private get displayColumns(): string[] {
    if (!this.isNarrow) return this.logsColumns;
    return this.logsColumns.filter((c) => NARROW_COLUMNS.includes(c));
  }

  // A phone cannot show a log line on one row: after timestamp and service there are
  // ~160px left. Wrapping turns each row into a stacked block instead of a line the user
  // has to swipe sideways to read. This is the same path the "Wrap lines" option uses,
  // so the measured virtualizer layout below is already exercised by it.
  private get wrapsLines(): boolean {
    return this.wrapLines || this.isNarrow;
  }

  // Header cells, body cells and the generated width CSS vars must agree, so all three
  // ask here rather than reaching into fixedColumnWidths themselves.
  private columnWidth(column: string): number | undefined {
    // A phone stacks each row into a block (see rowClass), so no cell gets a fixed
    // column width — they are full-width lines, not columns sitting side by side.
    if (this.isNarrow) return undefined;
    return this.columnMaxWidthMap[column] || this.fixedColumnWidths[column];
  }

  logTableHeading(column: string) {
    if (column === 'id') return html`<td class="p-0 m-0 whitespace-nowrap col-id pl-2.5"></td>`;

    const width = this.columnWidth(column);
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

    const { title = column, classes = 'shrink-0' } = config[column as keyof typeof config] || {};
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
      const effectiveLogsColumns = ov?.logsColumns ?? this.displayColumns;
      const isPatterns = effectiveMode === 'patterns';
      const isAggregate = isPatterns;
      const s = rowData.type === 'log' ? 'logs' : 'spans';
      const targetInfo: [string, string, string] = isAggregate
        ? ['', '', s]
        : requestDumpLogItemUrlPath(rowData.data, effectiveColIdxMap, s);
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
        'item-row relative p-0 flex group isolate cursor-pointer',
        // Wrap rather than a pure column: the marker, time and service belong on one
        // meta line (as a column stack each became a full-width band of dead space),
        // and only summary is forced onto its own line below them.
        this.isNarrow ? 'flex-wrap items-center gap-x-2 py-1.5 px-1 border-b border-strokeWeak' : 'whitespace-nowrap',
        rowData.id === this.focusedRowId && 'outline outline-2 -outline-offset-2 outline-strokeBrand-strong',
        rowData.id === this.openRowId && 'bg-fillBrand-strong',
        rowHoverBg,
        !ov && 'contain-layout-style',
        isPatterns && (this.wrapsLines ? 'items-start' : 'items-center'),
        // All non-wrapping, non-aggregate rows (including sessions) use the
        // dense 28px log row height for a consistent rhythm.
        !this.wrapsLines && !isAggregate && 'h-[28px] items-center',
        // Stacked rows size to their content; a 28px cap would clip the wrapped summary.
        this.isNarrow && 'h-auto',
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
          const hasWidth = this.columnWidth(column);
          // In aggregate child rows (ov), skip fixed summary width so it flexes to fill remaining space
          const skipFixedWidth = (ov || this.isNarrow) && column === 'summary';
          // Stacked cards cap the body: an unclamped raw log line wraps to ~10 lines and
          // one event fills the whole phone screen, which defeats scanning.
          // basis-full, not w-full: the summary cell also carries flex-1 (basis 0), which
          // wins over a width and keeps it on the meta line instead of wrapping below it.
          // pr-2: cells only carry pl-2, which on a phone left the wrapped summary running
          // flush into the right edge of the card with no inset.
          const narrowCell = this.isNarrow ? (column === 'summary' ? 'basis-full max-h-24 overflow-hidden pr-2' : 'w-auto shrink-0') : '';
          // break-words, not whitespace-break-spaces, on phones: the latter preserves the
          // template's own indentation as blank lines, which made empty cells ~90px tall.
          const wrapClasses = this.wrapsLines ? (this.isNarrow ? 'break-words' : 'break-all whitespace-break-spaces') : '';
          const cellClass = `${wrapClasses} ${narrowCell} ${cellBg} group-hover:bg-inherit relative pl-2 ${
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
      // The grid points aria-activedescendant here, so the id must be stable and unique.
      const rowId = `logrow-${rowData.id}`;
      const rowHtml = ov
        ? html`<div role="row" id=${rowId} data-row-id=${rowData.id} class=${rowClass} style=${rowStyle} @click=${rowClick}>
            ${cells}${latencyCell}
          </div>`
        : html`<tr id=${rowId} data-row-id=${rowData.id} class=${rowClass} style=${rowStyle} @click=${rowClick}>
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
    const finalWidth = width ?? this.columnWidth(column);
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
            <button class="cursor-pointer w-full text-left py-1 min-h-6" @click=${() => this.hideColumn(column)}>Hide column</button>
          </li>
          <li class="px-1 cursor-pointer hover:bg-fillWeak">
            <button class="cursor-pointer w-full text-left py-1 min-h-6" @click=${() => this.moveColumn(column, -1)}>
              Move column left
            </button>
          </li>
          <li class="px-1 cursor-pointer hover:bg-fillWeak">
            <button class="cursor-pointer w-full text-left py-1 min-h-6" @click=${() => this.moveColumn(column, 1)}>
              Move column right
            </button>
          </li>
          ${column === 'latency_breakdown'
            ? (['service', 'kind'] as LatencyDim[]).map(
                (dim) =>
                  html`<li class="px-1 cursor-pointer hover:bg-fillWeak">
                    <button
                      class="cursor-pointer w-full text-left py-1 min-h-6"
                      aria-pressed=${this.latencyDim === dim}
                      @click=${() => this.setLatencyDim(dim)}
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
        @click=${() => this.changeView(view)}
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

    const legendRows = this.isAggregate ? [] : this.dimLegend;
    const serviceTimes = legendRows.length > 1 ? legendRows : [];
    const shownServiceCount = Math.min(3, serviceTimes.length);
    const moreServiceCount = Math.max(0, serviceTimes.length - shownServiceCount);
    const dimensionLabel = this.latencyDim === 'service' ? 'service' : 'span kind';
    const dimensionPlural = this.latencyDim === 'service' ? 'services' : 'span kinds';
    const moreServiceLabel = `+${moreServiceCount} ${moreServiceCount === 1 ? dimensionLabel : dimensionPlural}`;
    const formatPercent = (item: (typeof serviceTimes)[number]) => (item.pct < 0.5 && item.ns > 0 ? '<1%' : `${Math.round(item.pct)}%`);
    const serviceItem = (item: (typeof serviceTimes)[number]) => html`
      <span class="service-time-item inline-flex items-center gap-1.5 min-w-0" role="listitem" title="${item.label} — ${fmtNs(item.ns)}">
        <span class=${`w-2 h-2 rounded-xs shrink-0 ${item.color}`} aria-hidden="true"></span>
        <span class="truncate max-w-[18ch] text-textStrong">${item.label}</span>
        <span class="tabular-nums">${formatPercent(item)}</span>
      </span>
    `;

    return html`
      <div class="results-toolbar w-full flex items-center justify-end px-2 gap-3 min-w-0">
        ${serviceTimes.length
          ? html`<div class="service-time-region flex-1 min-w-0 flex items-center justify-end overflow-hidden text-xs text-textWeak">
              <div class="service-time-items flex items-center justify-end gap-4 min-w-0 overflow-hidden">
                <!-- The +N trigger has to stay inside .service-time-items so the <880px rule
                     hides it along with the chips, but a role="list" may only contain its
                     listitems. display:contents gives the list a valid subtree while leaving
                     every chip a direct child of the same flex row. -->
                <div style="display: contents" role="list" aria-label="Time by ${dimensionLabel} across these results">
                  ${serviceTimes.slice(0, 3).map(serviceItem)}
                </div>
                ${moreServiceCount
                  ? html`<button
                      type="button"
                      class="service-time-trigger shrink-0 text-textWeak hover:text-textStrong active:text-textStrong underline-offset-2 hover:underline focus-visible:outline focus-visible:outline-2 focus-visible:outline-fillBrand-strong rounded-sm transition-colors duration-150 cursor-pointer "
                      popovertarget=${this.serviceTimePopoverId}
                      aria-haspopup="dialog"
                      aria-label="${moreServiceLabel} — show time breakdown for ${serviceTimes.length} ${dimensionPlural}"
                      style="anchor-name: --service-time-trigger"
                    >
                      ${moreServiceLabel}
                    </button>`
                  : nothing}
              </div>
              <button
                type="button"
                class="service-time-trigger service-time-summary hidden items-center shrink-0 text-textWeak hover:text-textStrong active:text-textStrong underline-offset-2 hover:underline focus-visible:outline focus-visible:outline-2 focus-visible:outline-fillBrand-strong rounded-sm transition-colors duration-150"
                popovertarget=${this.serviceTimePopoverId}
                aria-haspopup="dialog"
                style="anchor-name: --service-time-trigger"
              >
                <!-- No aria-label: the visible text is already a good name, and overriding it
                     with different wording put the two out of sync (WCAG 2.5.3). -->
                Time by ${dimensionLabel} · ${serviceTimes.length}
              </button>
              <div
                id=${this.serviceTimePopoverId}
                popover
                role="dialog"
                aria-labelledby="${this.serviceTimePopoverId}-title"
                class="dropdown bg-bgBase border border-strokeWeak rounded shadow p-3 w-72 max-w-[calc(100vw-1rem)] max-h-[min(32rem,80vh)] overflow-y-auto text-xs text-textWeak"
                style="position-anchor: --service-time-trigger; inset: auto; top: anchor(bottom); right: anchor(right); margin-top: 4px"
              >
                <div class="flex items-center justify-between gap-2 mb-2">
                  <div id="${this.serviceTimePopoverId}-title" class="font-medium text-sm text-textStrong">Time by ${dimensionLabel}</div>
                  <button
                    type="button"
                    class="service-time-trigger inline-flex items-center justify-center w-6 h-6 rounded text-textWeak hover:text-textStrong hover:bg-fillWeaker focus-visible:outline focus-visible:outline-2 focus-visible:outline-fillBrand-strong"
                    popovertarget=${this.serviceTimePopoverId}
                    popovertargetaction="hide"
                    aria-label="Close time by ${dimensionLabel}"
                  >
                    ${faSprite('xmark', 'regular', 'w-3 h-3')}
                  </button>
                </div>
                <div class="flex flex-col gap-2" role="list">
                  ${serviceTimes.map(
                    (item) =>
                      html`<div class="flex items-center gap-2" role="listitem">
                        <span class=${`w-2 h-2 rounded-xs shrink-0 ${item.color}`} aria-hidden="true"></span>
                        <span class="truncate flex-1 text-textStrong" title=${item.label}>${item.label}</span>
                        <span class="tabular-nums shrink-0">${formatPercent(item)} · ${fmtNs(item.ns)}</span>
                      </div>`
                  )}
                </div>
              </div>
            </div>`
          : html`<span class="flex-1"></span>`}
        <div class="tabs tabs-box tabs-md p-0 tabs-outline items-center border">
          ${viewButton('tree', 'tree', 'Tree')} ${viewButton('list', 'list-view', 'List')}
        </div>

        <div class="relative dropdown dropdown-end">
          <button
            tabindex="0"
            role="button"
            aria-label="Log display options"
            aria-haspopup="true"
            class=${`flex cursor-pointer items-center justify-center gap-1 px-2 min-h-6 min-w-6 text-xs rounded text-textWeak hover:text-textStrong focus-visible:outline focus-visible:outline-2 focus-visible:outline-strokeBrand-strong`}
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
                this.resumeBufferedRecentAtEdge = false;
              }
              this.requestUpdate();
            })}
            ${this.renderCheckbox('Wrap lines', 'wrap-text', this.wrapLines, (checked) => {
              this.wrapLines = checked;
              if (this.wrapsLines) {
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
                        <li>
                          <button
                            type="button"
                            class="w-full text-left px-1 py-0.5 hover:bg-fillWeak cursor-pointer"
                            @click=${() => {
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
                          </button>
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
                  <button class="hidden group-hover:inline-block cursor-pointer" @click=${() => this._removeColumn(index)}>
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
    .map((d) => ({ from: Math.max(row.startNs, d.startNs), to: Math.min(rowEnd, d.startNs + Math.max(0, d.duration)), d }))
    .filter((s) => s.to > s.from)
    .sort((a, b) => a.from - b.from);
  if (!spans.length) return [];

  const bounds = [...new Set(spans.flatMap((s) => [s.from, s.to]))].sort((a, b) => a - b);
  const parts: { from: number; to: number; label: string; color: string }[] = [];
  let next = 0;
  let active: typeof spans = [];
  for (let i = 0; i < bounds.length - 1; i++) {
    const [from, to] = [bounds[i], bounds[i + 1]];
    while (next < spans.length && spans[next].from <= from) active.push(spans[next++]);
    active = active.filter((s) => s.to > from);
    let win: (typeof spans)[number] | undefined;
    for (const s of active) if (!win || (s.d.depth ?? 1) > (win.d.depth ?? 1)) win = s;
    if (!win) continue;
    const prev = parts[parts.length - 1];
    if (prev && prev.to === from && prev.label === win.d.label && prev.color === win.d.color) prev.to = to;
    else parts.push({ from, to, label: win.d.label, color: win.d.color });
  }
  return parts.map((p) => ({
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
  const direct = descendants.filter((c) => (c.depth ?? 1) === 1);
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
  ns >= 1e9
    ? `${(ns / 1e9).toFixed(2)}s`
    : ns >= 1e6
      ? `${Math.round(ns / 1e6)}ms`
      : ns >= 1e3
        ? `${Math.round(ns / 1e3)}\u00b5s`
        : `${Math.round(ns)}ns`;

/**
 * What the bar is saying, in words — for the tooltip and for the screen reader, neither of
 * which can read a colour. Carries the trace offset the bar no longer encodes positionally.
 */
export function latencyTitle(
  dim: LatencyDim,
  row: { startNs: number; duration: number; traceStart: number },
  segments: LatencySegment[]
): string {
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

/**
 * The bar in words and numbers, as a hover card rather than a native `title`.
 *
 * `title=` could only ever be one flat string: it names services without connecting them to
 * the colours in the bar it is describing, waits about a second before appearing, can't be
 * reached from the keyboard, and can't be styled. This is the same data with the swatch beside
 * each name, which is what makes the colour legible — the mapping is stable (services are
 * coloured by a hash of their name), so reading it once here teaches it everywhere.
 *
 * Only called where there is a breakdown to show. A log has no duration and a childless span
 * has nothing under it, so their card could only restate the duration the row already prints —
 * and on a log-only view that was every row answering "0ns total, no service breakdown".
 *
 * On an expanded row this deliberately lists a different set than the bar beside it. The bar
 * has become a waterfall — where this span sits in the trace, with its direct children, since
 * every deeper span draws its own row below. The card stays what it always is: where this
 * span's time went, over its whole subtree. Making them agree would mean either dropping the
 * subtree summary or painting descendants the rows underneath already show. The header's
 * "+X into trace" is the seam between the two, naming the position the bar encodes.
 */
export function latencyTooltip(row: { duration: number; startNs: number; traceStart: number; color: string }, segments: LatencySegment[]) {
  const byLabel = new Map<string, { ns: number; color: string }>();
  for (const s of segments) {
    const at = byLabel.get(s.label) ?? { ns: 0, color: s.color };
    byLabel.set(s.label, { ns: at.ns + s.ns, color: at.color });
  }
  const self = Math.max(0, row.duration - segments.reduce((a, s) => a + s.ns, 0));
  // Self time is a row like any other: "40% of this request was spent here, not downstream"
  // is the same kind of answer as "50% was postgres", and hiding it makes the parts look
  // like they should sum to the total when they don't.
  const rows = [...byLabel.entries()]
    .map(([label, v]) => ({ label, ...v }))
    .concat(self > 0 ? [{ label: 'self', ns: self, color: row.color }] : []);
  rows.sort((a, b) => b.ns - a.ns);
  // A share that rounds to zero still isn't zero: a real 337µs call inside a 72ms request read
  // as "0%", which says the service did nothing rather than very little.
  const pct = (ns: number) => {
    if (!(row.duration > 0)) return '0%';
    const share = (ns / row.duration) * 100;
    return share > 0 && share < 0.5 ? '<1%' : `${Math.round(share)}%`;
  };
  return html`<div class="latency-card-body text-left text-textStrong font-normal">
    <div class="flex items-baseline justify-between gap-3 pb-1 mb-1 border-b border-strokeWeak">
      <span class="font-medium">${fmtNs(row.duration)} total</span>
      <span class="text-textWeak">+${fmtNs(Math.max(0, row.startNs - row.traceStart))} into trace</span>
    </div>
    ${rows.slice(0, 6).map(
      (r) =>
        html`<div class="flex items-center gap-2 whitespace-nowrap leading-5">
          <span class=${`w-2 h-2 rounded-xs shrink-0 ${r.color}`}></span>
          <span class="grow truncate max-w-[16ch]">${r.label}</span>
          <span class="tabular-nums text-textWeak">${fmtNs(r.ns)}</span>
          <span class="tabular-nums text-textWeak w-9 text-right">${pct(r.ns)}</span>
        </div>`
    )}
    ${rows.length > 6 ? html`<div class="text-textWeak pt-0.5">+${rows.length - 6} more</div>` : nothing}
  </div>`;
}

/**
 * Time attributed to each value of the breakdown dimension, across every loaded row.
 *
 * Each row contributes its *self* time — its duration minus what its direct children were
 * doing — to its own service. Summing whole durations instead would bill a request's time to
 * every service in its call chain and total far past the wall clock; self time partitions it,
 * so the shares are of one real quantity.
 *
 * Overlapping siblings are merged rather than added (`exclusiveSegments`), because two
 * children running concurrently occupy one stretch of the parent, not two.
 */
export function dimTotals(rows: EventLine[], colIdxMap: ColIdxMap, dim: LatencyDim): { label: string; ns: number }[] {
  const byLabel = new Map<string, number>();
  for (const row of rows) {
    if (!(row.duration > 0)) continue; // a log costs no time; a zero-length span has none to give
    const direct = (row.childrenTimeSpans ?? []).filter((c) => (c.depth ?? 1) === 1);
    const covered = exclusiveSegments(
      row,
      direct.map((c) => ({ ...c, label: '', color: '' }))
    ).reduce((a, s) => a + s.ns, 0);
    const self = Math.max(0, row.duration - covered);
    if (!self) continue;
    const label = lookupVecValue<string>(row.data, colIdxMap, dim) || 'unknown';
    byLabel.set(label, (byLabel.get(label) ?? 0) + self);
  }
  return [...byLabel.entries()].map(([label, ns]) => ({ label, ns })).sort((a, b) => b.ns - a.ns);
}

export function spanLatencyBreakdown({
  track,
  segments,
  title,
  card,
  barWidth,
  frame,
}: {
  track: string;
  segments: LatencySegment[];
  title: string;
  card: { id: string; body: () => TemplateResult } | null;
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
  const bar = html`<div class=${`flex h-5 relative rounded-sm overflow-hidden ${track}`} style=${`width:${barWidth}px`}>
    ${segments.map((s) => {
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
          <div
            class="absolute top-0 left-0 h-full border-l-2 border-strokeBrand-strong shadow-[0_0_4px_var(--color-strokeBrand-weak)]"
          ></div>
          <div
            class="absolute top-0 right-0 h-full border-r-2 border-strokeBrand-strong shadow-[0_0_4px_var(--color-strokeBrand-weak)]"
          ></div>
          <div
            class="absolute top-1/2 -translate-y-1/2 left-0 w-full h-px bg-strokeBrand-strong shadow-[0_0_2px_var(--color-strokeBrand-weak)]"
          ></div>
        </div>`
      : nothing}
  </div>`;

  // The card lives in the top layer, because a 28px row is no place for it. It began as an
  // escape from the row's paint containment, which clipped it to the header line; that
  // containment is gone now (see `.contain-layout-style`), but the top layer is still the
  // right home — the card is taller than the row and overlaps its neighbours by design, and
  // up there it also dismisses on Escape, which a CSS-only tooltip cannot do.
  //
  // `interestfor` opens it on hover *and* on keyboard focus with no script, and the popover
  // dismisses on Escape, which a CSS tooltip cannot do. Anchor positioning ties it back to the
  // bar it describes: the card is no longer a descendant, so the anchor name carries the
  // relationship the DOM no longer does. Left of the bar, since the latency column is last.
  //
  // No card means no button: a bar with nothing to break down is not a control, and making
  // every row's bar focusable would put one tab stop per visible row before anything after
  // the table. `aria-label` carries the flat sentence either way — a screen reader should not
  // have to walk a grid to hear what the bar says.
  if (!card) return html`<div class="-mt-1 shrink-0" role="img" aria-label=${title}>${bar}</div>`;
  return html`
    <button
      type="button"
      interestfor=${card.id}
      style=${`anchor-name:--${card.id}`}
      class="-mt-1 shrink-0 block p-0 m-0 border-0 bg-transparent cursor-default rounded-sm"
      aria-label=${title}
    >
      ${bar}
    </button>
    <div
      popover="hint"
      id=${card.id}
      style=${`position-anchor:--${card.id}`}
      class="latency-card"
      aria-hidden="true"
      @beforetoggle=${(event: Event) => {
        const target = event.currentTarget as HTMLElement;
        if (!target.hasChildNodes()) renderLit(card.body(), target);
      }}
    ></div>
  `;
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
          ${faSprite('circle-exclamation', 'regular', 'h-10 w-10 stroke-strokeError-strong fill-fillError-strong opacity-70')}
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
  let subText = `Nothing matched in the selected time range. Try widening the time picker or removing a filter. If this project is new, it may not have sent events yet.`;
  return html`
    <tr class="w-full flex justify-center">
      <td colspan=${String(cols)} class="w-full mx-auto">
        <div class="max-w-full mx-auto my-8 text-center px-5 py-10 flex flex-col items-center gap-2">
          ${faSprite('inbox-full', 'regular', 'w-6 h-6 text-iconNeutral')}
          <h2 class="text-base text-textStrong font-semibold">${title}</h2>
          <p class="text-sm max-w-md text-textWeak">${subText}</p>
          <a href="https://monoscope.tech/docs/sdks/" target="_BLANK" class="text-sm text-textBrand hover:text-textStrong underline mt-1">
            Read integration guides
          </a>
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
