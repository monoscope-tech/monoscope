'use strict';
import '@lit-labs/virtualizer';
import { LitElement, html, css, TemplateResult, nothing } from 'lit';
import { customElement, state, query, property } from 'lit/decorators.js';
import { ref, createRef } from 'lit/directives/ref.js';
import { APTEvent, ChildrenForLatency, ColIdxMap, EventLine, ServerTraceEntry, Trace, TraceDataMap } from './types/types';
import debounce from 'lodash/debounce';
import { includes, startsWith, map, forEach, compact, pick, chunk, chain, lt } from 'lodash';
// Import worker as URL instead of worker instance
import LogWorkerUrl from './log-worker?worker&url';
import { groupSpans } from './log-worker-functions';
import clsx from 'clsx';
import {
  formatTimestamp,
  lookupVecValue,
  getErrorClassification,
  faSprite,
  renderBadge,
  renderIconWithTooltip,
  getSkeletonColumnWidth,
  getStyleClass,
  CHAR_WIDTHS,
  MIN_COLUMN_WIDTH,
  parseSummaryElement,
  unescapeJsonString,
  calculateAutoBinWidth,
  createCachedIconRenderer,
  WEAK_TEXT_STYLES,
  RIGHT_PREFIX_REGEX,
  formatPatternCount,
  highlightPlaceholders,
  generateId,
  renderSparkline,
} from './log-list-utils';
import { unsafeHTML } from 'lit/directives/unsafe-html.js';

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

// Special item types for virtual list
type VirtualListItem = EventLine | { type: 'fetchRecent' } | { type: 'loadMore' };

@customElement('log-list')
export class LogList extends LitElement {
  @property({ type: String }) projectId: string = '';
  @property({ type: String }) initialFetchUrl: string = '';
  @property({ type: String }) mode: 'logs' | 'patterns' = 'logs';

  @state() private expandedTraces: Record<string, boolean> = {};
  @state() private flipDirection: boolean = false;
  @state() private spanListTree: EventLine[] = [];
  @state() private recentDataToBeAdded: EventLine[] = [];
  @state() private view: 'tree' | 'list' = 'tree';
  @state() private shouldScrollToBottom: boolean = false;
  @state() private logsColumns: string[] = [];
  @state() private wrapLines: boolean = false;
  @state() private hasMore: boolean = true;
  @state() private expandTimeRange: boolean = true;
  @state() private loadedCount: number = 0;
  @state() private totalCount: number = 0;
  @state() private totalPatterns: number = 0;
  @state() private isLiveStreaming: boolean = false;
  @state() private isLoading: boolean = false;
  @state() private isFetchingRecent: boolean = false;
  @state() private isLoadingMore: boolean = false;
  @state() private fetchedNew: boolean = false;
  @state() private visibleItems: EventLine[] = [];
  @state() private virtualListItems: VirtualListItem[] = [];
  @state() private fixedColumnWidths: Record<string, number> = {};

  // Refs for DOM elements
  @query('#logs_list_container_inner') private logsContainer?: HTMLElement;
  @query('#loader') private loaderElement?: HTMLElement;
  @query('#log_details_container') private logDetailsContainer?: HTMLElement;
  @query('#resizer-details_width-wrapper') private resizerWrapper?: HTMLElement;
  @query('#details_indicator') private detailsIndicator?: HTMLElement;

  private cachedServerTraces: ServerTraceEntry[] = [];
  private resizeTarget: string | null = null;
  private mouseState: { x: number } = { x: 0 };
  private colIdxMap: ColIdxMap = {};
  private serviceColors: Record<string, string> = {};
  private columnMaxWidthMap: ColIdxMap = {};
  private recentFetchUrl: string = '';
  private liveStreamInterval: NodeJS.Timeout | null = null;
  private barChart: any = null;
  private lineChart: any = null;
  private _loadMoreObserver: IntersectionObserver | null = null;
  private updateBatchTimer: NodeJS.Timeout | null = null;
  private pendingUpdates: Set<string> = new Set();
  private handleMouseUp: (() => void) | null = null;
  private sessionPlayerWrapper: HTMLElement | null = null;
  private containerRef = createRef<HTMLDivElement>();
  private nextFetchUrl = '';

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
  private isCalculatingWidths: boolean = false;
  private lastVisibilityRange: { first: number; last: number } | null = null;
  private isScrolling = false;
  private scrollEndTimer: ReturnType<typeof setTimeout> | null = null;
  private worker: Worker | null = null;
  private workerReqId = 0;
  private workerCallbacks = new Map<number, { resolve: Function; reject: Function }>();

  constructor() {
    super();

    this.worker = new Worker(LogWorkerUrl, { type: 'module' });
    this.worker.onmessage = (e) => this.handleWorkerMsg(e);
    this.worker.onerror = (e: ErrorEvent) => {
      console.error('[Worker] Error:', e.message, e.filename, e.lineno);
    };

    this.debouncedFetchData = debounce(this.fetchData.bind(this), 300);
    this.debouncedUpdateChartMarkArea = debounce(this.updateChartMarkArea.bind(this), 100);
    // Bind resize handler for immediate feedback
    this.boundHandleResize = this.handleResize.bind(this);

    this.expandTrace = this.expandTrace.bind(this);
    this.setupEventListeners();
  }

  private handleWorkerMsg(e: MessageEvent) {
    const { type, tree, meta, error, id } = e.data;
    const cb = this.workerCallbacks.get(id);
    if (!cb) {
      console.warn('[Worker] No callback found for message id:', id);
      return;
    }
    this.workerCallbacks.delete(id);
    type === 'success' ? cb.resolve({ tree, meta }) : cb.reject(new Error(error));
  }

  private async workerFetch(url: string): Promise<{ tree: any[]; meta: any }> {
    // Patterns mode: fetch directly, no span grouping needed
    if (this.mode === 'patterns') {
      const resp = await fetch(url, { headers: { Accept: 'application/json' }, credentials: 'include' });
      const data = await resp.json();
      if (data.error) throw new Error(data.message || 'Server error');
      const tree = (data.logsData || []).map((row: any[]) => ({
        id: generateId(), data: row, depth: 0, children: 0, traceId: '', parentIds: [],
        show: true, expanded: false, isLastChild: true, siblingsArr: [],
        childErrors: false, hasErrors: false, isNew: false,
        startNs: 0, duration: 0, traceStart: 0, traceEnd: 0,
        childrenTimeSpans: [], type: 'log' as const,
      }));
      return { tree, meta: { serviceColors: {}, nextUrl: '', cols: data.cols || [], colIdxMap: data.colIdxMap || {}, count: data.count || 0, totalPatterns: data.totalPatterns ?? 0, traces: [], hasMore: data.hasMore ?? false, queryResultCount: data.queryResultCount ?? 0 } };
    }

    // Use early fetch promise if available (set by server-rendered script in head)
    const earlyPromise = (window as any).logDataPromise;
    if (earlyPromise) {
      (window as any).logDataPromise = null;
      const data = await earlyPromise;
      if (!data.error) {
        const { logsData, serviceColors, nextUrl, recentUrl, cols, colIdxMap, count, traces } = data;
        const tree = logsData?.length ? groupSpans(logsData, colIdxMap, this.expandedTraces, this.flipDirection, traces || []) : [];
        return { tree, meta: { serviceColors, nextUrl, recentUrl, cols, colIdxMap, count, traces: traces || [], hasMore: data.hasMore ?? (logsData?.length > 0), queryResultCount: data.queryResultCount ?? logsData?.length ?? 0 } };
      }
    }
    // Fallback to worker
    if (!this.worker) throw new Error('Worker not initialized');
    const id = ++this.workerReqId;
    return new Promise((resolve, reject) => {
      this.workerCallbacks.set(id, { resolve, reject });
      this.worker!.postMessage({ type: 'fetch', url, colIdxMap: this.colIdxMap, expandedTraces: this.expandedTraces, flipDirection: this.flipDirection, id });
      setTimeout(() => { if (this.workerCallbacks.has(id)) { this.workerCallbacks.delete(id); reject(new Error('Worker timeout')); } }, 120000);
    });
  }

  updateChartDataZoom(start: number, end: number) {
    // Chart data zoom functionality - currently disabled
  }

  private setupEventListeners() {
    // Live streaming button
    const liveBtn = document.querySelector('#streamLiveData') as HTMLInputElement;
    if (liveBtn) {
      liveBtn.addEventListener('change', () => {
        if (liveBtn.checked) {
          this.isLiveStreaming = true;
          this.liveStreamInterval = setInterval(() => {
            this.fetchData(this.buildRecentFetchUrl(), false, true);
          }, 5000);
        } else {
          if (this.liveStreamInterval) {
            clearInterval(this.liveStreamInterval);
          }
          this.isLiveStreaming = false;
        }
        this.requestUpdate();
      });
    }

    // Global event listeners
    ['submit', 'add-query'].forEach((ev) => window.addEventListener(ev, this.debouncedRefetchLogs));

    // Form submit listener
    document.addEventListener('submit', this.handleFormSubmit);

    // Filter element update listener
    document.addEventListener('update-query', this.handleUpdateQuery);

    // Window lifecycle events
    window.addEventListener('pagehide', () => {
      if (this.liveStreamInterval) clearInterval(this.liveStreamInterval);
    });

    // Mouse events for resizing
    this.handleMouseUp = () => {
      this.resizeTarget = null;
      document.body.style.userSelect = 'auto';
    };
    window.addEventListener('mouseup', this.handleMouseUp);
    window.addEventListener('mousemove', this.boundHandleResize);

    // Chart initialization and events
    window.addEventListener('load', () => {
      this.barChart = (window as any).barChart;
      this.lineChart = (window as any).lineChart;
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
    });
  }

  private buildJsonUrl(): string {
    // Preserve all existing query parameters and add json=true
    if (this.initialFetchUrl) {
      const url = new URL(this.initialFetchUrl, window.location.origin);
      url.searchParams.set('json', 'true');
      return url.toString();
    } else {
      const p = new URLSearchParams(window.location.search);
      p.set('json', 'true');
      // Sync viz_type with current mode (URL update may be deferred via rAF)
      if (this.mode === 'patterns') p.set('viz_type', 'patterns');
      else if (p.get('viz_type') === 'patterns') p.delete('viz_type');
      const pathName = window.location.pathname;
      return `${window.location.origin}${pathName}?${p.toString()}`;
    }
  }

  private buildRecentFetchUrl(): string {
    // Always build from current browser URL to ensure we have latest query params
    const url = new URL(window.location.href);
    url.searchParams.set('json', 'true');

    // If we have data, update the 'from' parameter to fetch newer data
    if (this.spanListTree.length > 0) {
      const firstItem = this.flipDirection ? this.spanListTree[this.spanListTree.length - 1] : this.spanListTree[0];
      const timestamp = firstItem?.data?.[this.colIdxMap['timestamp'] || this.colIdxMap['created_at']];

      if (timestamp) {
        const date = new Date(timestamp);
        date.setTime(date.getTime() + 10); // Add 10ms
        url.searchParams.set('from', date.toISOString());
        // Remove cursor, since, and to params for recent fetch
        url.searchParams.delete('cursor');
        url.searchParams.delete('since');
        url.searchParams.delete('to');
      }
    }

    return url.toString();
  }

  private buildLoadMoreUrl(): string {
    // Patterns mode: increment pattern_skip based on loaded count
    if (this.mode === 'patterns') {
      const url = new URL(window.location.href);
      url.searchParams.set('json', 'true');
      url.searchParams.set('pattern_skip', String(this.spanListTree.length));
      return url.toString();
    }

    // If we have no data, use base URL
    if (this.spanListTree.length === 0) {
      console.warn('[LoadMore] No data in spanListTree, using buildJsonUrl');
      return this.buildJsonUrl();
    }

    // Get the actual oldest item from current data
    // flipDirection=false (newest first): oldest is at END of array
    // flipDirection=true (oldest first): oldest is at START of array
    const lastItem = this.flipDirection ? this.spanListTree[0] : this.spanListTree[this.spanListTree.length - 1];

    // Get timestamp column name (try timestamp first, then created_at)
    const timestampCol = this.colIdxMap['timestamp'] !== undefined ? 'timestamp' : 'created_at';
    const timestamp = lastItem?.data?.[this.colIdxMap[timestampCol]];

    if (!timestamp) {
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

    // Calculate cursor based on actual oldest timestamp
    const date = new Date(timestamp);
    date.setTime(date.getTime() - 10); // Subtract 10ms buffer to avoid missing items

    // Update cursor for pagination while preserving time range filters (from/to/since)
    // The cursor tells the server where to start pagination
    // The from/to/since tell the server what time range to query within
    url.searchParams.set('cursor', date.toISOString());

    return url.toString();
  }

  private expandTimeRangeUrl(): string {
    const baseUrl = this.nextFetchUrl ? new URL(this.nextFetchUrl, window.location.origin) : new URL(window.location.href);

    const url = new URL(baseUrl);
    const since = url.searchParams.get('since');
    const to = url.searchParams.get('to');

    let target = '1H';

    if (since) {
      const nextMap: Record<string, string> = {
        '5M': '15M',
        '15M': '30M',
        '30M': '1H',
        '1H': '3H',
        '3H': '6H',
        '6H': '12H',
        '12H': '24H',
        '24H': '3D',
        '3D': '7D',
        '7D': '14D',
      };
      target = nextMap[since] ?? '14D';
      url.searchParams.set('since', target);
    } else if (to) {
      const newTo = new Date(new Date(to).getTime() - 3 * 60 * 60 * 1000).toISOString();
      url.searchParams.set('to', newTo);
      target = `${url.searchParams.get('from')} - ${newTo}`;
    } else {
      target = '3H';
      url.searchParams.set('since', target);
    }

    // Use the timestamp of the oldest log currently in the list as the cursor
    // This ensures we fetch older logs when expanding the time range
    if (this.spanListTree.length > 0) {
      const oldestItem = this.flipDirection ? this.spanListTree[0] : this.spanListTree[this.spanListTree.length - 1];
      const oldestTimestamp = oldestItem?.data?.[this.colIdxMap['timestamp'] || this.colIdxMap['created_at']];

      if (oldestTimestamp) {
        url.searchParams.set('cursor', String(oldestTimestamp));
      }
    }

    // Ensure json=true and layout=loadmore for the API request
    url.searchParams.set('json', 'true');
    url.searchParams.set('layout', 'loadmore');

    // Save URL with cursor, json, and layout for the fetch
    this.nextFetchUrl = url.pathname + url.search;

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
    this.mode = vizType === 'patterns' ? 'patterns' : 'logs';
    this.fetchData(this.buildJsonUrl(), false);
  }

  async refetchLogs() {
    this.fetchData(this.buildJsonUrl(), true);
  }

  debouncedRefetchLogs = debounce(async () => {
    this.refetchLogs();
  }, 50);

  toggleColumnOnTable = (col: string) => {
    const p = new URLSearchParams(window.location.search);
    const cols = compact((p.get('cols') || '').split(','));
    const idx = cols.indexOf(col);
    const newCols =
      idx > -1
        ? cols.filter((_, i) => i !== idx)
        : [...cols.slice(0, cols.indexOf('summary')), col, ...cols.slice(cols.indexOf('summary'))];
    p.set('cols', newCols.join(','));
    window.history.replaceState({}, '', `${window.location.pathname}?${p}${window.location.hash}`);
    this.fetchData(this.buildJsonUrl(), true);
  };

  handleChartZoom = (params: { batch?: { startValue: string; endValue: string }[] }) => {
    const zoom = params.batch ? params.batch[0] : undefined;
    if (!zoom) return;
    let startValue = zoom.startValue;
    let endValue = zoom.endValue;
    if (startValue === undefined || endValue === undefined) return;
    startValue = new Date(startValue).toISOString();
    endValue = new Date(endValue).toISOString();
    const p = new URLSearchParams(window.location.search);
    p.set('from', startValue);
    p.set('to', endValue);
    p.delete('since');

    const newUrl = `${window.location.pathname}?${p.toString()}${window.location.hash}`;
    this.updateUrlStateAndQuery(newUrl, p.get('queryAST') || '', `${startValue} - ${endValue}`, 'chart-zoom');

    // Refetch logs with the new time range
    this.debouncedRefetchLogs();
  };

  private updateUrlStateAndQuery(newUrl: string, q: string, timeRange: string, source: string = 'default') {
    window.history.replaceState({}, '', newUrl);
    const rangeBox = document.getElementById('currentRange');
    if (rangeBox) {
      rangeBox.innerText = timeRange;
    }

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
    // Initialize empty state
    this.logsColumns = [];
    this.colIdxMap = {};
    this.serviceColors = {};
    this.spanListTree = [];
    this.visibleItems = [];
    this.hasMore = false;

    // Initialize fixed column widths
    this.initializeFixedColumnWidths();

    // Project ID is now passed as a property from the server

    // Fetch initial data from the JSON endpoint
    this.fetchInitialData();
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

  private updateRowCountDisplay() {
    const countElement = document.getElementById('row-count-display');
    if (!countElement) return;
    const suffixEl = document.getElementById('row-count-suffix');
    if (this.mode === 'patterns') {
      countElement.textContent = `${this.formatCount(this.totalPatterns)} patterns`;
      if (suffixEl) suffixEl.textContent = ` found (based on ${this.formatCount(this.totalCount)} logs)`;
    } else {
      countElement.textContent = this.formatCount(this.loadedCount);
      if (suffixEl) suffixEl.textContent = this.loadedCount < this.totalCount ? ` of ${this.formatCount(this.totalCount)} rows` : ' rows';
    }
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
      spinner.innerHTML =
        '<svg class="inline-block icon w-4 h-4 animate-spin text-textBrand"><use href="/public/assets/svgs/fa-sprites/regular.svg?v=ecf9d105#spinner"></use></svg>';
      countElement.parentElement?.appendChild(spinner);
    } else if (!show && spinner) {
      // Remove spinner
      spinner.remove();
    }
  }

  private formatCount(count: number): string {
    if (count >= 1000000) {
      return (count / 1000000).toFixed(1) + 'M';
    } else if (count >= 1000) {
      return (count / 1000).toFixed(1) + 'K';
    }
    return count.toString();
  }

  firstUpdated() {
    // Initialization handled by lit-virtualizer
  }

  updated(changedProperties: Map<string, any>) {
    if (this.shouldScrollToBottom && this.flipDirection) {
      requestAnimationFrame(() => this.scrollToBottom());
    }

    // Reset isNew flag after animation
    if (changedProperties.has('spanListTree') && this.fetchedNew) {
      setTimeout(() => {
        this.spanListTree.forEach((span) => {
          if (span.isNew) {
            span.isNew = false;
          }
        });
        this.fetchedNew = false;
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

    // Clean up all observers and timers
    if (this._loadMoreObserver) {
      this._loadMoreObserver.disconnect();
      this._loadMoreObserver = null;
    }
    if (this.updateBatchTimer) {
      clearTimeout(this.updateBatchTimer);
      this.updateBatchTimer = null;
    }
    if (this.liveStreamInterval) {
      clearInterval(this.liveStreamInterval);
      this.liveStreamInterval = null;
    }
    if (this.scrollEndTimer) {
      clearTimeout(this.scrollEndTimer);
      this.scrollEndTimer = null;
    }

    // Clean up event listeners
    window.removeEventListener('mousemove', this.boundHandleResize);
    if (this.handleMouseUp) {
      window.removeEventListener('mouseup', this.handleMouseUp);
    }
    ['submit', 'add-query'].forEach((ev) => window.removeEventListener(ev, this.debouncedRefetchLogs));
    document.removeEventListener('submit', this.handleFormSubmit);
    document.removeEventListener('update-query', this.handleUpdateQuery);

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
    let width = this.columnMaxWidthMap[this.resizeTarget];
    if (!width) width = 16;
    width += diff;
    if (width > 100) {
      this.columnMaxWidthMap[this.resizeTarget] = width;
      this.requestUpdate();
    }
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

  private updateVisibleItems() {
    let items: EventLine[];
    if (this.mode === 'patterns') {
      items = this.spanListTree;
    } else if (this.view === 'tree') {
      items = this.spanListTree.filter((e) => e.show);
    } else {
      items = this.spanListTree;
    }
    this.visibleItems = items;

    // Build virtual list with special items
    const virtualItems: VirtualListItem[] = [];

    if (this.mode === 'patterns') {
      virtualItems.push(...items);
      if (this.hasMore || items.length > 0) virtualItems.push({ type: 'loadMore' });
    } else {
      // Add fetch recent button at the start (for non-flipped) or end (for flipped)
      if (!this.flipDirection && items.length > 0) {
        virtualItems.push({ type: 'fetchRecent' });
      }

      // Add all data items
      virtualItems.push(...items);

      // Add load more button at the end (for non-flipped) or start (for flipped)
      if (!this.flipDirection && (this.hasMore || items.length > 0)) {
        virtualItems.push({ type: 'loadMore' });
      } else if (this.flipDirection) {
        // For flipped direction, add buttons in reverse order
        if (items.length > 0) {
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

  expandTrace = (tracId: string, spanId: string) => {
    this.shouldScrollToBottom = false;
    this.expandedTraces[spanId] = !this.expandedTraces[spanId];
    const expanded = this.expandedTraces[spanId];
    let found = false;
    for (let i = 0; i < this.spanListTree.length; i++) {
      const span = this.spanListTree[i];
      if (span.traceId === tracId) {
        if (span.id === spanId) {
          span.expanded = expanded;
          span.show = true;
        } else if (span.parentIds.includes(spanId)) {
          span.expanded = expanded;
          span.show = expanded;
          this.expandedTraces[span.id] = expanded;
        }
      }
    }
    // Update visible items after changing show properties
    this.updateVisibleItems();
    // For user interactions, update immediately
    this.requestUpdate();
  };

  fetchData = async (url: string, isRefresh = false, isRecentFetch = false, isLoadMore = false) => {
    if (isRecentFetch && this.isFetchingRecent) return;
    if (isLoadMore && this.isLoadingMore) return;
    if (!isRecentFetch && !isLoadMore && this.isLoading) return;

    if (isRecentFetch) this.isFetchingRecent = true;
    else if (isLoadMore) this.isLoadingMore = true;
    else this.isLoading = true;

    this.showLoadingSpinner(true);

    try {
      const { tree, meta } = await this.workerFetch(url);

      // Handle results
      if (tree.length === 0) {
        this.hasMore = meta.hasMore || false;
        this.expandTimeRange = !meta.hasMore;
        return;
      }

      this.hasMore = meta.hasMore !== false;
      if (isLoadMore || isRefresh || !this.spanListTree.length) this.nextFetchUrl = meta.nextUrl;
      if (isRecentFetch || !this.spanListTree.length) this.recentFetchUrl = meta.recentUrl;
      if (isLoadMore) {
        this.loadedCount += meta.queryResultCount ?? 0;
      } else {
        this.loadedCount = meta.queryResultCount ?? 0;
      }
      if (meta.count !== undefined && !isLoadMore) this.totalCount = meta.count;
      if (meta.totalPatterns !== undefined && !isLoadMore) this.totalPatterns = meta.totalPatterns;
      this.updateRowCountDisplay();
      if (meta.serviceColors) Object.assign(this.serviceColors, meta.serviceColors);
      this.logsColumns = meta.cols;
      this.colIdxMap = meta.colIdxMap;
      // Cache server traces for re-render (expand/collapse, flip direction)
      // Cap at 5000 entries to prevent unbounded growth during pagination
      if ((isLoadMore || isRecentFetch) && meta.traces?.length) {
        this.cachedServerTraces = [...this.cachedServerTraces, ...meta.traces].slice(-5000);
      } else if (meta.traces) {
        this.cachedServerTraces = meta.traces;
      }

      if (isRefresh) {
        this.spanListTree = tree;
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
          const shouldBuffer =
            this.isLiveStreaming && ((scrollTop > 30 && !this.flipDirection) || (!scrolledToBottom && this.flipDirection));
          if (shouldBuffer) {
            this.recentDataToBeAdded = this.addWithFlipDirection(this.recentDataToBeAdded, tree, isRecentFetch);
          } else {
            this.spanListTree = this.addWithFlipDirection(this.spanListTree, tree, isRecentFetch);
            this.updateVisibleItems();
          }
        }
      } else {
        this.spanListTree = this.addWithFlipDirection(this.spanListTree, tree, isRecentFetch);
        this.updateVisibleItems();
      }

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
      console.error(error);
      this.showErrorToast(error instanceof Error ? error.message : 'Network error');
    } finally {
      this.isLoading = false;
      this.isFetchingRecent = false;
      this.isLoadingMore = false;
      this.showLoadingSpinner(false);
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
    this.logsColumns = this.logsColumns.filter((col) => col !== column);
    this.requestUpdate();
  }
  handleColumnsChanged(e: { detail: string[] }) {
    this.logsColumns = e.detail;
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

  private addWithFlipDirection(current: any[], newData: any[], isRecentFetch: boolean) {
    const result = this.flipDirection
      ? isRecentFetch
        ? [...current, ...newData]
        : [...newData, ...current]
      : isRecentFetch
      ? [...newData, ...current]
      : [...current, ...newData];
    return result;
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
    // Use addWithFlipDirection for consistent ordering
    this.spanListTree = this.addWithFlipDirection(this.spanListTree, this.recentDataToBeAdded, true);
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
                  color: 'rgba(0, 104, 255, .2)',
                  borderColor: 'rgb(0 104 255)',
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

    return html`
      <style>
        @keyframes fadeBg {
          0% {
            background-color: rgba(26, 116, 168, 0.15);
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
            background-color: rgb(0, 104, 255);
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

        .content-visibility-auto {
          content-visibility: auto;
          contain-intrinsic-size: auto 28px;
        }

        /* Fixed table layout for performance */
        table {
          table-layout: fixed;
        }

        /* Prevent clicks on closed popovers */
        [popover]:not(:popover-open) {
          pointer-events: none;
        }

        /* Optimize virtualizer container */
        lit-virtualizer {
          will-change: transform;
          contain: strict;
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
          'relative group-hash-full shrink-1 min-w-0 pb-32 m-0 bg-bgBase w-full h-full c-scroll overflow-y-auto will-change-scroll contain-strict',
          isInitialLoading && 'overflow-hidden'
        )}
        id="logs_list_container_inner"
        style="min-height: 500px; overflow-anchor: none;"
      >
        ${!isPatterns && this.recentDataToBeAdded.length > 0 && !this.flipDirection
          ? html` <div class="sticky top-[30px] z-50 flex justify-center">
              <button
                class="cbadge-sm badge-neutral cursor-pointer bg-fillBrand-strong text-textInverse-strong shadow rounded-lg text-sm"
                @pointerdown=${this.handleRecentClick}
              >
                ${this.recentDataToBeAdded.length} new
              </button>
            </div>`
          : nothing}
        <table
          class="table-fixed ${isPatterns || this.wrapLines ? 'w-full' : 'w-max'} relative ctable table-pin-rows table-pin-cols text-sm"
          style=${Object.entries(
            this.logsColumns.reduce((acc, column) => {
              const width = this.columnMaxWidthMap[column] || this.fixedColumnWidths[column];
              if (width) {
                acc[`--col-${column}-width`] = `${width}px`;
              }
              return acc;
            }, {} as Record<string, string>)
          )
            .map(([k, v]) => `${k}: ${v}`)
            .join('; ')}
        >
          <thead class="z-10 sticky top-0 isolate">
            <tr class="text-textWeak border-b flex min-w-0 relative font-medium isolate">
              ${isInitialLoading
                ? html`
                    ${[...Array(this.logsColumns.length || 6)].map(
                      (_, idx) => html`
                        <td
                          class=${`p-0 m-0 whitespace-nowrap relative flex justify-between items-center pl-2.5 pr-2 text-sm font-normal bg-bgBase ${getSkeletonColumnWidth(
                            idx
                          )}`}
                        >
                          <div class="relative overflow-hidden">
                            <div class="h-4 rounded skeleton-shimmer w-16" style="animation-delay: ${idx * 0.1}s"></div>
                          </div>
                        </td>
                      `
                    )}
                  `
                : html`
                    ${this.logsColumns.filter((v) => v !== 'latency_breakdown').map((column) => this.logTableHeading(column))}
                    ${this.logsColumns.length > 0 && !isPatterns ? this.logTableHeading('latency_breakdown') : nothing}
                  `}
            </tr>
          </thead>
          ${isInitialLoading
            ? loadingSkeleton(this.logsColumns.length || 6)
            : html`
                <tbody class="min-w-0 text-xs">
                  <lit-virtualizer
                    .items=${this.virtualListItems}
                    .renderItem=${this.renderVirtualItem}
                    @visibilityChanged=${this.handleVisibilityChange}
                    .layout=${{
                      itemSize: {
                        ...(!this.wrapLines && !isPatterns && { height: 28 }), // Fixed height only when wrap is disabled and not patterns
                        width: '100%',
                      },
                    }}
                  ></lit-virtualizer>
                </tbody>
              `}
        </table>

        ${!isPatterns && !this.shouldScrollToBottom && this.flipDirection
          ? html` <div style="position: sticky;bottom: 0px;overflow-anchor: none;">
              <button
                @pointerdown=${() => {
                  this.shouldScrollToBottom = true;
                  this.scrollToBottom();
                  this.handleRecentConcatenation();
                }}
                data-tip="Scroll to bottom"
                class=${clsx(
                  'absolute tooltip tooltip-left right-8 bottom-2 group z-50 text-textInverse-strong flex justify-center items-center rounded-full shadow-lg h-10 w-10 transition-all duration-300 hover:shadow-xl hover:scale-110',
                  this.recentDataToBeAdded.length > 0
                    ? 'bg-gradient-to-br from-fillBrand-strong to-fillBrand-weak animate-pulse'
                    : 'bg-gradient-to-br from-fillStrong to-fillWeak'
                )}
              >
                ${this.recentDataToBeAdded.length > 0
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
    const summary = lookupVecValue<string[] | string>(dataArr, this.colIdxMap, 'summary');
    if (Array.isArray(summary)) return summary;
    try {
      return typeof summary === 'string' ? JSON.parse(summary) : [];
    } catch {
      return [];
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
          const firstKey = unescapeCache.keys().next().value;
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
      const result: TemplateResult[] = [];

      // Optimized single pass with early continues
      for (let i = 0; i < parsed.length; i++) {
        const p = parsed[i];

        // Skip right-aligned elements
        if (p.type !== 'plain' && RIGHT_PREFIX_REGEX.test(p.style)) continue;

        if (p.type === 'plain') {
          if (this.mode === 'patterns') {
            result.push(html`<span class=${`fill-textStrong ${wrapClass}`}>${highlightPlaceholders(p.content)}</span>`);
          } else {
            result.push(html`<span class=${`fill-textStrong ${wrapClass}`}>${unsafeHTML(getCachedUnescape(p.content))}</span>`);
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
          result.push(this.mode === 'patterns'
            ? html`<span class="text-textStrong">${highlightPlaceholders(value)}</span>`
            : html`<span class="text-textStrong">${value}</span>`);
        } else if (WEAK_TEXT_STYLES.has(style)) {
          result.push(this.mode === 'patterns'
            ? html`<span class="text-textWeak">${highlightPlaceholders(value)}</span>`
            : html`<span class="text-textWeak">${unsafeHTML(getCachedUnescape(value))}</span>`);
        } else {
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

    switch (key) {
      case 'pattern_count':
        const count = lookupVecValue<number>(dataArr, this.colIdxMap, key);
        const maxCount = this.mode === 'patterns' && this.visibleItems.length
          ? lookupVecValue<number>(this.visibleItems[0].data, this.colIdxMap, key) || 1
          : 1;
        const pct = (count / maxCount) * 100;
        return html`<div class="flex items-center gap-1.5 w-full min-w-0" title="${pct.toFixed(1)}% of total">
          <span class="text-sm tabular-nums text-textStrong w-10 shrink-0 text-right">${formatPatternCount(count)}</span>
          <div class="w-12 shrink-0 h-2 bg-strokeWeak/40 rounded-sm overflow-hidden">
            <div class="h-full bg-[#7ab8d0] rounded-sm" style="width:${pct}%"></div>
          </div>
        </div>`;
      case 'volume':
        const volBuckets = lookupVecValue<number[]>(dataArr, this.colIdxMap, key);
        return html`<div class="flex items-center w-full">${renderSparkline(volBuckets)}</div>`;
      case 'level':
        const lv = lookupVecValue<string>(dataArr, this.colIdxMap, key);
        if (!lv) return html`<span class="text-textWeak text-xs text-center w-full inline-block">-</span>`;
        const lvColors: Record<string, string> = { error: 'badge-error', warn: 'badge-warning', warning: 'badge-warning', info: 'badge-info', debug: 'badge-neutral' };
        return renderBadge(`cbadge-sm ${lvColors[lv.toLowerCase()] || 'badge-neutral'}`, lv);
      case 'id':
        if (this.mode === 'patterns') {
          return html`<div class="flex items-center justify-between w-3"><span class="col-span-1 h-5 rounded-sm flex w-1 bg-strokeBrand-weak"></span></div>`;
        }
        const { statusCode: status, hasErrors: errCount, className: errClass } = getErrorClassification(dataArr, this.colIdxMap);
        const isExpanded = expanded || rowData.parentIds?.some((pid: string) => this.expandedTraces[pid]);
        const indicatorClass = isExpanded ? errClass.replace('-weak', '-strong') : errClass;
        return html`
          <div class="flex items-center justify-between w-3">
            <span class="col-span-1 h-5 rounded-sm flex">
              ${renderIconWithTooltip(indicatorClass, `${errCount} errors attached; status ${status}`, html``)}
            </span>
          </div>
        `;
      case 'created_at':
      case 'timestamp':
        let timestamp = lookupVecValue<string>(dataArr, this.colIdxMap, key);
        return html`<div>
          <time class=${`monospace text-xs text-textWeak tooltip tooltip-right ${wrapClass}`} data-tip="timestamp" datetime=${timestamp}
            >${formatTimestamp(timestamp)}</time
          >
        </div>`;
      case 'latency_breakdown':
        // Cache rendered latency breakdown
        const currentWidth = this.columnMaxWidthMap['latency_breakdown'] || this.fixedColumnWidths['latency_breakdown'] || 120;
        if (!rowData._latencyCache || rowData._latencyCache.width !== currentWidth || rowData._latencyCache.expanded !== expanded) {
          const { traceStart, traceEnd, startNs, duration, childrenTimeSpans } = rowData;
          const color = this.serviceColors[lookupVecValue<string>(dataArr, this.colIdxMap, 'span_name')] || 'bg-black';
          const chil = childrenTimeSpans.map(({ startNs, duration, data }: { startNs: number; duration: number; data: any }) => ({
            startNs: startNs - traceStart,
            duration,
            color: this.serviceColors[lookupVecValue<string>(data, this.colIdxMap, 'span_name')] || 'bg-black',
          }));

          // Extract right-aligned badges from summary array
          const summaryArr = this.parseSummaryData(dataArr);
          const rightAlignedBadges: TemplateResult[] = [];

          // Use optimized parsing for right-aligned badges
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

            rightAlignedBadges.push(
              field === 'session' ? this.createSessionButton(value) : renderBadge(`cbadge-sm ${badgeStyle} bg-opacity-100`, value)
            );
          }

          const latencyHtml = html`
            <div class="flex justify-end items-center gap-1 text-textWeak pl-1 rounded-lg bg-bgBase" style="min-width:${currentWidth}px">
              ${rightAlignedBadges}
              ${spanLatencyBreakdown({
                start: startNs - traceStart,
                depth,
                duration,
                traceEnd,
                color,
                children: chil,
                barWidth: currentWidth - 12,
                expanded,
              })}
              <span class="w-1"></span>
            </div>
          `;

          rowData._latencyCache = {
            content: latencyHtml,
            width: currentWidth,
            expanded: expanded,
          };
        }
        return rowData._latencyCache.content;
      case 'summary':
        if (!rowData._summaryCache || rowData._summaryCache.wrapLines !== this.wrapLines) {
          const summaryArray = this.parseSummaryData(dataArr);
          rowData._summaryCache = { content: this.renderSummaryElements(summaryArray, this.wrapLines), wrapLines: this.wrapLines };
        }
        if (this.mode === 'patterns') {
          return html`<div class="break-all whitespace-break-spaces">${rowData._summaryCache.content}</div>`;
        }
        const errClas = hasErrors
          ? 'bg-fillError-strong text-textInverse-strong fill-textInverse-strong stroke-strokeError-strong'
          : childErrors
          ? 'border border-strokeError-strong bg-fillWeak text-textWeak fill-textWeak'
          : 'border border-strokeWeak bg-fillWeak text-textWeak fill-textWeak';
        const summaryContent = rowData._summaryCache.content;
        return html`<div class=${clsx('flex w-full gap-1', this.wrapLines ? 'items-start' : 'items-center')}>
          ${this.view === 'tree'
            ? html`
                <div class="flex items-center">
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
                        class=${`hover:border-strokeBrand-strong rounded-sm ml-1 cursor-pointer shrink-0 w-8 px-1 flex justify-center gap-[2px] text-xs items-center h-5 ${errClas}`}
                      >
                        ${expanded ? faSprite('minus', 'regular', 'w-3 h-1 shrink-0') : faSprite('plus', 'regular', 'w-3 h-3 shrink-0')}
                        ${children}
                      </button>`
                    : depth === 0
                    ? nothing
                    : html`<div class=${`rounded-sm ml-1 shrink-0 w-3 h-5 ${errClas}`}></div>`}
                </div>
              `
            : nothing}
          <div class=${clsx('flex items-center gap-1', this.wrapLines ? 'break-all flex-wrap' : 'overflow-hidden')}>${summaryContent}</div>
        </div>`;
      case 'service':
        let serviceData = lookupVecValue<string>(dataArr, this.colIdxMap, key);
        return renderBadge('cbadge-sm badge-neutral ' + wrapClass, serviceData, key);
      default:
        let v = lookupVecValue<string>(dataArr, this.colIdxMap, key);
        return html`<span class=${wrapClass} title=${key}>${v}</span>`;
    }
  };

  createLoadButton = (text: string) => html`<span class="text-textBrand underline font-semibold w-max mx-auto">${text}</span>`;

  createLoadingRow = (id: string | null, content: TemplateResult, onClick?: () => void) => html`
    <tr
      class=${clsx('w-full flex relative h-[28px]', onClick && 'cursor-pointer hover:bg-fillWeaker')}
      id=${id || nothing}
      @click=${onClick ? () => onClick() : nothing}
    >
      <td colspan=${String(this.logsColumns.length)} class="relative pl-[calc(40vw-10ch)]">
        <div class="h-7 flex items-center justify-center">${content}</div>
      </td>
    </tr>
  `;

  renderExpandTimeRangeButton = () => {
    const expandTimeRange = () => {
      this.fetchData(this.expandTimeRangeUrl(), false, false, true);
      this.expandTimeRange = false;
    };
    return this.createLoadingRow(
      null,
      this.isLoading || this.isLoadingMore
        ? html`<div class="loading loading-dots loading-md h-5"></div>`
        : this.createLoadButton('Expand time range to see more events'),
      this.isLoading || this.isLoadingMore ? undefined : expandTimeRange
    );
  };

  renderLoadMoreButton = () => {
    if (this.spanListTree.length === 0 && !this.isLoading && !this.hasMore && !this.flipDirection) {
      return emptyState(this.logsColumns.length);
    }
    if (this.expandTimeRange && !this.hasMore && !!this.spanListTree.length) return this.renderExpandTimeRangeButton();
    if (!this.hasMore || !this.spanListTree.length) return html`<tr></tr>`;

    const loadMore = () => this.fetchData(this.buildLoadMoreUrl(), false, false, true);

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

    const isLoadingState = this.isLoading || this.isLoadingMore;
    return html`
      <tr
        class=${clsx('w-full flex relative h-[28px]', !isLoadingState && 'cursor-pointer hover:bg-fillWeaker')}
        ${ref(loadMoreRef)}
        @click=${isLoadingState ? nothing : () => loadMore()}
      >
        <td colspan=${String(this.logsColumns.length)} class="relative pl-[calc(40vw-10ch)]">
          <div class="h-7 flex items-center justify-center">
            ${isLoadingState ? html`<div class="loading loading-dots loading-md h-5"></div>` : this.createLoadButton('Load more')}
          </div>
        </td>
      </tr>
    `;
  };

  renderFetchRecentButton = () => {
    if (this.spanListTree.length === 0 && !this.isLoading && !this.hasMore && this.flipDirection) {
      return emptyState(this.logsColumns.length);
    }

    if (!this.spanListTree.length) return html`<tr></tr>`;

    const fetchRecent = () => this.fetchData(this.buildRecentFetchUrl(), false, true);
    return this.createLoadingRow(
      'recent-logs',
      this.isLiveStreaming
        ? html`<p class="h-5 leading-5 m-0">Live streaming latest data...</p>`
        : this.isFetchingRecent
        ? html`<div class="loading loading-dots loading-md h-5"></div>`
        : this.createLoadButton('Check for recent data'),
      this.isLiveStreaming || this.isFetchingRecent ? undefined : fetchRecent
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
    const config = {
      pattern_count: { title: 'count', classes: 'shrink-0' },
      volume: { title: '~volume', classes: 'shrink-0' },
      level: { title: 'status', classes: 'shrink-0' },
      timestamp: { title: 'timestamp', classes: 'shrink-0' },
      created_at: { title: 'timestamp', classes: 'shrink-0' },
      latency_breakdown: { title: 'latency', classes: 'sticky right-0 shrink-0' },
      status_code: { title: 'status', classes: 'shrink-0' },
      method: { title: 'method', classes: 'shrink-0' },
      raw_url: { title: column, classes: 'shrink-0' },
      url_path: { title: column, classes: 'shrink-0' },
      service: { title: 'service', classes: 'shrink-0' },
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
    // Regular event line item
    return this.logItemRow(item as EventLine);
  };

  logItemRow = (rowData: EventLine) => {
    try {
      const isPatterns = this.mode === 'patterns';
      const s = rowData.type === 'log' ? 'logs' : 'spans';
      const targetInfo = isPatterns ? '' : requestDumpLogItemUrlPath(rowData.data, this.colIdxMap, s);
      const isNew = rowData.isNew;

      // Pre-calculate CSS custom properties for widths
      const columnStyles = this.logsColumns.reduce((acc, column) => {
        const width = this.columnMaxWidthMap[column] || this.fixedColumnWidths[column];
        if (width) {
          acc[`--col-${column}-width`] = `${width}px`;
        }
        return acc;
      }, {} as Record<string, string>);

      const rowHtml = html`
        <tr
          class=${clsx(
            'item-row relative p-0 flex group whitespace-nowrap hover:bg-fillWeaker contain-layout-style-paint content-visibility-auto isolate cursor-pointer',
            isPatterns && 'items-start',
            !this.wrapLines && !isPatterns && 'h-[28px]',
            isNew && 'animate-fadeBg'
          )}
          style=${Object.entries(columnStyles)
            .map(([k, v]) => `${k}: ${v}`)
            .join('; ')}
          @click=${isPatterns ? nothing : (event: any) => this.toggleLogRow(event, targetInfo, this.projectId)}
        >
          ${this.logsColumns
            .filter((v) => v !== 'latency_breakdown')
            .map((column) => {
              const hasWidth = this.columnMaxWidthMap[column] || this.fixedColumnWidths[column];
              return html`<td
                class=${`${this.wrapLines ? 'break-all whitespace-break-spaces' : ''} bg-bgBase group-hover:bg-inherit relative pl-2 ${
                  column === 'summary' ? 'flex-1 min-w-0' : 'flex-shrink-0'
                } ${hasWidth && !(isPatterns && column === 'summary') ? `col-${column}` : ''}`}
              >
                ${this.logItemCol(rowData, column)}
              </td>`;
            })}
          ${this.logsColumns.includes('latency_breakdown')
            ? html`<td class="sticky right-0 bg-bgBase z-10 pl-2">${this.logItemCol(rowData, 'latency_breakdown')}</td>`
            : nothing}
        </tr>
      `;
      return rowHtml;
    } catch (error) {
      return html`<tr>
        <td colspan="${this.logsColumns.length}">Error rendering row: ${error.message}</td>
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
        </ul>
        <div
          @mousedown=${(event: any) => {
            this.resizeTarget = column;
            this.mouseState = { x: event.clientX };
            document.body.style.userSelect = 'none';
          }}
          class="w-3 text-textWeak text-right select-none hover:text-textBrand overflow-hidden font-bold absolute right-0 top-1/2 -translate-y-1/2 h-4 cursor-ew-resize"
        >
          |
        </div>
      </td>
    `;
  }

  createSessionButton = (sessionId: string) => html`
    <button
      class="flex items-center gap-1 shrink-0 cbadge-sm badge-neutral cursor-pointer"
      @click=${(e: any) => {
        e.stopPropagation();
        e.preventDefault();
      }}
      @pointerdown=${(e: any) => {
        e.stopPropagation();
        e.preventDefault();
        window.dispatchEvent(new CustomEvent('loadSessionReplay', { detail: { sessionId }, bubbles: true, cancelable: false }));
        // Cache the wrapper reference
        let wrapper = this.sessionPlayerWrapper;
        if (!wrapper) {
          wrapper = document.querySelector('#sessionPlayerWrapper');
          this.sessionPlayerWrapper = wrapper;
        }
        if (wrapper) wrapper.classList.remove('hidden');
      }}
    >
      ${faSprite('play', 'regular', 'w-4 h-4')} Play recording
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
            class=${`flex cursor-pointer items-center justify-center gap-1 px-2 py-1 text-xs rounded focus:bg-fillBrand-strong focus:text-white focus:fill-white`}
          >
            ${faSprite('gear', 'regular', `h-3 w-3 `)}
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
                this.spanListTree = this.addWithFlipDirection(this.spanListTree, this.recentDataToBeAdded, true);
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

function spanLatencyBreakdown({
  start,
  duration,
  traceEnd,
  depth,
  color,
  children,
  barWidth,
  expanded,
}: {
  start: number;
  duration: number;
  traceEnd: number;
  depth: number;
  color: string;
  barWidth: number;
  children: (ChildrenForLatency & { color: string })[];
  expanded?: boolean;
}) {
  const width = (duration / traceEnd) * barWidth;
  const left = (start / traceEnd) * barWidth;

  // Enhanced base visualization with subtle gradient
  const baseVisualization = html`
    <div class="flex h-5 relative bg-fillWeak overflow-x-hidden rounded-sm" style=${`width:${barWidth}px`}>
      <div
        class=${`h-full absolute top-0 rounded-sm ${depth === 0 || children.length === 0 ? color : ''}`}
        style=${`width:${width}px; left:${left}px; background-image: linear-gradient(to right, transparent, rgba(255,255,255,0.1), transparent)`}
      ></div>
      ${children.map((child) => {
        const cWidth = (child.duration / traceEnd) * barWidth;
        const cLeft = (child.startNs / traceEnd) * barWidth;
        return html`<div class=${`h-full absolute top-0 rounded-sm ${child.color}`} style=${`width:${cWidth}px; left:${cLeft}px`}></div>`;
      })}
    </div>
  `;

  // For child spans (depth > 0) OR expanded root spans, add the frame overlay
  if (depth > 0 || (depth === 0 && expanded)) {
    return html`<div class="-mt-1 shrink-0">
      <div class="flex h-5 relative" style=${`width:${barWidth}px`}>
        ${baseVisualization}

        <!-- Enhanced overlay frame elements with glow effect -->
        <!-- Full width boundary markers at the start and end -->
        <div
          class="absolute top-0 h-full border-l-2 border-strokeBrand-strong pointer-events-none"
          style="left:0; box-shadow: 0 0 4px rgba(26, 116, 168, 0.3)"
        ></div>
        <div
          class="absolute top-0 h-full border-r-2 border-strokeBrand-strong pointer-events-none"
          style=${`left:${barWidth - 2}px; box-shadow: 0 0 4px rgba(26, 116, 168, 0.3)`}
        ></div>

        <!-- Horizontal line representing the full timeline -->
        <div
          class="absolute top-1/2 -translate-y-1/2 h-[1px] bg-strokeBrand-strong pointer-events-none"
          style=${`width:${barWidth}px; left:0; box-shadow: 0 0 2px rgba(26, 116, 168, 0.2)`}
        ></div>
      </div>
    </div>`;
  }

  // For root spans that are not expanded, return just the base visualization
  return html`<div class="-mt-1 shrink-0">${baseVisualization}</div>`;
}

const skeletonCell = (idx: number, totalCols: number) => {
  const classes = clsx(
    'bg-bgBase relative pl-2',
    idx === 0 && 'w-3',
    idx === totalCols - 1 && 'sticky right-0 z-10',
    idx > 0 && idx < totalCols - 1 && getSkeletonColumnWidth(idx)
  );

  if (idx === 0) {
    return html`<td class=${classes}>
      <div class="w-1 h-5 bg-fillBrand-strong opacity-20 rounded-full skeleton-glow"></div>
    </td>`;
  }

  return html`<td class=${classes}>
    <div class="relative overflow-hidden">
      <div class="h-4 rounded skeleton-shimmer skeleton-wave ${idx === totalCols - 1 ? 'w-24' : 'w-3/4'}"></div>
      ${idx === totalCols - 1
        ? html`<div class="absolute right-0 top-0 h-full w-16 bg-gradient-to-r from-transparent to-bgBase"></div>`
        : nothing}
    </div>
  </td>`;
};

const skeletonRow = (rowIdx: number, cols: number) => html`
  <tr class="item-row relative p-0 flex items-center group whitespace-nowrap" style="--row-index: ${rowIdx}">
    ${map(Array(cols), (_, idx) => skeletonCell(idx, cols))}
  </tr>
`;

function loadingSkeleton(cols: number) {
  const actualCols = cols || 6;
  return html`
    <tbody class="min-w-0 text-xs">
      <tr class="w-full flex justify-center">
        <td colspan=${String(actualCols)} class="w-full">
          <div class="text-center py-6">
            <span class="loading loading-spinner loading-md text-fillBrand-strong"></span>
            <p class="text-sm text-textWeak mt-3">Loading events...</p>
          </div>
        </td>
      </tr>
      ${map(Array(10), (_, rowIdx) => skeletonRow(rowIdx, actualCols))}
    </tbody>
  `;
}

function emptyState(cols: number) {
  let title = `No Events found`;
  let subText = `You're either not sending events to Monoscope yet or no results matched your query/filter`;
  return html`
    <tr class="w-full flex justify-center">
      <td colspan=${String(cols)} class="w-full mx-auto">
        <div class="w-max mx-auto my-8 text-center p-5 sm:py-14 sm:px-24 flex flex-col gap-4">
          <div class="relative">
            <div class="absolute inset-0 -m-8">
              <div class="w-full h-full rounded-full bg-gradient-to-b from-fillBrand-weak to-transparent opacity-20 blur-xl"></div>
            </div>
            <div class="relative">
              ${faSprite('empty', 'regular', 'h-24 w-24 mx-auto stroke-strokeBrand-strong fill-fillBrand-strong opacity-80')}
            </div>
          </div>
          <div class="flex flex-col gap-3">
            <h2 class="text-2xl text-textStrong font-bold bg-gradient-to-r from-textStrong to-textBrand bg-clip-text text-transparent">
              ${title}
            </h2>
            <p class="text-sm max-w-md font-medium text-textWeak leading-relaxed">${subText}</p>
            <a
              href="https://monoscope.tech/docs/sdks/"
              target="_BLANK"
              class="btn text-sm w-max mx-auto btn-primary bg-gradient-to-r from-fillBrand-strong to-fillBrand-weak hover:shadow-lg transition-all duration-300 hover:scale-105 border-0"
            >
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
