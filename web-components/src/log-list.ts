'use strict';
import { virtualize } from '@lit-labs/virtualizer/virtualize.js';
import { LitElement, html, css, TemplateResult, nothing } from 'lit';
import { customElement, state, query, property } from 'lit/decorators.js';
import { APTEvent, ChildrenForLatency, ColIdxMap, EventLine, Trace, TraceDataMap } from './types/types';
import { RangeChangedEvent, VisibilityChangedEvent } from '@lit-labs/virtualizer';
import debounce from 'lodash/debounce';
import memoize from 'lodash/memoize';

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
`;

@customElement('log-list')
export class LogList extends LitElement {
  @property({ type: String }) private windowTarget: string | null = null;

  @state() private expandedTraces: Record<string, boolean> = {};
  @state() private flipDirection: boolean = false;
  @state() private spanListTree: EventLine[] = [];
  @state() private recentDataToBeAdded: any[] = [];
  @state() private view: 'tree' | 'list' = 'tree';
  @state() private shouldScrollToBottom: boolean = false;
  @state() private logsColumns: string[] = [];
  @state() private wrapLines: boolean = false;
  @state() private hasMore: boolean = false;
  @state() private isLiveStreaming: boolean = false;
  @state() private loadingState: 'idle' | 'loading' | 'loading-recent' | 'loading-replace' = 'idle';
  @state() private initialDataLoaded: boolean = false;
  @state() private showRefreshLoader: boolean = false;
  
  // Refs for DOM elements
  @query('#logs_list_container_inner') private logsContainer?: HTMLElement;
  @query('#loader') private loaderElement?: HTMLElement;
  @query('#log_details_container') private logDetailsContainer?: HTMLElement;
  @query('#resizer-details_width-wrapper') private resizerWrapper?: HTMLElement;
  @query('#details_indicator') private detailsIndicator?: HTMLElement;

  private resizeTarget: string | null = null;
  private mouseState: { x: number } = { x: 0 };
  private colIdxMap: ColIdxMap = {};
  private serviceColors: Record<string, string> = {};
  private columnMaxWidthMap: ColIdxMap = {};
  private nextFetchUrl: string = '';
  private recentFetchUrl: string = '';
  private fetchedNew: boolean = false;
  private liveStreamInterval: NodeJS.Timeout | null = null;
  private barChart: any = null;
  private projectId: string = '';
  private resetLogsUrl: string = '';
  private lineChart: any = null;
  private _observer: IntersectionObserver | null = null;
  private fetchDebounceTimer: NodeJS.Timeout | null = null;
  private pendingFetchUrl: string | null = null;
  private totalCount: number = 0;
  private updateBatchTimer: NodeJS.Timeout | null = null;
  private pendingUpdates: Set<string> = new Set();
  private handleMouseUp: (() => void) | null = null;
  private summaryDataCache: WeakMap<any[], string[]> = new WeakMap();
  private sessionPlayerWrapper: HTMLElement | null = null;
  
  // Memoized functions
  private memoizedBuildSpanListTree: any;
  private memoizedRenderSummaryElements: any;
  
  // Debounced functions
  private debouncedHandleScroll: any;
  private debouncedHandleResize: any;
  private debouncedFetchData: any;
  
  // Bound functions for event listeners
  private boundHandleResize: any;

  constructor() {
    super();

    // Initialize log list component

    // Bind methods that need this context
    [
      'logItemRow',
      'fetchData',
      'expandTrace',
      'handleChartZoom',
      'updateColumnMaxWidthMap',
      'toggleLogRow',
      'logItemCol',
      'toggleColumnOnTable',
    ].forEach((m) => (this[m] = this[m].bind(this)));
    
    // Initialize debounced functions
    this.debouncedHandleScroll = debounce(this.handleScroll.bind(this), 150);
    this.debouncedHandleResize = debounce(this.handleResize.bind(this), 50);
    this.debouncedFetchData = debounce(this.fetchData.bind(this), 300);
    
    // Bind resize handler for immediate feedback
    this.boundHandleResize = this.handleResize.bind(this);
    
    // Initialize memoized functions
    this.memoizedBuildSpanListTree = memoize(
      (logs: any[][]) => groupSpans(logs, this.colIdxMap, this.expandedTraces, this.flipDirection),
      (logs) => {
        // Create a more efficient cache key
        const logCount = logs.length;
        const firstId = logs[0]?.[this.colIdxMap['id']] || '';
        const lastId = logs[logs.length - 1]?.[this.colIdxMap['id']] || '';
        const expandedCount = Object.values(this.expandedTraces).filter(Boolean).length;
        return `${logCount}-${firstId}-${lastId}-${expandedCount}-${this.flipDirection}`;
      }
    );
    this.memoizedRenderSummaryElements = memoize(
      (summaryArray: string[], wrapLines: boolean) => this._renderSummaryElementsImpl(summaryArray, wrapLines),
      (summaryArray, wrapLines) => {
        // Use a hash of the array content for efficient caching
        const hash = summaryArray.length + '-' + summaryArray[0] + '-' + summaryArray[summaryArray.length - 1] + '-' + wrapLines;
        return hash;
      }
    );

    const liveBtn = document.querySelector('#streamLiveData') as HTMLInputElement;
    if (liveBtn) {
      liveBtn.addEventListener('change', () => {
        if (liveBtn.checked) {
          this.isLiveStreaming = true;
          this.liveStreamInterval = setInterval(() => {
            this.fetchData(this.recentFetchUrl, true);
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

    // Listen to all relevant events with debouncing
    ['submit', 'add-query', 'update-query'].forEach((ev) => window.addEventListener(ev, () => this.debouncedRefetchLogs()));
    
    // Also listen to form submit and update-query from filterElement
    document.addEventListener('submit', (e) => {
      if ((e.target as HTMLElement)?.id === 'log_explorer_form') {
        e.preventDefault();
        this.debouncedRefetchLogs();
      }
    });
    
    document.addEventListener('update-query', (e) => {
      if ((e.target as HTMLElement)?.id === 'filterElement') this.debouncedRefetchLogs();
    });

    window.addEventListener('pagehide', () => {
      if (this.liveStreamInterval) clearInterval(this.liveStreamInterval);
    });

    this.handleMouseUp = () => {
      this.resizeTarget = null;
      document.body.style.userSelect = 'auto';
    };
    window.addEventListener('mouseup', this.handleMouseUp);
    window.addEventListener('mousemove', this.boundHandleResize);

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

  updateChartDataZoom(start: number, end: number) {
    // Chart data zoom functionality - currently disabled
  }

  private buildJsonUrl(): string {
    // Preserve all existing query parameters and add json=true
    const p = new URLSearchParams(window.location.search);
    p.set('json', 'true');
    const pathName = window.location.pathname;
    return `${window.location.origin}${pathName}?${p.toString()}`;
  }

  async fetchInitialData() {
    this.fetchData(this.buildJsonUrl(), false, true);
  }

  async refetchLogs() {
    this.showRefreshLoader = true;
    this.requestUpdate();
    this.showLoadingSpinner(true);
    this.fetchData(this.buildJsonUrl(), false, true);
  }

  debouncedRefetchLogs() {
    // Clear any existing timer
    if (this.fetchDebounceTimer) {
      clearTimeout(this.fetchDebounceTimer);
    }
    
    // Show refresh loader and spinner immediately for user feedback
    this.showRefreshLoader = true;
    this.requestUpdate();
    this.showLoadingSpinner(true);
    
    // Set new timer
    this.fetchDebounceTimer = setTimeout(() => {
      this.fetchDebounceTimer = null;
      this.refetchLogs();
    }, 300);
  }

  toggleColumnOnTable(col: string) {
    const p = new URLSearchParams(window.location.search);
    const cols = (p.get('cols') || '').split(',').filter(Boolean);
    const idx = cols.indexOf(col);
    const newCols =
      idx > -1
        ? cols.filter((_, i) => i !== idx)
        : [...cols.slice(0, cols.indexOf('summary')), col, ...cols.slice(cols.indexOf('summary'))];
    p.set('cols', newCols.join(','));
    window.history.replaceState({}, '', `${window.location.pathname}?${p}${window.location.hash}`);
    this.fetchData(this.buildJsonUrl(), false, true);
  }

  handleChartZoom(params: { batch?: { startValue: string; endValue: string }[] }) {
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
    window.history.replaceState({}, '', newUrl);
    const rangeBox = document.getElementById('currentRange');
    if (rangeBox) {
      rangeBox.innerText = `${startValue} - ${endValue}`;
    }

    this.dispatchEvent(
      new CustomEvent('update-query', {
        bubbles: true,
        detail: {
          ast: p.get('queryAST') || '',
        },
      })
    );

    // Refetch logs with the new time range
    this.debouncedRefetchLogs();
  }

  // updateTableData method is no longer needed as we fetch data directly

  toggleWrapLines = () => {
    this.wrapLines = !this.wrapLines;
    this.requestUpdate();
  };

  connectedCallback() {
    super.connectedCallback();
    const globalKey = `${this.windowTarget}Data`;
    const data = (window as any)[globalKey];
    if (this.windowTarget && data) {
      // Store the initial configuration
      this.projectId = data.projectId ?? '';

      // Clear the global data early to free memory
      (window as any)[globalKey] = null;

      // Initialize with empty state
      this.logsColumns = [];
      this.colIdxMap = {};
      this.serviceColors = {};
      this.nextFetchUrl = '';
      this.recentFetchUrl = '';
      this.expandedTraces = {};
      this.spanListTree = [];
      this.hasMore = false;
      this.fetchedNew = false;

      // Fetch initial data from the JSON endpoint
      this.fetchInitialData();
    }
  }

  private updateRowCountDisplay(count: number) {
    // Find the row count element in the parent page and update it
    const countElement = document.getElementById('row-count-display');
    if (countElement) {
      countElement.textContent = this.formatCount(count);
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
      spinner.innerHTML = '<svg class="inline-block icon w-4 h-4 animate-spin text-textBrand"><use href="/public/assets/svgs/fa-sprites/regular.svg?v=ecf9d105#spinner"></use></svg>';
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
    this.setupIntersectionObserver();
  }

  updated(changedProperties: Map<string, any>) {
    if (this.shouldScrollToBottom && this.flipDirection) {
      requestAnimationFrame(() => this.scrollToBottom());
    }
    if (changedProperties.has('spanListTree') && this.fetchedNew) {
      // Use requestIdleCallback for non-critical updates
      if ('requestIdleCallback' in window) {
        requestIdleCallback(() => {
          this.spanListTree.forEach((span) => {
            if (span.isNew) {
              span.isNew = false;
            }
          });
          this.fetchedNew = false;
        });
      } else {
        setTimeout(() => {
          this.spanListTree.forEach((span) => {
            if (span.isNew) {
              span.isNew = false;
            }
          });
          this.fetchedNew = false;
        }, 10);
      }
    }
  }

  scrollToBottom() {
    // Use ref instead of DOM query
    if (this.logsContainer) {
      this.logsContainer.scrollTop = this.logsContainer.scrollHeight;
    }
  }

  setupIntersectionObserver() {
    if (this._observer) {
      this._observer.disconnect();
    }

    // Use refs instead of DOM queries
    if (!this.loaderElement || !this.logsContainer) {
      setTimeout(() => {
        this.setupIntersectionObserver();
      }, 1000);
      return;
    }

    const observer = new IntersectionObserver(
      ([entry]) => {
        if (entry.isIntersecting) {
          this.debouncedFetchData(this.nextFetchUrl);
        }
      },
      {
        root: this.logsContainer,
        threshold: [0, 0.2, 0.4, 0.6, 0.8, 1],
      }
    );
    observer.observe(this.loaderElement);
    this._observer = observer;
  }

  disconnectedCallback() {
    // Clean up all observers and timers
    if (this._observer) {
      this._observer.disconnect();
      this._observer = null;
    }
    if (this.fetchDebounceTimer) {
      clearTimeout(this.fetchDebounceTimer);
      this.fetchDebounceTimer = null;
    }
    if (this.updateBatchTimer) {
      clearTimeout(this.updateBatchTimer);
      this.updateBatchTimer = null;
    }
    if (this.liveStreamInterval) {
      clearInterval(this.liveStreamInterval);
      this.liveStreamInterval = null;
    }
    
    // Clean up event listeners
    window.removeEventListener('mousemove', this.boundHandleResize);
    window.removeEventListener('mouseup', this.handleMouseUp);
    ['submit', 'add-query', 'update-query'].forEach((ev) => 
      window.removeEventListener(ev, this.debouncedRefetchLogs)
    );
    
    // Clean up chart event handlers
    if (this.barChart) {
      this.barChart.off('datazoom', this.handleChartZoom);
    }
    if (this.lineChart) {
      this.lineChart.off('datazoom', this.handleChartZoom);
    }
    
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
  
  private handleScroll(event: any) {
    const container = event.target;
    if (this.flipDirection) {
      if (container.scrollTop + container.clientHeight >= container.scrollHeight - 1) {
        this.shouldScrollToBottom = true;
      } else {
        this.shouldScrollToBottom = false;
        this.handleRecentConcatenation();
      }
    } else {
      if (container.scrollTop === 0) this.handleRecentConcatenation();
    }
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
    // Use memoized version for better performance
    return this.memoizedBuildSpanListTree(logs);
  }

  expandTrace(tracId: string, spanId: string) {
    this.shouldScrollToBottom = false;
    this.expandedTraces[spanId] = !this.expandedTraces[spanId];
    const expanded = this.expandedTraces[spanId];

    const affectedSpans = this.spanListTree.filter((span) => span.traceId === tracId);
    affectedSpans.forEach((span) => {
      if (span.id === spanId) {
        span.expanded = expanded;
        span.show = true;
      } else if (span.parentIds.includes(spanId)) {
        span.expanded = expanded;
        span.show = expanded;
        this.expandedTraces[span.id] = expanded;
      }
    });

    // For user interactions, update immediately
    this.requestUpdate();
  }

  fetchData(url: string, isNewData = false, isRefresh = false) {
    const loadingState = isNewData ? 'loading-recent' : isRefresh ? 'loading-replace' : 'loading';
    if (this.loadingState !== 'idle') {
      // If a request is already in flight, store this URL to fetch after current one completes
      if (isRefresh) {
        this.pendingFetchUrl = url;
      }
      return;
    }
    this.loadingState = loadingState;
    this.showLoadingSpinner(true);
    fetch(url, {
      method: 'GET',
      headers: {
        Accept: 'application/json',
      },
    })
      .then((response) => response.json())
      .then((data) => {
        if (!data.error) {
          let { logsData, serviceColors, nextUrl, recentUrl, cols, colIdxMap, resetLogsUrl, count } = data;

          // Validate required fields - but allow empty arrays
          if (!Array.isArray(logsData)) {
            this.showErrorToast('Invalid data format received');
            this.initialDataLoaded = true; // Mark as loaded even on error
            this.requestUpdate();
            return;
          }

          // Update state
          if (!isNewData) {
            this.hasMore = logsData.length > 0;
            this.nextFetchUrl = nextUrl || '';
          } else if (isNewData && logsData.length > 0) {
            this.recentFetchUrl = recentUrl || '';
          }

          this.resetLogsUrl = resetLogsUrl || this.resetLogsUrl;

          // Update the count if provided
          if (count !== undefined) {
            this.totalCount = count;
            this.updateRowCountDisplay(count);
          }

          // Only update service colors if new ones are provided
          if (serviceColors && Object.keys(serviceColors).length > 0) {
            Object.assign(this.serviceColors, serviceColors);
          }
          let tree = this.buildSpanListTree(logsData);

          if (isRefresh) {
            this.logsColumns = cols;
            this.colIdxMap = colIdxMap;
            this.nextFetchUrl = nextUrl;
            this.recentFetchUrl = recentUrl;
            this.spanListTree = tree;
            // Scroll to bottom after initial load
            if (this.spanListTree.length > 0) {
              this.scrollToBottom();
            }
          } else {
            if (isNewData) {
              this.fetchedNew = true;
              tree.forEach((t) => (t.isNew = true));
              const container = document.querySelector('#logs_list_container_inner')!;
              const scrolledToBottom = container.scrollTop + container.clientHeight >= container.scrollHeight - 1;
              if (container && scrolledToBottom) {
                this.shouldScrollToBottom = true;
              }
              if (this.isLiveStreaming && container && container.scrollTop > 30 && !this.flipDirection) {
                this.recentDataToBeAdded = this.addWithFlipDirection(this.recentDataToBeAdded, tree);
              } else if (this.isLiveStreaming && !scrolledToBottom && this.flipDirection) {
                this.recentDataToBeAdded = this.addWithFlipDirection(this.recentDataToBeAdded, tree);
              } else {
                // More efficient array operations
                if (this.flipDirection) {
                  this.spanListTree.push(...tree);
                } else {
                  this.spanListTree.unshift(...tree);
                }
              }
            } else {
              // For pagination loading
              if (this.flipDirection) {
                this.spanListTree = [...tree, ...this.spanListTree];
              } else {
                this.spanListTree.push(...tree);
              }
            }
          }

          this.updateColumnMaxWidthMap(logsData);
        } else {
          this.showErrorToast(data.message || 'Failed to fetch logs');
          // Still need to mark as loaded to hide skeleton
          this.initialDataLoaded = true;
          this.batchRequestUpdate('fetchError');
        }
      })
      .catch((error) => {
        this.showErrorToast('Network error: Unable to fetch logs');
      })
      .finally(() => {
        this.loadingState = 'idle';
        this.initialDataLoaded = true;
        this.showRefreshLoader = false;
        this.showLoadingSpinner(false);
        this.requestUpdate();
        
        // If there's a pending fetch, execute it now
        if (this.pendingFetchUrl) {
          const url = this.pendingFetchUrl;
          this.pendingFetchUrl = null;
          setTimeout(() => this.fetchData(url, false, true), 100);
        }
      });
  }

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
  updateColumnMaxWidthMap(recVecs: any[][]) {
    // Defer non-critical calculations
    requestAnimationFrame(() => {
      const columnDefaults = { summary: 450 * 8.5, latency_breakdown: 100 };
      const charWidths = { timestamp: 6.5, default: 8.5 };

      // Process in batches for better performance
      const batchSize = 100;
      for (let i = 0; i < recVecs.length; i += batchSize) {
        const batch = recVecs.slice(i, Math.min(i + batchSize, recVecs.length));
        
        batch.forEach((vec) => {
          Object.entries(this.colIdxMap).forEach(([key, value]) => {
            if (key === 'id') return;

            // Set defaults for special columns
            if (columnDefaults[key] && !this.columnMaxWidthMap[key]) {
              this.columnMaxWidthMap[key] = columnDefaults[key];
            }

            // Skip if already set for special columns
            if ((key === 'latency_breakdown' || key === 'summary') && this.columnMaxWidthMap[key]) return;

            const chPx = charWidths[key] || charWidths.default;
            const target = String(vec[value]).length * chPx;

            this.columnMaxWidthMap[key] = Math.max(this.columnMaxWidthMap[key] || 12 * chPx, target);
          });
        });
      }
    });
  }
  toggleLogRow(event: any, targetInfo: [string, string, string], pid: string) {
    // Use refs when available, fallback to querySelector
    const sideView = this.logDetailsContainer || document.querySelector('#log_details_container')! as HTMLElement;
    const resizerWrapper = this.resizerWrapper || document.querySelector('#resizer-details_width-wrapper');
    const width = Number(getComputedStyle(sideView).width.replace('px', ''));
    this.shouldScrollToBottom = false;
    if (width < 50) {
      sideView.style.width = `550px`;
      updateUrlState('details_width', '550');
    }
    // Always show the resizer when a log row is clicked
    if (resizerWrapper) {
      resizerWrapper.classList.remove('hidden', 'opacity-0', 'pointer-events-none');
    }
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
  }

  moveColumn(column: string, direction: number) {
    const index = this.logsColumns.indexOf(column);
    if (index === -1) return;
    const newIndex = index + direction;
    if (newIndex < 0 || newIndex >= this.logsColumns.length) return;
    this.logsColumns[index] = this.logsColumns[newIndex];
    this.logsColumns[newIndex] = column;
    this.requestUpdate();
  }

  addWithFlipDirection(current: any[], newData: any[]) {
    // Avoid creating new arrays when possible
    if (this.flipDirection) {
      current.push(...newData);
      return current;
    } else {
      newData.push(...current);
      return newData;
    }
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
    // Use more efficient array concatenation
    if (this.flipDirection) {
      this.spanListTree.push(...this.recentDataToBeAdded);
    } else {
      this.spanListTree.unshift(...this.recentDataToBeAdded);
    }
    this.recentDataToBeAdded = [];
    this.batchRequestUpdate('recentConcatenation');
  }

  // Comment to allow classes be rendered.
  render() {
    const list: (EventLine | 'start' | 'end')[] = this.view === 'tree' ? this.spanListTree.filter((sp) => sp.show) : [...this.spanListTree];
    // end is used to render the load more button"
    list.unshift('start');
    list.push('end');
    
    // Check if we're in initial loading state
    const isInitialLoading = !this.initialDataLoaded;

    return html`
      ${this.options()}
      <div
        @scroll=${this.debouncedHandleScroll}
        class="relative h-full shrink-1 min-w-0 p-0 m-0 bg-bgBase w-full c-scroll pb-12 overflow-y-auto ${isInitialLoading ? 'overflow-hidden' : ''}"
        id="logs_list_container_inner"
        style="min-height: 500px;"
      >
        ${this.recentDataToBeAdded.length > 0 && !this.flipDirection
          ? html` <div class="sticky left-1/2 -translate-x-1/2 top-[30px] z-50">
              <button
                class="relative cursor-pointer bg-gradient-to-r from-fillBrand-strong to-fillBrand-weak text-textInverse-strong shadow-lg rounded-full px-4 py-1.5 text-sm font-medium transition-all duration-300 hover:shadow-xl hover:scale-105 animate-pulse"
                @pointerdown=${this.handleRecentClick}
              >
                <span class="absolute inset-0 rounded-full bg-fillBrand-strong opacity-30 blur animate-ping"></span>
                <span class="relative flex items-center gap-2">
                  ${faSprite('arrow-up', 'solid', 'h-3 w-3')}
                  ${this.recentDataToBeAdded.length} new event${this.recentDataToBeAdded.length > 1 ? 's' : ''}
                </span>
              </button>
            </div>`
          : nothing}
        <table class="table-auto w-max relative ctable table-pin-rows table-pin-cols">
          <thead class="z-10 sticky top-0">
            <tr class="text-textStrong border-b flex min-w-0 relative font-medium ">
              ${isInitialLoading
                ? html`
                    ${[...Array(6)].map((_, idx) => html`
                      <td class="p-0 m-0 whitespace-nowrap relative flex justify-between items-center pl-2.5 pr-2 text-sm font-normal bg-bgBase ${getSkeletonColumnWidth(idx)}">
                        <div class="relative overflow-hidden">
                          <div class="h-4 rounded skeleton-shimmer w-16" style="animation-delay: ${idx * 0.1}s"></div>
                        </div>
                      </td>
                    `)}
                  `
                : html`
                    ${this.logsColumns.filter((v) => v !== 'latency_breakdown').map((column) => this.logTableHeading(column))}
                    ${this.logsColumns.length > 0 ? this.logTableHeading('latency_breakdown') : nothing}
                  `
              }
            </tr>
          </thead>
          ${isInitialLoading
            ? loadingSkeleton(this.logsColumns.length || 6)
            : html`
                <tbody
                  class="min-w-0 text-sm"
                  @rangeChanged=${(event: RangeChangedEvent) => {
                    this.setupIntersectionObserver();
                  }}
                  @visibilityChanged=${(event: VisibilityChangedEvent) => {
                    this.updateChartDataZoom(event.first, event.last);
                  }}
                >
                  ${virtualize({
                      items: list,
                      renderItem: this.logItemRow,
                      layout: {
                        itemSize: {
                          height: 28, // Fixed row height for better performance
                          width: '100%'
                        }
                      }
                    })
                  }
                </tbody>
              `}
        </table>

        ${!this.shouldScrollToBottom && this.flipDirection
          ? html` <div style="position: sticky;bottom: 0px;overflow-anchor: none;">
              <button
                @pointerdown=${() => {
                  this.shouldScrollToBottom = true;
                  this.scrollToBottom();
                  this.handleRecentConcatenation();
                }}
                data-tip="Scroll to bottom"
                class=${`absolute tooltip tooltip-left right-8 bottom-2 group z-50 ${
                  this.recentDataToBeAdded.length > 0 
                    ? 'bg-gradient-to-br from-fillBrand-strong to-fillBrand-weak animate-pulse' 
                    : 'bg-gradient-to-br from-fillStrong to-fillWeak'
                } text-textInverse-strong flex justify-center items-center rounded-full shadow-lg h-10 w-10 transition-all duration-300 hover:shadow-xl hover:scale-110`}
              >
                ${this.recentDataToBeAdded.length > 0 
                  ? html`<span class="absolute inset-0 rounded-full bg-fillBrand-strong opacity-30 blur animate-ping"></span>`
                  : nothing
                }
                <span class="relative">
                  ${faSprite('arrow-down', 'regular', 'h-6 w-6 fill-textInverse-strong stroke-textInverse-strong')}
                </span>
              </button>
            </div>`
          : nothing}
          
        ${this.showRefreshLoader
          ? html`
              <div class="absolute inset-0 z-50 flex items-center justify-center bg-black/5 backdrop-blur-sm">
                <div class="relative">
                  <!-- Outer rotating ring -->
                  <div class="absolute inset-0 rounded-full border-4 border-transparent border-t-fillBrand-strong animate-spin"></div>
                  <!-- Inner pulsing circle -->
                  <div class="absolute inset-2 rounded-full bg-gradient-to-br from-fillBrand-strong to-fillBrand-weak opacity-20 animate-pulse"></div>
                  <!-- Center spinner -->
                  <div class="relative flex items-center justify-center w-20 h-20">
                    <span class="loading loading-spinner loading-lg text-fillBrand-strong"></span>
                  </div>
                  <!-- Loading text -->
                  <div class="absolute -bottom-8 left-1/2 -translate-x-1/2 whitespace-nowrap">
                    <p class="text-sm font-medium text-textBrand animate-pulse">Refreshing logs...</p>
                  </div>
                </div>
              </div>
            `
          : nothing}
      </div>
    `;
  }
  createRenderRoot() {
    return this;
  }

  private parseSummaryData(dataArr: any[]): string[] {
    // Check cache first
    const cached = this.summaryDataCache.get(dataArr);
    if (cached) return cached;
    
    const summaryData = lookupVecTextByKey(dataArr, this.colIdxMap, 'summary');
    let summaryArray: string[] = [];

    if (Array.isArray(summaryData)) {
      summaryArray = summaryData;
    } else if (typeof summaryData === 'string') {
      // Handle new format: "\"[item1, item2, ...]\""
      let cleaned = summaryData;
      if (summaryData.startsWith('"') && summaryData.endsWith('"')) {
        cleaned = summaryData.slice(1, -1); // Remove outer quotes
      }
      if (cleaned.startsWith('[') && cleaned.endsWith(']')) {
        cleaned = cleaned.slice(1, -1); // Remove brackets
      }
      // Split by comma, but not commas inside braces {}
      summaryArray = cleaned.match(/[^,]+(?:{[^}]*}[^,]*)?/g) || [];
      summaryArray = summaryArray.map((s) => s.trim());
    }
    
    // Cache the result
    this.summaryDataCache.set(dataArr, summaryArray);
    return summaryArray;
  }

  renderSummaryElements(summaryArray: string[], wrapLines: boolean): any {
    if (!Array.isArray(summaryArray)) return nothing;
    
    // Use memoized version for better performance
    return this.memoizedRenderSummaryElements(summaryArray, wrapLines);
  }
  
  private _renderSummaryElementsImpl(summaryArray: string[], wrapLines: boolean): any {
    const wrapClass = wrapLines ? 'whitespace-break-spaces' : 'whitespace-nowrap';

    return summaryArray
      .filter((el) => {
        // Optimize string checking
        const sepIndex = el.indexOf('⇒');
        if (sepIndex === -1) return true;
        const semicolonIndex = el.indexOf(';');
        if (semicolonIndex === -1 || semicolonIndex > sepIndex) return true;
        const style = el.substring(semicolonIndex + 1, sepIndex);
        return !style.startsWith('right-');
      })
      .map((element) => {
        const sepIndex = element.indexOf('⇒');
        if (sepIndex === -1) {
          // Unescape any JSON content in plain text elements
          const unescapedElement = element.replace(/\\"/g, '"').replace(/\\\\/g, '\\');
          return html`<span class=${`fill-textStrong ${wrapClass}`}>${unescapedElement}</span>`;
        }

        const semicolonIndex = element.indexOf(';');
        if (semicolonIndex === -1 || semicolonIndex > sepIndex) {
          // Malformed element, treat as plain text
          return html`<span class=${`fill-textStrong ${wrapClass}`}>${element}</span>`;
        }
        
        const field = element.substring(0, semicolonIndex);
        const style = element.substring(semicolonIndex + 1, sepIndex);
        const value = element.substring(sepIndex + 1);

        // Special icon handling
        const iconConfig = {
          request_type: () =>
            renderIconWithTippy(
              'w-4',
              `${value} Request`,
              faSprite(
                value === 'incoming' ? 'arrow-down-left' : 'arrow-up-right',
                'solid',
                value === 'incoming' ? 'h-3 fill-iconNeutral' : 'h-3 fill-iconBrand'
              )
            ),
          kind: () =>
            value === 'internal' ? renderIconWithTippy('w-4 ml-2', 'Internal span', faSprite('function', 'regular', 'h-3 w-3')) : nothing,
          'db.system': () => renderIconWithTippy('w-4 ml-2', value, faSprite('database', 'regular', 'h-3 w-3 fill-iconNeutral')),
        };

        if (iconConfig[field]) return iconConfig[field]();

        // Text rendering
        if (style === 'text-weak' || style === 'text-textWeak') {
          // Unescape JSON strings
          const unescapedValue = value.replace(/\\"/g, '"').replace(/\\\\/g, '\\');
          return html`<span class="text-textWeak">${unescapedValue}</span>`;
        }

        if (style === 'text-textStrong') return html`<span class="text-textStrong">${value}</span>`;

        return renderBadge(`cbadge-sm ${this.getStyleClass(style)} ${wrapClass}`, value);
      });
  }

  getStyleClass(style: string): string {
    if (style.startsWith('badge-')) return style;
    if (style.startsWith('right-')) return style.substring(6);

    const styleMap = {
      'info-strong': 'badge-info',
      'info-weak': 'badge-neutral',
      'error-strong': 'badge-error',
      'error-weak': 'badge-4xx',
      'warning-strong': 'badge-warning',
      'warning-weak': 'badge-3xx',
      'success-strong': 'badge-success',
      'success-weak': 'badge-2xx',
      neutral: 'badge-neutral',
      right: 'ml-auto badge-neutral',
      'text-weak': '',
      'text-textWeak': '',
      'text-textStrong': '',
    };
    return styleMap[style] || 'badge-neutral';
  }

  logItemCol(rowData: any, key: string): any {
    const { data: dataArr, depth, children, traceId, childErrors, hasErrors, expanded, type, id, isLastChild, siblingsArr } = rowData;
    const wrapClass = this.wrapLines ? 'whitespace-break-spaces' : 'whitespace-nowrap';

    switch (key) {
      case 'id':
        let [status, errCount, errClass] = errorClass(dataArr, this.colIdxMap);
        return html`
          <div class="flex items-center justify-between w-3">
            <span class="col-span-1 h-5 rounded-sm flex">
              ${renderIconWithTippy(errClass, `${errCount} errors attached; status ${status}`, html``)}
            </span>
          </div>
        `;
      case 'created_at':
      case 'timestamp':
        let timestamp = lookupVecTextByKey(dataArr, this.colIdxMap, key);
        return html`<div>
          <time class="monospace text-textStrong tooltip tooltip-right ${wrapClass}" data-tip="timestamp" datetime=${timestamp}
            >${displayTimestamp(timestamp)}</time
          >
        </div>`;
      case 'latency_breakdown':
        const { traceStart, traceEnd, startNs, duration, childrenTimeSpans } = rowData;
        const color = this.serviceColors[lookupVecTextByKey(dataArr, this.colIdxMap, 'span_name')] || 'bg-black';
        const chil = childrenTimeSpans.map(({ startNs, duration, data }: { startNs: number; duration: number; data: any }) => ({
          startNs: startNs - traceStart,
          duration,
          color: this.serviceColors[lookupVecTextByKey(data, this.colIdxMap, 'span_name')] || 'bg-black',
        }));
        const width = this.columnMaxWidthMap['latency_breakdown'] || 200;

        // Extract right-aligned badges from summary array
        const summaryArr = this.parseSummaryData(dataArr);

        const rightAlignedBadges: TemplateResult[] = [];

        summaryArr.forEach((element: string) => {
          if (element.includes(';') && element.includes('⇒')) {
            const [fieldAndStyle, value] = element.split('⇒');
            const [field, style] = fieldAndStyle.split(';');

            // Only process elements with styles starting with "right-"
            if (style.startsWith('right-')) {
              const badgeStyle = this.getStyleClass(style);
              if (field === 'session') {
                const pB = html`<button
                  class="flex items-center gap-1 shrink-0 cbadge-sm badge-neutral cursor-pointer"
                  @click=${(e: any) => {
                    e.stopPropagation();
                    e.preventDefault();
                  }}
                  @pointerdown=${(e: any) => {
                    e.stopPropagation();
                    e.preventDefault();
                    window.dispatchEvent(
                      new CustomEvent('loadSessionReplay', { detail: { sessionId: value }, bubbles: true, cancelable: false })
                    );
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
                </button>`;
                rightAlignedBadges.push(pB);
              } else {
                rightAlignedBadges.push(renderBadge(`cbadge-sm ${badgeStyle} bg-opacity-100`, value));
              }
            }
          }
        });

        const latencyHtml = html`
          <div class="flex justify-end items-center gap-1 text-textWeak pl-1 rounded-lg bg-bgBase" style="min-width:${width}px">
            ${rightAlignedBadges}
            ${spanLatencyBreakdown({
              start: startNs - traceStart,
              depth,
              duration,
              traceEnd,
              color,
              children: chil,
              barWidth: width - 12,
              expanded,
            })}
            <span class="w-1"></span>
          </div>
        `;
        return latencyHtml;
      case 'summary':
        const summaryArray = this.parseSummaryData(dataArr);
        const errClas = hasErrors
          ? 'bg-fillError-strong text-textInverse-strong fill-textInverse-strong stroke-strokeError-strong'
          : childErrors
            ? 'border border-strokeError-strong bg-fillWeak text-textWeak fill-textWeak'
            : 'border border-strokeWeak bg-fillWeak text-textWeak fill-textWeak';
        return html`<div class="flex w-full ${this.wrapLines ? 'items-start' : 'items-center'} gap-1">
          ${this.view === 'tree'
            ? html`
                <div class="flex items-center">
                  ${depth > 1
                    ? new Array(depth - 1)
                        .fill(1)
                        .map(
                          (_, i) =>
                            html`<div class="w-8 h-5 shrink-0 flex items-center justify-center">
                              ${siblingsArr[i] ? faSprite('tree-straight', 'regular', 'w-8 h-5 text-iconNeutral') : nothing}
                            </div>`
                        )
                    : nothing}
                  ${depth > 0
                    ? html`<div class="w-8 h-5 shrink-0 flex items-center justify-center">
                        ${isLastChild
                          ? faSprite('tree-angle', 'regular', 'w-8 h-5 text-iconNeutral')
                          : faSprite('tree-tee', 'regular', 'w-8 h-5 text-iconNeutral')}
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
          <div class=${`flex items-center gap-1 ${this.wrapLines ? 'break-all flex-wrap' : 'overflow-hidden'}`}>
            ${this.renderSummaryElements(summaryArray, this.wrapLines)}
          </div>
        </div>`;
      case 'service':
        let serviceData = lookupVecTextByKey(dataArr, this.colIdxMap, key);
        return renderBadge('cbadge-sm badge-neutral ' + wrapClass, serviceData, key);
      default:
        let v = lookupVecTextByKey(dataArr, this.colIdxMap, key);
        return html`<span class=${wrapClass} title=${key}>${v}</span>`;
    }
  }

  renderLoadMore() {
    // Check if we have no data (empty state)
    if (this.spanListTree.length === 0 && this.initialDataLoaded && !this.hasMore && !this.flipDirection) {
      return emptyState(this.logsColumns.length);
    }
    
    if (!this.hasMore || this.windowTarget === 'sessionList' || !this.initialDataLoaded) return html`<tr></tr>`;
    return html`<tr class="w-full flex relative">
      <td colspan=${String(this.logsColumns.length)} class="relative pl-[calc(40vw-10ch)]">
        <div class="absolute -top-[500px] w-[1px] h-[500px] left-0" id="loader"></div>
        ${this.loadingState === 'loading'
          ? html`<div class="loading loading-dots loading-md"></div>`
          : html`<button
              class="cursor-pointer text-textBrand underline font-semibold w-max mx-auto"
              @pointerdown=${() => this.fetchData(this.nextFetchUrl)}
            >
              Load more
            </button>`}
      </td>
    </tr>`;
  }

  fetchRecent() {
    // Check if we have no data (empty state) when flipped
    if (this.spanListTree.length === 0 && this.initialDataLoaded && !this.hasMore && this.flipDirection) {
      return emptyState(this.logsColumns.length);
    }
    
    if (this.windowTarget === 'sessionList' || !this.initialDataLoaded) return html`<tr></tr>`;
    return html`<tr class="w-full flex relative" id="recent-logs">
      <td colspan=${String(this.logsColumns.length)} class="relative pl-[calc(40vw-10ch)]">
        ${this.isLiveStreaming
          ? html`<p>Live streaming latest data...</p>`
          : this.loadingState === 'loading-recent'
            ? html`<div class="loading loading-dots loading-md"></div>`
            : html`<button
                class="cursor-pointer text-textBrand underline font-semibold w-max mx-auto"
                @pointerdown=${() => this.fetchData(this.recentFetchUrl, true)}
              >
                Check for recent data
              </button>`}
      </td>
    </tr>`;
  }

  logTableHeading(column: string) {
    if (column === 'id') return html`<td class="p-0 m-0 whitespace-nowrap w-3 pl-2.5"></td>`;

    const config = {
      timestamp: { title: 'timestamp', classes: 'w-[17ch] shrink-0' },
      created_at: { title: 'timestamp', classes: 'w-[17ch] shrink-0' },
      latency_breakdown: { title: 'latency', classes: 'sticky right-0 shrink-0' },
      status_code: { title: 'status', classes: 'shrink-0 w-[12ch]' },
      method: { title: 'method', classes: 'shrink-0 w-[12ch]' },
      raw_url: { title: column, classes: 'w-[25ch] shrink-0' },
      url_path: { title: column, classes: 'w-[25ch] shrink-0' },
      service: { title: 'service', classes: 'w-[16ch] shrink-0' },
      summary: { title: 'summary', classes: 'w-[1400px] shrink-1' },
    };

    const { title = column, classes = 'w-[16ch] shrink-0' } = config[column] || {};
    return this.tableHeadingWrapper(title, column, classes);
  }

  logItemRow(rowData: EventLine | 'end' | 'start') {
    if (rowData === 'end') {
      if (this.flipDirection) {
        return this.fetchRecent();
      }
      return this.renderLoadMore();
    }
    if (rowData === 'start') {
      if (this.flipDirection) {
        return this.renderLoadMore();
      }
      return this.fetchRecent();
    }
    try {
      const s = rowData.type === 'log' ? 'logs' : 'spans';
      const targetInfo = requestDumpLogItemUrlPath(rowData.data, this.colIdxMap, s);
      let isNew = rowData.isNew;
      const sessionId = lookupVecTextByKey(rowData.data, this.colIdxMap, 'session_id');
      const rowHtml = html`
        <tr
          class=${`item-row relative p-0 flex items-center group cursor-pointer whitespace-nowrap transition-all duration-200 hover:bg-fillWeaker hover:shadow-sm ${isNew ? 'animate-fadeBg' : ''}`}
          @click=${(event: any) => this.toggleLogRow(event, targetInfo, this.projectId)}
        >
          ${this.logsColumns
            .filter((v) => v !== 'latency_breakdown')
            .map((column) => {
              const tableDataWidth = getColumnWidth(column);
              let width = this.columnMaxWidthMap[column];
              return html`<td
                class=${`${this.wrapLines ? 'break-all whitespace-wrap' : ''} bg-bgBase relative pl-2 ${
                  column === 'summary' ? '' : tableDataWidth
                }`}
                style=${width ? `width: ${width}px;` : ''}
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
      console.error('Error in logItemRow:', error);
      console.error('rowData:', rowData);
      console.error('Stack trace:', error.stack);
      return html`<tr>
        <td colspan="${this.logsColumns.length}">Error rendering row: ${error.message}</td>
      </tr>`;
    }
  }

  tableHeadingWrapper(title: string, column: string, classes: string) {
    let width = this.columnMaxWidthMap[column];
    if (column === 'latency_breakdown' && !width) {
      width = 100;
    }

    return html`
      <td
        class=${`cursor-pointer p-0 m-0 whitespace-nowrap relative flex justify-between items-center pl-2.5 pr-2 text-sm font-normal bg-bgBase ${
          classes ? classes : ''
        }`}
        style=${width ? `width: ${width}px` : ''}
      >
        <div class="dropdown font-medium text-base" data-tippy-content=${title}>
          <div tabindex="0" role="button" class="py-1">
            ${title.split('•').reverse()[0]}
            <span class="ml-1 p-0.5 border border-strokeWeak rounded-sm inline-flex">
              ${faSprite('chevron-down', 'regular', 'w-3 h-3')}
            </span>
          </div>
          <ul
            tabindex="0"
            class="dropdown-content z-1 flex flex-col font-normal bg-bgBase border w-64 border-strokeWeak p-2 text-sm rounded shadow"
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
        </div>
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

  renderCheckbox(label: string, icon: string, checked: boolean, onChange: (checked: boolean) => void) {
    return html` <label class="flex items-center cursor-pointer w-full gap-1 px-2 py-1 text-sm rounded text-textWeak hover:bg-fillWeaker">
      <input
        type="checkbox"
        class="checkbox checkbox-xs checkbox-primary mr-1"
        .checked=${checked}
        @change=${(e: any) => onChange(e.target.checked)}
      />
      ${faSprite(icon, 'regular', 'h-4 w-4')}
      <span class="sm:inline hidden">${label}</span>
    </label>`;
  }

  options() {
    const viewButton = (view: 'tree' | 'list', icon: string, label: string) =>
      html` <button
        @pointerdown=${() => (this.view = view)}
        class=${`flex items-center cursor-pointer justify-center gap-1 px-2 py-1 text-xs rounded ${
          this.view === view ? 'bg-fillWeak text-textStrong' : 'text-textWeak hover:bg-fillWeaker'
        }`}
      >
        ${faSprite(icon, 'regular', 'h-4 w-4')}
        <span class="sm:inline hidden">${label}</span>
      </button>`;

    return html`
      <div class="w-full flex justify-end px-2 pb-1 gap-3">
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
              this.spanListTree.reverse();
              this.recentDataToBeAdded.reverse();
              if (this.recentDataToBeAdded.length > 0) {
                this.spanListTree.push(...this.recentDataToBeAdded);
                this.recentDataToBeAdded = [];
              }
              this.requestUpdate();
            })}
            ${this.renderCheckbox('Wrap lines', 'wrap-text', this.wrapLines, (checked) => {
              this.wrapLines = checked;
              if (this.wrapLines) {
                let width = Number(window.getComputedStyle(document.getElementById('logs_list_container_inner')!).width.replace('px', ''));
                this.logsColumns.forEach((col) => {
                  if (col !== 'summary' && this.columnMaxWidthMap[col]) {
                    width -= this.columnMaxWidthMap[col] + 8;
                  }
                });
                this.columnMaxWidthMap['summary'] = width - 20;
              } else {
                this.columnMaxWidthMap['summary'] = 450 * 8;
              }
              this.requestUpdate();
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
    'parent_span_id',
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

const faSprite = (iconName: string, kind: string, classes: string) =>
  html`<svg class="${classes}"><use href="/public/assets/svgs/fa-sprites/${kind}.svg#${iconName}"></use></svg>`;

const displayTimestamp = (input: string) => {
  const date = new Date(input);
  if (!date.getTime()) return '';

  return (
    date
      .toLocaleString('en-US', {
        month: 'short',
        day: '2-digit',
        hour: '2-digit',
        minute: '2-digit',
        second: '2-digit',
        hour12: false,
      })
      .replace(',', '') + `.${String(date.getUTCMilliseconds()).padStart(3, '0')}`
  );
};

function renderBadge(classes: string, title: string, tippy = '') {
  return html`<span class=${`relative ${classes} ${tippy ? 'tooltip tooltip-right' : ''} transition-all duration-200 hover:shadow-sm`} data-tip=${tippy}>${title}</span>`;
}

const lookupVecText = (vec: any[], idx: number) => (Array.isArray(vec) && idx >= 0 && idx < vec.length ? vec[idx] : '');

const lookupVecTextByKey = (vec: any[], colIdxMap: ColIdxMap, key: string) => lookupVecText(vec, colIdxMap[key] ?? -1);

function renderIconWithTippy(cls: string, tip: string, icon: TemplateResult<1>) {
  return html`<span class=${'shrink-0 inline-flex tooltip tooltip-right ' + cls} data-tip=${tip}>${icon}</span>`;
}

const errorClass = (reqVec: any[], colIdxMap: ColIdxMap) => {
  const hasErrors = lookupVecTextByKey(reqVec, colIdxMap, 'errors');
  const status = lookupVecTextByKey(reqVec, colIdxMap, 'http_attributes')?.status_code || 0;
  const errStatus = lookupVecTextByKey(reqVec, colIdxMap, 'status');

  const errClass =
    hasErrors || errStatus === 'ERROR'
      ? 'w-1 bg-fillError-strong'
      : status >= 400
        ? 'w-1 bg-fillWarning-strong'
        : 'w-1 bg-fillBrand-weak status-indicator';

  return [status, hasErrors, errClass];
};

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
        class=${`h-full absolute top-0 rounded-sm transition-all duration-300 ${depth === 0 || children.length === 0 ? color : ''}`}
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
        <div class="absolute top-0 h-full border-l-2 border-strokeBrand-strong pointer-events-none" style="left:0; box-shadow: 0 0 4px rgba(26, 116, 168, 0.3)"></div>
        <div class="absolute top-0 h-full border-r-2 border-strokeBrand-strong pointer-events-none" style=${`left:${barWidth - 2}px; box-shadow: 0 0 4px rgba(26, 116, 168, 0.3)`}></div>

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

function loadingSkeleton(cols: number) {
  const actualCols = cols || 6;
  
  return html`
    <tbody class="min-w-0 text-sm">
      <tr class="w-full flex justify-center">
        <td colspan=${String(actualCols)} class="w-full">
          <div class="text-center py-6">
            <span class="loading loading-spinner loading-md text-fillBrand-strong"></span>
            <p class="text-sm text-textWeak mt-3">Loading events...</p>
          </div>
        </td>
      </tr>
      ${[...Array(10)].map((_, rowIdx) => html`
        <tr class="item-row relative p-0 flex items-center group whitespace-nowrap" style="--row-index: ${rowIdx}">
          ${[...Array(actualCols)].map((_, idx) => html`
            <td class="bg-bgBase relative pl-2 ${idx === 0 ? 'w-3' : idx === actualCols - 1 ? 'sticky right-0 z-10' : getSkeletonColumnWidth(idx)}">
              ${idx === 0 
                ? html`<div class="w-1 h-5 bg-fillBrand-strong opacity-20 rounded-full skeleton-glow"></div>`
                : html`
                    <div class="relative overflow-hidden">
                      <div class="h-4 rounded skeleton-shimmer skeleton-wave ${idx === actualCols - 1 ? 'w-24' : 'w-3/4'}"></div>
                      ${idx === actualCols - 1 
                        ? html`<div class="absolute right-0 top-0 h-full w-16 bg-gradient-to-r from-transparent to-bgBase"></div>`
                        : nothing
                      }
                    </div>
                  `
              }
            </td>
          `)}
        </tr>
      `)}
    </tbody>
  `;
}

function getSkeletonColumnWidth(idx: number) {
  const widths = ['w-[17ch]', 'w-[16ch]', 'w-[25ch]', 'w-[12ch]', 'w-[450px]'];
  return widths[idx % widths.length] + ' shrink-0';
}

function emptyState(cols: number) {
  let title = `No Events found`;
  let subText = `You're either not sending events to APItoolkit yet or no results matched your query/filter`;
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
            <h2 class="text-2xl text-textStrong font-bold bg-gradient-to-r from-textStrong to-textBrand bg-clip-text text-transparent">${title}</h2>
            <p class="text-sm max-w-md font-medium text-textWeak leading-relaxed">${subText}</p>
            <a 
              href="https://apitoolkit.io/docs/sdks/" 
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
  const rdId = lookupVecTextByKey(rd, colIdxMap, 'id');
  const rdCreatedAt = lookupVecTextByKey(rd, colIdxMap, 'created_at') || lookupVecTextByKey(rd, colIdxMap, 'timestamp');
  return [rdId, rdCreatedAt, source]; // Source parameter is preserved for future use
}

function groupSpans(data: any[][], colIdxMap: ColIdxMap, expandedTraces: Record<string, boolean>, flipDirection: boolean) {
  const traceMap: TraceDataMap = new Map();
  const TRACE_INDEX = colIdxMap['trace_id'];
  const SPAN_INDEX = colIdxMap['latency_breakdown'];
  const PARENT_SPAN_INDEX = colIdxMap['parent_span_id'];
  const TIMESTAMP_INDEX = colIdxMap['timestamp'];
  const SPAN_DURATION_INDEX = colIdxMap['duration'];
  const START_TIME_NS = colIdxMap['start_time_ns'];
  const ERROR_INDEX = colIdxMap['errors'];
  const BODY_INDEX = colIdxMap['body'];
  const KIND_INDEX = colIdxMap['kind'];

  data.forEach((span: any[]) => {
    let traceId = span[TRACE_INDEX];
    let spanId = span[SPAN_INDEX];
    const parentSpanId = span[PARENT_SPAN_INDEX];
    const body = span[BODY_INDEX];
    const id = span[colIdxMap['id']];
    if (traceId === '' || traceId === null) {
      // generate random id
      traceId = generateStrId();
      span[TRACE_INDEX] = traceId;
    }
    if (spanId === '' || spanId === null) {
      spanId = generateStrId();
      span[SPAN_INDEX] = spanId;
    }
    let traceData = traceMap.get(traceId);
    if (traceData === undefined) {
      traceData = {
        traceId,
        spans: new Map(),
        minStart: Infinity,
        duration: 0,
        startTime: 0,
        trace_start_time: null,
      };
      traceMap.set(traceId, traceData);
    }

    const timestamp = new Date(span[TIMESTAMP_INDEX]);
    const duration = span[SPAN_DURATION_INDEX];
    const startTime = span[START_TIME_NS];

    if (!traceData.trace_start_time || timestamp < traceData.trace_start_time) {
      traceData.trace_start_time = timestamp;
    }
    traceData.minStart = Math.min(traceData.minStart, startTime);
    traceData.duration = Math.max(traceData.duration, duration);
    const isLog = span[KIND_INDEX] === 'log';
    traceData.spans.set(isLog ? id : spanId, {
      id: spanId,
      startNs: startTime,
      hasErrors: isLog ? false : span[ERROR_INDEX],
      duration: isLog ? 0 : duration,
      children: [],
      parent: isLog ? null : parentSpanId,
      data: span,
      type: isLog ? 'log' : 'span',
    });
  });

  traceMap.forEach((traceData) => {
    const spanTree = new Map<string, APTEvent>();
    traceData.spans.forEach((span) => {
      const parentId = span.type === 'log' ? span.id : span.parent || '';
      const parentSpan = traceData.spans.get(parentId);

      if (parentSpan) {
        parentSpan.children.push(span);
      } else {
        spanTree.set(span.id, span);
      }
    });

    // Sort children after all have been added
    traceData.spans.forEach((span) => {
      if (span.children.length > 1) {
        span.children.sort((a, b) => a.startNs - b.startNs);
      }
    });
    // Convert to array after processing
    const sortedSpans = Array.from(spanTree.values()).sort((a, b) => a.startNs - b.startNs);
    traceData.spans = sortedSpans as any;
  });

  const result = Array.from(traceMap.values()).map((trace) => {
    const r: Trace = {
      traceId: trace.traceId,
      spans: Object.values(trace.spans),
      startTime: trace.minStart,
      duration: trace.duration,
    };
    return r;
  });

  if (flipDirection) {
    result.reverse();
  }

  return flattenSpanTree(result, expandedTraces);
}

function flattenSpanTree(traceArr: Trace[], expandedTraces: Record<string, boolean> = {}): EventLine[] {
  const result: EventLine[] = [];

  function traverse(
    span: APTEvent,
    traceId: string,
    parentIds: string[],
    traceStart: number,
    traceEnd: number,
    depth = 0,
    isLastChild = false,
    hasSiblingsArr: boolean[] = []
  ): [number, boolean] {
    let childrenCount = span.children.length;
    let childErrors = false;

    const spanInfo: EventLine = {
      depth,
      traceStart,
      traceEnd,
      traceId,
      childErrors,
      isNew: false,
      parentIds: parentIds,
      show: expandedTraces[traceId] || depth === 0,
      expanded: expandedTraces[traceId],
      isLastChild,
      siblingsArr: hasSiblingsArr,
      ...span,
      children: childrenCount,
      childrenTimeSpans: span.children.map((child) => ({
        startNs: child.startNs,
        duration: child.duration,
        data: child.data,
      })),
    };
    result.push(spanInfo);
    const hasSiling = span.children.length > 1;
    span.children.forEach((child, index) => {
      childErrors = child.hasErrors || childErrors;
      const lastChild = index === span.children.length - 1;
      const newSiblingsArr = hasSiling && !lastChild ? [...hasSiblingsArr, true] : [...hasSiblingsArr, false];
      const [count, errors] = traverse(child, traceId, [...parentIds, span.id], traceStart, traceEnd, depth + 1, lastChild, newSiblingsArr);
      childrenCount += count;
      childErrors = childErrors || errors;
    });
    spanInfo.children = childrenCount;
    spanInfo.childErrors = childErrors;
    return [childrenCount, childErrors];
  }

  traceArr.forEach((trace) => {
    trace.spans.forEach((span) => {
      traverse(span, trace.traceId, [], trace.startTime, trace.duration, 0);
    });
  });
  return result;
}

const getColumnWidth = (column: string) => {
  const widths = {
    status: 'w-[12ch] shrink-0',
    method: 'w-[12ch] shrink-0',
    status_code: 'w-[12ch] shrink-0',
    raw_url: 'w-[25ch] shrink-0 overflow-hidden',
    url_path: 'w-[25ch] shrink-0 overflow-hidden',
    summary: 'w-3/4 shrink-1',
  };
  return widths[column] || (column === 'id' || column === 'service' ? '' : 'w-[16ch] shrink-0');
};

function generateStrId() {
  return Math.random().toString(36).substring(2, 15);
}
