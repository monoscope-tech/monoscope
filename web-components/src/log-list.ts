'use strict';
import { virtualize } from '@lit-labs/virtualizer/virtualize.js';
import { LitElement, html, css, TemplateResult, nothing } from 'lit';
import { customElement, state, query } from 'lit/decorators.js';
import { APTEvent, ChildrenForLatency, ColIdxMap, EventLine, Trace, TraceDataMap } from './types/types';
import { RangeChangedEvent, VisibilityChangedEvent } from '@lit-labs/virtualizer';

// Ensure all badge classes are included in the final CSS build
// prettier-ignore
const _ensureBadgeClasses = html`
  <span class="badge-postgres badge-mysql badge-redis badge-mongo badge-mongodb badge-elastic badge-elasticsearch badge-cassandra badge-dynamodb badge-memcached badge-sqlite badge-clickhouse"></span>
  <span class="badge-2xx badge-3xx badge-4xx badge-5xx badge-error badge-success badge-warning badge-info badge-fatal badge-neutral"></span>
  <span class="badge-GET badge-POST badge-PUT badge-DELETE badge-PATCH"></span>
  <span class="cbadge cbadge-sm"></span>
  <span class="bg-blue-600 bg-orange-600 bg-red-600 bg-green-600 bg-yellow-500 bg-teal-600 bg-purple-600 bg-indigo-600 bg-gray-600 bg-amber-600"></span>
`;

@customElement('log-list')
export class LogList extends LitElement {
  @state() private expandedTraces: Record<string, boolean> = {};
  @state() private flipDirection: boolean = false;
  @state() private isLoadingRecent: boolean = false;
  @state() private spanListTree: EventLine[] = [];
  @state() private recentDataToBeAdded: any[] = [];
  @state() private isLoading: boolean = false;
  @state() private view: 'tree' | 'list' = 'tree';
  @state() private shouldScrollToBottom: boolean = false;
  @state() private logsColumns: string[] = [];
  @state() private wrapLines: boolean = false;
  @state() private hasMore: boolean = false;
  @state() private isLiveStreaming: boolean = false;
  @state() private isLoadingReplace: boolean = false;

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
  private lineChart: any = null;
  private _observer: IntersectionObserver | null = null;

  constructor() {
    super();

    // Bind all methods at once
    const methods = [
      'logItemRow',
      'fetchData',
      'expandTrace',
      'renderLoadMore',
      'updateTableData',
      'handleChartZoom',
      'updateColumnMaxWidthMap',
      'addWithFlipDirection',
      'toggleLogRow',
      'logItemCol',
    ];
    methods.forEach((m) => (this[m] = this[m].bind(this)));
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

    ['submit', 'add-query', 'update-query'].forEach((ev) => window.addEventListener(ev, () => this.refetchLogs()));

    window.addEventListener('pagehide', () => {
      if (this.liveStreamInterval) clearInterval(this.liveStreamInterval);
    });

    window.addEventListener('mouseup', () => {
      this.resizeTarget = null;
      document.body.style.userSelect = 'auto';
    });
    window.addEventListener('mousemove', (event) => {
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
    });

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

  async refetchLogs() {
    const p = new URLSearchParams(window.location.search);
    const pathName = window.location.pathname;
    const url = `${window.location.origin}${pathName}?json=true&${p.toString()}`;
    this.fetchData(url, false, true);
  }

  handleChartZoom(params: { batch?: { startValue: string; endValue: string }[] }) {
    console.log('handleChartZoom');
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

    // set from and to to the startValue and endValue in search params
  }

  updateTableData = (
    ves: any[][],
    cols: string[],
    colIdxMap: ColIdxMap,
    serviceColors: Record<string, string>,
    nextFetchUrl: string,
    recentFetchUrl: string
  ) => {
    Object.assign(this, {
      isLoadingReplace: false,
      logsColumns: [...cols],
      colIdxMap: { ...colIdxMap },
      hasMore: ves.length > 0,
      serviceColors: { ...serviceColors },
      nextFetchUrl,
      recentFetchUrl,
      spanListTree: this.buildSpanListTree(ves),
    });
    this.updateColumnMaxWidthMap(ves);
  };

  toggleWrapLines = () => {
    this.wrapLines = !this.wrapLines;
    this.requestUpdate();
  };

  connectedCallback() {
    super.connectedCallback();
    if (window.virtualListData) {
      const logs = window.virtualListData.requestVecs;
      this.logsColumns = window.virtualListData.cols;
      this.colIdxMap = window.virtualListData.colIdxMap;
      this.serviceColors = window.virtualListData.serviceColors;
      this.nextFetchUrl = window.virtualListData.nextFetchUrl;
      this.recentFetchUrl = window.virtualListData.recentFetchUrl;
      this.projectId = window.virtualListData.projectId;
      this.expandedTraces = {};
      this.spanListTree = this.buildSpanListTree(logs);
      this.updateColumnMaxWidthMap(logs);
      this.hasMore = logs.length > 0;
      this.requestUpdate();
      this.fetchedNew = false;

      window.virtualListData = null;
    }
  }

  firstUpdated() {
    this.scrollToBottom();
    this.setupIntersectionObserver();
    window.logListTable = document.querySelector('#resultTable');
  }

  updated(changedProperties: Map<string, any>) {
    if (this.shouldScrollToBottom && this.flipDirection) {
      this.scrollToBottom();
    }
    if (changedProperties.has('spanListTree') && this.fetchedNew) {
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

  scrollToBottom() {
    const container = document.getElementById('logs_list_container_inner');
    if (container) {
      container.scrollTop = container.scrollHeight;
    }
  }

  setupIntersectionObserver() {
    if (this._observer) {
      this._observer.disconnect();
    }

    const loader = document.querySelector('#loader');
    const container = document.querySelector('#logs_list_container_inner');
    if (!loader || !container) {
      setTimeout(() => {
        this.setupIntersectionObserver();
      }, 1000);
      return;
    }

    const observer = new IntersectionObserver(
      ([entry]) => {
        if (entry.isIntersecting) {
          this.fetchData(this.nextFetchUrl);
        }
      },
      {
        root: container,
        threshold: [0, 0.2, 0.4, 0.6, 0.8, 1],
      }
    );
    observer.observe(loader);
    this._observer = observer;
  }

  disconnectedCallback() {
    if (this._observer) {
      this._observer.disconnect();
    }
    super.disconnectedCallback();
  }

  buildSpanListTree(logs: any[][]) {
    return groupSpans(logs, this.colIdxMap, this.expandedTraces, this.flipDirection);
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

    this.requestUpdate();
  }

  fetchData(url: string, isNewData = false, isRefresh = false) {
    const loadingKey = isNewData ? 'isLoadingRecent' : isRefresh ? 'isLoadingReplace' : 'isLoading';
    if (this[loadingKey]) return;
    this[loadingKey] = true;
    fetch(url, {
      method: 'GET',
      headers: {
        Accept: 'application/json',
      },
    })
      .then((response) => response.json())
      .then((data) => {
        if (!data.error) {
          let { logsData, serviceColors, nextUrl, recentUrl, cols, colIdxMap } = data;

          // Validate required fields
          if (!logsData || !Array.isArray(logsData)) {
            console.error('Invalid response: missing or invalid logsData');
            return;
          }
          if (!isNewData) {
            this.hasMore = logsData.length > 0;
          }
          if (!isNewData) {
            this.nextFetchUrl = nextUrl;
          } else if (isNewData && logsData.length > 0) {
            this.recentFetchUrl = recentUrl;
          }

          this.serviceColors = { ...serviceColors, ...this.serviceColors };
          let tree = this.buildSpanListTree([...logsData]);

          if (isRefresh) {
            this.logsColumns = cols;
            this.colIdxMap = colIdxMap;
            this.nextFetchUrl = nextUrl;
            this.recentFetchUrl = recentUrl;
            this.spanListTree = tree;
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
                this.spanListTree = this.addWithFlipDirection(this.spanListTree, tree);
              }
            } else {
              this.spanListTree = this.addWithFlipDirection(tree, this.spanListTree);
            }
          }

          this.updateColumnMaxWidthMap(logsData);
        } else {
          console.error('Server returned error:', data.message || 'Unknown error');
          if (isRefresh && data.message) this.showErrorToast(data.message);
        }
      })
      .catch((error) => {
        console.error('Error fetching logs:', error);
        if (isRefresh) this.showErrorToast('Network error: Unable to fetch logs');
      })
      .finally(() => {
        this[loadingKey] = false;
        this.requestUpdate();
      });
  }

  showErrorToast(message: string) {
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
    const columnDefaults = { summary: 450 * 8.5, latency_breakdown: 100 };
    const charWidths = { timestamp: 6.5, default: 8.5 };

    recVecs.forEach((vec) => {
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
  toggleLogRow(event: any, targetInfo: [string, string, string], pid: string) {
    const sideView = document.querySelector('#log_details_container')! as HTMLElement;
    const resizer = document.querySelector('#resizer-details_width');
    const width = Number(getComputedStyle(sideView).width.replace('px', ''));
    this.shouldScrollToBottom = false;
    if (width < 50) {
      sideView.style.width = `550px`;
      if (resizer) {
        resizer.classList.remove('hidden');
      }
      updateUrlState('details_width', '550');
    }
    const rows = document.querySelectorAll('.item-row.bg-fillBrand-strong');
    rows.forEach((row) => row.classList.remove('bg-fillBrand-strong'));
    event.currentTarget.classList.add('bg-fillBrand-strong');
    const indicator = document.querySelector('#details_indicator');
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

  addWithFlipDirection(current: any, newData: any[]) {
    return this.flipDirection ? [...current, ...newData] : [...newData, ...current];
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
    this.spanListTree = this.addWithFlipDirection(this.spanListTree, this.recentDataToBeAdded);
    this.recentDataToBeAdded = [];
  }

  // Comment to allow classes be rendered.
  render() {
    const list: (EventLine | 'start' | 'end')[] = this.view === 'tree' ? this.spanListTree.filter((sp) => sp.show) : [...this.spanListTree];
    // end is used to render the load more button"
    list.unshift('start');
    list.push('end');

    return html`
      ${this.options()}
      <div
        @scroll=${(event: any) => {
          const container = event.target;
          if (this.flipDirection) {
            if (container.scrollTop + container.clientHeight >= container.scrollHeight - 1) {
              this.shouldScrollToBottom = true;
            } else {
              this.shouldScrollToBottom = false;
              this.handleRecentConcatenation();
            }
          } else {
            if (container.scrollTop === 0) {
              this.handleRecentConcatenation();
            }
          }
        }}
        class="relative h-full shrink-1 min-w-0 p-0 m-0 bg-bgBase w-full c-scroll pb-12 overflow-y-scroll"
        id="logs_list_container_inner"
      >
        ${this.recentDataToBeAdded.length > 0 && !this.flipDirection
          ? html` <div class="sticky left-1/2 -translate-y-1/2 top-[30px] z-50">
              <button
                class="cbadge-sm badge-neutral cursor-pointer bg-fillBrand-strong text-textInverse-strong shadow rounded-lg text-sm absolute"
                @pointerdown=${this.handleRecentClick}
              >
                ${this.recentDataToBeAdded.length} new
              </button>
            </div>`
          : nothing}
        ${this.isLoadingReplace
          ? html`<div class="absolute z-50 top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2">
              <span class="loading loading-dots"></span>
            </div>`
          : nothing}
        <table class="table-auto w-max relative ctable table-pin-rows table-pin-cols">
          <thead class="z-10 sticky top-0">
            <tr class="text-textStrong border-b flex min-w-0 relative font-medium ">
              ${this.logsColumns.filter((v) => v !== 'latency_breakdown').map((column) => this.logTableHeading(column))}
              ${this.logTableHeading('latency_breakdown')}
            </tr>
          </thead>
          ${list.length === 1 ? emptyState(this.logsColumns.length) : nothing}
          <tbody
            class="min-w-0 text-sm"
            id="log-item-table-body"
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
            })}
          </tbody>
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
                  this.recentDataToBeAdded.length > 0 ? 'bg-fillBrand-strong' : 'bg-bgInverse'
                } text-white flex justify-center items-center rounded-full shadow-lg h-10 w-10`}
              >
                ${faSprite('arrow-down', 'regular', 'h-6 w-6 fill-white stroke-white')}
              </button>
            </div>`
          : nothing}
      </div>
    `;
  }
  createRenderRoot() {
    return this;
  }

  renderSummaryElements(summaryArray: string[], wrapLines: boolean): any {
    if (!Array.isArray(summaryArray)) return nothing;

    const wrapClass = wrapLines ? 'whitespace-break-spaces' : 'whitespace-nowrap';

    return summaryArray
      .filter((el) => {
        if (!el.includes(';') || !el.includes('⇒')) return true;
        const [, style] = el.split('⇒')[0].split(';');
        return !style.startsWith('right-');
      })
      .map((element) => {
        if (!element.includes(';') || !element.includes('⇒')) {
          return html`<span class=${`fill-textStrong ${wrapClass}`}>${element}</span>`;
        }

        const [fieldAndStyle, value] = element.split('⇒');
        const [field, style] = fieldAndStyle.split(';');

        // Special icon handling
        const iconConfig = {
          request_type: () =>
            renderIconWithTippy(
              'w-4',
              `${value} Request`,
              faSprite(
                value === 'incoming' ? 'arrow-down-left' : 'arrow-up-right',
                'solid',
                value === 'incoming' ? 'h-3 fill-iconNeutral' : 'h-3 fill-blue-700'
              )
            ),
          kind: () =>
            value === 'internal' ? renderIconWithTippy('w-4 ml-2', 'Internal span', faSprite('function', 'regular', 'h-3 w-3')) : nothing,
          'db.system': () => renderIconWithTippy('w-4 ml-2', value, faSprite('database', 'regular', 'h-3 w-3 fill-slate-500')),
        };

        if (iconConfig[field]) return iconConfig[field]();

        // Text or badge rendering
        if (style === 'text-weak' || style === 'text-textWeak') {
          return html`<span class="text-textWeak">${value}</span>`;
        }

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
        const summaryArr = lookupVecTextByKey(dataArr, this.colIdxMap, 'summary') || [];
        const rightAlignedBadges: TemplateResult[] = [];

        summaryArr.forEach((element: string) => {
          if (element.includes(';') && element.includes('⇒')) {
            const [fieldAndStyle, value] = element.split('⇒');
            const [field, style] = fieldAndStyle.split(';');

            // Only process elements with styles starting with "right-"
            if (style.startsWith('right-')) {
              const badgeStyle = this.getStyleClass(style);
              rightAlignedBadges.push(renderBadge(`cbadge-sm ${badgeStyle}`, value));
            }
          }
        });

        return html`
          <div class="flex justify-end items-center gap-1 text-textWeak pl-1 rounded-lg bg-bgBase " style="min-width:${width}px">
            ${rightAlignedBadges}
            ${spanLatencyBreakdown({
              start: startNs - traceStart,
              depth,
              duration,
              traceEnd,
              color,
              children: chil,
              barWidth: width - 12,
            })}
            <span class="w-1"></span>
          </div>
        `;
      case 'summary':
        const summaryData = lookupVecTextByKey(dataArr, this.colIdxMap, key) || '';
        // Check if summary is already an array, otherwise parse it
        const summaryArray = Array.isArray(summaryData) ? summaryData : summaryData ? String(summaryData).split(',') : [];
        const errClas = hasErrors
          ? 'bg-fillError-strong text-white fill-white stroke-strokeError-strong'
          : childErrors
            ? 'border border-strokeError-strong bg-fillWeak text-textWeak fill-textWeak'
            : 'border border-strokeWeak bg-fillWeak text-textWeak fill-textWeak';
        return html`<div class="flex w-full ${this.wrapLines ? 'items-start' : 'items-center'} gap-1">
          ${this.view === 'tree'
            ? html`
                <div class="flex items-center gap-1">
                  ${depth > 1
                    ? new Array(depth - 1)
                        .fill(1)
                        .map((_, i) => html`<div class=${`ml-[15px] w-4 h-5 shrink-0 ${siblingsArr[i] ? 'border-l' : ''}`}></div>`)
                    : nothing}
                  ${depth > 0
                    ? html`<div class=${`border-l ml-[15px] w-4 ${isLastChild ? 'h-3' : 'h-5'} relative shrink-0`}>
                        <span class=${`border-b w-full absolute left-0 ${isLastChild ? 'bottom-0' : 'top-1/2 -translate-y-1/2'}`}></span>
                      </div>`
                    : nothing}
                  ${children > 0
                    ? html`<button
                        @pointerdown=${(e: any) => {
                          e.stopPropagation();
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
        return renderBadge('cbadge-sm badge-neutral bg-fillWeak ' + wrapClass, serviceData, key);
      default:
        let v = lookupVecTextByKey(dataArr, this.colIdxMap, key);
        return html`<span class=${wrapClass} title=${key}>${v}</span>`;
    }
  }

  renderLoadMore() {
    return this.hasMore
      ? html`<tr class="w-full flex relative">
          <td colspan=${String(this.logsColumns.length)} class="relative  pl-[calc(40vw-10ch)]">
            <div
              class="absolute -top-[500px] w-[1px] h-[500px] left-0 flex flex-col justify-end bg-transparent items-center"
              id="loader"
            ></div>
            ${this.isLoading
              ? html`<div class="loading loading-dots loading-md"></div>`
              : html`
                  <button
                    class="cursor-pointer text-textBrand underline font-semibold w-max mx-auto"
                    @pointerdown=${() => this.fetchData(this.nextFetchUrl)}
                  >
                    Load more
                  </button>
                `}
          </td>
        </tr>`
      : html`<tr></tr>`;
  }

  fetchRecent() {
    return html`<tr class="w-full flex relative" id="recent-logs">
      <td colspan=${String(this.logsColumns.length)} class="relative pl-[calc(40vw-10ch)]" id="recent-logs">
        ${this.isLiveStreaming
          ? html`<p>Live streaming latest data...</p>`
          : this.isLoadingRecent
            ? html`<div class="loading loading-dots loading-md"></div>`
            : html`
                <button
                  class="cursor-pointer text-textBrand underline font-semibold w-max mx-auto"
                  @pointerdown=${() => {
                    this.fetchData(this.recentFetchUrl, true);
                  }}
                >
                  Check for recent data
                </button>
              `}
      </td>
    </tr>`;
  }

  logTableHeading(column: string) {
    if (column === 'id') return html`<td class="p-0 m-0 whitespace-nowrap w-3"></td>`;

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
    const s = rowData.type === 'log' ? 'logs' : 'spans';
    const targetInfo = requestDumpLogItemUrlPath(rowData.data, this.colIdxMap, s);
    let isNew = rowData.isNew;
    return html`
      <tr
        class=${`item-row relative p-0 flex items-center cursor-pointer whitespace-nowrap ${isNew ? 'animate-fadeBg' : ''}`}
        @pointerdown=${(event: any) => this.toggleLogRow(event, targetInfo, this.projectId)}
      >
        ${this.logsColumns
          .filter((v) => v !== 'latency_breakdown')
          .map((column) => {
            const tableDataWidth = getColumnWidth(column);
            let width = this.columnMaxWidthMap[column];
            return html`<td
              class=${`${this.wrapLines ? 'break-all whitespace-wrap' : ''} bg-bgBase relative ${
                column === 'summary' ? '' : tableDataWidth
              }`}
              style=${width ? `width: ${width}px;` : ''}
            >
              ${this.logItemCol(rowData, column)}
            </td>`;
          })}
        ${this.logsColumns.includes('latency_breakdown')
          ? html`<td
              class="sticky right-0 "
              style=${this.columnMaxWidthMap['latency_breakdown'] ? "width: ${this.columnMaxWidthMap['latency_breakdown']}px;" : ''}
            >
              ${this.logItemCol(rowData, 'latency_breakdown')}
            </td>`
          : nothing}
      </tr>
    `;
  }

  tableHeadingWrapper(title: string, column: string, classes: string) {
    let width = this.columnMaxWidthMap[column];
    if (column === 'latency_breakdown' && !width) {
      width = 100;
    }

    return html`
      <td
        class=${`cursor-pointer p-0 m-0 whitespace-nowrap relative flex justify-between items-center pl-1 text-sm font-normal bg-bgBase ${
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
          class="w-3 text-gray-200 text-right select-none hover:text-textBrand overflow-hidden font-bold absolute right-0 top-1/2 -translate-y-1/2 h-4 cursor-ew-resize"
        >
          |
        </div>
      </td>
    `;
  }

  renderCheckbox(label: string, icon: string, checked: boolean, onChange: (checked: boolean) => void) {
    return html` <label class="flex items-center cursor-pointer w-full gap-1 px-2 py-1 text-sm rounded text-textWeak hover:bg-gray-100">
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
          this.view === view ? 'bg-gray-200 text-gray-800' : 'text-textWeak hover:bg-gray-100'
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
              this.spanListTree = this.buildSpanListTree(this.spanListTree.map((span) => span.data).reverse());
              this.recentDataToBeAdded = this.buildSpanListTree(this.recentDataToBeAdded.map((span) => span.data).reverse());
              this.spanListTree = [...this.spanListTree, ...this.recentDataToBeAdded];
              this.recentDataToBeAdded = [];
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
                    ? html`<li class="px-3 py-2 text-gray-400">No results</li>`
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
                    ${faSprite('trash-can', 'regular', 'h-3 w-3 text-iconNeutral fill-red-600')}
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
  return html`<span class=${`relative  ${classes} ${tippy ? 'tooltip tooltip-right' : ''}`} data-tip=${tippy}>${title}</span>`;
}

const lookupVecText = (vec: any[], idx: number) => (Array.isArray(vec) && idx >= 0 && idx < vec.length ? vec[idx] : '');

const lookupVecTextByKey = (vec: any[], colIdxMap: ColIdxMap, key: string) => lookupVecText(vec, colIdxMap[key] ?? -1);

function renderIconWithTippy(cls: string, tip: string, icon: TemplateResult<1>) {
  return html`<span class=${'shrink-0 inline-flex tooltip tooltip-right ' + cls} data-tip=${tip}>${icon}</span>`;
}

const getDurationNSMS = (durationS: string) => {
  const duration = parseInt(durationS) || 0;
  const units = [
    { threshold: 1e9, divisor: 1e9, suffix: ' s' },
    { threshold: 1e6, divisor: 1e6, suffix: ' ms' },
    { threshold: 1e3, divisor: 1e3, suffix: ' µs' },
  ];

  const unit = units.find((u) => duration >= u.threshold);
  return unit ? (duration / unit.divisor).toFixed(1) + unit.suffix : duration.toFixed(1) + ' ns';
};

const errorClass = (reqVec: any[], colIdxMap: ColIdxMap) => {
  const hasErrors = lookupVecTextByKey(reqVec, colIdxMap, 'errors');
  const status = lookupVecTextByKey(reqVec, colIdxMap, 'http_attributes')?.status_code || 0;
  const errStatus = lookupVecTextByKey(reqVec, colIdxMap, 'status');

  const errClass =
    hasErrors || errStatus === 'ERROR' ? 'w-1 bg-red-500' : status >= 400 ? 'w-1 bg-yellow-500' : 'w-1 bg-blue-200 status-indicator';

  return [status, hasErrors, errClass];
};

const getSeverityColor = (severity: string | undefined) =>
  ({
    debug: 'text-gray-500 bg-gray-100',
    info: 'text-brand bg-blue-100',
    warning: 'text-yellow-700 bg-yellow-100',
    error: 'text-red-500 bg-red-100',
    critical: 'text-red-700 bg-red-200 font-bold',
    notice: 'text-green-500 bg-green-100',
    alert: 'text-orange-600 bg-orange-100 font-bold',
  })[severity?.toLowerCase() || 'unset'] || 'text-black badge-neutral text-textWeak bg-fillWeak';

const getSpanStatusColor = (status: string) =>
  ({
    ERROR: 'cbadge-sm badge-error',
    OK: 'cbadge-sm badge-success',
  })[status] || 'cbadge-sm badge-neutral bg-fillWeak';
function spanLatencyBreakdown({
  start,
  duration,
  traceEnd,
  depth,
  color,
  children,
  barWidth,
}: {
  start: number;
  duration: number;
  traceEnd: number;
  depth: number;
  color: string;
  barWidth: number;
  children: (ChildrenForLatency & { color: string })[];
}) {
  const width = (duration / traceEnd) * barWidth;
  const left = (start / traceEnd) * barWidth;
  return html`<div class="-mt-1 shrink-0">
    <div class="flex h-5 relative bg-fillWeak overflow-x-hidden" style=${`width:${barWidth}px`}>
      <div
        class=${`h-full absolute top-0 ${depth === 0 || children.length === 0 ? color : ''}`}
        style=${`width:${width}px; left:${left}px`}
      ></div>
      ${children.map((child) => {
        const cWidth = (child.duration / traceEnd) * barWidth;
        const cLeft = (child.startNs / traceEnd) * barWidth;
        return html`<div class=${`h-full absolute top-0 ${child.color}`} style=${`width:${cWidth}px; left:${cLeft}px`}></div>`;
      })}
    </div>
  </div>`;
}

function emptyState(cols: number) {
  let title = `No Events found`;
  let subText = `You're either not sending events to APItoolkit yet or no results matched your query/filter`;
  return html`
    <tr class="w-full flex justify-center">
      <td colspan=${String(cols)} class="w-full mx-auto">
        <div class="w-max mx-auto my-8 text-center p-5 sm:py-14 sm:px-24 flex flex-col gap-4">
          <div>${faSprite('empty', 'regular', 'h-24 w-24 mx-auto stroke-blue-500 fill-blue-500')}</div>
          <div class="flex flex-col gap-2">
            <h2 class="text-xl text-textStrong font-bold">${title}</h2>
            <p class="text-sm max-w-4xl font-medium text-gray-500">${subText}</p>
            <a href="https://apitoolkit.io/docs/sdks/" target="_BLANK" class="btn text-sm w-max mx-auto btn-primary"
              >Read integration guides</a
            >
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
        // Insertion sort to maintain order
        let i = parentSpan.children.length - 1;
        while (i > 0 && parentSpan.children[i].startNs < parentSpan.children[i - 1].startNs) {
          [parentSpan.children[i], parentSpan.children[i - 1]] = [parentSpan.children[i - 1], parentSpan.children[i]];
          i--;
        }
      } else {
        spanTree.set(span.id, span);
      }
    });

    traceData.spans = Array.from(spanTree.values()).sort((a, b) => a.startNs - b.startNs) as any;
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
