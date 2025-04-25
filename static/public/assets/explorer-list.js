'use strict'
import { LitElement, html, nothing } from './js/thirdparty/lit.js'
import { virtualize, virtualizerRef } from '@lit-labs/virtualizer/virtualizer'

export class LogList extends LitElement {
  static properties = {
    logsData: [],
    logsColumns: [],
    colIdxMap: {},
    hasMore: { type: Boolean },
    isLoading: { type: Boolean },
    isLoadingRecent: { type: Boolean },
    nextFetchUrl: { type: String },
    isError: { type: Boolean },
    fetchError: { type: String },
    projectId: { type: String },
    expandedTraces: {},
    columnMaxWidthMap: {},
    spanListTree: [],
    flipDirection: { type: Boolean },
    view: { type: String },
    shouldScrollToBottom: { type: Boolean },
  }
  constructor() {
    super()

    this.resizeTarget = null
    this.mouseState = { x: 0 }

    this.logsColumns = []
    this.colIdxMap = {}
    this.serviceColors = {}
    this.hasMore = false
    this.expandedTraces = {}
    this.spanListTree = []
    this.isLoading = false
    this.columnMaxWidthMap = {}
    this.isLoadingRecent = false
    this.isError = false
    this.shouldScrollToBottom = true
    this.flipDirection = false
    this.logItemRow = this.logItemRow.bind(this)
    this.fetchData = this.fetchData.bind(this)
    this.expandTrace = this.expandTrace.bind(this)
    this.renderLoadMore = this.renderLoadMore.bind(this)
    this.updateTableData = this.updateTableData.bind(this)
    this.updateColumnMaxWidthMap = this.updateColumnMaxWidthMap.bind(this)
    this.latestLogsURLQueryValsFn = this.latestLogsURLQueryValsFn.bind(this)
    this.toggleLogRow = this.toggleLogRow.bind(this)
    this.logItemCol = this.logItemCol.bind(this)
    this.wrapLines = false
    this.view = 'tree'
    this.newDataCount = 0
    const liveBtn = document.querySelector('#streamLiveData')
    if (liveBtn) {
      liveBtn.addEventListener('change', () => {
        if (liveBtn.checked) {
          this.isLiveStreaming = true
          this.liveStreamInterval = setInterval(() => {
            const url = this.latestLogsURLQueryValsFn(this.nextFetchUrl)
            this.fetchData(url, true)
          }, 5000)
        } else {
          clearInterval(this.liveStreamInterval)
          this.isLiveStreaming = false
        }
        this.requestUpdate()
      })
    }

    window.addEventListener('mouseup', () => {
      this.resizeTarget = null
      document.body.style.userSelect = 'auto'
    })
    window.addEventListener('mousemove', event => {
      if (this.resizeTarget === null) return
      const diff = event.clientX - this.mouseState.x
      let width = this.columnMaxWidthMap[this.resizeTarget]
      if (!width) width = 16
      width += diff
      if (width > 100) {
        this.columnMaxWidthMap[this.resizeTarget] = width
        this.requestUpdate()
      }
      this.mouseState = { x: event.clientX }
    })
  }

  latestLogsURLQueryValsFn() {
    const latestIndex = this.flipDirection ? this.spanListTree.length - 1 : 0
    const latestData = this.spanListTree[latestIndex].data
    const timeIndex = this.colIdxMap['timestamp']
    const datetime = latestData[timeIndex]
    const to = datetime ? new Date(new Date(datetime).getTime() + 1).toISOString() : params().to
    const from = params().from
    const url = new URL(this.nextFetchUrl, window.location.origin)
    const p = url.searchParams
    p.set('from', from)
    p.delete('cursor')
    p.set('to', to)
    return url.toString()
  }

  updateTableData = (ves, cols, colIdxMap, serviceColors, nextFetchUrl) => {
    this.logsColumns = [...cols]
    this.colIdxMap = { ...colIdxMap }
    this.hasMore = ves.length >= 50
    this.serviceColors = { ...serviceColors }
    this.nextFetchUrl = nextFetchUrl
    this.spanListTree = this.buildSpanListTree(ves)
    this.updateColumnMaxWidthMap(ves)
    this.requestUpdate()
  }

  toggleWrapLines = () => {
    this.wrapLines = !this.wrapLines
    this.requestUpdate()
  }

  connectedCallback() {
    super.connectedCallback()
    if (window.virtualListData) {
      const logs = window.virtualListData.requestVecs
      this.logsColumns = window.virtualListData.cols
      this.colIdxMap = window.virtualListData.colIdxMap
      this.serviceColors = window.virtualListData.serviceColors
      this.nextFetchUrl = window.virtualListData.nextFetchUrl
      this.resetLogsUrl = window.virtualListData.resetLogsUrl
      this.projectId = window.virtualListData.projectId
      this.expandedTraces = {}
      this.spanListTree = this.buildSpanListTree(logs)
      this.updateColumnMaxWidthMap(logs)
      this.hasMore = logs.length >= 50
      this.requestUpdate()

      window.virtualListData = null
    }
  }

  firstUpdated() {
    this.scrollToBottom()
    this.setupIntersectionObserver()
    window.logListTable = document.querySelector('#resultTable')
  }

  updated(changedProperties) {
    if (this.shouldScrollToBottom && this.flipDirection) {
      this.scrollToBottom()
    }
    if (this.newDataFetched) {
      this.spanListTree.forEach(t => {
        t.isNew = false
      })
      this.newDataFetched = false
    }
  }

  scrollToBottom() {
    const container = document.getElementById('logs_list_container_inner')
    if (container) {
      container.scrollTop = container.scrollHeight
    }
  }

  setupIntersectionObserver() {
    if (this._observer) {
      this._observer.disconnect()
    }

    const loader = document.querySelector('#loader')
    const container = document.querySelector('#logs_list_container_inner')
    if (!loader || !container) {
      setTimeout(() => {
        this.setupIntersectionObserver()
      }, 1000)
      return
    }

    const observer = new IntersectionObserver(
      ([entry]) => {
        if (entry.isIntersecting) {
          this.fetchData(this.nextFetchUrl)
        }
      },
      {
        root: container,
        threshold: [0, 0.2, 0.4, 0.6, 0.8, 1],
      },
    )
    observer.observe(loader)
    this._observer = observer
  }

  disconnectedCallback() {
    if (this._observer) {
      this._observer.disconnect()
    }
    super.disconnectedCallback()
  }

  buildSpanListTree(logs) {
    return groupSpans(logs, this.colIdxMap, this.expandedTraces)
  }

  expandTrace(tracId, spanId) {
    this.shouldScrollToBottom = false
    if (!this.expandedTraces[spanId]) {
      this.expandedTraces[spanId] = false
    }
    this.expandedTraces[spanId] = !this.expandedTraces[spanId]
    const expanded = this.expandedTraces[spanId]
    const affectedSpans = this.spanListTree.filter(span => span.traceId === tracId)
    affectedSpans.forEach(span => {
      if (span.id === spanId) {
        span.expanded = expanded
        span.show = true
      }
      if (span.children > 0) {
        affectedSpans.forEach(span => {
          if (span.parentIds.includes(spanId)) {
            span.expanded = expanded
            span.show = expanded
            this.expandedTraces[span.id] = expanded
          }
        })
      }
    })
    this.requestUpdate()
  }

  fetchData(url, isNewData = false) {
    if ((this.isLoading && !isNewData) || (this.isLoadingRecent && isNewData)) return
    if (isNewData) {
      this.isLoadingRecent = true
    } else {
      this.isLoading = true
    }
    fetch(url)
      .then(response => response.json())
      .then(data => {
        if (!data.error) {
          let { logsData, serviceColors, nextUrl } = data
          if (!isNewData) {
            this.hasMore = logsData.length >= 50
            this.nextFetchUrl = nextUrl
          }
          if (logsData.length > 0) {
            this.serviceColors = { ...serviceColors, ...this.serviceColors }
            let tree = this.buildSpanListTree([...logsData])
            if (isNewData) {
              this.newDataFetched
              tree.forEach(tr => (tr.isNew = true))
              const container = document.querySelector('#logs_list_container_inner')
              if (container && container.scrollTop + container.clientHeight >= container.scrollHeight - 1) {
                this.shouldScrollToBottom = true
              }
              this.spanListTree = this.flipDirection ? [...this.spanListTree, ...tree] : [...tree, ...this.spanListTree]
            } else {
              this.spanListTree = this.flipDirection ? [...tree, ...this.spanListTree] : [...this.spanListTree, ...tree]
            }
            this.updateColumnMaxWidthMap(logsData)
          }
        } else {
          this.fetchError = data.error
        }
      })
      .catch(error => {
        this.isError = true
        console.error('Error fetching logs:', error)
      })
      .finally(() => {
        if (isNewData) {
          this.isLoadingRecent = false
        } else {
          this.isLoading = false
        }
      })
    this.requestUpdate()
  }

  hideColumn(column) {
    this.logsColumns = this.logsColumns.filter(col => col !== column)
  }

  updateColumnMaxWidthMap(recVecs) {
    recVecs.forEach(vec => {
      Object.entries(this.colIdxMap).forEach(([key, value]) => {
        let chPx = 8.5

        if (!['id'].includes(key)) {
          if (key === 'timestamp') {
            chPx = 6.5
          }
          let target = String(vec[value]).length * chPx

          if (key === 'rest' && !this.columnMaxWidthMap[key]) {
            target = 450 * chPx
            this.columnMaxWidthMap[key] = target
          }
          if (key === 'latency_breakdown' && !this.columnMaxWidthMap[key]) {
            target = 100
          }
          if ((key === 'latency_breakdown' || key === 'rest') && this.columnMaxWidthMap[key]) {
            return
          } else {
            if (this.columnMaxWidthMap[key] === undefined) {
              this.columnMaxWidthMap[key] = 12 * chPx
            }
            if (this.columnMaxWidthMap[key] < target) {
              this.columnMaxWidthMap[key] = target
            }
          }
        }
      })
    })
  }
  toggleLogRow(event, targetInfo, pid) {
    const sideView = document.querySelector('#log_details_container')
    const logsView = document.querySelector('#logs_list_container')
    const resizer = document.querySelector('#resizer')
    const width = Number(getComputedStyle(sideView).width.replace('px', ''))
    this.shouldScrollToBottom = false
    if (width < 50) {
      const lW = getComputedStyle(logsView).width.replace('px', '')
      logsView.style.width = `${lW - 550}px`
      resizer.classList.remove('hidden')
      updateUrlState('details_width', logsView.style.width)
    }
    const rows = document.querySelectorAll('.item-row.bg-fillBrand-weak')
    rows.forEach(row => row.classList.remove('bg-fillBrand-weak'))
    event.currentTarget.classList.add('bg-fillBrand-strong')
    const indicator = document.querySelector('#details_indicator')
    indicator.classList.add('htmx-request')
    const [rdId, rdCreatedAt, source] = targetInfo
    const url = `/p/${pid}/log_explorer/${rdId}/${rdCreatedAt}/detailed?source=spans`
    updateUrlState('target_event', `${rdId}/${rdCreatedAt}/detailed?source=spans`)
    htmx.ajax('GET', url, { target: '#log_details_container', swap: 'innerHTML', indicator: '#details_indicator' })
  }

  render() {
    const list = this.view === 'tree' ? this.spanListTree.filter(sp => sp.show) : [...this.spanListTree]
    // end is used to render the load more button"
    list.unshift('start')
    list.push('end')

    return html`
      ${this.options()}
      <div
        @scroll=${event => {
          const container = event.target
          if (container.scrollTop + container.clientHeight >= container.scrollHeight - 1) {
            this.shouldScrollToBottom = true
          } else {
            this.shouldScrollToBottom = false
          }
        }}
        class="relative h-full shrink-1 min-w-0 p-0 m-0 bg-white w-full c-scroll pb-12 overflow-y-scroll scroll-smooth"
        id="logs_list_container_inner"
      >
        <table class="table-auto w-max relative ctable table-pin-rows table-pin-cols">
          <thead class="z-10 sticky top-0">
            <tr class="text-textStrong border-b flex min-w-0 relative font-medium ">
              ${this.logsColumns.filter(v => v !== 'latency_breakdown').map(column => this.logTableHeading(column))}
              ${this.logTableHeading('latency_breakdown')}
            </tr>
          </thead>
          ${list.length === 1 ? emptyState(this.source, this.logsColumns.length) : nothing}
          <tbody
            class="min-w-0 text-sm"
            id="log-item-table-body"
            @rangeChanged=${event => {
              this.setupIntersectionObserver()
            }}
          >
            ${virtualize({
              items: list,
              renderItem: this.logItemRow,
            })}
          </tbody>
        </table>
        ${!this.shouldScrollToBottom && this.flipDirection
          ? html`<div style="position: sticky;bottom: 0px;overflow-anchor: none;">
              <button
                @click=${() => {
                  this.shouldScrollToBottom = true
                  this.scrollToBottom()
                }}
                data-tip="Scroll to bottom"
                class="absolute tooltip tooltip-left right-8 bottom-2 group z-50 bg-bgInverse text-white flex justify-center items-center rounded-full shadow-lg h-10 w-10"
              >
                ${faSprite('arrow-down', 'regular', 'h-6 w-6 fill-white stroke-white')}
              </button>
            </div> `
          : nothing}
      </div>
    `
  }
  createRenderRoot() {
    return this
  }

  logItemCol(rowData, source, colIdxMap, key, serviceColors, toggleTrace, columnMaxWidthMap, wrapLines) {
    const dataArr = rowData.data
    const wrapClass = wrapLines ? 'whitespace-break-spaces' : 'whitespace-nowrap'
    switch (key) {
      case 'id':
        let [status, errCount, errClass] = errorClass(false, dataArr, colIdxMap)
        return html`
          <div class="flex items-center justify-between w-3">
            <span class="col-span-1 h-5 rounded-sm flex"> ${renderIconWithTippy(errClass, `${errCount} errors attached; status ${status}`)} </span>
          </div>
        `
      case 'created_at':
      case 'timestamp':
        let timestamp = lookupVecTextByKey(dataArr, colIdxMap, key)
        return html`<div>
          <time class="monospace text-textStrong ${wrapClass}" data-tippy-content="timestamp" datetime=${timestamp}>${displayTimestamp(timestamp)}</time>
        </div>`
      case 'status_code':
        let statusCode = lookupVecTextByKey(dataArr, colIdxMap, 'status_code')
        let statusCls = getStatusColor(statusCode)
        return statusCode == 'UNSET' ? nothing : renderBadge(statusCls, statusCode, 'status code')
      case 'method':
        let method = lookupVecTextByKey(dataArr, colIdxMap, key)
        let methodCls = getMethodColor(method)
        return renderBadge('min-w-[4rem] cbadge ' + methodCls, method, 'method')
      case 'request_type':
        let requestType = lookupVecTextByKey(dataArr, colIdxMap, key)
        if (requestType === 'Incoming')
          return renderIconWithTippy('w-4', 'Incoming Request => Server', faSprite('arrow-down-left', 'solid', ' h-3 fill-slate-500'))
        return renderIconWithTippy('w-4', 'Outgoing Request => Client', faSprite('arrow-up-right', 'solid', ' h-3 fill-blue-700'))
      case 'duration':
        let dur = rowData.type === 'log' ? 'log' : getDurationNSMS(lookupVecTextByKey(dataArr, colIdxMap, key))
        return renderBadge('cbadge-sm badge-neutral font-normal bg-fillWeak', dur, 'latency')
      case 'severity_text':
        let severity = lookupVecTextByKey(dataArr, colIdxMap, key) || 'UNSET'
        let severityClass = getSeverityColor(severity)
        return severity === 'UNSET' ? nothing : renderBadge('cbadge-sm cbadge ' + severityClass, severity)
      case 'body':
        let body = lookupVecTextByKey(dataArr, colIdxMap, key)
        return renderBadge('space-x-2 ' + wrapClass, body)
      case 'status':
        let st = lookupVecTextByKey(dataArr, colIdxMap, key)
        let statsCls = getSpanStatusColor(st)
        return !st || st.toLowerCase() === 'unset' || st === 'ERROR' ? nothing : renderBadge(statsCls, st)
      case 'span_name':
        let spanName = lookupVecTextByKey(dataArr, colIdxMap, key)
        return renderBadge('cbadge-sm badge-neutral bg-fillWeak ' + wrapClass, spanName, 'span name')
      case 'service':
        let service = lookupVecTextByKey(dataArr, colIdxMap, key)
        return html` <div class="w-[16ch]">${renderBadge('cbadge-sm badge-neutral bg-fillWeak ' + wrapClass, service, 'service name')}</div>`
      case 'kind':
        let kind = lookupVecTextByKey(dataArr, colIdxMap, key)
        return kind.toLowerCase() === 'internal' ? renderIconWithTippy('w-4 ml-2', 'Internal span', faSprite('function', 'regular', 'h-3 w-3 ')) : nothing
      case 'latency_breakdown':
        const { traceStart, traceEnd, startNs, duration, childrenTimeSpans, depth: d } = rowData
        const color = serviceColors[lookupVecTextByKey(dataArr, colIdxMap, 'span_name')] || 'bg-black'
        const chil = childrenTimeSpans.map(({ startNs, duration, data }) => ({
          start: startNs - traceStart,
          duration,
          color: serviceColors[lookupVecTextByKey(data, colIdxMap, 'span_name')] || 'bg-black',
        }))
        const width = columnMaxWidthMap['latency_breakdown'] || 200
        const db = lookupVecObjectByKey(dataArr, colIdxMap, 'db_attributes')
        const http = lookupVecObjectByKey(dataArr, colIdxMap, 'http_attributes')
        const rpc = lookupVecObjectByKey(dataArr, colIdxMap, 'rpc_attributes')
        const errStatus = lookupVecTextByKey(dataArr, colIdxMap, 'status')
        return html`
          <div class="flex justify-end items-center gap-1 text-textWeak" style="min-width:${width}px">
            ${errStatus === 'ERROR' ? renderBadge(getSpanStatusColor('ERROR'), 'ERROR') : nothing}
            ${db.system ? renderBadge('cbadge-sm badge-neutral bg-fillWeak border border-strokeWeak', db.system) : nothing}
            ${http.method && http.url ? renderBadge('cbadge-sm badge-neutral bg-fillWeak border border-strokeWeak', 'http') : nothing}
            ${rpc.system ? renderBadge('cbadge-sm badge-neutral bg-fillWeak border border-strokeWeak', rpc.system) : nothing}
            <div class="overflow-visible shrink-0 font-normal">${this.logItemCol(rowData, source, colIdxMap, 'duration')}</div>
            ${spanLatencyBreakdown({ start: startNs - traceStart, depth: d, duration, traceEnd, color, children: chil, barWidth: width - 12 })}
            <span class="w-1"></span>
          </div>
        `
      case 'http_attributes':
        const attributes = lookupVecObjectByKey(dataArr, colIdxMap, key)
        const { method: m, url, status_code: statusCode_ } = attributes
        if (m || url || statusCode_) {
          let k = lookupVecTextByKey(dataArr, colIdxMap, 'kind')
          let methodCls_ = getMethodColor(m)
          let statusCls_ = getStatusColor(statusCode_)
          let wrapCls = wrapLines ? 'whitespace-break-spaces' : 'whitespace-nowrap'
          return html`
            ${k.toLowerCase() === 'server'
              ? renderIconWithTippy('w-4 ml-2', 'Incoming Request => Server', faSprite('arrow-down-left', 'solid', ' h-3 fill-slate-500'))
              : k.toLowerCase() === 'client'
              ? renderIconWithTippy('w-4 ml-2', 'Outgoing Request  => Client', faSprite('arrow-up-right', 'solid', ' h-3 fill-blue-700'))
              : nothing}
            ${statusCode_ && statusCode_ !== 'UNSET' ? renderBadge(statusCls_, statusCode_, 'status code') : nothing}
            ${m ? renderBadge('min-w-[4rem] text-center cbadge cbadge-sm ' + methodCls_, m, 'method') : nothing}
            ${url ? renderBadge('cbadge-sm badge-neutral bg-fillWeak ' + wrapCls, url, 'url') : nothing}
          `
        }
        break
      case 'db_attributes':
        const dbAttributes = lookupVecObjectByKey(dataArr, colIdxMap, key)
        const { system, statement } = dbAttributes
        if (system || statement) {
          return html`
            ${renderIconWithTippy('w-4 ml-2', system, faSprite('database', 'regular', 'h-3 w-3 fill-slate-500'))}
            ${statement ? renderBadge('cbadge-sm badge-neutral bg-fillWeak', statement) : nothing}
          `
        }
        break
      case 'rpc_attributes':
        const rpcAttributes = lookupVecObjectByKey(dataArr, colIdxMap, key)
        const { system: rpcSystem, method: rpcMethod } = rpcAttributes
        let k = lookupVecTextByKey(dataArr, colIdxMap, 'kind')
        if (rpcSystem || rpcMethod) {
          return html`
            ${k.toLowerCase() === 'server'
              ? renderIconWithTippy('w-4 ml-2', 'Incoming Request => Server', faSprite('arrow-down-left', 'solid', ' h-3 fill-slate-500'))
              : k.toLowerCase() === 'client'
              ? renderIconWithTippy('w-4 ml-2', 'Outgoing Request  => Client', faSprite('arrow-up-right', 'solid', ' h-3 fill-blue-700'))
              : nothing}
            ${rpcMethod ? renderBadge('cbadge-sm badge-neutral bg-fillWeak', rpcMethod) : nothing}
          `
        }
        break
      case 'rest':
        let val = lookupVecTextByKey(dataArr, colIdxMap, key)
        const { depth, children, traceId, childErrors, hasErrors, expanded, type, id, isLastChild, siblingsArr } = rowData
        const errClas = hasErrors
          ? 'bg-fillError-strong text-white fill-white stroke-strokeError-strong'
          : childErrors
          ? 'border border-strokeError-strong bg-fillWeak text-textWeak fill-textWeak'
          : 'border border-strokeWeak bg-fillWeak text-textWeak fill-textWeak'
        return html`<div class="flex w-full ${wrapLines ? 'items-start' : 'items-center'} gap-1">
          ${this.view === 'tree'
            ? html`
                <div class="flex items-center gap-1">
                  ${depth > 1
                    ? new Array(depth - 1).fill(1).map((_, i) => html`<div class=${`ml-[15px] w-4 h-5 shrink-0 ${siblingsArr[i] ? 'border-l' : ''}`}></div>`)
                    : nothing}
                  ${depth > 0
                    ? html`<div class=${`border-l ml-[15px] w-4 ${isLastChild ? 'h-3' : 'h-5'} relative shrink-0`}>
                        <span class=${`border-b w-full absolute left-0 ${isLastChild ? 'bottom-0' : 'top-1/2 -translate-y-1/2'}`}></span>
                      </div>`
                    : nothing}
                  ${children > 0
                    ? html`<button
                        @click=${e => {
                          e.stopPropagation()
                          toggleTrace(traceId, id)
                        }}
                        class=${`rounded-sm ml-1 cursor-pointer shrink-0 w-8 px-1 flex justify-center gap-[2px] text-xs items-center h-5 ${errClas}`}
                      >
                        ${expanded ? faSprite('minus', 'regular', 'w-3 h-1 shrink-0') : faSprite('plus', 'regular', 'w-3 h-3 shrink-0')} ${children}
                      </button>`
                    : depth === 0
                    ? nothing
                    : html`<div class=${`rounded-sm ml-1 shrink-0 w-3 h-5 ${errClas}`}></div>`}
                </div>
              `
            : nothing}
          <div class=${`flex items-center gap-1 ${wrapLines ? 'break-all flex-wrap' : 'overflow-hidden'}`}>
            ${type === 'log'
              ? ['severity_text', 'body'].map(k => this.logItemCol(rowData, source, colIdxMap, k, undefined, undefined, undefined, wrapLines))
              : ['kind', 'http_attributes', 'db_attributes', 'rpc_attributes', 'status', 'span_name'].map(k =>
                  this.logItemCol(rowData, source, colIdxMap, k, undefined, undefined, undefined, wrapLines),
                )}
            <span class=${'fill-slate-700 ' + wrapClass}>${val}</span>
          </div>
        </div>`
      default:
        let v = lookupVecTextByKey(dataArr, colIdxMap, key)
        return renderBadge('cbadge-sm badge-neutral bg-fillWeak ' + wrapClass, v, key)
    }
  }

  renderLoadMore() {
    return this.hasMore
      ? html`<tr class="w-full flex relative">
          <td colspan=${String(this.logsColumns.length)} class="relative  pl-[calc(40vw-10ch)]">
            <div class="absolute -top-[500px] w-[1px] h-[500px] left-0 flex flex-col justify-end bg-transparent items-center" id="loader"></div>
            ${this.isLoading
              ? html`<div class="loading loading-dots loading-md"></div>`
              : html`
                  <button class="cursor-pointer text-textBrand underline font-semibold w-max mx-auto" @click=${() => this.fetchData(this.nextFetchUrl)}>
                    Load more
                  </button>
                `}
          </td>
        </tr>`
      : html`<tr></tr>`
  }

  fetchRecent() {
    return html`<tr class="w-full flex relative">
      <td colspan=${String(this.logsColumns.length)} class="relative pl-[calc(40vw-10ch)]">
        ${this.isLiveStreaming
          ? html`<p>Live streaming latest data...</p>`
          : this.isLoadingRecent
          ? html`<div class="loading loading-dots loading-md"></div>`
          : html`
              <button
                class="cursor-pointer text-textBrand underline font-semibold w-max mx-auto"
                @click=${() => {
                  const updatedUrl = this.latestLogsURLQueryValsFn()
                  this.fetchData(updatedUrl, true)
                }}
              >
                Check for recent data
              </button>
            `}
      </td>
    </tr>`
  }

  logTableHeading(column) {
    switch (column) {
      case 'id':
        return html`<td class="p-0 m-0 whitespace-nowrap w-3"></td>`
      case 'timestamp':
      case 'created_at':
        return this.tableHeadingWrapper('timestamp', column, 'w-[17ch] shrink-0')
      case 'latency_breakdown':
        return this.tableHeadingWrapper('latency', column, 'sticky right-0 shrink-0 bg-fillError-strong')
      case 'status_code':
        return this.tableHeadingWrapper('status', column, 'shrink-0 w-[12ch]')
      case 'method':
        return this.tableHeadingWrapper('method', column, 'shrink-0 w-[12ch]')
      case 'raw_url':
      case 'url_path':
        return this.tableHeadingWrapper(column, column, 'w-[25ch] shrink-0')
      case 'service':
        return this.tableHeadingWrapper('service', column, 'w-[16ch] shrink-0')
      case 'rest':
        return this.tableHeadingWrapper('summary', column, 'w-[1400px] shrink-1')
      default:
        return this.tableHeadingWrapper(column, column, 'w-[16ch] shrink-0')
    }
  }

  logItemRow(rowData) {
    if (rowData === 'end') {
      if (this.flipDirection) {
        return this.fetchRecent()
      }
      return this.renderLoadMore()
    }
    if (rowData === 'start') {
      if (this.flipDirection) {
        return this.renderLoadMore()
      }
      return this.fetchRecent()
    }
    const s = rowData.type === 'log' ? 'logs' : this.source
    const targetInfo = requestDumpLogItemUrlPath(rowData.data, this.colIdxMap, s)
    const isNew = rowData.isNew
    return html`
      <tr
        class=${`item-row relative p-0 flex items-center cursor-pointer whitespace-nowrap ${isNew ? 'animate-fadeBg' : ''}`}
        @click=${event => this.toggleLogRow(event, targetInfo, this.projectId)}
      >
        ${this.logsColumns
          .filter(v => v !== 'latency_breakdown')
          .map(column => {
            const tableDataWidth = getColumnWidth(column)
            let width = this.columnMaxWidthMap[column]
            return html`<td
              class=${`${this.wrapLines ? 'break-all whitespace-wrap' : ''} bg-white relative ${column === 'rest' ? '' : tableDataWidth}`}
              style=${width ? `width: ${width}px;` : ''}
            >
              ${this.logItemCol(rowData, this.source, this.colIdxMap, column, this.serviceColors, this.expandTrace, this.columnMaxWidthMap, this.wrapLines)}
            </td>`
          })}
        ${this.logsColumns.includes('latency_breakdown')
          ? html`<td
              class="bg-white sticky right-0 overflow-x-hidden"
              style=${this.columnMaxWidthMap['latency_breakdown'] ? "width: ${this.columnMaxWidthMap['latency_breakdown']}px;" : ''}
            >
              ${this.logItemCol(
                rowData,
                this.source,
                this.colIdxMap,
                'latency_breakdown',
                this.serviceColors,
                this.expandTrace,
                this.columnMaxWidthMap,
                this.wrapLines,
                this.view,
              )}
            </td>`
          : nothing}
      </tr>
    `
  }

  tableHeadingWrapper(title, column, classes) {
    let width = this.columnMaxWidthMap[column]
    if (column === 'latency_breakdown' && !width) {
      width = 100
    }
    return html`
      <td
        class=${`cursor-pointer p-0 m-0 whitespace-nowrap relative flex justify-between items-center pl-1 text-sm font-normal bg-white ${
          classes ? classes : ''
        }`}
        style=${width ? `width: ${width}px` : ''}
      >
        <div class="dropdown font-medium text-base" data-tippy-content=${title}>
          <div tabindex="0" role="button" class="py-1">
            ${title}
            <span class="ml-1 p-0.5 border border-slate-200 rounded-sm inline-flex"> ${faSprite('chevron-down', 'regular', 'w-3 h-3')} </span>
          </div>
          <ul tabindex="0" class="dropdown-content z-1 menu p-2 shadow-sm bg-bgBase rounded-box min-w-[15rem]">
            <li>
              <button @click=${() => this.hideColumn(column)}>Hide column</button>
            </li>
          </ul>
        </div>
        <div
          @mousedown=${event => {
            this.resizeTarget = column
            this.mouseState = { x: event.clientX }
            document.body.style.userSelect = 'none'
          }}
          class="w-3 text-gray-200 text-right select-none hover:text-textBrand overflow-hidden font-bold absolute right-0 top-1/2 -translate-y-1/2 h-4 cursor-ew-resize"
        >
          |
        </div>
      </td>
    `
  }

  options() {
    return html`
      <div class="w-full flex justify-end px-2 pb-1 gap-3 ">
        ${html`
          <div class="tabs tabs-box tabs-md p-0 tabs-outline items-center border">
            <button
              @click=${() => (this.view = 'tree')}
              class=${`flex items-center justify-center gap-1 px-2 py-1 text-xs rounded ${
                this.view === 'tree' ? 'bg-gray-200 text-gray-800' : 'text-textWeak  hover:bg-gray-100'
              }`}
            >
              ${faSprite('tree', 'regular', 'h-4 w-4')}
              <span class="sm:inline hidden">Tree</span>
            </button>

            <button
              @click=${() => (this.view = 'list')}
              class=${`flex items-center justify-center gap-1 px-2 py-1 text-xs rounded ${
                this.view === 'list' ? 'bg-gray-200 text-gray-800' : 'text-textWeak  hover:bg-gray-100'
              }`}
            >
              ${faSprite('list-view', 'regular', 'h-4 w-4')}
              <span class="sm:inline hidden">List</span>
            </button>
          </div>
        `}
        <button
          class=${`flex items-center justify-center gap-1 px-2 py-1 text-xs rounded ${
            this.flipDirection ? 'bg-gray-200 text-gray-800' : 'text-textWeak  hover:bg-gray-100'
          }`}
          @click=${() => {
            this.flipDirection = !this.flipDirection
            this.spanListTree = this.buildSpanListTree(this.spanListTree.map(span => span.data).reverse())
            this.requestUpdate()
          }}
        >
          ${faSprite('flip-vertical', 'regular', 'h-4 w-4')}
          <span class="sm:inline hidden">Flip direction</span>
        </button>

        <button
          class=${`flex items-center justify-center gap-1 px-2 py-1 text-xs rounded ${
            this.wrapLines ? 'bg-gray-200 text-gray-800' : 'text-textWeak  hover:bg-gray-100'
          }`}
          @click=${() => {
            this.wrapLines = !this.wrapLines
            if (this.wrapLines) {
              let width = Number(window.getComputedStyle(document.getElementById('logs_list_container_inner')).width.replace('px', ''))
              this.logsColumns.forEach(col => {
                if (col !== 'rest' && this.columnMaxWidthMap[col]) {
                  width -= this.columnMaxWidthMap[col] + 8
                }
              })
              this.columnMaxWidthMap['rest'] = width - 20 // margin left and right and id width
            } else {
              this.columnMaxWidthMap['rest'] = 450 * 8
            }
            this.requestUpdate()
          }}
        >
          ${faSprite('wrap-text', 'regular', 'h-4 w-4')}
          <span class="sm:inline hidden">Wrap lines</span>
        </button>
      </div>
    `
  }
}

customElements.define('log-list', LogList)

const faSprite = (iconName, kind, classes) => html`<svg class="${classes}"><use href="/public/assets/svgs/fa-sprites/${kind}.svg#${iconName}"></use></svg>`

function displayTimestamp(inputDateString) {
  const date = new Date(inputDateString)
  if (isNaN(date)) return ''

  const options = {
    month: 'short',
    day: '2-digit',
    hour: '2-digit',
    minute: '2-digit',
    second: '2-digit',
    hour12: false,
    timeZone: 'UTC',
  }

  const formatted = date.toLocaleString('en-US', options)
  return formatted.replace(',', '')
}

function renderBadge(classes, title, tippy) {
  return html`<span class=${`relative  ${classes} ${tippy ? 'tooltip tooltip-right' : ''}`} data-tip=${tippy}>${title}</span>`
}

const lookupVecText = (vec, idx) => {
  if (!Array.isArray(vec) || idx < 0 || idx >= vec.length) {
    return ''
  }
  return vec[idx]
}

const lookupVecTextByKey = (vec, colIdxMap, key) => {
  if (!Object.prototype.hasOwnProperty.call(colIdxMap, key)) {
    return ''
  }
  const idx = colIdxMap[key]
  return lookupVecText(vec, idx)
}

const lookupVecObjectByKey = (vec, colIdxMap, key) => {
  if (!Object.prototype.hasOwnProperty.call(colIdxMap, key)) {
    return {}
  }
  const idx = colIdxMap[key]
  return lookupVecText(vec, idx) || {}
}

function getStatusColor(status) {
  const st = Number(status)
  if (!Number.isNaN(st)) {
    if (st < 200) return 'cbadge-sm badge-neutral'
    if (st < 300) return 'cbadge-sm badge-2xx'
    if (st < 400) return 'cbadge-sm badge-3xx'
    if (st >= 400) return 'cbadge-sm badge-4xx'
  }
  return getSpanStatusColor(status)
}

function getMethodColor(method) {
  const methodColors = {
    POST: 'cbadge-sm badge-pink',
    PUT: 'cbadge-sm badge-lime',
    DELETE: 'cbadge-sm badge-error',
    PATCH: 'cbadge-sm badge-cyan',
    GET: 'cbadge-sm badge-blue',
  }
  return methodColors[method] || 'cbadge-sm badge-neutral bg-fillWeak '
}

function renderIconWithTippy(cls, tip, icon) {
  return html`<span class=${'shrink-0 inline-flex tooltip tooltip-right ' + cls} data-tip=${tip}>${icon}</span>`
}

function getDurationNSMS(duration) {
  try {
    duration = parseInt(duration)
  } catch (e) {
    return '0 ns'
  }
  if (duration >= 1e9) {
    return (duration / 1e9).toFixed(1) + ' s'
  } else if (duration >= 1e6) {
    return (duration / 1e6).toFixed(1) + ' ms'
  } else if (duration >= 1e3) {
    return (duration / 1e3).toFixed(1) + ' µs'
  } else {
    return duration.toFixed(1) + ' ns'
  }
}

function errorClass(expandedSection, reqVec, colIdxMap) {
  const hasErrors = lookupVecTextByKey(reqVec, colIdxMap, 'errors')
  const status = lookupVecTextByKey(reqVec, colIdxMap, 'http_attributes').status_code || 0
  const errStatus = lookupVecTextByKey(reqVec, colIdxMap, 'status') || 0

  let errClass = ' w-1 bg-blue-200 status-indicator '
  if (hasErrors || errStatus === 'ERROR') {
    errClass = ' w-1 bg-red-500 '
  } else if (status >= 400) {
    errClass = ' w-1 bg-yellow-500 '
  } else if (expandedSection) {
    errClass = ' w-1 bg-blue-200 '
  }

  return [status, hasErrors, errClass]
}

function getSeverityColor(severity) {
  severity = severity ? severity.toLowerCase() : 'unset'
  return (
    {
      debug: 'text-gray-500 bg-gray-100',
      info: 'text-brand bg-blue-100',
      warning: 'text-yellow-700 bg-yellow-100',
      error: 'text-red-500 bg-red-100',
      critical: 'text-red-700 bg-red-200 font-bold',
      notice: 'text-green-500 bg-green-100',
      alert: 'text-orange-600 bg-orange-100 font-bold',
    }[severity] || 'text-black badge-neutral text-textWeak bg-fillWeak'
  )
}

function getSpanStatusColor(status) {
  return (
    {
      ERROR: 'cbadge-sm badge-error',
      OK: 'cbadge-sm badge-success',
    }[status] || 'cbadge-sm badge-neutral bg-fillWeak'
  )
}
function spanLatencyBreakdown({ start, duration, traceEnd, depth, color, children, barWidth }) {
  const width = (duration / traceEnd) * barWidth
  const left = (start / traceEnd) * barWidth
  return html`<div class="-mt-1 shrink-0">
    <div class="flex h-5 relative bg-fillWeak overflow-x-hidden" style=${`width:${barWidth}px`}>
      <div class=${`h-full absolute top-0 ${depth === 0 || children.length === 0 ? color : ''}`} style=${`width:${width}px; left:${left}px`}></div>
      ${children.map(child => {
        const cWidth = (child.duration / traceEnd) * barWidth
        const cLeft = (child.start / traceEnd) * barWidth
        return html`<div class=${`h-full absolute top-0 ${child.color}`} style=${`width:${cWidth}px; left:${cLeft}px`}></div>`
      })}
    </div>
  </div>`
}

function emptyState(source, cols) {
  let title = `No ${source} found`
  let subText = `You're either not sending ${source} to APItoolkit yet or no results matched your query/filter`
  return html`
    <tr class="w-full flex justify-center">
      <td colspan=${String(cols)} class="w-full mx-auto">
        <div class="w-max mx-auto my-8 text-center p-5 sm:py-14 sm:px-24 flex flex-col gap-4">
          <div>${faSprite('empty', 'regular', 'h-24 w-24 mx-auto stroke-blue-500 fill-blue-500')}</div>
          <div class="flex flex-col gap-2">
            <h2 class="text-xl text-slate-800 font-bold">${title}</h2>
            <p class="text-sm max-w-4xl font-medium text-gray-500">${subText}</p>
            <a href="https://apitoolkit.io/docs/sdks/" target="_BLANK" class="btn text-sm w-max mx-auto btn-primary">Read integration guides</a>
          </div>
        </div>
      </td>
    </tr>
  `
}

function requestDumpLogItemUrlPath(rd, colIdxMap, source) {
  const rdId = lookupVecTextByKey(rd, colIdxMap, 'id')
  const rdCreatedAt = lookupVecTextByKey(rd, colIdxMap, 'created_at') || lookupVecTextByKey(rd, colIdxMap, 'timestamp')
  return [rdId, rdCreatedAt, source]
}

function groupSpans(data, colIdxMap, expandedTraces, flipDirection) {
  const traceMap = new Map()
  const TRACE_INDEX = colIdxMap['trace_id']
  const SPAN_INDEX = colIdxMap['latency_breakdown']
  const PARENT_SPAN_INDEX = colIdxMap['parent_span_id']
  const TIMESTAMP_INDEX = colIdxMap['timestamp']
  const SPAN_DURATION_INDEX = colIdxMap['duration']
  const START_TIME_NS = colIdxMap['start_time_ns']
  const ERROR_INDEX = colIdxMap['errors']
  const BODY_INDEX = colIdxMap['body']
  const KIND_INDEX = colIdxMap['kind']

  data.forEach(span => {
    let traceId = span[TRACE_INDEX]
    let spanId = span[SPAN_INDEX]
    const parentSpanId = span[PARENT_SPAN_INDEX]
    const body = span[BODY_INDEX]
    const id = span[colIdxMap['id']]
    if (traceId === '' || traceId === null) {
      // generate random id
      traceId = generateStrId()
      span[TRACE_INDEX] = traceId
    }
    if (spanId === '' || spanId === null) {
      spanId = generateStrId()
      span[SPAN_INDEX] = spanId
    }
    let traceData = traceMap.get(traceId)
    if (!traceData) {
      traceData = {
        traceId,
        spans: new Map(),
        minStart: Infinity,
        duration: 0,
        trace_start_time: null,
      }
      traceMap.set(traceId, traceData)
    }

    const timestamp = new Date(span[TIMESTAMP_INDEX])
    const duration = span[SPAN_DURATION_INDEX]
    const startTime = span[START_TIME_NS]

    if (!traceData.trace_start_time || timestamp < traceData.trace_start_time) {
      traceData.trace_start_time = timestamp
    }
    traceData.minStart = Math.min(traceData.minStart, startTime)
    traceData.duration = Math.max(traceData.duration, duration)
    if (span[KIND_INDEX] === 'log') {
      traceData.spans.set(id, {
        id: spanId,
        startNs: startTime,
        children: [],
        spandId: spanId,
        data: span,
        type: 'log',
      })
    } else {
      traceData.spans.set(spanId, {
        id: spanId,
        startNs: startTime,
        hasErrors: span[ERROR_INDEX],
        duration,
        children: [],
        parent: parentSpanId,
        data: span,
        type: 'span',
      })
    }
  })

  traceMap.forEach(traceData => {
    const spanTree = new Map()
    traceData.spans.forEach(span => {
      if (span.type === 'log') {
        const parentSpan = traceData.spans.get(span.spandId)
        if (parentSpan) {
          parentSpan.children.push(span)
          let i = parentSpan.children.length - 1
          while (i > 0 && parentSpan.children[i].startNs < parentSpan.children[i - 1].startNs) {
            ;[parentSpan.children[i], parentSpan.children[i - 1]] = [parentSpan.children[i - 1], parentSpan.children[i]]
            i--
          }
        } else {
          spanTree.set(span[0], span)
        }
      } else {
        const parentSpan = traceData.spans.get(span.parent)
        if (parentSpan) {
          parentSpan.children.push(span)
          let i = parentSpan.children.length - 1
          while (i > 0 && parentSpan.children[i].startNs < parentSpan.children[i - 1].startNs) {
            ;[parentSpan.children[i], parentSpan.children[i - 1]] = [parentSpan.children[i - 1], parentSpan.children[i]]
            i--
          }
        } else {
          spanTree.set(span.id, span)
        }
      }
    })

    traceData.spans = Array.from(spanTree.values()).sort((a, b) => a.startNs - b.startNs)
  })

  const result = Array.from(traceMap.values()).map(trace => ({
    traceId: trace.traceId,
    spans: Object.values(trace.spans),
    startTime: trace.minStart,
    duration: trace.duration,
  }))

  if (flipDirection) {
    result.reverse()
  }

  return flattenSpanTree(result, expandedTraces)
}

function flattenSpanTree(traceArr, expandedTraces = {}) {
  const result = []

  function traverse(span, traceId, parentIds, traceStart, traceEnd, depth = 0, isLastChild = false, hasSiblingsArr = []) {
    let childrenCount = span.children.length
    let childErrors = false

    const spanInfo = {
      depth,
      traceStart,
      traceEnd,
      traceId,
      childErrors,
      parentIds: parentIds,
      show: expandedTraces[traceId] || depth === 0,
      expanded: expandedTraces[traceId],
      isLastChild,
      siblingsArr: hasSiblingsArr,
      ...span,
      children: childrenCount,
      childrenTimeSpans: span.children.map(child => ({
        startNs: child.startNs,
        duration: child.duration,
        data: child.data,
      })),
    }
    result.push(spanInfo)
    const hasSiling = span.children.length > 1
    span.children.forEach((child, index) => {
      childErrors = child.hasErrors || childErrors
      const lastChild = index === span.children.length - 1
      const newSiblingsArr = hasSiling && !lastChild ? [...hasSiblingsArr, true] : [...hasSiblingsArr, false]
      const [count, errors] = traverse(child, traceId, [...parentIds, span.id], traceStart, traceEnd, depth + 1, lastChild, newSiblingsArr)
      childrenCount += count
      childErrors = childErrors || errors
    })
    spanInfo.children = childrenCount
    spanInfo.childErrors = childErrors
    return [childrenCount, childErrors]
  }

  traceArr.forEach(trace => {
    trace.spans.forEach(span => {
      traverse(span, trace.traceId, [], trace.startTime, trace.duration, 0)
    })
  })
  return result
}

function getColumnWidth(column) {
  if (!['rest', 'service', 'id', 'method', 'status_code', 'raw_url', 'url_path'].includes(column)) return 'w-[16ch] shrink-0'
  switch (column) {
    case 'status':
    case 'method':
    case 'status_code':
      return 'w-[12ch] shrink-0'
    case 'raw_url':
    case 'url_path':
      return 'w-[25ch] shrink-0 overflow-hidden'
    case 'rest':
      return 'w-3/4 shrink-1'
    default:
      return ''
  }
}

function generateStrId() {
  return Math.random().toString(36).substring(2, 15)
}
