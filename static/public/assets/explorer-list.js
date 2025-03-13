'use strict'
import { LitElement, html, nothing } from './js/thirdparty/lit.js'
import '@lit-labs/virtualizer'
import { virtualize } from '@lit-labs/virtualizer/virtualizer'

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
    groupedData: [],
    expandedTraces: {},
  }
  constructor() {
    super()
    const container = document.querySelector('#resultTable')
    this.source = new URLSearchParams(window.location.search).get('source') || 'requests'

    try {
      this.colIdxMap = JSON.parse(container.dataset.colidxmap || '{}')
      this.logsData = JSON.parse(container.dataset.results || '[]')
      this.traceLogs = JSON.parse(container.dataset.tracelogs || '[]')
      this.logsColumns = JSON.parse(container.dataset.columns || '[]')
      this.serviceColors = JSON.parse(container.dataset.servicecolors || '{}')
    } catch (e) {
      console.error('Error parsing JSON data:', e)
      this.colIdxMap = {}
      this.logsData = []
      this.traceLogs = []
      this.logsColumns = []
      this.serviceColors = {}
    }

    this.hasMore = this.logsData.length > 199
    this.expandedTraces = {}
    this.spanListTree = this.source === 'spans' ? this.buildSpanListTree() : []
    this.projectId = container.dataset.projectid
    this.nextFetchUrl = container.dataset.nextfetchurl
    this.isLoading = false
    this.isLoadingRecent = false
    this.isError = false
    this.logItemRow = this.logItemRow.bind(this)
    this.fetchData = this.fetchData.bind(this)
    this.expandTrace = this.expandTrace.bind(this)
    this.renderLoadMore = this.renderLoadMore.bind(this)
    this.updateTableData = this.updateTableData.bind(this)
    this.latestLogsURLQueryValsFn = this.latestLogsURLQueryValsFn.bind(this)

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
  }

  latestLogsURLQueryValsFn() {
    const datetime = document.querySelector('#log-item-table-body time')?.getAttribute('datetime')
    const to = datetime ? new Date(new Date(datetime).getTime() + 1).toISOString() : params().to
    const from = params().from
    const url = new URL(this.nextFetchUrl, window.location.origin)
    const p = url.searchParams
    p.set('from', from)
    p.delete('cursor')
    p.set('to', to)
    return url.toString()
  }

  updateTableData = (ves, cols, colIdxMap, serviceColors, nextFetchUrl, traceLogs) => {
    this.logsData = [...ves]
    this.logsColumns = [...cols]
    this.colIdxMap = { ...colIdxMap }
    this.traceLogs = [...traceLogs]
    this.hasMore = ves.length > 199
    this.serviceColors = { ...serviceColors }
    this.nextFetchUrl = nextFetchUrl
    this.spanListTree = this.source === 'spans' ? this.buildSpanListTree() : []

    this.requestUpdate()
  }

  connectedCallback() {
    super.connectedCallback()
  }

  firstUpdated() {
    this.setupIntersectionObserver()
    window.logListTable = document.querySelector('#resultTable')
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

  buildSpanListTree() {
    return groupSpans(this.logsData, this.traceLogs, this.colIdxMap, this.expandedTraces)
  }

  renderLoadMore() {
    return this.hasMore
      ? html`<tr class="w-full flex relative">
          <td colspan=${String(this.logsColumns.length)} class="relative">
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
      <td colspan=${String(this.logsColumns.length)} class="relative">
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

  expandTrace(tracId) {
    if (!this.expandedTraces[tracId]) {
      this.expandedTraces[tracId] = false
    }
    this.expandedTraces[tracId] = !this.expandedTraces[tracId]
    const expanded = this.expandedTraces[tracId]
    const affectedSpans = this.spanListTree.filter(span => span.traceId === tracId)
    affectedSpans.forEach(span => {
      if (span.depth != 0) {
        span.show = expanded
      } else {
        span.expanded = expanded
      }
    })
    this.requestUpdate()
  }

  logItemRow(rowData) {
    if (rowData === 'end') return this.renderLoadMore()
    if (rowData === 'start') return this.fetchRecent()
    const s = this.source === 'spans' && rowData.type === 'log' ? 'logs' : this.source
    const [url] = requestDumpLogItemUrlPath(this.projectId, this.source === 'spans' ? rowData.data : rowData, this.colIdxMap, s)
    return html`
      <tr class="item-row flex items-center cursor-pointer whitespace-nowrap" @click=${event => toggleLogRow(event, url)}>
        ${this.logsColumns
          .filter(v => v !== 'latency_breakdown')
          .map(column => {
            const tableDataWidth = getColumnWidth(column)
            return html`<td class=${column === 'rest' ? 'w-[1400px] shrink-1 overflow-x-hidden grow-1' : tableDataWidth}>
              ${logItemCol(rowData, this.source, this.colIdxMap, column, this.serviceColors, this.expandTrace)}
            </td>`
          })}
        ${this.source === 'spans' && this.logsColumns.includes('latency_breakdown')
          ? html`<td class="bg-white sticky right-0">
              ${logItemCol(rowData, this.source, this.colIdxMap, 'latency_breakdown', this.serviceColors, this.expandTrace)}
            </td>`
          : nothing}
      </tr>
    `
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
          const { logsData, serviceColors, nextUrl, traceLogs } = data
          this.logsData = isNewData ? [...logsData, ...this.logsData] : [...this.logsData, ...logsData]
          this.traceLogs = [...this.traceLogs, ...traceLogs]
          this.serviceColors = { ...this.serviceColors, ...serviceColors }
          if (!isNewData) {
            this.hasMore = logsData.length > 199
            this.nextFetchUrl = nextUrl
          }
          this.spanListTree = this.source === 'spans' ? this.buildSpanListTree() : []
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

  tableHeadingWrapper(title, column, classes) {
    return html`
      <td class=${'cursor-pointer p-0 m-0 whitespace-nowrap ' + classes ? classes : ''}>
        <span class="text-slate-200">|</span>
        <div class="dropdown pl-2" data-tippy-content=${title}>
          <div tabindex="0" role="button" class="py-1">
            ${title}
            <span class="ml-1 p-0.5 border border-slate-200 rounded inline-flex"> ${faSprite('chevron-down', 'regular', 'w-3 h-3')} </span>
          </div>
          <ul tabindex="0" class="dropdown-content z-[1] menu p-2 shadow bg-bgBase rounded-box min-w-[15rem]">
            <li>
              <button @click=${() => this.hideColumn(column)}>Hide column</button>
            </li>
          </ul>
        </div>
      </td>
    `
  }

  logTableHeading(column) {
    switch (column) {
      case 'id':
        return html`<td class="p-0 m-0 whitespace-nowrap w-3"></td>`
      case 'timestamp':
      case 'created_at':
        return this.tableHeadingWrapper('timestamp', column, 'w-[16ch] shrink-0')
      case 'latency_breakdown':
        return this.tableHeadingWrapper('latency', column, 'sticky right-0 shrink-0 w-[200px]')
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

  render() {
    const list = this.source === 'spans' ? this.spanListTree.filter(sp => sp.show) : [...this.logsData]
    // end is used to render the load more button"
    list.unshift('start')
    list.push('end')
    return html`
      <div class="relative h-full p-0 m-0 shrink-1 w-full bg-white c-scroll pb-12 overflow-y-scroll " id="logs_list_container_inner">
        <table class="table-auto w-max relative ctable table-pin-rows table-pin-cols">
          <thead class="z-10 sticky top-0">
            <tr class="text-textStrong border-b flex min-w-0 relative font-medium border-y">
              ${this.logsColumns.filter(v => v !== 'latency_breakdown').map(column => this.logTableHeading(column))}
              ${this.source === 'spans' ? this.logTableHeading('latency_breakdown') : nothing}
            </tr>
          </thead>
          ${list.length === 1 ? emptyState(this.source, this.logsColumns.length) : nothing}
          <tbody
            class="min-w-0"
            id="log-item-table-body"
            @rangeChanged=${() => {
              this.setupIntersectionObserver()
              const tableBody = document.querySelector('#logs_list_container_inner')
              if (tableBody && tableBody.scrollTop === 0) {
                tableBody.scrollTop = 30
              }
            }}
          >
            ${virtualize({
              items: list,
              renderItem: this.logItemRow,
            })}
          </tbody>
        </table>
      </div>
    `
  }
  createRenderRoot() {
    return this
  }
}

customElements.define('log-list', LogList)

const faSprite = (iconName, kind, classes) => html`<svg class="${classes}"><use href="/public/assets/svgs/fa-sprites/${kind}.svg#${iconName}"></use></svg>`

function logItemCol(rowData, source, colIdxMap, key, serviceColors, toggleTrace) {
  const dataArr = source === 'spans' ? rowData.data : rowData
  switch (key) {
    case 'id':
      let [status, errCount, errClass] = errorClass(false, dataArr, colIdxMap)
      return html`
        <div class="flex items-center justify-between w-3">
          <span class="col-span-1 h-5 rounded flex"> ${renderIconWithTippy(errClass, `${errCount} errors attached; status ${status}`)} </span>
        </div>
      `
    case 'created_at':
    case 'timestamp':
      let timestamp = lookupVecTextByKey(dataArr, colIdxMap, key)
      return html`<div class="w-[16ch]">
        <time class="monospace whitespace-nowrap text-slate-600 w-[16ch]" data-tippy-content="timestamp" datetime=${timestamp}
          >${displayTimestamp(timestamp)}</time
        >
      </div>`
    case 'status_code':
      let statusCode = lookupVecTextByKey(dataArr, colIdxMap, 'status_code')
      let statusCls = getStatusColor(Number(statusCode))
      return renderBadge(statusCls, statusCode, 'status code')
    case 'method':
      let method = lookupVecTextByKey(dataArr, colIdxMap, key)
      let methodCls = getMethodColor(method)
      return renderBadge('min-w-[4rem] cbadge ' + methodCls, method, 'method')
    case 'request_type':
      let requestType = lookupVecTextByKey(dataArr, colIdxMap, key)
      if (requestType === 'Incoming') return renderIconWithTippy('w-4', 'Incoming Request', faSprite('arrow-down-left', 'solid', ' h-3 fill-slate-500'))
      return renderIconWithTippy('w-4', 'Outgoing Request', faSprite('arrow-up-right', 'solid', ' h-3 fill-blue-700'))
    case 'duration':
      let dur = rowData.type === 'log' ? 'log' : getDurationNSMS(lookupVecTextByKey(dataArr, colIdxMap, key))
      return renderBadge('cbadge-sm badge-neutral font-normal border border-strokeWeak bg-fillWeak', dur, 'latency')
    case 'severity_text':
      let severity = lookupVecTextByKey(dataArr, colIdxMap, key) || 'UNSET'
      let severityClass = getSeverityColor(severity)
      return renderBadge('cbadge-sm cbadge ' + severityClass, severity)
    case 'body':
      let body = lookupVecTextByKey(dataArr, colIdxMap, key)
      return renderBadge('space-x-2 whitespace-nowrap', body)
    case 'status':
      let st = lookupVecTextByKey(dataArr, colIdxMap, key)
      let statsCls = getSpanStatusColor(st)
      return renderBadge(statsCls, st)
    case 'span_name':
      let spanName = lookupVecTextByKey(dataArr, colIdxMap, key)
      return renderBadge('cbadge-sm badge-neutral border  border-strokeWeak  bg-fillWeak', spanName, 'span name')
    case 'service':
      colIdxMap = rowData.type === 'log' ? { ...colIdxMap, service: dataArr.length - 1 } : colIdxMap
      let service = lookupVecTextByKey(dataArr, colIdxMap, key)
      return html` <div class="w-[16ch]">${renderBadge('cbadge-sm badge-neutral border  border-strokeWeak bg-fillWeak', service, 'service name')}</div>`
    case 'kind':
      let kind = lookupVecTextByKey(dataArr, colIdxMap, key)
      return renderBadge('cbadge-sm badge-neutral border border-strokeWeak bg-fillWeak', kind, 'span kind')
    case 'latency_breakdown':
      const { traceStart, traceEnd, startNs, duration, childrenTimeSpans, depth: d } = rowData
      const color = serviceColors[lookupVecTextByKey(dataArr, colIdxMap, 'span_name')] || 'black'
      const chil = childrenTimeSpans.map(({ startNs, duration, data }) => ({
        start: startNs - traceStart,
        duration,
        color: serviceColors[lookupVecTextByKey(data, colIdxMap, 'span_name')] || 'black',
      }))
      return html`
        <div class="w-[200px] flex h-10 justify-end items-center gap-1 text-textWeak">
          <div class="w-24 overflow-visible  shrink-0 font-normal">${logItemCol(rowData, source, colIdxMap, 'duration')}</div>
          ${spanLatencyBreakdown({ start: startNs - traceStart, depth: d, duration, traceEnd, color, children: chil })}
        </div>
      `
    case 'http_attributes':
      const attributes = lookupVecObjectByKey(dataArr, colIdxMap, key)
      const { method: m, url, status_code: statusCode_ } = attributes
      if (m || url || statusCode_) {
        let k = lookupVecTextByKey(dataArr, colIdxMap, 'kind')
        let methodCls_ = getMethodColor(m)
        let statusCls_ = getStatusColor(Number(statusCode_))
        return html`
          ${k === 'SERVER'
            ? renderIconWithTippy('w-4 ml-2', 'Incoming Request', faSprite('arrow-down-left', 'solid', ' h-3 fill-slate-500'))
            : k === 'CLIENT'
            ? renderIconWithTippy('w-4 ml-2', 'Outgoing Request', faSprite('arrow-up-right', 'solid', ' h-3 fill-blue-700'))
            : nothing}
          ${statusCode_ ? renderBadge(statusCls_, statusCode_, 'status code') : nothing}
          ${m ? renderBadge('min-w-[4rem] text-center cbadge cbadge-sm ' + methodCls_, m, 'method') : nothing}
          ${url ? renderBadge('cbadge-sm badge-neutral border border-strokeWeak bg-fillWeak', url, 'url') : nothing}
        `
      }
      break
    case 'db_attributes':
      const dbAttributes = lookupVecObjectByKey(dataArr, colIdxMap, key)
      const { system, statement } = dbAttributes
      if (system || statement) {
        return html`
          ${system ? renderBadge('cbadge-sm badge-neutral border border-strokeWeak bg-fillWeak', system) : nothing}
          ${statement ? renderBadge('cbadge-sm badge-neutral border border-strokeWeak bg-fillWeak', statement) : nothing}
        `
      }
      break
    case 'rest':
      let val = lookupVecTextByKey(dataArr, colIdxMap, key)
      const { depth, children, traceId, childErrors, hasErrors, expanded, type } = rowData
      const errClas = hasErrors
        ? 'bg-red-500 text-white fill-white stroke-white'
        : childErrors
        ? 'border border-red-500 bg-fillWeak text-textWeak fill-textWeak'
        : 'border border-strokeWeak bg-fillWeak text-textWeak fill-textWeak'
      return source == 'logs'
        ? html`${logItemCol(rowData, source, colIdxMap, 'severity_text')} ${logItemCol(rowData, source, colIdxMap, 'body')}`
        : source === 'spans'
        ? html`<div class="flex w-full items-center gap-1">
            <div class="w-full flex items-center">
              ${depth > 1 ? new Array(depth - 1).fill(1).map(() => html`<div class="ml-[15px] border-l w-4 h-5 shrink-0"></div>`) : nothing}
              ${depth > 0
                ? html`<div class="border-l ml-[15px] w-4 h-5 relative shrink-0">
                    <span class="border-b w-full absolute left-0 top-1/2 -translate-y-1/2"></span>
                  </div>`
                : nothing}
              ${children > 0
                ? html`<button
                    @click=${e => {
                      e.stopPropagation()
                      if (depth === 0) toggleTrace(traceId)
                    }}
                    class=${`rounded shrink-0 w-8 px-1 flex justify-center gap-[2px] text-xs items-center h-5 ${errClas}`}
                  >
                    ${depth === 0 ? (expanded ? faSprite('minus', 'regular', 'w-3 h-1 shrink-0') : faSprite('plus', 'regular', 'w-3 h-3 shrink-0')) : nothing}
                    ${children}
                  </button>`
                : depth === 0
                ? nothing
                : html`<div class=${`rounded shrink-0 w-3 h-5 ${errClas}`}></div>`}
              ${type === 'log'
                ? ['severity_text', 'body'].map(k => logItemCol(rowData, source, { severity_text: 5, body: 6 }, k))
                : ['http_attributes', 'db_attributes', 'status', 'kind', 'span_name'].map(k => logItemCol(rowData, source, colIdxMap, k))}
            </div>
            <!-- <div class="w-24 overflow-visible shrink-0">${logItemCol(rowData, source, colIdxMap, 'duration')}</div>
            ${logItemCol(rowData, source, colIdxMap, 'latency_breakdown', serviceColors)} -->
          </div>`
        : html`
            ${logItemCol(rowData, source, colIdxMap, 'request_type')} ${logItemCol(rowData, source, colIdxMap, 'status_code')}
            ${logItemCol(rowData, source, colIdxMap, 'method')} ${logItemCol(rowData, source, colIdxMap, 'url_path')}
            ${logItemCol(rowData, source, colIdxMap, 'duration')} ${logItemCol(rowData, source, colIdxMap, 'host')}
            <span class="whitespace-nowrap overflow-x-hidden max-w-lg fill-slate-700 ">${val}</span>
          `
    default:
      let v = lookupVecTextByKey(dataArr, colIdxMap, key)
      return renderBadge('cbadge-sm badge-neutral border border-strokeWeak bg-fillWeak', v, key)
  }
}

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
  return html`<span class=${`ml-2 relative  ${classes} ${tippy ? 'tooltip tooltip-right' : ''}`} data-tip=${tippy}>${title}</span>`
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
  if (status < 200) return 'cbadge-sm badge-neutral'
  if (status < 300) return 'cbadge-sm badge-2xx'
  if (status < 400) return 'cbadge-sm badge-3xx'
  return 'cbadge-sm badge-4xx'
}

function getMethodColor(method) {
  const methodColors = {
    POST: 'cbadge-sm badge-pink',
    PUT: 'cbadge-sm badge-lime',
    DELETE: 'cbadge-sm badge-error',
    PATCH: 'cbadge-sm badge-cyan',
    GET: 'cbadge-sm badge-blue',
  }
  return methodColors[method] || 'cbadge-sm badge-neutral'
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
  const errCount = lookupVecTextByKey(reqVec, colIdxMap, 'errors_count') || 0
  const status = lookupVecTextByKey(reqVec, colIdxMap, 'status_code') || 0

  let errClass = ' w-1 bg-blue-200 status-indicator '
  if (errCount > 0) {
    errClass = ' w-1 bg-red-500 '
  } else if (status >= 400) {
    errClass = ' w-1 bg-warning '
  } else if (expandedSection) {
    errClass = ' w-1 bg-blue-200 '
  }

  return [status, errCount, errClass]
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
    }[status] || 'cbadge-sm badge-neutral'
  )
}

function spanLatencyBreakdown({ start, duration, traceEnd, depth, color, children }) {
  const barWidth = 100
  const width = (duration / traceEnd) * barWidth
  const left = (start / traceEnd) * barWidth
  return html`<div class="-mt-1 shrink-0">
    <div class="flex h-5 w-[100px] relative bg-fillWeak">
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

function toggleLogRow(event, source) {
  const sideView = document.querySelector('#log_details_container')
  const logsView = document.querySelector('#logs_list_container')
  const resizer = document.querySelector('#resizer')
  if (sideView.style.width === '0px') {
    const lW = getComputedStyle(logsView).width.replace('px', '')
    logsView.style.width = `${lW - 550}px`
    sideView.style.width = '550px'
    resizer.classList.remove('hidden')
    updateUrlState('details_width', sideView.style.width)
  }
  const rows = document.querySelectorAll('.item-row.bg-fillBrand-weak')
  rows.forEach(row => row.classList.remove('bg-fillBrand-weak'))
  event.currentTarget.classList.add('bg-fillBrand-weak')
  const indicator = document.querySelector('#details_indicator')
  indicator.classList.add('htmx-request')
  htmx.ajax('GET', source, { target: '#log_details_container', swap: 'innerHTML', indicator: '#details_indicator' })
}

function requestDumpLogItemUrlPath(pid, rd, colIdxMap, source) {
  const rdId = lookupVecTextByKey(rd, colIdxMap, 'id')
  const rdCreatedAt = lookupVecTextByKey(rd, colIdxMap, 'created_at') || lookupVecTextByKey(rd, colIdxMap, 'timestamp')
  return [`/p/${pid}/log_explorer/${rdId}/${rdCreatedAt}/detailed?source=${source}`, rdId]
}

function groupSpans(data, logs, colIdxMap, expandedTraces) {
  const traceMap = new Map()
  const TRACE_INDEX = colIdxMap['trace_id']
  const SPAN_INDEX = colIdxMap['latency_breakdown']
  const PARENT_SPAN_INDEX = colIdxMap['parent_span_id']
  const TIMESTAMP_INDEX = colIdxMap['timestamp']
  const SPAN_DURATION_INDEX = colIdxMap['duration']
  const START_TIME_NS = colIdxMap['start_time_ns']
  const ERROR_INDEX = colIdxMap['errors']

  const logMap = logs.reduce((map, log) => {
    if (!map.has(log[2])) map.set(log[2], [])
    map.get(log[2]).push(log)
    return map
  }, new Map())

  data.forEach(span => {
    const traceId = span[TRACE_INDEX]
    const spanId = span[SPAN_INDEX]
    const parentSpanId = span[PARENT_SPAN_INDEX]

    let traceData = traceMap.get(traceId)
    if (!traceData) {
      traceData = {
        traceId,
        spans: new Map(),
        minStart: Infinity,
        duration: 0,
        logs: logMap.get(traceId) || [],
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
  })

  logs.forEach(log => {
    const traceData = traceMap.get(log[2])
    if (traceData) {
      traceData.spans.set(log[0], {
        data: log,
        type: 'log',
        spandId: log[3],
        startNs: log[4],
        children: [],
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

  return flattenSpanTree(result, expandedTraces)
}

function flattenSpanTree(traceArr, expandedTraces = {}) {
  const result = []

  function traverse(span, traceId, traceStart, traceEnd, depth = 0) {
    let childrenCount = span.children.length
    let childErrors = false

    const spanInfo = {
      depth,
      traceStart,
      traceEnd,
      traceId,
      childErrors,
      show: expandedTraces[traceId] || depth === 0,
      expanded: expandedTraces[traceId] && depth === 0,
      ...span,
      children: childrenCount,
      childrenTimeSpans: span.children.map(child => ({
        startNs: child.startNs,
        duration: child.duration,
        data: child.data,
      })),
    }

    result.push(spanInfo)

    span.children.forEach(child => {
      childErrors = child.hasErrors || childErrors
      const [count, errors] = traverse(child, traceId, traceStart, traceEnd, depth + 1)
      childrenCount += count
      childErrors = childErrors || errors
    })

    spanInfo.children = childrenCount
    spanInfo.childErrors = childErrors
    return [childrenCount, childErrors]
  }

  traceArr.forEach(trace => {
    trace.spans.forEach(span => {
      traverse(span, trace.traceId, trace.startTime, trace.duration, 0)
    })
  })

  return result
}

function getColumnWidth(column) {
  if (!['rest', 'service', 'id', 'status', 'method', 'raw_url', 'url_path'].includes(column)) return 'w-[16ch] shrink-0'
  switch (column) {
    case 'status':
    case 'method':
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
