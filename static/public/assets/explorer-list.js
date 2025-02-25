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
    this.colIdxMap = JSON.parse(container.dataset.colidxmap)
    this.logsData = JSON.parse(container.dataset.results)
    this.traceLogs = JSON.parse(container.dataset.tracelogs)
    this.hasMore = this.logsData.length > 199
    this.logsColumns = JSON.parse(container.dataset.columns)
    this.expandedTraces = {}
    this.spanListTree = this.source === 'spans' ? groupSpans(this.logsData, this.traceLogs, this.colIdxMap, this.expandedTraces) : []
    this.serviceColors = JSON.parse(container.dataset.servicecolors)
    this.projectId = container.dataset.projectid
    this.nextFetchUrl = container.dataset.nextfetchurl
    this.isLoading = false
    this.isError = false
    this.logItemRow = this.logItemRow.bind(this)
    this.fetchData = this.fetchData.bind(this)
    this.renderSpan = this.renderSpan.bind(this)
    this.renderTrace = this.renderTrace.bind(this)
    this.expandTrace = this.expandTrace.bind(this)
  }

  firstUpdated() {
    const loader = document.querySelector('#loader')
    const container = document.querySelector('#logs_list_container') // The scrollable parent

    if (!loader || !container) {
      console.error('Loader or container not found', { loader, container })
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
        threshold: 0,
      }
    )

    setTimeout(() => {
      observer.observe(loader)
    }, 3000)
  }

  renderTrace(traceData) {
    return traceData.spans.map((span) => {
      return this.renderSpan(span)
    })
  }

  renderSpan(span) {
    return html`${this.logItemRow(span)}`
  }

  expandTrace(tracId) {
    if (!this.expandedTraces[tracId]) {
      this.expandedTraces[tracId] = false
    }
    this.expandedTraces[tracId] = !this.expandedTraces[tracId]
    this.spanListTree = groupSpans(this.logsData, this.traceLogs, this.colIdxMap, this.expandedTraces)
    this.requestUpdate()
  }

  logItemRow(rowData) {
    const s = rowData.type === 'log' ? 'logs' : this.source
    const [url] = requestDumpLogItemUrlPath(this.projectId, this.source === 'spans' ? rowData.data : rowData, this.colIdxMap, s)
    return html`
      <tr class="item-row cursor-pointer whitespace-nowrap overflow-hidden" @click=${(event) => toggleLogRow(event, url)}>
        ${this.logsColumns.map((column) => html`<td>${logItemCol(rowData, this.source, this.colIdxMap, column, this.serviceColors, this.expandTrace)}</td>`)}
      </tr>
    `
  }

  fetchData(url) {
    if (this.isLoading) return
    this.isLoading = true
    fetch(url)
      .then((response) => response.json())
      .then((data) => {
        if (!data.error) {
          const { logsData, serviceColors, nextUrl, traceLogs } = data
          this.logsData = this.logsData.concat(logsData)
          this.traceLogs = this.traceLogs.concat(traceLogs)
          this.spanListTree = this.source === 'spans' ? groupSpans(this.logsData, this.traceLogs, this.colIdxMap, this.expandedTraces) : []
          this.serviceColors = { ...serviceColors, ...this.serviceColors }
          this.nextFetchUrl = nextUrl
          this.hasMore = logsData.length > 199
        } else {
          this.fetchError = data.error
        }
      })
      .catch((error) => {
        this.isError = true
        console.error('Error fetching logs:', error)
      })
      .finally(() => {
        this.isLoading = false
      })
    this.requestUpdate()
  }

  hideColumn(column) {
    this.logsColumns = this.logsColumns.filter((col) => col !== column)
  }

  tableHeadingWrapper(pid, title, column, classes) {
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

  logTableHeading(pid, column) {
    switch (column) {
      case 'id':
        return html`<td class="p-0 m-0 whitespace-nowrap w-3"></td>`
      case 'timestamp':
      case 'created_at':
        return this.tableHeadingWrapper(pid, 'timestamp', column, 'w-[16ch]')
      case 'latency_breakdown':
        return this.tableHeadingWrapper(pid, 'latency_breakdown', column, 'w-[20ch]')
      case 'status_code':
        return this.tableHeadingWrapper(pid, 'status', column)
      case 'service':
        return this.tableHeadingWrapper(pid, 'service', column, 'w-[20ch]')
      case 'rest':
        return this.tableHeadingWrapper(pid, 'summary', column)
      default:
        return this.tableHeadingWrapper(pid, column, column)
    }
  }

  render() {
    return html`
      <div class="relative overflow-y-scroll overflow-x-hidden w-full min-h-full c-scroll pb-10" id="logs_list_container">
        <table class="w-full table-auto ctable min-h-full table-pin-rows table-pin-cols overflow-x-hidden" style="height:1px; --rounded-box:0;">
          <thead class="w-full overflow-hidden">
            <tr class="text-textStrong border-b font-medium border-y">
              ${this.logsColumns.map((column) => this.logTableHeading('', column))}
            </tr>
          </thead>
          <tbody class="w-full log-item-table-body" @rangechanged=${(e) => this.handleVirtualListEvent(e)}>
            ${this.source === 'spans'
              ? virtualize({
                  items: this.spanListTree,
                  renderItem: this.renderSpan,
                })
              : virtualize({
                  items: this.logsData,
                  renderItem: this.logItemRow,
                })}
          </tbody>
        </table>
        ${this.hasMore
          ? html`<div class="w-full flex justify-center relative">
              <div class="absolute -top-[600px]  w-full h-[600px] -z-10 left-0 flex flex-col justify-end bg-[rgba(0,0,0,0.2)] items-center" id="loader"></div>
              ${this.isLoading
                ? html`<div class="mx-auto loading loading-dots loading-md"></div>`
                : html` <button class="cursor-pointer text-textBrand underline font-semibold w-max mx-auto" @click=${() => this.fetchData(this.nextFetchUrl)}>Load more</button> `}
            </div>`
          : ''}
      </div>
    `
  }
  createRenderRoot() {
    return this
  }
}

customElements.define('log-list', LogList)

function faSprite(iconName, kind, classes) {
  return html`<svg class="${classes}"><use href="/public/assets/svgs/fa-sprites/${kind}.svg#${iconName}"></use></svg>`
}

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
        <time class="monospace whitespace-nowrap text-slate-600 w-[16ch]" data-tippy-content="timestamp" datetime=${timestamp}>${displayTimestamp(timestamp)}</time>
      </div>`
    case 'status_code':
      let statusCode = lookupVecTextByKey(dataArr, colIdxMap, 'status_code')
      let statusCls = getStatusColor(Number(statusCode))
      return renderBadge(statusCls, statusCode)
    case 'method':
      let method = lookupVecTextByKey(dataArr, colIdxMap, key)
      let methodCls = getMethodColor(method)
      return renderBadge('min-w-[4rem] cbadge ' + methodCls, method)
    case 'request_type':
      let requestType = lookupVecTextByKey(dataArr, colIdxMap, key)
      if (requestType === 'Incoming') return renderIconWithTippy('w-4', 'Incoming Request', faSprite('arrow-down-left', 'solid', ' h-3 fill-slate-500'))
      return renderIconWithTippy('w-4', 'Outgoing Request', faSprite('arrow-up-right', 'solid', ' h-3 fill-blue-700'))
    case 'duration':
      let dur = rowData.type === 'log' ? 'log' : getDurationNSMS(lookupVecTextByKey(dataArr, colIdxMap, key))
      return renderBadge('cbadge-sm badge-neutral border border-strokeWeak bg-fillWeak', dur)
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
      return renderBadge('cbadge-sm badge-neutral border  border-strokeWeak  bg-fillWeak', spanName)
    case 'service':
      let service = lookupVecTextByKey(dataArr, colIdxMap, key)
      return html` <div class="w-[20ch]">${renderBadge('cbadge-sm badge-neutral border  border-strokeWeak bg-fillWeak', service)}</div>`
    case 'kind':
      let kind = lookupVecTextByKey(dataArr, colIdxMap, key)
      return renderBadge('cbadge-sm badge-neutral border border-strokeWeak bg-fillWeak', kind)
    case 'latency_breakdown':
      const { traceStart, traceEnd, startNs, duration, childrenTimeSpans } = rowData
      const color = serviceColors[lookupVecTextByKey(dataArr, colIdxMap, 'span_name')] || 'black'
      const chil = childrenTimeSpans.map(({ start, duration, data }) => ({
        start: start - traceStart,
        duration,
        color: serviceColors[lookupVecTextByKey(data, colIdxMap, 'span_name')] || 'black',
      }))
      return spanLatencyBreakdown({ start: startNs - traceStart, duration, traceEnd, color, children: chil })
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
        ? html`<div class="flex items-center w-auto gap-1">
            <div class="w-[800px] overflow-hidden flex items-center">
              ${depth > 1 ? new Array(depth - 1).fill(1).map(() => html`<div class="ml-[15px] border-l w-4 h-5 shrink-0"></div>`) : nothing}
              ${depth > 0 ? html`<div class="border-l ml-[15px] w-4 h-5 relative shrink-0"><span class="border-b w-full absolute left-0 top-1/2 -translate-y-1/2"></span></div>` : nothing}
              ${children > 0
                ? html`<button
                    @click=${(e) => {
                      e.stopPropagation()
                      if (depth === 0) toggleTrace(traceId)
                    }}
                    class=${`rounded shrink-0 w-8 px-1 flex justify-center gap-[2px] text-xs items-center h-5 ${errClas}`}
                  >
                    ${depth === 0 ? (expanded ? faSprite('minus', 'regular', 'w-3 h-1 shrink-0') : faSprite('plus', 'regular', 'w-3 h-3 shrink-0')) : nothing} ${children}
                  </button>`
                : html`<div class=${`rounded shrink-0 w-3 h-5 ${errClas}`}></div>`}
              ${type === 'log'
                ? ['severity_text', 'body'].map((k) => logItemCol(rowData, source, { severity_text: 5, body: 6 }, k))
                : ['status', 'kind', 'span_name'].map((k) => logItemCol(rowData, source, colIdxMap, k))}
            </div>
            <div class="w-24 overflow-visible shrink-0">${logItemCol(rowData, source, colIdxMap, 'duration')}</div>
            ${logItemCol(rowData, source, colIdxMap, 'latency_breakdown', serviceColors)}
          </div>`
        : html`
            ${logItemCol(rowData, source, colIdxMap, 'request_type')} ${logItemCol(rowData, source, colIdxMap, 'status_code')} ${logItemCol(rowData, source, colIdxMap, 'method')}
            ${logItemCol(rowData, source, colIdxMap, 'url_path')} ${logItemCol(rowData, source, colIdxMap, 'duration')} ${logItemCol(rowData, source, colIdxMap, 'host')}
            <span class="whitespace-nowrap overflow-x-hidden max-w-lg fill-slate-700 ">${val}</span>
          `
    default:
      let v = lookupVecTextByKey(dataArr, colIdxMap, key)
      return renderBadge('cbadge-sm badge-neutral border border-strokeWeak bg-fillWeak', v)
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

function renderBadge(classes, title) {
  return html`<span class=${'ml-2 ' + classes}>${title}</span>`
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
  return html` <a class=${'shrink-0 inline-flex ' + cls} data-tippy-content=${tip}>${icon}</span>`
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

function spanLatencyBreakdown({ start, duration, traceEnd, color, children }) {
  const width = (duration / traceEnd) * 200
  const left = (start / traceEnd) * 200
  return html`<div class="w-[20ch] -mt-1 shrink-0">
    <div class="flex h-5 w-[200px] relative bg-fillWeak">
      <div class=${`h-full absolute top-0 ${color}`} style=${`width:${width}px; left:${left}px`}></div>
      ${children.map((child) => {
        const cWidth = (child.duration / traceEnd) * 200
        const cLeft = (child.start / traceEnd) * 200
        return html`<div class=${`h-full absolute top-0 ${child.color}`} style=${`width:${cWidth}px; left:${cLeft}px`}></div>`
      })}
    </div>
  </div>`
}

function toggleLogRow(event, source) {
  const sideView = document.querySelector('#log_details_container')
  if (sideView.style.width === '0px') {
    sideView.style.width = '550px'
    updateUrlState('details_width', sideView.style.width)
  }
  const rows = document.querySelectorAll('.item-row.bg-fillBrand-weak')
  rows.forEach((row) => row.classList.remove('bg-fillBrand-weak'))
  event.currentTarget.classList.add('bg-fillBrand-weak')
  const indicator = document.querySelector('#details_indicator')
  indicator.classList.add('htmx-request')
  htmx.ajax('GET', source, { target: '#log_details_container', swap: 'innerHTML', indicator: '#details_indicator' })
  // indicator.classList.remove('htmx-request')
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

  data.forEach((span) => {
    const traceId = span[TRACE_INDEX]
    const spanId = span[SPAN_INDEX]
    const parentSpanId = span[PARENT_SPAN_INDEX]
    if (!traceMap.has(traceId)) {
      traceMap.set(traceId, { traceId, spans: new Map(), minStart: Infinity, duration: 0, logs: logs.filter((log) => log[2] === traceId) })
    }
    const traceData = traceMap.get(traceId)
    const timestamp = new Date(span[TIMESTAMP_INDEX])
    const duration = span[SPAN_DURATION_INDEX]
    const startTime = span[START_TIME_NS]
    if (!traceData.trace_start_time || timestamp < traceData.trace_start_time) traceData.trace_start_time = timestamp
    if (traceData.minStart > startTime) traceData.minStart = startTime
    if (traceData.duration < duration) traceData.duration = duration
    traceData.spans.set(spanId, {
      id: spanId,
      startNs: span[START_TIME_NS],
      hasErrors: span[ERROR_INDEX],
      duration: span[SPAN_DURATION_INDEX],
      children: [],
      parent: parentSpanId,
      data: span,
      type: 'span',
    })
  })

  logs.forEach((log) => {
    const traceData = traceMap.get(log[2])
    traceData.spans.set(log[0], { data: log, type: 'log', spandId: log[3], startNs: log[4], children: [] })
  })

  traceMap.forEach((traceData) => {
    const spanTree = new Map()
    traceData.spans.forEach((span, spanId) => {
      if (span.type === 'log') {
        if (traceData.spans.has(span.spandId)) {
          traceData.spans.get(span.spandId).children.push(span)
          traceData.spans.get(span.spandId).children.sort((a, b) => a.startNs - b.startNs)
        } else {
          spanTree.set(span[0], span)
        }
      } else {
        if (traceData.spans.has(span.parent)) {
          traceData.spans.get(span.parent).children.push(span)
          traceData.spans.get(span.parent).children.sort((a, b) => a.startNs - b.startNs)
        } else {
          spanTree.set(spanId, span)
        }
      }
    })

    traceData.spans = Array.from(spanTree.values()).sort((a, b) => a.startNs - b.startNs)
  })

  const result = Array.from(traceMap.values()).map((trace) => ({
    traceId: trace.traceId,
    spans: Object.values(trace.spans),
    startTime: trace.minStart,
    duration: trace.duration,
  }))

  return flattenSpanTree(result, expandedTraces)
}

function flattenSpanTree(traceArr, expandedTraces = {}) {
  let result = []
  function traverse(span, traceId, traceStart, traceEnd, depth = 0) {
    let childrenCount = span.children.length
    let childErrors = false
    let spanInfo = {
      depth,
      traceStart,
      traceEnd,
      traceId: traceId,
      childErrors,
      expanded: !!expandedTraces[traceId] && depth === 0,
      ...span,
      children: childrenCount,
      childrenTimeSpans: span.children.map((child) => {
        return { startNs: child.startNs, duration: child.duration, data: child.data }
      }),
    }
    if (expandedTraces[traceId] || depth === 0) result.push(spanInfo)
    span.children.forEach((span) => {
      childErrors = span.hasErrors ? true : childErrors
      const [count, errors] = traverse(span, traceId, traceStart, traceEnd, depth + 1)
      childrenCount += count
      childErrors = childErrors || errors
    })
    spanInfo.children = childrenCount
    spanInfo.childErrors = childErrors
    return [childrenCount, childErrors]
  }

  traceArr.forEach((trace) => {
    trace.spans.forEach((span) => {
      traverse(span, trace.traceId, trace.startTime, trace.duration, 0)
    })
  })
  return result
}
