'use strict'
import { LitElement, html, repeat } from './js/thirdparty/lit.js'
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
    canLoadMore: { type: Boolean },
  }

  constructor() {
    super()
    const container = document.querySelector('#logsList')
    this.logsData = JSON.parse(container.dataset.results)
    this.hasMore = this.logsData.length > 199
    this.logsColumns = JSON.parse(container.dataset.columns)
    this.colIdxMap = JSON.parse(container.dataset.colidxmap)
    const childSpans = JSON.parse(container.dataset.childspans)
    this.childSpansMap = new Map()
    childSpans.forEach((span) => {
      if (this.childSpansMap.has(span[1])) {
        this.childSpansMap.get(span[1]).push(span)
      } else {
        this.childSpansMap.set(span[1], [span])
      }
    })

    this.projectId = container.dataset.projectid
    this.nextFetchUrl = container.dataset.nextfetchurl
    this.source = new URLSearchParams(window.location.search).get('source') || 'requests'
    this.isLoading = false
    this.isError = false
    this.logItemRow = this.logItemRow.bind(this)
    this.fetchData = this.fetchData.bind(this)
    this.canLoadMore = false
  }

  firstUpdated() {
    const loader = document.querySelector('#loader')
    const observer = new IntersectionObserver(
      ([entry]) => {
        if (entry.isIntersecting) {
          this.fetchData(this.nextFetchUrl)
        }
      },
      { threshold: 0.5 }
    )
    if (loader) {
      window.setTimeout(() => {
        observer.observe(loader)
      }, 3000)
    } else {
      console.log('loader not found', loader)
    }
  }

  logItemRow(rowData) {
    const [url] = requestDumpLogItemUrlPath(this.projectId, rowData, this.colIdxMap, this.source)
    return html`
      <tr class="cursor-pointer whitespace-nowrap overflow-hidden" @click=${() => toggleLogRow(url)}>
        ${this.logsColumns.map((column) => html`<td>${logItemCol(rowData, this.source, this.colIdxMap, column, this.childSpansMap)}</td>`)}
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
          const { logsData, childSpans, nextUrl } = data
          this.logsData = this.logsData.concat(logsData)
          childSpans.forEach((span) => {
            if (this.childSpansMap.has(span[1])) {
              this.childSpansMap.get(span[1]).push(span)
            } else {
              this.childSpansMap.set(span[1], [span])
            }
          })
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
  handleVirtualListEvent(e) {
    console.log('The first visible index is', e.firstVisible)
    console.log('The last visible index is', e.lastVisible)
    this.canLoadMore = true
  }
  render() {
    return html`
      <div class="relative overflow-y-scroll overflow-x-hidden w-full pb-16 min-h-full c-scroll" id="logs_list_container">
        <table class="w-full table-auto ctable min-h-full table-pin-rows table-pin-cols overflow-x-hidden" style="height:1px; --rounded-box:0;">
          <thead class="w-full overflow-hidden">
            <tr class="text-textStrong border-b font-medium border-y">
              ${this.logsColumns.map((column) => this.logTableHeading('', column))}
            </tr>
          </thead>
          <tbody class="w-full log-item-table-body" @rangechanged=${(e) => this.handleVirtualListEvent(e)}>
            ${virtualize({
              items: this.logsData,
              renderItem: this.logItemRow,
            })}
          </tbody>
        </table>
        ${this.hasMore
          ? html`<div class="w-full flex justify-center items-center" id="loader">
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

function logTableHeading(pid, column) {
  switch (column) {
    case 'id':
      return html`<td class="p-0 m-0 whitespace-nowrap w-3"></td>`
    case 'timestamp':
    case 'created_at':
      return tableHeadingWrapper(pid, 'timestamp', 'w-[16ch]')
    case 'latency_breakdown':
      return tableHeadingWrapper(pid, 'latency_breakdown', 'w-[20ch]')
    case 'status_code':
      return tableHeadingWrapper(pid, 'status')
    case 'service':
      return tableHeadingWrapper(pid, 'service', 'w-[20ch]')
    case 'rest':
      return tableHeadingWrapper(pid, 'summary')
    default:
      return tableHeadingWrapper(pid, column)
  }
}

customElements.define('log-list', LogList)

function faSprite(iconName, kind, classes) {
  return html`<svg class="${classes}"><use href="/public/assets/svgs/fa-sprites/${kind}.svg#${iconName}"></use></svg>`
}

function logItemCol(dataArr, source, colIdxMap, key, childSpansMap) {
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
      let duration = getDurationNSMS(lookupVecTextByKey(dataArr, colIdxMap, key))
      return renderBadge('cbadge-sm badge-neutral border border-strokeWeak bg-fillWeak', duration)
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
      let spanId = lookupVecTextByKey(dataArr, colIdxMap, key)
      let spans = childSpansMap.get(spanId) || []
      return spanLatencyBreakdown(spans)
    case 'rest':
      let val = lookupVecTextByKey(dataArr, colIdxMap, key)
      return source == 'logs'
        ? html`${logItemCol(dataArr, source, colIdxMap, 'severity_text')} ${logItemCol(dataArr, source, colIdxMap, 'body')}`
        : source === 'spans'
        ? ['status', 'kind', 'duration', 'span_name'].map((k) => logItemCol(dataArr, source, colIdxMap, k))
        : html`
            ${logItemCol(dataArr, source, colIdxMap, 'request_type')} ${logItemCol(dataArr, source, colIdxMap, 'status_code')} ${logItemCol(dataArr, source, colIdxMap, 'method')}
            ${logItemCol(dataArr, source, colIdxMap, 'url_path')} ${logItemCol(dataArr, source, colIdxMap, 'duration')} ${logItemCol(dataArr, source, colIdxMap, 'host')}
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

function barSeverityClass(reqVec, colIdxMap) {
  const severity = lookupVecTextByKey(reqVec, colIdxMap, 'severity') || 'INFO'

  return (
    {
      ERROR: 'bg-red-500',
      WARNING: 'bg-warning',
      INFO: 'bg-blue-200',
      DEBUG: 'bg-gray-300',
      FATAL: 'bg-purple-500',
    }[severity] || 'bg-blue-200'
  )
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

function spanLatencyBreakdown(spans) {
  const totalDuration = spans.reduce((sum, sp) => sum + sp[2], 0)
  return html`<div class="w-[20ch] h-0 overflow-visible">
    <div class="flex h-5 w-[200px]">
      ${spans.map((sps, i) => {
        const width = (sps[2] / totalDuration) * 200
        const color = sps[3] || 'bg-black'
        const roundL = i === 0 ? 'rounded-l-sm' : ''
        const roundR = i === spans.length - 1 ? 'rounded-r-sm' : ''
        const dur = getDurationNSMS(sps[2])
        return html`
          <div
            class=${`h-full overflow-hidden ${roundL} ${roundR} ${color}`}
            style=${`width:${width}px`}
            title=${`Span name: ${sps[0]} Duration: ${dur}`}
            data-tippy-content=${`Span name: ${sps[0]} Duration: ${dur}`}
          >
            <div class="h-full w-full"></div>
          </div>
        `
      })}
    </div>
  </div>`
}

function toggleLogRow(source) {
  const sideView = document.querySelector('#log_details_container')
  if (sideView.style.width === '0px') {
    sideView.style.width = '550px'
    updateUrlState('details_width', sideView.style.width)
  }
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
