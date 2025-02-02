'use strict'
import { LitElement, html } from './js/thirdparty/lit.js'
import '@lit-labs/virtualizer'

export class LogList extends LitElement {
  static properties = {
    logsData: [],
    logsColumns: [],
    colIdxMap: {},
    isLoading: { type: Boolean },
    isError: { type: Boolean },
    source: { type: String },
  }

  constructor() {
    super()
    const container = document.querySelector('#logsList')
    this.logsData = JSON.parse(container.dataset.results)
    this.logsColumns = JSON.parse(container.dataset.columns)
    this.colIdxMap = JSON.parse(container.dataset.colidxmap)
    this.source = new URLSearchParams(window.location.search).get('source') || 'requests'
    this.isLoading = false
    this.isError = false
  }

  render() {
    return html`
      <div class="relative  overflow-y-scroll overflow-x-hidden w-full pb-16">
        <table class="w-full  table-auto ctable table-pin-rows table-pin-cols overflow-x-hidden" style="height:1px; --rounded-box:0">
          <thead>
            <tr class="text-slate-700 border-b font-medium border-y">
              ${this.logsColumns.map((column) => logTableHeading('', column))}
            </tr>
          </thead>
          <tbody class="w-full log-item-table-body">
            ${logItemRows(this.logsData, this.colIdxMap, this.logsColumns, this.source)}
          </tbody>
        </table>
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
      return tableHeadingWrapper(pid, 'timestamp', 'w-[16ch]')

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

function tableHeadingWrapper(pid, title, classes, child) {
  return html`
    <td class=${'cursor-pointer p-0 m-0 whitespace-nowrap ' + classes ? classes : ''}>
      <span class="text-slate-200">|</span>
      <div class="dropdown pl-2" data-tippy-content=${title}>
        <div tabindex="0" role="button" class="py-1">
          ${title}
          <span class="ml-1 p-0.5 border border-slate-200 rounded inline-flex"> ${faSprite('chevron-down', 'regular', 'w-3 h-3')} </span>
        </div>
        <ul tabindex="0" class="dropdown-content z-[1] menu p-2 shadow bg-bgBase rounded-box min-w-[15rem]">
          <li class="underline underline-offset-2">${title}</li>
          <li>
            <a> Hide column </a>
          </li>
        </ul>
      </div>
    </td>
  `
}

customElements.define('log-list', LogList)

function faSprite(iconName, kind, classes) {
  return html`<svg class="${classes}"><use href="/public/assets/svgs/fa-sprites/${kind}.svg#${iconName}"></use></svg>`
}

// logItemCol_ :: Text -> Projects.ProjectId -> V.Vector AE.Value -> HM.HashMap Text Int -> Text -> V.Vector Telemetry.SpanRecord -> Html ()
// logItemCol_ source pid reqVec colIdxMap "id" chSpns = do
//   let (status, errCount, errClass) = errorClass False reqVec colIdxMap
//   let severityClass = barSeverityClass reqVec colIdxMap
//   div_ [class_ "grid grid-cols-3 items-center max-w-12 min-w-10"] do
//     span_ [class_ "col-span-1 h-5 rounded flex"] $ renderIconWithTippy (if source == "logs" then severityClass else errClass) (show errCount <> " errors attached; status " <> show status) " "
//     faSprite_ "chevron-right" "solid" "h-3 col-span-1 text-gray-500 chevron log-chevron "
// logItemCol_ _ _ reqVec colIdxMap "created_at" _ = renderTimestamp "created_at" reqVec colIdxMap
// logItemCol_ _ _ reqVec colIdxMap "timestamp" _ = renderTimestamp "timestamp" reqVec colIdxMap
// logItemCol_ _ _ reqVec colIdxMap "status_code" _ = renderStatusCode reqVec colIdxMap
// logItemCol_ _ _ reqVec colIdxMap "method" _ = renderMethod reqVec colIdxMap
// logItemCol_ _ _ reqVec colIdxMap "severity_text" _ = renderLogBadge "severity_text" reqVec colIdxMap (getSeverityColor $ T.toLower $ fromMaybe "" $ lookupVecTextByKey reqVec colIdxMap "severity_text")
// logItemCol_ _ _ reqVec colIdxMap "duration" _ = renderBadge "cbadge-sm badge-neutral border  border-strokeWeak  bg-fillWeak" (toText (getDurationNSMS $ toInteger $ lookupVecIntByKey reqVec colIdxMap "duration")) "duration"
// logItemCol_ _ _ reqVec colIdxMap "span_name" _ = renderLogBadge "span_name" reqVec colIdxMap "cbadge-sm badge-neutral border  border-strokeWeak  bg-fillWeak"
// logItemCol_ _ _ reqVec colIdxMap "service" _ = renderLogBadge "service" reqVec colIdxMap "cbadge-sm badge-neutral border  border-strokeWeak  bg-fillWeak"
// logItemCol_ _ pid reqVec colIdxMap "latency_breakdown" childSpans =
//   let spanId = lookupVecTextByKey reqVec colIdxMap "latency_breakdown"
//    in Spans.spanLatencyBreakdown $ V.filter (\s -> s.parentSpanId == spanId) childSpans
// logItemCol_ _ _ reqVec colIdxMap "body" _ = renderLogBadge "body" reqVec colIdxMap "space-x-2 whitespace-nowrap"
// logItemCol_ _ _ reqVec colIdxMap "kind" _ = renderBadge "cbadge-sm badge-neutral border  border-strokeWeak  bg-fillWeak" (fromMaybe "" $ lookupVecTextByKey reqVec colIdxMap "kind") "kind"
// logItemCol_ _ _ reqVec colIdxMap "status" _ = renderLogBadge "status" reqVec colIdxMap (getSpanStatusColor $ fromMaybe "" $ lookupVecTextByKey reqVec colIdxMap "status")
// logItemCol_ source pid reqVec colIdxMap "rest" _ = div_ [class_ "space-x-2 whitespace-nowrap max-w-8xl overflow-hidden "] do
//   let key = "rest"
//   case source of
//     "logs" -> forM_ ["severity_text", "body"] \v -> logItemCol_ source pid reqVec colIdxMap v []
//     "spans" -> forM_ ["status", "kind", "duration", "span_name"] \v -> logItemCol_ source pid reqVec colIdxMap v []
//     _ -> do
//       if lookupVecTextByKey reqVec colIdxMap "request_type" == Just "Incoming"
//         then renderIconWithTippy "text-slate-500" "Incoming Request" (faSprite_ "arrow-down-left" "solid" "h-3")
//         else renderIconWithTippy "text-blue-700" "Outgoing Request" (faSprite_ "arrow-up-right" "solid" "h-3")
//       logItemCol_ source pid reqVec colIdxMap "status_code" []
//       logItemCol_ source pid reqVec colIdxMap "method" []
//       renderLogBadge "url_path" reqVec colIdxMap "cbadge-sm badge-neutral border  border-strokeWeak  bg-fillWeak"
//       logItemCol_ source pid reqVec colIdxMap "duration" []
//       renderLogBadge "host" reqVec colIdxMap "cbadge-sm badge-neutral border  border-strokeWeak  bg-fillWeak"
//       span_ $ toHtml $ maybe "" (unwrapJsonPrimValue True) (lookupVecByKey reqVec colIdxMap key)
// logItemCol_ _ _ reqVec colIdxMap key _ = renderBadge "space-nowrap overflow-x-hidden max-w-lg" (maybe "" (unwrapJsonPrimValue True) (lookupVecByKey reqVec colIdxMap key)) key

// let (status, errCount, errClass) = errorClass False reqVec colIdxMap
// let severityClass = barSeverityClass reqVec colIdxMap
// div_ [class_ "grid grid-cols-3 items-center max-w-12 min-w-10"] do
//   span_ [class_ "col-span-1 h-5 rounded flex"] $ renderIconWithTippy (if source == "logs" then severityClass else errClass) (show errCount <> " errors attached; status " <> show status) " "
//   faSprite_ "chevron-right" "solid" "h-3 col-span-1 text-gray-500 chevron log-chevron "

function logItemCol(dataArr, source, colIdxMap, key, childSpans) {
  switch (key) {
    case 'id':
      let [status, errCount, errClass] = errorClass(false, dataArr, colIdxMap)
      return html`
        <div class="flex items-center justify-between w-10">
          <span class="col-span-1 h-5 rounded flex"> ${renderIconWithTippy(errClass, `${errCount} errors attached; status ${status}`)} </span>
          ${faSprite('chevron-right', 'solid', 'h-3 col-span-1 text-gray-500 chevron log-chevron ')}
        </div>
      `
    case 'created_at':
    case 'timestamp':
      let timestamp = lookupVecTextByKey(dataArr, colIdxMap, key)
      return html`<time class="monospace whitespace-nowrap text-slate-600" data-tippy-content="timestamp" datetime=${timestamp}>${displayTimestamp(timestamp)}</time>`
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
      return renderBadge('cbadge-sm badge-neutral border  border-strokeWeak  bg-fillWeak', service)
    case 'kind':
      let kind = lookupVecTextByKey(dataArr, colIdxMap, key)
      return renderBadge('cbadge-sm badge-neutral border border-strokeWeak bg-fillWeak', kind)
    case 'rest':
      let val = lookupVecTextByKey(dataArr, colIdxMap, key)
      return source == 'logs'
        ? html`${logItemCol(dataArr, source, colIdxMap, 'severity_text')} ${logItemCol(dataArr, source, colIdxMap, 'body')}`
        : source === 'spans'
        ? ['status', 'kind', 'duration', 'span_name'].map((k) => logItemCol(dataArr, source, colIdxMap, k))
        : html`
            ${logItemCol(dataArr, source, colIdxMap, 'request_type')} ${logItemCol(dataArr, source, colIdxMap, 'status_code')} ${logItemCol(dataArr, source, colIdxMap, 'method')}
            ${logItemCol(dataArr, colIdxMap, 'url_path')} ${logItemCol(dataArr, source, colIdxMap, 'duration')} ${logItemCol(dataArr, source, colIdxMap, 'host')}
            <span class="whitespace-nowrap overflow-x-hidden max-w-lg fill-slate-700 ">${val}</span>
          `
    default:
      let v = lookupVecTextByKey(dataArr, colIdxMap, key)
      return renderBadge('cbadge-sm badge-neutral border border-strokeWeak bg-fillWeak', v)
  }
}

// case source of
// "logs" -> forM_ ["severity_text", "body"] \v -> logItemCol_ source pid reqVec colIdxMap v []
// "spans" -> forM_ ["status", "kind", "duration", "span_name"] \v -> logItemCol_ source pid reqVec colIdxMap v []
// _ -> do
//   if lookupVecTextByKey reqVec colIdxMap "request_type" == Just "Incoming"
//     then renderIconWithTippy "text-slate-500" "Incoming Request" (faSprite_ "arrow-down-left" "solid" "h-3")
//     else renderIconWithTippy "text-blue-700" "Outgoing Request" (faSprite_ "arrow-up-right" "solid" "h-3")
//   logItemCol_ source pid reqVec colIdxMap "status_code" []
//   logItemCol_ source pid reqVec colIdxMap "method" []
//   renderLogBadge "url_path" reqVec colIdxMap "cbadge-sm badge-neutral border  border-strokeWeak  bg-fillWeak"
//   logItemCol_ source pid reqVec colIdxMap "duration" []
//   renderLogBadge "host" reqVec colIdxMap "cbadge-sm badge-neutral border  border-strokeWeak  bg-fillWeak"
//   span_ $ toHtml $ maybe "" (unwrapJsonPrimValue True) (lookupVecByKey reqVec colIdxMap key)

function logItemRows(dataArr, colIdxMap, columns, source, childSpans) {
  return dataArr.map(
    (arr) => html`
      <tr class="log-row cursor-pointer whitespace-nowrap overflow-hidden" _="on click toggle .hidden on next <tr/> then toggle .expanded-log on me">
        ${columns.map((column) => html`<td class="pl-3">${logItemCol(arr, source, colIdxMap, column, childSpans)}</td>`)}
      </tr>
      <tr class="hidden">
        <td class="pl-4"><a class="inline-block h-full" data-tippy-content=${'0 errors attached to this request'}></a></td>
        <td colspan=${columns.length - 1}><div class="loading loading-dots loading-md"></div></td>
      </tr>
    `
  )
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

function getKindColor(kind) {
  return (
    {
      INTERNAL: 'badge-info',
      SERVER: 'badge-success',
      CLIENT: 'badge-warning',
      PRODUCER: 'badge-success',
      CONSUMER: 'badge-warning',
    }[kind] || 'badge-outline'
  )
}
