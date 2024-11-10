// buildCurlRequest converts a log explorer result item into a curl and copies the curl to clipboard.
window.buildCurlRequest = function buildCurlRequest(event) {
  const data = JSON.parse(event.currentTarget.dataset.reqjson)
  const request_headers = data.request_headers
  const request_body = data.request_body
  const url = 'https://' + data.host + data.raw_url + ' \\\n'
  const method = data.method
  let curlCommand = 'curl -X ' + method + ' ' + url + ' '
  let curlHeaders = ''
  if (typeof request_headers === 'object') {
    try {
      curlHeaders = Object.entries(request_headers)
        .map(([key, value]) => '-H "' + key + ' ' + value + '" \\\n')
        .join('')
    } catch (error) {}
  }
  if (curlHeaders != '') curlCommand += curlHeaders
  let reqBody = ''
  if (method.toLowerCase() != 'get') {
    try {
      reqBody = " -d '" + JSON.stringify(request_body) + "' \\\n"
    } catch (error) {
      // none json request body
      reqBody = '-data-raw ' + '"' + request_body + '"  \\\n'
    }
  }
  if (reqBody !== " -d ''") {
    curlCommand += reqBody
  }
  navigator.clipboard.writeText(curlCommand).then(() => {
    const event = new CustomEvent('successToast', {
      detail: { value: ['Curl command copied'] },
      bubbles: true,
      composed: true,
    })
    document.querySelector('body').dispatchEvent(event)
  })
}

window.setQueryParamAndReload = (key, value) => {
  const url = new URL(window.location.href)
  url.searchParams.set(key, value)
  url.searchParams.delete('query')
  url.searchParams.delete('target-spans')
  window.location.href = url.toString()
}

window.getQueryFromEditor = (target) => {
  const toggler = document.getElementById('toggleQueryEditor')
  let val = ''
  if (toggler.checked) {
    val = window.editor.getValue()
  } else {
    val = window.queryBuilderValue || ''
  }
  if (target === 'errors') {
    let source = document.querySelector('#reqsChartsErrP').dataset.source
    let srcErrs = source === 'logs' ? `severityText == "ERROR" OR severityText == "FATAL"` : source === 'spans' ? `status == "ERROR"` : 'status_code > 399'
    val = val.length === 0 ? srcErrs : `${val} AND ${srcErrs}`
  }
  return val
}

window.downloadJson = function (event) {
  event.stopPropagation()
  const json = event.currentTarget.dataset.reqjson
  var blob = new Blob([json], { type: 'application/json' })
  var a = document.createElement('a')
  a.href = URL.createObjectURL(blob)
  a.download = 'request-data-' + new Date().toString() + '.json'
  a.textContent = ''
  document.body.appendChild(a)
  a.click()
  document.body.removeChild(a)
}

window.evalScriptsFromContent = function (container) {
  container.querySelectorAll('script').forEach((oldScript) => {
    const newScript = document.createElement('script')
    newScript.text = oldScript.textContent || oldScript.innerHTML

    // Copy attributes using the spread operator
    ;[...oldScript.attributes].forEach((attr) => newScript.setAttribute(attr.name, attr.value))

    // Append and remove to execute
    document.body.append(newScript)
    newScript.remove()
  })
}

var params = () =>
  new Proxy(new URLSearchParams(window.location.search), {
    get: (searchParams, prop) => searchParams.get(prop) ?? '',
  })
window.params = params

var getTimeRange = function () {
  const rangeInput = document.getElementById('custom_range_input')
  const range = rangeInput.value.split('/')
  if (range.length == 2) {
    return { from: range[0], to: range[1], since: '' }
  }
  if (range[0] != '') {
    return { since: range[0], from: '', to: '' }
  }
  if (params().since == '') {
    return { since: '14D', from: params().from, to: params().to }
  }
  return { since: params().since, from: params().from, to: params().to }
}
window.getTimeRange = getTimeRange

var toggleColumnToSummary = (e) => {
  const cols = (params().cols ?? '').split(',').filter((x) => x != '')
  const subject = e.target.closest('.log-item-field-parent').dataset.fieldPath
  if (cols.includes(subject)) {
    return [...new Set(cols.filter((x) => x != subject))].join(',')
  }
  cols.push(subject)
  return [...new Set(cols)].join(',')
}

var removeNamedColumnToSummary = (namedCol) => {
  const cols = (params().cols ?? '').split(',').filter((x) => x != '')
  const subject = namedCol

  cols.forEach((x) => console.log(subject, x.replaceAll('.', '•').replaceAll('[', '❲').replaceAll(']', '❳')))

  return [...new Set(cols.filter((x) => subject.toLowerCase() != x.replaceAll('.', '•').replaceAll('[', '❲').replaceAll(']', '❳').toLowerCase()))].join(',')
}

// Unified Timepicker
//
window.picker = new easepick.create({
  element: '#startTime',
  css: ['https://cdn.jsdelivr.net/npm/@easepick/bundle@1.2.0/dist/index.css'],
  inline: true,
  plugins: ['RangePlugin', 'TimePlugin'],
  autoApply: false,
  setup(picker) {
    picker.on('select', (e) => {
      const start = JSON.stringify(e.detail.start).slice(1, -1)
      const end = JSON.stringify(e.detail.end).slice(1, -1)
      const rangeInput = document.getElementById('custom_range_input')
      rangeInput.value = start + '/' + end
      document.getElementById('timepickerBox').classList.toggle('hidden')
      document.getElementById('currentRange').innerText = start.split('T')[0] + ' - ' + end.split('T')[0]
      htmx.trigger('#log_explorer_form', 'submit')
    })
  },
})
function navigateSpans(spans, direction) {
  const container = document.querySelector('#currentSpanIndex')
  const currentSpan = Number(container.dataset.span)
  if (direction == 'next' && currentSpan >= spans.length) {
    return
  }
  if (direction == 'prev' && currentSpan <= 0) {
    return
  }
  const spandInd = direction == 'next' ? currentSpan + 1 : currentSpan - 1
  const span = spans[spandInd]
  container.dataset.span = spandInd
  htmx.trigger('#trigger-span-' + span, 'click')
}
