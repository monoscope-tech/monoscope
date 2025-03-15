htmx.defineExtension('debug', {
  onEvent: function (name, evt) {
    if (console.debug) {
      console.debug(name, evt)
    } else if (console) {
      console.log('DEBUG:', name, evt)
    } else {
      throw new Error('NO CONSOLE SUPPORTED')
    }
  },
})

// buildCurlRequest converts a log explorer result item into a curl and copies the curl to clipboard.
window.buildCurlRequest = function (event) {
  const { request_headers, request_body, method, host, raw_url } = JSON.parse(event.currentTarget.dataset.reqjson)
  let curlCommand = `curl -X ${method} https://${host}${raw_url} \\\n `

  const curlHeaders =
    typeof request_headers === 'object'
      ? Object.entries(request_headers)
          .map(([key, value]) => `-H "${key} ${value}" \\\n`)
          .join('')
      : ''
  curlCommand += curlHeaders

  const reqBody =
    method.toLowerCase() !== 'get'
      ? typeof request_body === 'object'
        ? ` -d '${JSON.stringify(request_body)}' \\\n`
        : `-data-raw "${request_body}"  \\\n`
      : ''
  if (reqBody) curlCommand += reqBody

  navigator.clipboard.writeText(curlCommand).then(() => {
    document.querySelector('body').dispatchEvent(
      new CustomEvent('successToast', {
        detail: { value: ['Curl command copied'] },
        bubbles: true,
        composed: true,
      }),
    )
  })
}

window.setQueryParamAndReload = (key, value) => {
  const url = new URL(window.location.href)
  url.searchParams.set(key, value)
  if (key === 'source') {
    url.searchParams.delete('queryAST')
    url.searchParams.delete('target-spans')
  }
  window.location.href = url.toString()
}

// TODO: implement correctly. errors doesnt work
window.getQueryFromEditor = target => {
  const toggler = document.getElementById('toggleQueryEditor')
  const queryAST = document.getElementById('filterElement').ast
  let val = ''
  if (toggler.checked) {
    val = window.editor.getValue()
  } else {
    // val = window.queryBuilderValue || ''
    return JSON.stringify(queryAST)
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
  container.querySelectorAll('script').forEach(oldScript => {
    const newScript = document.createElement('script')
    newScript.text = oldScript.textContent || oldScript.innerHTML

    // Copy attributes using the spread operator
    ;[...oldScript.attributes].forEach(attr => newScript.setAttribute(attr.name, attr.value))

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

var toggleColumnToSummary = e => {
  const cols = (params().cols ?? '').split(',').filter(x => x != '')
  const subject = e.target.closest('.log-item-field-parent').dataset.fieldPath
  if (cols.includes(subject)) {
    return [...new Set(cols.filter(x => x != subject))].join(',')
  }
  cols.push(subject)
  return [...new Set(cols)].join(',')
}
var removeNamedColumnToSummary = namedCol => {
  const cols = (params().cols ?? '').split(',').filter(x => x != '')
  const subject = namedCol

  return [...new Set(cols.filter(x => subject.toLowerCase() != x.replaceAll('.', '•').replaceAll('[', '❲').replaceAll(']', '❳').toLowerCase()))].join(',')
}

// Example usage
// setParams({ param1: 'newValue1' }, true); // Will trigger a full page load with new state
// setParams({ param2: 'newValue2' });       // Will update URL without reloading
window.setParams = (
  (state = { ...Object.fromEntries(new URLSearchParams(window.location.search)) }) =>
  (newState, load = false) => {
    Object.assign(state, newState)

    const url =
      '?' +
      new URLSearchParams(
        Object.entries(state)
          .filter(([_key, value]) => value != null)
          .sort(([keyA], [keyB]) => keyA.localeCompare(keyB)),
      ).toString()

    load ? window.location.assign(url) : history.replaceState(null, '', url)
  }
)()

function updateMarkAreas(chartId, warningVal, incidentVal) {
  const warning = parseInt(warningVal, 10),
    incident = parseInt(incidentVal, 10),
    myChart = echarts.getInstanceByDom(document.getElementById(chartId)),
    options = myChart.getOption()

  options.series.forEach(series => {
    series.markArea = {
      label: { show: false },
      data: [
        ...(!isNaN(warning)
          ? [
              [
                {
                  name: 'Warning',
                  yAxis: warning,
                  itemStyle: { color: 'rgba(255, 212, 0, 0.4)' },
                },
                { yAxis: incident },
              ],
            ]
          : []),
        [
          {
            name: 'Incident',
            yAxis: incident,
            itemStyle: { color: 'rgba(255, 173, 177, 0.5)' },
          },
          { yAxis: 'max' },
        ],
      ],
    }
  })
  myChart.setOption({ series: options.series }, false)
}
