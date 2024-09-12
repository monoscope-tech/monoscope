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
    val = val.length === 0 ? 'status_code > 399' : val + ' AND status_code > 399'
  }
  console.log(val)
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
