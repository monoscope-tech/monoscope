import { html } from './js/thirdparty/lit.js'

export async function makeRequestAndProcessResponse(requestObject) {
  try {
    // Extract necessary information from the requestObject
    const url = replaceVariables(requestObject._url)
    const method = requestObject._method
    const headers = requestObject.headers || {}
    const rawBody = requestObject._requestBody ? requestObject._requestBody : requestObject._json || null

    // Prepare the fetch options
    const options = {
      method: method,
      headers: { ...headers, contentType: requestObject._requestType },
      redirect: requestObject.followRedirects ? 'follow' : 'manual',
    }
    if (method !== 'GET') {
      options.body = rawBody
    }

    // Manually add headers typically injected by fetch (for display purposes only)
    const injectedHeaders = {
      'user-agent': navigator.userAgent || 'CustomAgent/1.0', // Example value
      accept: '*/*',
      connection: 'keep-alive',
      host: url.host,
    }

    const startTime = performance.now()
    // Make the fetch request
    const response = await fetch(url, options)
    const endTime = performance.now()
    const duration_ms = endTime - startTime

    // Get the status, headers, and raw text from the response
    const status = response.status
    const responseHeaders = {}
    response.headers.forEach((value, key) => {
      responseHeaders[key] = value
    })
    const rawResponse = await response.text()

    let jsonResponse = {}
    try {
      // Try to parse the response as JSON
      jsonResponse = JSON.parse(rawResponse)
    } catch (error) {
      // If parsing fails, leave jsonResponse empty
    }

    // Construct the result object
    return {
      resp: {
        status: status,
        duration_ms: duration_ms.toFixed(2),
        headers: responseHeaders,
        json: jsonResponse,
        raw: rawResponse,
      },
      req: {
        http_method: method,
        headers: headers,
        json: rawBody ? JSON.parse(rawBody) : null,
        raw: rawBody,
      },
    }
  } catch (error) {
    // Handle errors and return a meaningful error object
    console.log(error)
    return {
      error: `Request failed: ${error.message}`,
    }
  }
}

export function generateRequestPreviewFromObject(requestObject) {
  const url = new URL(replaceVariables(requestObject._url))
  const method = requestObject._requestType === 'raw' ? 'POST' : 'GET' // Default to GET if not raw
  const userHeaders = requestObject.headers || {}
  const httpVersion = requestObject.httpVersion || 'HTTP/1.1' // Default to HTTP/1.1 if not provided

  // Build the request preview with method, URL, and HTTP version
  let requestPreview = `<p>${method.toUpperCase()} ${url.href} ${httpVersion}<p/>`

  // Add HTTP/2 pseudo-headers if HTTP/2.0 is used
  let headersPreview = ''
  if (httpVersion === 'HTTP/2.0') {
    headersPreview += `<p>:authority: ${url.host}<p/>`
    headersPreview += `<p>:method: ${method.toUpperCase()}<p/>`
    headersPreview += `<p>:path: ${url.pathname}<p/>`
  }

  // Simulate browser-injected headers
  const injectedHeaders = {
    'user-agent': navigator.userAgent || 'CustomAgent/1.0', // Use navigator if available
    accept: '*/*',
    connection: 'keep-alive',
    host: url.host,
  }

  // Combine user-defined headers and injected headers
  const allHeaders = { ...injectedHeaders, ...userHeaders }

  // Add combined headers to the preview
  Object.entries(allHeaders).forEach(([key, value]) => {
    headersPreview += `<p>${key.toLowerCase()}: ${value}<p/>`
  })

  // Full request preview string
  return requestPreview + headersPreview
}

// Recursive function to render JSON with padding
export function renderJsonWithIndentation(json, addAssertion, path = '', depth = 0) {
  const padding = `${depth * 20}px`
  return html`
    ${Object.entries(json).map(([key, value]) => {
      const currentPath = Array.isArray(json) ? `${path}.[${key}]` : `${path}.${key}`
      const assertionObj = {
        type: 'body',
        operation: 'jsonpath',
        jsonpath: currentPath,
        subOperation: 'equals',
        value: typeof value !== 'object' ? value : '',
        status: 'PASSED',
      }
      return html`
        <div style="padding-left: ${padding};">
          <span class="hover:bg-gray-200 cursor-pointer" @click="${(e) => addAssertion(e, assertionObj)}"> ${key}: ${typeof value === 'object' && value ? '' : JSON.stringify(value)} </span><br />
          ${typeof value === 'object' && value ? renderJsonWithIndentation(value, addAssertion, currentPath, depth + 1) : ''}
        </div>
      `
    })}
  `
}
