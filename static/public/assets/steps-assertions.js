import { html } from './js/thirdparty/lit.js'
import jsonpath from './js/thirdparty/jsonpath.js'
// Base Operations List
const baseOperations = [
  { value: 'equals', label: 'equals' },
  { value: 'notEquals', label: 'does not equal' },
  { value: 'greaterThan', label: 'is greater than' },
  { value: 'lessThan', label: 'is less than' },
  { value: 'greaterThanOrEqual', label: 'is greater than' },
  { value: 'lessThanOrEqual', label: 'is less than' },
  { value: 'contains', label: 'contains' },
  { value: 'notContains', label: 'does not contain' },
  { value: 'null', label: 'is null' },
  { value: 'exists', label: 'exists' },
  { value: 'empty', label: 'is empty' },
  { value: 'notEmpty', label: 'is not empty' },
  { value: 'boolean', label: 'is boolean' },
  { value: 'number', label: 'is number' },
  { value: 'string', label: 'is string' },
  { value: 'array', label: 'is array' },
  { value: 'date', label: 'is date' },
]

// Additional Operations for Body Type
const bodyAdditionalOperations = [
  { value: 'jsonpath', label: 'jsonpath' },
  // { value: 'jsonschema', label: 'jsonschema' },
  // { value: 'jsonschema', label: 'jsonschema' },
]

const renderInput = (type, value, onChange, placeholder, className) => html`
  <input type=${type} placeholder=${placeholder} class="input input-sm input-bordered grow shadow-none ${className}" .value=${value} @input=${onChange} />
`

const renderTextarea = (value, onChange, placeholder) => html`
  <textarea placeholder=${placeholder} class="textarea textarea-sm textarea-bordered h-24 grow shadow-none" .value=${value} @input=${onChange}></textarea>
`

const renderDropdown = (options, value, onChange, className) => html`
  <select class="select select-sm select-bordered max-w-xs shadow-none ${className}" .value=${value} @change=${onChange}>
    ${options.map((option) => html` <option value=${option.value} ?selected=${option.value === value}>${option.label}</option> `)}
  </select>
`

// Main renderInputs function
function renderInputs(assertion, index, updateAssertion) {
  const update = (field) => (e) => updateAssertion(index, { [field]: e.target.value })
  const inputs = []
  if (assertion.type === 'responseTime') {
    inputs.push(
      renderDropdown(
        [
          { value: 'includeDNS', label: 'Including DNS' },
          { value: 'excludeDNS', label: 'Excluding DNS' },
        ],
        assertion.includeDNS ? 'includeDNS' : 'excludeDNS',
        (e) => updateAssertion(index, { includeDNS: e.target.value === 'includeDNS' }),
        'select select-bordered w-48'
      )
    )
  }

  if (assertion.operation === 'jsonpath') {
    inputs.push(renderInput('text', assertion.jsonpath || '', update('jsonpath'), 'JSON path'), renderDropdown(baseOperations, assertion.subOperation, update('subOperation')))
    if (assertion.subOperation !== 'isUndefined') {
      inputs.push(renderInput('text', assertion.value || '', update('value'), 'Value'))
    }
  } else if (assertion.operation === 'jsonschema') {
    inputs.push(renderTextarea(assertion.value || '', update('value'), 'JSON Schema'))
  } else if (assertion.operation !== 'isUndefined') {
    const inputConfig = {
      header: { type: 'text', placeholder: 'Header Value', className: 'w-5' },
      statusCode: { type: 'number', placeholder: 'Status Code', className: 'w-5' },
      responseTime: { type: 'number', placeholder: 'Time (ms)', className: 'w-32' },
      default: { type: 'text', placeholder: 'Value', className: 'w-64' },
    }

    const config = inputConfig[assertion.type] || inputConfig.default

    inputs.push(
      assertion.type == 'body'
        ? renderTextarea(assertion.value || '', update('value', config.placeholder), 'Body content')
        : renderInput(config.type, assertion.value || '', update('value'), config.placeholder, config.className)
    )
  }

  return html`<div class="${assertion.type == 'body' ? 'basis-full' : ''} grow inline-flex gap-2">${inputs}</div>`
}

// Generic function to render assertion content
function renderAssertionContent(assertion, index, updateAssertion) {
  const isBodyType = assertion.type === 'body'

  // Determine operations based on type
  const operationOptions = isBodyType ? [...baseOperations, ...bodyAdditionalOperations] : baseOperations
  const inputs = []

  if (assertion.type === 'header') {
    inputs.push(renderInput('text', assertion.headerName || '', (e) => updateAssertion(index, { headerName: e.target.value }), 'Header name', ''))
  }

  inputs.push(renderDropdown(operationOptions, assertion.operation, (e) => updateAssertion(index, { operation: e.target.value })))

  // Additional Inputs based on Operation
  inputs.push(renderInputs(assertion, index, updateAssertion))

  return html`${inputs}`
}

// Placeholder for assertion evaluation logic
function evaluateAssertion(assertion, result, response) {
  try {
    if (response) {
      if (assertion.type === 'statusCode') {
        const operation = getOperationFromText(assertion.operation)
        const expression = `${response.status} ${operation} ${assertion.value}`
        return eval(expression)
      }
      if (assertion.type === 'header') {
        const operation = getOperationFromText(assertion.operation)
        const expression = `"${response.headers[assertion.headerName]}" ${operation} "${assertion.value}"`
        return eval(expression)
      }
      if (assertion.type === 'body') {
        if (assertion.operation === 'jsonpath') {
          const operation = getOperationFromText(assertion.subOperation)

          const val = jsonpath.query(response.json, assertion.jsonpath)
          for (let v of val) {
            const expression = `${v} ${operation} ${assertion.value}`
            if (eval(expression)) {
              return true
            }
          }
          return false
        } else {
          const operation = getOperationFromText(assertion.operation)
          return eval(`${response.raw} ${operation} ${assertion.value}`)
        }
      }
      if (assertion.type === 'responseTime') {
        const operation = getOperationFromText(assertion.operation)
        return eval(`${response.duration_ms} ${operation} ${assertion.value}`)
      }
    }

    if (result && result.ok && result.ok === true) {
      return true
    }
    return assertion.status === 'PASSED'
  } catch (e) {
    return false
  }
}

// Main Render Function
export function renderAssertionBuilder({
  assertions,
  result,
  updateAssertion, // Single update function
  addAssertion,
  removeAssertion,
}) {
  return html`
    <div class="">
      ${assertions.map((assertion, index) => {
        let aResult = undefined
        if (result && result.assert_results) {
          aResult = result.assert_results[index]
        }
        const passed = evaluateAssertion(assertion, aResult, result ? result.resp : undefined)

        let error = passed ? '' : aResult ? aResult.err?.advice : ''
        return html`
          <div class="pt-2">
            <div class="flex gap-2 items-center">
              <span class="block w-12 shrink-0 pt-2">${index == 0 ? 'When' : 'And'}</span>
              <div class="flex flex-wrap items-center gap-2 grow ">
                ${renderDropdown(
                  [
                    { value: 'body', label: 'body' },
                    { value: 'header', label: 'header' },
                    { value: 'statusCode', label: 'status code' },
                    { value: 'responseTime', label: 'response time' },
                  ],
                  assertion.type,
                  (e) => updateAssertion(index, { type: e.target.value, operation: undefined, jsonpath: undefined, subOperation: undefined, value: undefined, status: undefined }),
                  ''
                )}
                ${renderAssertionContent(assertion, index, updateAssertion)}
              </div>
              <div class="flex-shrink-0">
                <div class="flex gap-3 pt-2 items-center">
                  ${passed ? html`<span class="badge badge-success">Passed</span>` : html`<span class="badge badge-error">Failed</span>`}
                  <a class="cursor-pointer text-slate-600" @click=${removeAssertion(index)}> ${faSprite_('trash', 'regular', 'rounded-full border bg-white shadow p-1 w-5 h-5 stroke-red-500')} </a>
                </div>
              </div>
            </div>
            <p class="text-sm text-red-500 text-center">${error}</p>
          </div>
        `
      })}
      <button class="flex items-center gap-1 mt-4 text-sm" @click=${addAssertion}>${faSprite_(
    'plus',
    'regular',
    'w-4 h-4  text-textWeak'
  )} <span class="underline  text-textWeak font-semibold">New assertion<span></button>
    </div>
  `
}

function faSprite_(iconName, kind, classes) {
  return html`<svg class="${classes}"><use href="/public/assets/svgs/fa-sprites/${kind}.svg#${iconName}"></use></svg>`
}
