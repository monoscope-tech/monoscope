import { LitElement, html, ref, createRef } from 'https://cdn.jsdelivr.net/gh/lit/dist@3/all/lit-all.min.js'

function getFieldAndValue(filter) {
  const parts = filter.trim().split(/\s*([=<>!]+)\s*/)
  if (parts.length !== 3) {
    return []
  }
  const [field, operator, value] = parts
  return [field, operator, value]
}
export class MyElement extends LitElement {
  static properties = {
    filters: {},
    showFilterSearch: {},
  }

  constructor() {
    super()
    this.showFilterSearch = false
    this.addEventListener('add-filter', this.handleChildEvent)
    this.addEventListener('remove-filter', (e) => {
      this.removeFilter(e.detail.filter)
    })
    this.addEventListener('close-search', () => {
      this.showFilterSearch = false
    })
    const body = document.querySelector('body')
    this.filters = []
    window.setQueryBuilderFromParams = () => {
      const urlSearchParams = new URLSearchParams(window.location.search)
      const query = urlSearchParams.get('query')
      if (query) {
        this.setBuilderValue(query)
      }
    }

    body.addEventListener('click', () => {
      this.showFilterSearch = false
    })
  }

  setBuilderValue = (value) => {
    const joiners = value.split(' ').filter((v) => v.toUpperCase() === 'AND' || v.toUpperCase() == 'OR')
    let current = 0
    const fls = value.split(/\s+AND\s+|\s+OR\s+/i).flatMap((element, index, array) => {
      return index < array.length - 1 ? [element, (joiners[current++] || 'AND').toUpperCase()] : [element]
    })
    this.upadteFilters(fls)
  }

  handleChildEvent(event) {
    let joiner = 'AND'
    const [newField, newOperator, value] = getFieldAndValue(event.detail.filter)
    for (let filter of this.filters) {
      const [field, operator, val] = getFieldAndValue(filter)
      if (newField == field) {
        joiner = 'OR'
      }
      if (newField === field && newOperator === operator && value === val) {
        this.showFilterSearch = false
        return
      }
    }
    if (event.detail.pos) {
      this.filters[event.detail.pos] = event.detail.filter
      this.upadteFilters([...this.filters])
    } else {
      if (this.filters.length === 0) {
        this.upadteFilters([event.detail.filter])
      } else {
        this.upadteFilters([...this.filters, joiner, event.detail.filter])
      }
    }
    this.showFilterSearch = false
  }

  upadteFilters(newVal) {
    this.filters = newVal.filter((v) => v !== '')
    const val = newVal.join(' ')
    window.queryBuilderValue = val
    if (window.editor) {
      window.editor.setValue(val)
    }
  }

  removeFilter(filter) {
    const index = this.filters.indexOf(filter)
    if (index > 0) {
      this.filters[index - 1] = filter
    } else {
      if (this.filters.length > 0) {
        this.filters[index + 1] = filter
      }
    }
    this.upadteFilters(this.filters.filter((f) => f != filter))
  }

  isValidFilter(filter) {
    const parts = filter.trim().split(/\s*([=<>!]+)\s*/)
    if (parts.length !== 3) {
      return false
    }
    const [field, operator, value] = parts
    if (!field || !value) {
      return false
    }
    return true
  }

  toggleJoinOperator(index) {
    if (this.filters[index] === 'AND') {
      this.filters[index] = 'OR'
    } else {
      this.filters[index] = 'AND'
    }
    this.upadteFilters([...this.filters])
  }

  render() {
    return html`
      <div class="relative w-full" @click=${(e) => e.stopPropagation()}>
        <div class="flex items-center flex-wrap gap-2 border border-1 border-slate-400 px-4 py-2 w-ful rounded-lg">
          <i class="fa-regular fa-filter h-4 w-4 text-gray-500"></i>
          <div class="flex flex-wrap gap-2">
            ${this.filters.map((filter, index) => {
              return html`
                ${filter === 'AND' || filter === 'OR'
                  ? html`<button type="button" @click=${() => this.toggleJoinOperator(index)} class="text-gray-500 bg-gray-100 text-xs  px-2 py-1 rounded-full">
                      ${filter.toLowerCase()}
                      <i class="fa-solid fa-sliders-simple"></i>
                    </button>`
                  : html`<filter-item filter=${filter} pos=${index}></filter-item>`}
              `
            })}
            ${this.showFilterSearch ? html`<filter-suggestions></filter-suggestions>` : null}
            ${this.filters.length == 0
              ? html`<button type="button" @click=${() => (this.showFilterSearch = !this.showFilterSearch)} class="text-gray-500">Click to add filter...</button>`
              : html`<button type="button" @click=${() => (this.showFilterSearch = !this.showFilterSearch)} class="px-2 py-1 border rounded text-gray-500 hover:bg-gray-100">
                  <i class="fa-solid fa-plus"></i>
                </button>`}
          </div>
        </div>
      </div>
    `
  }
  createRenderRoot() {
    return this
  }
}

customElements.define('filter-element', MyElement)

class FilterItem extends LitElement {
  static properties = {
    filter: {},
    pos: {},
  }
  constructor() {
    super()
    this.showFieldModal = false
    this.showOperatoinModal = false
    this.showValueModal = false
    this.fields = FIELDS
    this.operators = []
    this.values = []
    this.fieldType = 'string'

    const body = document.querySelector('body')
    body.addEventListener('click', () => {
      this.showFieldModal = false
      this.showOperatoinModal = false
      this.showValueModal = false
      this.requestUpdate()
    })
  }

  triggerFilterChange(field, op, value) {
    this.showFieldModal = false
    this.showOperatoinModal = false
    this.showValueModal = false
    let pureVal = this.fieldType === 'string' && !value.startsWith('"') && !value.endsWith('"') ? `"${value}"` : value
    const filter = `${field} ${op} ${pureVal}`
    const event = new CustomEvent('add-filter', {
      detail: {
        filter,
        pos: this.pos,
      },
      bubbles: true,
      composed: true,
    })
    this.dispatchEvent(event)
  }
  removeFilter() {
    const event = new CustomEvent('remove-filter', {
      detail: {
        filter: this.filter,
        pos: this.pos,
      },
      bubbles: true,
      composed: true,
    })
    this.dispatchEvent(event)
  }

  render() {
    const [field, operator, value] = getFieldAndValue(this.filter)
    this.fieldType = filterAutoComplete[field]?.type || 'string'
    this.operators = filterAutoComplete[field]?.operators || ['==', '!=']
    this.values = filterAutoComplete[field]?.values || []

    return html`
    <div class="border flex font-medium shrink-0 text-sm text-blue-500 rounded">
    <div type="button"  class="cursor-pointer relative py-1 px-2 hover:bg-blue-50" @click=${() => {
      this.showFieldModal = !this.showFieldModal
      this.showOperatoinModal = false
      this.showValueModal = false
      this.requestUpdate()
    }}>
    <span>${field}</span>
    ${
      this.showFieldModal
        ? html`<div style="heigh" class="absolute z-50 bg-white border rounded w-56 text-gray-500 font-normal top-10 left-0" @click=${(e) => e.stopPropagation()}>
            <input
              type="text"
              class="w-full border-b text-sm outline-none focus:outline-none px-2 py-1"
              placeholder="Search field"
              @input=${(e) => {
                this.fields = this.fields.filter((field) => field.toLowerCase().includes(e.target.value.toLowerCase()))
                if (e.target.value.length == 0) {
                  this.fields = FIELDS
                }
                this.requestUpdate()
              }}
              @keydown=${(e) => {
                if (e.key === 'Enter') {
                  this.triggerFilterChange(e.target.value, operator, value)
                }
              }}
            />
            <div class="max-h-48 overflow-y-auto">
              <div class="flex flex-col">
                ${this.fields.map((field) => {
                  return html`<button type="button" class="flex items-center gap-2 px-2 py-1 hover:bg-gray-100" @click=${(e) => this.triggerFilterChange(field, operator, value)}>${field}</button>`
                })}
              </div>
            </div>
          </div>`
        : null
    }
    </div>
    <div type="button"  class="cursor-pointer relative border-l border-r px-2 py-1 hover:bg-blue-50" @click=${() => {
      this.showFieldModal = false
      this.showOperatoinModal = !this.showOperatoinModal
      this.showValueModal = false
      this.requestUpdate()
    }}>
    <span>${operator}<span>
    ${
      this.showOperatoinModal
        ? html`<div style="heigh" class="absolute z-50 bg-white border rounded w-56 text-gray-500 font-normal top-10 left-0" @click=${(e) => e.stopPropagation()}>
            <div class="max-h-48 overflow-y-auto">
              <div class="flex flex-col">
                ${this.operators.map((op) => {
                  return html`<button type="button" class="flex items-center gap-2 px-2 py-1 hover:bg-gray-100" @click=${(e) => this.triggerFilterChange(field, op, value)}>${op}</button>`
                })}
              </div>
            </div>
          </div>`
        : null
    }
    </div>
    <div type="button"  class="cursor-pointer relative py-1 px-2 border-r hover:bg-blue-50" @click=${() => {
      this.showFieldModal = false
      this.showOperatoinModal = false
      this.showValueModal = !this.showValueModal
      this.requestUpdate()
    }}>
    <span>${value}<span>
    ${
      this.showValueModal
        ? html`<div style="heigh" class="absolute z-50 bg-white border rounded w-56 text-gray-500 font-normal top-10 left-0" @click=${(e) => e.stopPropagation()}>
            <input
              type="text"
              class="w-full border-b text-sm outline-none focus:outline-none px-2 py-1"
              placeholder="Search field"
              @input=${(e) => {
                this.fields = this.fields.filter((field) => field.toLowerCase().includes(e.target.value.toLowerCase()))
                if (e.target.value.length == 0) {
                  this.fields = FIELDS
                }
                this.requestUpdate()
              }}
              @keydown=${(e) => {
                if (e.key === 'Enter') {
                  this.triggerFilterChange(field, operator, e.target.value)
                }
              }}
            />
            <div class="max-h-48 overflow-y-auto">
              <div class="flex flex-col">
                ${this.values.map((v) => {
                  return html`<button type="button" class="flex items-center gap-2 px-2 py-1 hover:bg-gray-100" @click=${(e) => this.triggerFilterChange(field, operator, v)}>${v}</button>`
                })}
              </div>
            </div>
          </div>`
        : null
    }
    </div>
    ${html`<span class="px-2 hover:bg-blue-50 py-1" @click=${() => this.removeFilter(this.pos)}>
      <i class="fa-sharp fa-xmark"></i>
    </span>`}
  </div>`
  }
  createRenderRoot() {
    return this
  }
}
customElements.define('filter-item', FilterItem)

const FIELDS = [
  'method',
  'status_code',
  'url_path',
  'duration_ns',
  'request_body',
  'has_errors',
  'request_header',
  'response_body',
  'response_header',
  'host',
  'raw_url',
  'referer',
  'query_param',
  'path_param',
  'request_type',
  'service_version',
]

const string_operators = ['==', '!=']
const number_operators = ['==', '>', '<', '!=', '>=', '<=']

const filterAutoComplete = {
  method: {
    type: 'string',
    operators: string_operators,
    values: ['GET', 'POST', 'PUT', 'PATCH', 'DELETE', 'OPTIONS', 'HEAD', 'CONNECT', 'TRACE'],
  },
  status_code: {
    type: 'number',
    operators: number_operators,
    values: [200, 201, 202, 203, 204, 300, 301, 302, 400, 401, 402, 403, 404, 405, 406, 500, 501, 502, 503, 504],
  },
  request_type: {
    type: 'string',
    operators: string_operators,
    values: ['Outgoing', 'Incoming'],
  },
  duration_ns: {
    operators: number_operators,
    type: 'number',
    values: [],
  },
  has_errors: {
    operators: ['=='],
    type: 'boolean',
    values: ['true', 'false'],
  },
}

class Filter extends LitElement {
  static properties = {
    hasQuery: { type: Boolean },
  }

  static properties = {
    matches: {},
    inputVal: {},
    fetchAutocomplete: {},
  }

  constructor() {
    super()
    this.inputVal = ''
    this.fetchAutocomplete = false
    this.matches = FIELDS
    this.projectId = window.location.pathname.split('/')[2]
    const builderContainer = document.getElementById('queryBuilder')
    if (builderContainer) {
      // const url_paths = JSON.parse(builderContainer.dataset.url_paths)
      // this.filterAutoComplete.url_path.values = (url_paths || []).sort()
      // this.filterAutoComplete.raw_url.values = (JSON.parse(builderContainer.dataset.raw_urls) || []).sort()
    }
  }

  inputRef = createRef()
  boxClassName = `flex absolute overflow-hidden -bottom-full flex-col border rounded text-left bg-white w-96 ${this.hasQuery ? 'left-1/2 -translate-x-1/2' : 'left-0'}`
  render() {
    return html`
      <div class="relative text-gray-500">
        <div class="h-8"></div>
        <div style="z-index:99" class="${this.boxClassName}">
          <input
            type="text"
            placeholder="Type query..."
            autofocus
            ${ref(this.inputRef)}
            class="border-b px-2 w-full py-1 outline-none focus:outline-0"
            @input=${(event) => {
              this.handleChange(event.target.value)
              this.adjustInputWidthAndFocus()
            }}
            .value=${this.inputVal}
            @keydown=${(e) => {
              if (e.key === 'Enter') {
                this.triggerCustomEvent(e.target.value)
              }
            }}
          />
          <div class="flex flex-col gap-2 w-full overflow-auto" style="max-height:250px;">
            ${this.matches.map(
              (match) => html`
                <button
                  type="button"
                  class="match_buttons px-2 py-1 text-sm text-left hover:bg-gray-100"
                  @click=${(e) => {
                    this.autoCompleteInput(match)
                    this.adjustInputWidthAndFocus()
                  }}
                  @keydown=${(e) => {
                    if (e.key === 'Enter') {
                      this.autoCompleteInput(match)
                      this.adjustInputWidthAndFocus()
                    }
                    if (e.key === 'Tab') {
                      e.preventDefault()
                      const buttons = document.querySelectorAll('.match_buttons')
                      const activeButton = document.activeElement
                      const index = Array.from(buttons).indexOf(activeButton)

                      if (index !== -1) {
                        const nextIndex = (index + 1) % buttons.length
                        buttons[nextIndex].focus()
                      }
                    }
                  }}
                >
                  ${match}
                </button>
              `
            )}
          </div>
        </div>
      </div>
    `
  }

  createRenderRoot() {
    return this
  }

  adjustInputWidthAndFocus() {
    const input = this.inputRef.value
    const inputWidth = Math.max((this.inputVal.length + 1) * 7, 200)
    this.inputRef.value.style.width = `${inputWidth}px`
    this.inputRef.value.focus()
  }

  autoCompleteInput(val) {
    this.inputVal = val
    if (this.isValidFilter(val)) {
      this.triggerCustomEvent(this.inputVal)
    } else {
      this.handleChange(val)
    }
  }

  isValidFilter(filter) {
    const parts = filter.trim().split(/\s*([=<>!]+)\s*/)
    if (parts.length !== 3) {
      return false
    }
    const [field, operator, value] = parts
    if (!field || !value) {
      return false
    }
    return true
  }

  triggerCustomEvent(value) {
    if (!this.isValidFilter(value)) return
    const event = new CustomEvent('add-filter', {
      detail: {
        filter: value,
      },
      bubbles: true,
      composed: true,
    })
    this.dispatchEvent(event)
  }

  getField(filter) {
    const parts = filter.trim().split(/\s*([=<>!]+)\s*/)
    if (parts.length !== 3) {
      return ''
    }
    return parts[0]
  }

  getOperator(filter) {
    const parts = filter.trim().split(/\s*([=<>!]+)\s*/)
    if (parts.length !== 3) {
      return ''
    }
    return parts[1]
  }

  getValue(filter) {
    const parts = filter
      .replace(' ', '')
      .trim()
      .split(/\s*([=<>!]+)\s*/)
    if (parts.length !== 3) {
      return ''
    }
    return parts[2]
  }

  needsAutoComplete(filter) {
    if (this.isValidFilter(filter)) {
      const value = this.getValue(filter).replace(/^"|"$/g, '')
      if (value.length < 2) return null
      const field = this.getField(filter)
      const operator = this.getOperator(filter)
      if (['host', 'referer', 'raw_url', 'url_path'].includes(field)) {
        return { key: field, operator, prefix: value }
      }
    }
    return null
  }

  needsAutoCompleteKeyPath(filter) {
    let rootEnd = filter.indexOf('.')
    if (rootEnd == -1) return { result: false, field: null, prefix: null }

    const field = filter.substring(0, rootEnd)
    const prefix = filter.substring(rootEnd + 1)
    if (prefix.length < 2) return { result: false, field: null, prefix: null }

    if (['request_body', 'response_body', 'request_header', 'response_header', 'query_param', 'path_param'].includes(field)) {
      return {
        result: true,
        field: field,
        prefix: prefix,
      }
    }
    if (rootEnd == -1) return { result: false, field: null, prefix: null }
  }

  handleChange(val) {
    this.inputVal = val.trim()
    if (!this.inputVal) {
      this.matches = FIELDS
      return
    }
    let filters = FIELDS.filter((v) => v.startsWith(this.inputVal) || this.inputVal.startsWith(v))
    let auto_complete = []
    filters.forEach((filter) => {
      let target = filter
      let target_info = filterAutoComplete[target]
      if (!target_info) {
        const key = this.needsAutoCompleteKeyPath(this.inputVal)
        if (key.result) {
          fetch(`/p/${this.projectId}/query_builder/autocomplete?category=${key.field}&prefix=.${key.prefix}`)
            .then((res) => res.json())
            .then((data) => {
              this.matches = data.map((d) => key.field + d)
            })
          return
        } else {
          const result = this.needsAutoComplete(this.inputVal)
          if (result) {
            fetch(`/p/${this.projectId}/query_builder/autocomplete?category=${result.key}&prefix=${result.prefix}`)
              .then((res) => res.json())
              .then((data) => {
                this.matches = data.map((d) => `${result.key} ${result.operator} "${d}"`)
              })
            return
          } else {
            target_info = { operators: string_operators, values: [] }
          }
        }
      }

      if (filter == this.inputVal.trim() || filter.length > this.inputVal.length) {
        for (let op of target_info.operators) {
          auto_complete.push(`${filter} ${op} `)
        }
      } else {
        target_info.values.forEach((v) => {
          const valTyped = this.getValue(this.inputVal)
          if (String(v).startsWith(valTyped) || `"${v}`.startsWith(valTyped) || `"${v} "`.startsWith(valTyped)) {
            if (target_info.type === 'number' || target_info.type === 'boolean') {
              auto_complete.push(`${this.inputVal.replace(valTyped, '')} ${v}`)
            } else {
              auto_complete.push(`${this.inputVal.replace(valTyped, '')} "${v}"`)
            }
          }
        })
      }
    })

    this.matches = auto_complete
  }
}
customElements.define('filter-suggestions', Filter)
