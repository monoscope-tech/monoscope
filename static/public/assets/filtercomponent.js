import { LitElement, html, ref } from 'https://cdn.jsdelivr.net/gh/lit/dist@3/all/lit-all.min.js'
import jsonpath from './js/thirdparty/jsonpath.js'
// prettier-ignore
const httpStatusCodes = [
  // 1xx Informational
  100, 101, 102, 103,
  // 2xx Success
  200, 201, 202, 203, 204, 205, 206, 207, 208, 226,
  // 3xx Redirection
  300, 301, 302, 303, 304, 305, 307, 308,
  // 4xx Client Errors
  400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416, 417, 418, 421, 422, 423, 424, 425, 426, 428, 429, 431, 451,
  // 5xx Server Errors
  500, 501, 502, 503, 504, 505, 506, 507, 508, 510, 511,
]
const STRING_OPERATORS = ['==', '!=']
const NUMBER_OPERATORS = ['==', '>', '<', '!=', '>=', '<=']
// prettier-ignore
const REQ_SCHEMA = [
  ['method', STRING_OPERATORS, ['GET', 'POST', 'PUT', 'PATCH', 'DELETE', 'OPTIONS', 'HEAD', 'CONNECT', 'TRACE']],
  ['status_code', NUMBER_OPERATORS, httpStatusCodes],
  ['request_type', STRING_OPERATORS, ['Outgoing', 'Incoming']],
  ['duration_ns', NUMBER_OPERATORS, []],
  ['url_path', STRING_OPERATORS, []],
  ['duration_ns', NUMBER_OPERATORS, []],
  ['host', STRING_OPERATORS, []],
  ['raw_url', STRING_OPERATORS, []],
  ['request_header', STRING_OPERATORS, []],
  ['response_header', STRING_OPERATORS, []],
  ['request_body', STRING_OPERATORS, []],
  ['response_body', STRING_OPERATORS, []],
  ['query_param', STRING_OPERATORS, []],
  ['path_param', STRING_OPERATORS, []],
]
const AUTOCOMPLETE_SUPPORTED_FIELDS = ['request_body', 'response_body', 'request_header', 'response_header']
const OPERATORS = new Proxy(
  {
    Eq: '==',
    NotEq: '!=',
    GT: '>',
    LT: '<',
    GTEq: '>=',
    LTEq: '<=',
    And: 'and',
    Or: 'or',
    '==': 'Eq',
    '!=': 'NotEq',
    '>': 'GT',
    '<': 'LT',
    '>=': 'GTEq',
    '<=': 'LTEq',
  },
  { get: (t, p) => t[p] ?? p },
)

class SuggestionService {
  static properties = { projectid: { type: String } }
  constructor(projectid) {
    this.projectid = projectid
  }

  parseInput = input => {
    input = String(input)
    const patterns = [/^([\w.\[\]\*]+)\s*(==|!=|>=|<=|>|<)\s*"?([^"]*)"?$/i, /^([\w.\[\]\*]+)\s*"?([^"]*)"?$/i]
    for (const pattern of patterns) {
      const match = input.match(pattern)
      if (!match) continue
      const [_, field, opOrValue, value] = match
      return [field, opOrValue ?? '==', value ?? opOrValue ?? ''].map(x => x?.trim()?.replace(/^["']|["']$/g, '') ?? '')
    }
    return [input.trim()]
  }

  async fetchAutocomplete(category, prefix) {
    try {
      const response = await fetch(`/p/${this.projectid}/query_builder/autocomplete?category=${category}&prefix=${prefix}`)
      return response.ok ? await response.json() : []
    } catch (error) {
      console.error('Autocomplete fetch error:', error)
      return []
    }
  }

  getCurrentList(suggestions, qs, editContext) {
    // editContext.exprParts == {field: 'method', op: '==', value: 'PUT'}
    if (editContext) {
      const baseField = editContext?.exprParts?.field?.split('.')[0]
      const fieldSchema = REQ_SCHEMA.find(([f]) => f === baseField)
      if (editContext.editType === 'op' && editContext?.exprParts?.field) return fieldSchema?.[1] ?? STRING_OPERATORS
      if (editContext.editType === 'value' && editContext?.exprParts?.field) return fieldSchema?.[2] ?? []
      return REQ_SCHEMA.filter(([f]) => f.includes(editContext?.exprParts?.currentValue ?? ''))
    }
    if (suggestions.length) return suggestions
    const baseField = qs.field?.split('.')[0]
    const fieldSchema = REQ_SCHEMA.find(([f]) => f === baseField)
    if (qs.editType === 'op' && qs.field) return fieldSchema?.[1] ?? STRING_OPERATORS
    if (qs.editType === 'value' && qs.field) return fieldSchema?.[2] ?? []
    if (!qs.field) return REQ_SCHEMA.filter(([f]) => !qs.input || f.includes(this.parseInput(qs.input)[0]))
    return !qs.op ? (fieldSchema?.[1] ?? []) : (fieldSchema?.[2] ?? [])
  }

  async updateStateFromInput(input) {
    const [field] = this.parseInput(input)
    const baseField = field?.split('.')?.[0]
    const suggestions =
      field && AUTOCOMPLETE_SUPPORTED_FIELDS.includes(baseField)
        ? await this.fetchAutocomplete(baseField, field === baseField ? '' : field.split('.').slice(1).join('.'))
        : []
    const fieldSchema = REQ_SCHEMA.find(([f]) => f === baseField)
    const validOps = fieldSchema?.[1] ?? STRING_OPERATORS
    return {
      qs: {
        input,
        field: fieldSchema ? field : null,
        op: fieldSchema && validOps.some(validOp => input.includes(validOp)) ? validOps.find(validOp => input.includes(validOp)) : null,
      },
      suggestions,
    }
  }

  async handleSelection(type, value, currentQs) {
    if (type === 'field') {
      const baseField = value.split('.')[0]
      const suggestions = AUTOCOMPLETE_SUPPORTED_FIELDS.includes(baseField) ? await this.fetchAutocomplete(baseField, value.split('.').slice(1).join('.')) : []
      return { qs: { input: value, field: value, op: null }, suggestions }
    }
    if (type === 'value') return { newNode: { tag: OPERATORS[currentQs.op] || 'Eq', contents: [currentQs.field, value] } }
    return { qs: { input: `${currentQs.field}${value}`, field: currentQs.field, op: value } }
  }

  handleEditContext(editContext, value) {
    return {
      type: editContext.editType,
      path: editContext.path,
      exprParts: editContext.exprParts,
      value,
    }
  }
}

export class QueryInputElement extends LitElement {
  static properties = {
    projectid: { type: String },
    suggestions: { type: Array },
    selected: { type: Number },
    qs: { type: Object },
    editContext: { type: Object },
    ast: { type: Array },
    onAddQuery: { type: Function },
    onUpdateQuery: { type: Function },
    onCloseQuery: { type: Function },
  }

  constructor() {
    super()
    this.suggestionService = new SuggestionService(this.projectid)
    this.qs = {}
    this.selected = 0
    this.suggestions = []
  }

  async #updateStateFromInput(input) {
    const { qs, suggestions } = await this.suggestionService.updateStateFromInput(input)

    this.qs = qs
    this.suggestions = suggestions
    this.selected = 0

    this.requestUpdate()
    this.dispatchEvent(new CustomEvent('query-state-change', { detail: this.qs }))
  }

  #getCurrentList = () => this.suggestionService.getCurrentList(this.suggestions, this.qs, this.editContext)

  #handleKey = e => {
    const list = this.#getCurrentList()
    const [field, op, value] = this.suggestionService.parseInput(this.qs.input ?? '')
    const baseField = field?.split('.')?.[0]
    const fieldSchema = REQ_SCHEMA.find(([f]) => f === baseField)

    const handlers = {
      ArrowDown: () => (this.selected = Math.min(this.selected + 1, list.length - 1)),
      ArrowUp: () => (this.selected = Math.max(0, this.selected - 1)),
      Enter: () => {
        if (!this.selected && field && op && value) {
          const isNumber = baseField && fieldSchema?.[1] === NUMBER_OPERATORS
          const parsedValue = isNumber ? Number(value) : value

          if (!isNumber || !isNaN(parsedValue)) {
            this.#select('value', parsedValue)
            return
          }
        }

        if (this.selected && list[this.selected - 1]) {
          if (!this.qs.field) {
            this.#select('field', list[this.selected - 1][0])
          } else if (!this.qs.op) {
            this.#select('op', list[this.selected - 1])
          } else {
            this.#select('value', list[this.selected - 1])
          }
        }
      },
      Tab: e => {
        e.preventDefault()
        handlers.Enter()
      },
      Escape: () => {
        if (this.selected) {
          this.selected = 0
        } else {
          this.onCLoseQuery()
        }
      },
    }

    handlers[e.key]?.()
    this.requestUpdate()
  }

  #select = async (type, value) => {
    if (this.editContext) {
      const detail = this.suggestionService.handleEditContext(this.editContext, value)
      this.onUpdateQuery(detail)
    } else {
      const result = await this.suggestionService.handleSelection(type, value, this.qs)
      if (result.qs) {
        this.qs = result.qs
      }
      if (result.suggestions !== undefined) {
        this.suggestions = result.suggestions
      }
      if (result.newNode) this.onAddQuery(result.newNode)
    }

    this.selected = 0
    this.requestUpdate()
  }
  render() {
    const list = this.#getCurrentList()
    const [parsedField, _parsedOp] = this.suggestionService.parseInput((this.editContext?.currentValue || this.qs?.input) ?? '')
    const isValid = this.editContext || !parsedField || REQ_SCHEMA.some(([f]) => parsedField.startsWith(f))
    const selectionType = this.editContext?.editType || (!this.qs.field ? 'field' : !this.qs.op ? 'op' : 'value')

    return html`
      <div class="query-input absolute z-50 bg-bgBase border rounded-sm font-normal top-10 left-0">
        <input
          type="text"
          autofocus
          ${ref(el => el?.focus())}
          class="w-full min-w-56 border-b text-sm outline-hidden px-2 py-1 ${isValid ? '' : 'text-red-500'}"
          .value=${this.editContext?.currentValue || this.qs.input || ''}
          @input=${e => this.#updateStateFromInput(e.target.value)}
          @keydown=${this.#handleKey}
          placeholder=${this.editContext ? `Edit ${this.editContext.editType}...` : 'Type query/field ...'}
        />
        <div class="min-w-56 max-h-56 overflow-y-auto">
          ${list.map(
            (item, i) => html`
              <a
                class="w-full text-left px-2 py-1 hover:bg-fillWeaker flex justify-between cursor-pointer ${i === this.selected - 1 ? 'bg-blue-100' : ''}"
                @mouseenter=${() => (this.selected = i + 1)}
                @mouseleave=${() => (this.selected = 0)}
                @click=${() => this.#select(selectionType, Array.isArray(item) ? item[0] : item)}
              >
                ${Array.isArray(item) ? item[0] : item}
                ${!this.suggestions.length && Array.isArray(item)
                  ? html`
                      <span class="text-xs text-gray-500">${item[1] === NUMBER_OPERATORS ? 'number' : item[1] === STRING_OPERATORS ? 'string' : 'field'}</span>
                    `
                  : ''}
              </a>
            `,
          )}
        </div>
      </div>
    `
  }

  createRenderRoot() {
    return this
  }
}
customElements.define('query-input-element', QueryInputElement)

// *******************************************************************************
// FilterElement holds the parent ui for filter elements
// *******************************************************************************
export class FilterElement extends LitElement {
  static properties = { ast: { type: Array }, newQuery: { type: Boolean }, editingPath: { type: String }, projectid: { type: String } }

  constructor() {
    super()
    this.ast = []
    this.newQuery = false
    this.editingPath = ''
  }

  #remove = (nodePath, e) => {
    e?.stopPropagation()
    const parentPath = nodePath.match(/(.*?)\.contents(?:\[\d+\])?$/)?.[1]
    const node = jsonpath.query(this.ast, `$${nodePath}`)[0]
    const parent = parentPath ? jsonpath.query(this.ast, `$${parentPath}`)[0] : null
    if (!parent) return

    if (parent.tag === 'Search') {
      this.ast = []
    } else {
      const sibling = parent.contents.find(n => n !== node)
      if (parentPath) {
        // Replace the parent's content with the sibling.
        jsonpath.apply(this.ast, `$${parentPath}`, () => sibling)
      } else {
        // If there's no parent path, assume it's the top level under Search.
        this.ast = [{ tag: 'Search', contents: sibling }]
      }
    }
    this.#updateQueryASTInURI()
    this.dispatchEvent(
      new CustomEvent('update-query', {
        detail: { ast: this.ast },
        composed: true,
        bubbles: true,
      }),
    )
    this.requestUpdate()
  }

  #toggleAndOr = (path, _e) => {
    jsonpath.apply(this.ast, `$${path}`, op => (op == 'And' ? 'Or' : 'And')) && this.requestUpdate()
    this.#updateQueryASTInURI()
    this.dispatchEvent(new CustomEvent('update-query', { detail: { ast: this.ast }, composed: true, bubbles: true }))
  }

  #updateQueryASTInURI = () => {
    const url = new URL(window.location.href)
    // Note: using encodeURIComponent ensures the JSON is URL safe.
    url.searchParams.set('queryAST', encodeURIComponent(JSON.stringify(this.ast)))
    history.replaceState(null, '', url)
  }

  _handleAddQuery = newNode => {
    if (Array.isArray(newNode) && newNode.some(item => item.tag === 'Search')) {
      this.ast = newNode
    } else {
      const search = this.ast.find(e => e.tag === 'Search')
      search ? (search.contents = { tag: 'And', contents: [search.contents, newNode] }) : this.ast.push({ tag: 'Search', contents: newNode })
    }
    this.newQuery = false
    this.#updateQueryASTInURI()
    this.dispatchEvent(new CustomEvent('add-query', { detail: { newNode: newNode, ast: this.ast }, composed: true, bubbles: true }))
    this.requestUpdate()
  }

  handleAddQuery = this._handleAddQuery

  _handleUpdateQuery = detail => {
    jsonpath.apply(this.ast, `$${path}`, () => (type === 'op' ? OPERATORS[value] : value))
    this.editingPath = ''
    this.#updateQueryASTInURI()
    this.dispatchEvent(new CustomEvent('update-query', { detail: { ast: this.ast }, composed: true, bubbles: true }))
    this.requestUpdate()
  }

  _handleCloseQuery = () => {
    this.newQuery = false
    this.editingPath = ''
    this.#updateQueryASTInURI()
    this.requestUpdate()
  }

  #renderFilter = (node, path = '') => {
    if (!node) return null

    if (['And', 'Or'].includes(node.tag)) {
      return html` <div class="flex flex-wrap gap-2">
        ${node.contents.map(
          (content, i) => html`
            ${this.#renderFilter(content, `${path}.contents[${i}]`)}
            ${i < node.contents.length - 1
              ? html`
                  <a
                    class="bg-gray-200 text-xs px-2 py-1 rounded-full cursor-pointer inline-flex items-center gap-1"
                    @click=${() => this.#toggleAndOr(`${path}.tag`)}
                    data-path="${path}.tag"
                  >
                    ${OPERATORS[node.tag]}
                    <svg class="h-3 w-3 icon"><use href="/public/assets/svgs/fa-sprites/regular.svg#sliders-simple" /></svg>
                  </a>
                `
              : ''}
          `,
        )}
      </div>`
    }

    const renderEditableField = (fieldPath, value, editType, exprParts) => html`
      <div class="relative">
        <a
          class="p-1 cursor-pointer hover:bg-[#d5f0ff]"
          @click=${() => {
            this.editingPath = fieldPath
            this.requestUpdate()
          }}
        >
          ${editType === 'value' ? JSON.stringify(value) : value}
        </a>
        ${this.editingPath === fieldPath
          ? html`
              <query-input-element
                .editContext=${{ editType, path: fieldPath, currentValue: value, exprParts }}
                .onAddQuery=${this._handleAddQuery}
                .onUpdateQuery=${this._handleUpdateQuery}
                .onCloseQuery=${this._handleCloseQuery}
              >
              </query-input-element>
            `
          : ''}
      </div>
    `

    const exprParts = { field: node.contents[0], op: OPERATORS[node.tag], value: node.contents[1] }
    return html`
      <div class="text-[#067cff] bg-[#edf9ff] rounded-xl border border-[#b5e5ff] justify-start items-center inline-flex px-1" data-path=${path}>
        <div class="divide-x divide-[#b5e5ff] inline-flex">
          ${renderEditableField(`${path}.contents[0]`, node.contents[0], 'field', exprParts)}
          ${renderEditableField(`${path}.tag`, OPERATORS[node.tag], 'op', exprParts)}
          ${renderEditableField(`${path}.contents[1]`, node.contents[1], 'value', exprParts)}
        </div>
        <span class="rounded-full bg-white inline-flex p-1 ml-1 cursor-pointer" @click=${() => this.#remove(path)}>
          <svg class="h-3 w-3 icon"><use href="/public/assets/svgs/fa-sprites/regular.svg#xmark" /></svg>
        </span>
      </div>
    `
  }

  #closeOnClickOutside = e =>
    (this.newQuery || this.editingPath) &&
    !e.composedPath().find(el => el.classList?.contains('query-input')) &&
    (this.editingPath = '') &&
    (this.newQuery = false)

  connectedCallback() {
    super.connectedCallback()
    window.addEventListener('click', this.#closeOnClickOutside)

    // Load the component with AST from URI
    const params = new URLSearchParams(window.location.search)
    const queryAST = params.get('queryAST')
    if (queryAST) {
      try {
        // Decode and parse the AST
        this.ast = JSON.parse(decodeURIComponent(params.get('queryAST')))
        this.requestUpdate()
      } catch (error) {
        console.error('Invalid queryAST in URL', error)
      }
    }
  }

  disconnectedCallback() {
    super.disconnectedCallback()
    window.removeEventListener('click', this.#closeOnClickOutside)
  }

  render() {
    const searchASTIdx = this.ast?.findIndex(n => n.tag === 'Search')
    return html`
      <div class="relative flex items-center flex-wrap gap-2" @click=${e => e.stopPropagation()}>
        <svg class="h-4 w-4 icon"><use href="/public/assets/svgs/fa-sprites/regular.svg#filter" /></svg>
        <div class="flex flex-wrap gap-2">
          ${searchASTIdx != -1 ? this.#renderFilter(this.ast[searchASTIdx].contents, `[${searchASTIdx}].contents`) : ''}
          <div class="relative inline-block">
            ${typeof this.ast[searchASTIdx]?.contents === 'object'
              ? html`<a
                  class="rounded-lg bg-white inline-flex p-2 items-center shadow-xs cursor-pointer"
                  @click=${() => {
                    this.newQuery = true
                    this.requestUpdate()
                  }}
                >
                  <svg class="h-4 w-4 icon"><use href="/public/assets/svgs/fa-sprites/regular.svg#plus" /></svg>
                </a>`
              : html`<a
                  @click=${() => {
                    this.newQuery = true
                    this.requestUpdate()
                  }}
                  class="cursor-pointer p-2 "
                  >Click to add filter...</a
                >`}
            ${this.newQuery
              ? html`
                  <query-input-element .onCloseQuery=${this._handleCloseQuery} .onAddQuery=${this._handleAddQuery} .onUpdateQuery=${this._handleUpdateQuery}>
                  </query-input-element>
                `
              : ''}
          </div>
        </div>
      </div>
    `
  }

  createRenderRoot() {
    return this
  }
}

customElements.define('filter-element', FilterElement)
