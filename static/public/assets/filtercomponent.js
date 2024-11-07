import { LitElement, html, ref, createRef } from 'https://cdn.jsdelivr.net/gh/lit/dist@3/all/lit-all.min.js'
import jsonpath from './js/thirdparty/jsonpath.js'
const httpStatusCodes = [
  // 1xx Informational
  100, 101, 102, 103,
  // 2xx Success
  200, 201, 202, 203, 204, 205, 206, 207, 208, 226,
  // 3xx Redirection
  300, 301, 302, 303, 304, 305, 307, 308,
  // 4xx Client Errors
  400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410,
  411, 412, 413, 414, 415, 416, 417, 418, 421, 422, 423,
  424, 425, 426, 428, 429, 431, 451,
  // 5xx Server Errors
  500, 501, 502, 503, 504, 505, 506, 507, 508, 510, 511
];
const string_operators = ['==', '!=']
const number_operators = ['==', '>', '<', '!=', '>=', '<=']
const reqSchema = [
  ["method", string_operators, ['GET', 'POST', 'PUT', 'PATCH', 'DELETE', 'OPTIONS', 'HEAD', 'CONNECT', 'TRACE']],
  ["status_code", number_operators, httpStatusCodes],
  ["request_type", string_operators, ["Outgoing", "Incoming"]],
  ["duration_ns", number_operators, []],
  ["url_path"],
  ["duration_ns"],
  ["has_errors"],
  ["host"],
  ["raw_url"],
  ["request_header"],
  ["response_header"],
  ["request_body"],
  ["response_body"],
  ["query_param"],
  ["path_param"]
]
const AUTOCOMPLETE_SUPPORTED_FIELDS = ['request_body', 'response_body', 'request_header', 'response_header'];

export class FilterElement extends LitElement {
  static properties = { ast: { type: Array }, newQuery: { type: Boolean }, qs: { type: Object }, selected: { type: Number }, suggestions: { type: Array }, editingPath: { type: String } };

  constructor() {
    super();
    this.qs = {};
    this.selected = 0;
    this.suggestions = [];
    this.editingPath = {}

    window.setQueryBuilderFromParams = () => {
      const urlSearchParams = new URLSearchParams(window.location.search)
      const query = urlSearchParams.get('query')
      if (query) {
        this.setBuilderValue(query)
      }
    }
  }

  #ops = new Proxy({
    Eq: '==', GT: '>', LT: '<', GTE: '>=', LTE: '<=', And: 'and', Or: 'or',
    '==': 'Eq', '>': 'GT', '<': 'LT', '>=': 'GTE', '<=': 'LTE'
  }, { get: (t, p) => t[p] ?? p });

  #remove = (path, e) => {
    e?.stopPropagation();
    const [parent, node] = [path.match(/(.*?)\.contents(?:\[\d+\])?$/)?.[1], path]
      .map(p => p && jsonpath.query(this.ast, `$${p}`)[0])
      .filter(Boolean);

    if (!parent?.contents) return;

    if (parent.tag === 'Search') {
      this.ast = []; // If removing from root Search node, clear the whole AST
    } else if (parent.tag === 'And') {
      // For And nodes, replace parent with the remaining child
      const remainingNode = Array.isArray(parent.contents) ?
        parent.contents.find(n => n !== node) :
        undefined;

      const grandParentPath = path.match(/(.*?)\.contents\.contents(?:\[\d+\])?$/)?.[1];
      if (grandParentPath) {
        jsonpath.apply(this.ast, `$${grandParentPath}`, () => remainingNode);
      }
    }

    this.requestUpdate();
  };

  #toggle = (path, _e) => {
    jsonpath.apply(this.ast, `$${path}`, op => op == 'And' ? 'Or' : 'And')
    this.requestUpdate();
  };


  render = () => html`
    <div class="relative flex items-center flex-wrap gap-2" @click=${(e) => e.stopPropagation()}>
      <svg class="h-4 w-4 icon">
        <use href="/public/assets/svgs/fa-sprites/regular.svg#filter"/>
      </svg>
      <div class="flex flex-wrap gap-2">
        ${(i => i >= 0 ? this.#renderFilter(this.ast[i].contents, `[${i}].contents`) : '')(this.ast?.findIndex(n => n.tag === 'Search') ?? -1)}
        <div class="relative inline-block">        
          <span class="rounded-lg bg-white inline-flex p-2 items-center shadow-sm cursor-pointer" 
            @click=${() => { this.newQuery = true; this.requestUpdate() }}>
            <svg class="h-4 w-4 icon"><use href="/public/assets/svgs/fa-sprites/regular.svg#plus"/></svg>
          </span>
          ${this.newQuery ? this.#renderQueryInput(null, null, {}) : ''}
        </div>
      </div>
    </div>
  `;

  // To allow closing the dropdowns when user clicks outside
  #closeOnClickOutside = e =>
    this.newQuery && !e.composedPath().find(el => el.classList?.contains('query-input')) &&
    (this.editingPath = "") &&
    (this.newQuery = false);

  connectedCallback() {
    super.connectedCallback(); window.addEventListener('click', this.#closeOnClickOutside);
  }

  disconnectedCallback() {
    super.disconnectedCallback(); window.removeEventListener('click', this.#closeOnClickOutside);
  }

  #renderFilter = (node, path = '') => !node ? null :
    ['And', 'Or'].includes(node.tag) ? html`
      <div class="flex flex-wrap gap-2">
        ${node.contents.flatMap((c, i) => [
      this.#renderFilter(c, `${path}.contents[${i}]`),
      i < node.contents.length - 1 && html`
            <button class="bg-gray-200 text-xs px-2 py-1 rounded-full" type="button" 
              @click=${(e) => this.#toggle(`${path}.tag`, e)} data-path="${path}.tag">
              ${this.#ops[node.tag]} <svg class="h-3 w-3 icon"><use href="/public/assets/svgs/fa-sprites/regular.svg#sliders-simple"/></svg>
            </button>`
    ]).filter(Boolean)}
      </div>
    ` : html`
      <div class="text-[#067cff] bg-[#edf9ff] rounded-xl border border-[#b5e5ff] justify-start items-center inline-flex px-1"
        data-path=${path}>
        <div class="divide-x divide-[#b5e5ff] inline-flex">
          <div class="relative"><a class="p-1 cursor-pointer hover:bg-[#d5f0ff]" @click=${() => { this.editingPath = `${path}.contents[0]`; this.requestUpdate() }}>
            ${node.contents[0]}
          </a>${this.editingPath == `${path}.contents[0]` ? this.#renderQueryInput('field', `${path}.contents[0]`, { field: node.contents[0], ops: node.tag, value: node.contents[1] }) : ''}</div>
          <div class="relative"><a class="p-1 cursor-pointer hover:bg-[#d5f0ff]"  @click=${() => { this.editingPath = `${path}.tag`; this.requestUpdate() }}>
            ${this.#ops[node.tag]}
          </a>${this.editingPath == `${path}.tag` ? this.#renderQueryInput('op', `${path}.tag`, { field: node.contents[0], ops: node.tag, value: node.contents[1] }) : ''}</div>
          <div class="relative"><a class="p-1 cursor-pointer hover:bg-[#d5f0ff]" @click=${() => { this.editingPath = `${path}.contents[1]`; this.requestUpdate() }}>
            ${JSON.stringify(node.contents[1])}
          </a>${this.editingPath == `${path}.contents[1]` ? this.#renderQueryInput('value', `${path}.contents[1]`, { field: node.contents[0], ops: node.tag, value: node.contents[1] }) : ''}</div>
        </div>
        <span class="rounded-full bg-white inline-flex p-1 ml-1 cursor-pointer" 
          @click=${(e) => this.#remove(path, e)}>
          <svg class="h-3 w-3 icon">
            <use href="/public/assets/svgs/fa-sprites/regular.svg#xmark"/>
          </svg>
        </span>
      </div>
    `;

  async #fetchAutocomplete(category, prefix) {
    try {
      const response = await fetch(`/p/e3754fd3-565b-4be5-9428-f890f9cc9237/query_builder/autocomplete?category=${category}&prefix=${prefix}`);
      if (!response.ok) throw new Error('Failed to fetch suggestions');
      return await response.json();
    } catch (error) {
      console.error('Autocomplete fetch error:', error);
      return [];
    }
  }
  #parseInput = input => {
    // Enhanced regex patterns to support array paths with wildcards
    const patterns = [
      /^([\w.\[\]\*]+)\s*(==|!=|>=|<=|>|<)\s*"?([^"]*)"?$/i,
      /^([\w.\[\]\*]+)\s+(?:is|equals?|=)\s*"?([^"]*)"?$/i,
      /^([\w.\[\]\*]+)\s+(?:>|greater|more|above|after)\s*"?([^"]*)"?$/i,
      /^([\w.\[\]\*]+)\s+(?:<|less|below|before)\s*"?([^"]*)"?$/i,
      /^([\w.\[\]\*]+)\s*"?([^"]*)"?$/i
    ];

    for (const pattern of patterns) {
      const match = input.match(pattern);
      if (!match) continue;
      const [_, field, opOrValue, value] = match;
      const op = pattern === patterns[1] ? '==' :
        pattern === patterns[2] ? '>' :
          pattern === patterns[3] ? '<' :
            opOrValue;
      return [
        field.trim(),
        op?.trim?.() ?? '==',
        value?.trim() ?? opOrValue?.trim() ?? ''
      ].map(x => x.replace(/^["']|["']$/g, ''));
    }
    return [input.trim()];
  };
  async #updateStateFromInput(input) {
    const [field] = this.#parseInput(input);
    const baseField = field?.split('.')?.[0];

    // Enhanced autocomplete behavior
    if (field && AUTOCOMPLETE_SUPPORTED_FIELDS.includes(baseField)) {
      // If it's just the base field, show all initial paths
      if (field === baseField) {
        this.suggestions = await this.#fetchAutocomplete(baseField, '');
      } else {
        // For nested paths, use everything after the base field as prefix
        const pathParts = field.split('.');
        const prefix = pathParts.slice(1).join('.');
        this.suggestions = await this.#fetchAutocomplete(baseField, prefix);
      }
    } else {
      this.suggestions = [];
    }

    // Rest of the validation logic
    const fieldSchema = reqSchema.find(([f]) => f === baseField);
    const [_, op, value] = this.#parseInput(input);
    const validOps = fieldSchema?.[1] ?? string_operators;

    this.qs = {
      input,
      field: fieldSchema ? field : null,
      op: null,
    };

    if (this.qs.field && validOps.some(validOp => input.includes(validOp))) {
      this.qs.op = validOps.find(validOp => input.includes(validOp));
    }

    this.selected = 0;
    this.requestUpdate();
  }

  #getCurrentList = () => {
    if (this.suggestions.length > 0) return this.suggestions;

    // If editing operator, only show valid operators for the field
    if (this.qs.editType === 'op' && this.qs.field) {
      const baseField = this.qs.field.split('.')[0];
      const fieldSchema = reqSchema.find(([f]) => f === baseField);
      return fieldSchema?.[1] ?? string_operators;
    }

    // If editing value and field has predefined values, show them
    if (this.qs.editType === 'value' && this.qs.field) {
      const baseField = this.qs.field.split('.')[0];
      const fieldSchema = reqSchema.find(([f]) => f === baseField);
      return fieldSchema?.[2] ?? [];
    }

    // Existing logic for new filters
    if (!this.qs.field) {
      return reqSchema.filter(([f]) => !this.qs.input || f.includes(this.#parseInput(this.qs.input)[0]));
    }

    if (!this.qs.op) {
      const baseField = this.qs.field.split('.')[0];
      const fieldSchema = reqSchema.find(([f]) => f === baseField);
      return fieldSchema?.[1] ?? [];
    }

    const baseField = this.qs.field.split('.')[0];
    const fieldSchema = reqSchema.find(([f]) => f === baseField);
    return fieldSchema?.[2] ?? [];
  };

  #handleKey = e => {
    const list = this.#getCurrentList();
    const [field, op, value] = this.#parseInput(this.qs.input ?? '');
    const baseField = field?.split('.')?.[0];
    const fieldSchema = reqSchema.find(([f]) => f === baseField);
    const validOps = fieldSchema?.[1] ?? string_operators; // Default to string ops for nested paths

    const handlers = {
      ArrowDown: () => this.selected = Math.min((this.selected + 1), list.length - 1),
      ArrowUp: () => this.selected = Math.max(0, this.selected - 1),
      Enter: () => {
        // Direct input mode - complete query
        if (!this.selected && field && op && value) {
          // For nested paths, always treat as string unless explicitly number
          const isNumber = baseField && fieldSchema?.[1] === number_operators;
          const parsedValue = isNumber ? Number(value) : value;

          if (!isNumber || !isNaN(parsedValue)) {
            this.#select('value', parsedValue);
            return;
          }
        }

        // Selection mode remains unchanged
        if (this.selected && list[this.selected - 1]) {
          if (!this.qs.field) {
            this.#select('field', list[this.selected - 1][0]);
          } else if (!this.qs.op) {
            this.#select('op', list[this.selected - 1]);
          } else {
            this.#select('value', list[this.selected - 1]);
          }
        }
      },
      Tab: e => { e.preventDefault(); handlers.Enter() },
      Escape: () => {
        if (this.selected) {
          this.selected = 0;
        } else {
          this.newQuery = false;
          this.qs = {};
        }
      }
    };

    handlers[e.key]?.();
    this.requestUpdate();
  };

  // Update the suggestion rendering to show full paths
  #select = async (type, value) => {
    if (type === 'field') {
      const baseField = value.split('.')[0];
      if (AUTOCOMPLETE_SUPPORTED_FIELDS.includes(baseField)) {
        this.qs = {
          input: value,
          field: value,
          op: null
        };
        const prefix = value.split('.').slice(1).join('.');
        this.suggestions = await this.#fetchAutocomplete(baseField, prefix);
      } else {
        this.qs = {
          input: value,
          field: value,
          op: null
        };
        this.suggestions = [];
      }
    } else if (type === 'value') {
      const operator = this.qs.op;
      const opTag = this.#ops[operator] || 'Eq';
      const newNode = {
        tag: opTag,
        contents: [this.qs.field, value]
      };

      if (!this.ast?.length) {
        // First node
        this.ast = [{
          tag: 'Search',
          contents: newNode
        }];
      } else {
        // Add to existing tree
        const currentContents = this.ast[0].contents;
        this.ast = [{
          tag: 'Search',
          contents: {
            tag: 'And',
            contents: [currentContents, newNode]
          }
        }];
      }

      this.newQuery = false;
      this.qs = {};
      this.suggestions = [];
    } else {
      const newInput = `${this.qs.field}${value}`;
      this.qs = {
        input: newInput,
        field: this.qs.field,
        op: value
      };
    }

    this.selected = 0;
    this.requestUpdate();
  };
  #renderQueryInputx = (kind, path) => {
    const { field, op, input } = this.qs;
    const [parsedField, parsedOp, parsedValue] = this.#parseInput(input ?? '');
    const list = this.#getCurrentList();
    const baseField = parsedField?.split('.')?.[0];
    const fieldSchema = reqSchema.find(([f]) => f === baseField);

    const isValid = !parsedField || (
      reqSchema.some(([f]) => parsedField.startsWith(f)) &&
      (!op || fieldSchema?.[1]?.includes(parsedOp))
    );

    return html`
    <div class="query-input absolute z-50 bg-slate-50 border rounded font-normal top-10 left-0">
      <input type="text" 
        autofocus
        ${ref(el => el?.focus())}
        style="field-sizing: content;"
        class="w-full min-w-56 border-b text-sm outline-none px-2 py-1 ${isValid ? '' : 'text-red-500'}"
        .value=${this.qs.input ?? ''} 
        @input=${e => this.#updateStateFromInput(e.target.value)}
        @keydown=${this.#handleKey}
        placeholder="Type query/field ..." />
      <div class="flex">
        <div class="min-w-56 max-h-56 overflow-y-auto ${!field ? '' : !op ? 'border-l' : 'border-l'}">
          ${list.map((item, i) => html`
            <button class="w-full text-left px-2 py-1 hover:bg-slate-100 flex justify-between
              ${i === this.selected - 1 ? 'bg-blue-100' : ''}"
              @mouseenter=${() => this.selected = i + 1}
              @mouseleave=${() => this.selected = 0}
              type="button"
              @click=${() => this.#select(!field ? 'field' : !op ? 'op' : 'value', !field ? (this.suggestions.length ? baseField + '.' + item : item[0]) : item)}>
              ${!field ? html`
                ${this.suggestions.length ? baseField + '.' + item : item[0]}
                ${!this.suggestions.length ? html`
                  <span class="text-xs text-gray-500">
                    ${item[1] === number_operators ? 'number' :
            item[1] === string_operators ? 'string' : 'field'}
                  </span>` : ''}
              ` : item}
            </button>
          `)}
        </div>
      </div>
    </div>
  `;
  };

  #renderQueryInput = (kind, path, { attachedField, attachedOperator }) => {
    // Determine editing context from kind and path
    const editContext = kind ? {
      editType: kind,
      path: path,
      currentValue: path ? jsonpath.query(this.ast, `$${path}`)[0] : null
    } : null;

    const { qField, qOp, input } = this.qs;
    const field = attachedField ? attachedField : qField;
    const op = attachedOperator ? attachedOperator : qOp;
    const [parsedField, parsedOp, parsedValue] = this.#parseInput(input ?? '');
    const list = this.#getCurrentList();
    const baseField = parsedField?.split('.')?.[0];
    const fieldSchema = reqSchema.find(([f]) => f === baseField);

    const isValid = !parsedField || (
      reqSchema.some(([f]) => parsedField.startsWith(f)) &&
      (!op || fieldSchema?.[1]?.includes(parsedOp))
    );

    const handleSelection = (type, value) => {
      if (editContext) {
        // Handle path-based updates
        if (editContext.editType === 'field') {
          jsonpath.apply(this.ast, `$${editContext.path}`, () => value);
        } else if (editContext.editType === 'op') {
          jsonpath.apply(this.ast, `$${editContext.path}`, () => this.#ops[value]);
        } else if (editContext.editType === 'value') {
          const baseField = editContext.path.split('.')[0];
          const fieldSchema = reqSchema.find(([f]) => f === baseField);
          const isNumber = fieldSchema?.[1] === number_operators;
          const parsedValue = isNumber ? Number(value) : value;
          jsonpath.apply(this.ast, `$${editContext.path}`, () => parsedValue);
        }

        // Clear the  editingPath state and query input
        this.editingPath = '';
        this.qs = {};
        this.suggestions = [];
      } else {
        // Handle new query creation (existing behavior)
        this.#select(type, value);
      }
      this.requestUpdate();
    };

    const handleKeyDown = (e) => {
      const list = this.#getCurrentList();

      if (e.key === 'Enter' && !this.selected && editContext) {
        // Handle direct input for path updates
        const value = e.target.value;
        if (editContext.editType === 'field') {
          const field = value;
          if (reqSchema.some(([f]) => field.startsWith(f))) {
            handleSelection('field', field);
          }
        } else if (editContext.editType === 'op') {
          const baseField = jsonpath.query(this.ast, `$${editContext.path.replace('.tag', '.contents[0]')}`)[0];
          const fieldSchema = reqSchema.find(([f]) => f === baseField?.split('.')?.[0]);
          const validOps = fieldSchema?.[1] ?? string_operators;
          if (validOps.includes(value)) {
            handleSelection('op', value);
          }
        } else if (editContext.editType === 'value') {
          handleSelection('value', value);
        }
        return;
      }

      // Regular key handling
      const handlers = {
        ArrowDown: () => this.selected = Math.min((this.selected + 1), list.length - 1),
        ArrowUp: () => this.selected = Math.max(0, this.selected - 1),
        Enter: () => {
          if (this.selected && list[this.selected - 1]) {
            handleSelection(
              editContext?.editType || (!field ? 'field' : !op ? 'op' : 'value'),
              list[this.selected - 1]
            );
          }
        },
        Tab: e => { e.preventDefault(); handlers.Enter(); },
        Escape: () => {
          if (this.selected) {
            this.selected = 0;
          } else {
            if (editContext) {
              this.editingPath = "";
            } else {
              this.newQuery = false;
            }
            this.qs = {};
          }
        }
      };

      handlers[e.key]?.();
      this.requestUpdate();
    };

    // Get initial input value based on context
    const getInitialValue = () => {
      if (editContext) {
        return editContext.currentValue || '';
      }
      return this.qs.input || '';
    };

    return html`
    <div class="query-input absolute z-50 bg-slate-50 border rounded font-normal top-10 left-0">
      <input type="text" 
        autofocus
        ${ref(el => el?.focus())}
        style="field-sizing: content;"
        class="w-full min-w-56 border-b text-sm outline-none px-2 py-1 ${isValid ? '' : 'text-red-500'}"
        .value=${getInitialValue()} 
        @input=${e => this.#updateStateFromInput(e.target.value)}
        @keydown=${handleKeyDown}
        placeholder=${editContext ?
        `Edit ${editContext.editType}...` :
        "Type query/field ..."} />
      <div class="flex">
        <div class="min-w-56 max-h-56 overflow-y-auto ${!field ? '' : !op ? 'border-l' : 'border-l'}">
          ${list.map((item, i) => html`
            <button class="w-full text-left px-2 py-1 hover:bg-slate-100 flex justify-between
              ${i === this.selected - 1 ? 'bg-blue-100' : ''}"
              @mouseenter=${() => this.selected = i + 1}
              @mouseleave=${() => this.selected = 0}
              type="button"
              @click=${() => handleSelection(
          editContext?.editType || (!field ? 'field' : !op ? 'op' : 'value'),
          !field ? (this.suggestions.length ? baseField + '.' + item : item[0]) : item
        )}>
              ${!field ? html`
                ${this.suggestions.length ? baseField + '.' + item : item[0]}
                ${!this.suggestions.length ? html`
                  <span class="text-xs text-gray-500">
                    ${item[1] === number_operators ? 'number' :
                item[1] === string_operators ? 'string' : 'field'}
                  </span>` : ''}
              ` : item}
            </button>
          `)}
        </div>
      </div>
    </div>
  `;
  };
  createRenderRoot() { return this; }
}

customElements.define('filter-element', FilterElement)
