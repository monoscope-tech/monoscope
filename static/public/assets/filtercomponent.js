import { LitElement, html, css } from 'https://cdn.jsdelivr.net/gh/lit/dist@3/core/lit-core.min.js';
export class MyElement extends LitElement {

  static properties = {
    greeting: {},
    planet: {},
    filters: {},
    showFilterSearch: {}
  };

  constructor() {
    super();
    this.greeting = 'Hello';
    this.planet = 'World';
    this.filters = []
    this.showFilterSearch = false
    this.addEventListener('add-filter', this.handleChildEvent)

  }

  getFieldAndValue(filter) {
    const parts = filter.trim().split(/\s*([=<>!]+)\s*/);
    if (parts.length !== 3) {
      return []
    }
    const [field, operator, value] = parts;
    return [field, operator, value]
  }

  handleChildEvent(event) {
    let joiner = "&"
    const [newField, newOperator, value] = this.getFieldAndValue(event.detail.filter)
    for (let filter of this.filters) {
      const [field, operator, val] = this.getFieldAndValue(filter)
      if (newField == field) {
        joiner = "|"
      }
      if (newField === field && newOperator === operator && value === val) {
        this.showFilterSearch = false
        return
      }
    }
    if (this.filters.length === 0) {
      this.filters = [event.detail.filter]
    } else {
      this.filters = [...this.filters, joiner, event.detail.filter]
    }
    this.showFilterSearch = false
  }

  removeFilter(filter) {
    this.filters = this.filters.filter(f => f != filter)
  }
  toggleJoinOperator(index) {
    if (this.filters[index] === "&") {
      this.filters[index] = "||"

    } else {
      this.filters[index] = "&"
    }
    this.filters = [...this.filters]
  }

  render() {
    return html`
  <div class="relative w-full">
    <div class="flex items-center flex-wrap gap-2 m-2 border border-1 border-slate-400 px-4 py-2 w-ful rounded">
           <i class="fa-regular fa-filter h-4 w-4 text-gray-500"></i>
            <div class="flex flex-wrap gap-2">
             ${this.filters.map(
      (filter, index) => html` ${filter === "&" || filter === "|" ?
        html`<button type="button" @click=${() => this.toggleJoinOperator(index)} class="text-gray-500 bg-gray-100  px-2 py-1 rounded-full">
        ${filter}
        <i class="fa-solid fa-sliders-simple"></i>
        </button>` :
        html` <button  type="button" class="bg-green-50 shrink-0 text-sm font-bold px-2 text-green-500 rounded-lg py-1">
                              ${filter} 
                              ${html`<span class="ml-2 text-xs hover:bg-green-200 p-1 rounded-full" @click=${() => this.removeFilter(filter)}>
                                     <i class="fa-sharp fa-xmark"></i>
                              </span>`}
                              </button>`
        } `
    )}
         </div>
     ${this.filters.length == 0 ?
        html`<button type="button" @click=${() => this.showFilterSearch = !this.showFilterSearch} class="text-gray-500" >Click to add filter...</button>`
        :
        html`<button type="button" @click=${() => this.showFilterSearch = !this.showFilterSearch} class="px-2 py-1 border rounded text-gray-500 hover:bg-gray-100"><i class="fa-solid fa-plus"></i></button>`
      }
    </div>
    ${this.showFilterSearch ? html`<filter-suggestions></filter-suggestions>` : null}
  </div>
    `;
  }
  createRenderRoot() {
    return this
  }

}

customElements.define('filter-element', MyElement);


class Filter extends LitElement {
  fields = [
    "method", "status_code", "duration_ns", "request_body", "request_headers", "response_body", "response_headers",
    "host", "url_path", "raw_url", "referer", "query_params", "path_params"
  ]

  string_operators = ["=", "!="]
  number_operators = ["=", "!=", ">=", "<="]

  filterAutoComplete = {
    method: {
      type: "string",
      operators: this.string_operators,
      values: [`"GET"`, `"POST"`, `"PUT"`, `"DELETE"`, `"PATCH"`, `"HEAD"`]
    },
    status_code: {
      type: "number",
      operators: this.number_operators,
      values: [200, 201, 400, 404, 500, 300, 100]
    },
    duration_ns: { operators: this.number_operators, type: "number" },
    "others": { operators: this.string_operators, values: [""] }
  }

  static properties = {
    matches: {},
    inputVal: {},
  }

  constructor() {
    super()
    this.inputVal = ''
    this.matches = this.fields
    this.previous = "group"
  }

  render() {
    return html`
        <div class="z-10 h-[31.625rem] overflow-auto p-4 flex flex-col gap-2 shadow bg-white w-2/3 absolute left-1/2 -translate-x-1/2 -bottom-3 text-gray-500">
          <input type="text" class="border px-4 py-2 rounded focus:ring-1"
              @input=${(event) => {
        this.handleChange(event.target.value)
      }} 
              .value=${this.inputVal}
              @change=${(e) => {
        e.preventDefault();
        this.triggerCustomEvent(e.target.value)
      }} />
          <div>
            <div class="flex flex-col text-left">
             ${this.matches.map(
        (match) => html`
                   <button type="button"  class="px-4 py-1 text-base text-left hover:bg-gray-100" @click=${() => this.autoCompleteInput(match)}>${match}</button>
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

  autoCompleteInput(val) {
    this.inputVal = val
    if (this.isValidFilter(val)) {
      this.triggerCustomEvent(this.inputVal)
      this.previous = "filter"
    } else {
      if (val == "and" || val == "or") {
        this.previous = "group"
      }
      this.handleChange(val)
    }
  }

  isValidFilter(filter) {
    const parts = filter.trim().split(/\s*([=<>!]+)\s*/);
    if (parts.length !== 3) {
      return false;
    }
    const [field, operator, value] = parts;
    if (!field || !value) {
      return false
    }
    return true;
  }

  triggerCustomEvent(value) {
    if (!this.isValidFilter(value)) return
    const event = new CustomEvent('add-filter', {
      detail: {
        filter: value,
      },
      bubbles: true,
      composed: true,
    });
    this.dispatchEvent(event);
  }

  getField(filter) {
    const parts = filter.trim().split(/\s*([=<>!]+)\s*/);
    if (parts.length !== 3) {
      return ""
    }
    const [field, operator, value] = parts;
    return field
  }

  handleChange(val) {
    this.inputVal = val.trim()
    if (!this.inputVal) {
      this.matches = this.fields
      return
    }
    let filters = this.fields.filter(v => v.startsWith(this.inputVal) || this.inputVal.startsWith(v))
    let auto_complete = []
    filters.forEach(filter => {
      let target = filter
      if (target !== "method" && target !== "status_code" && target !== "duration_ns") {
        target = "others"
      }
      let target_info = this.filterAutoComplete[target]

      if (filter == this.inputVal.trim() || filter.length > this.inputVal.length) {
        for (let op of target_info.operators) {
          auto_complete.push(`${filter} ${op}`)
        }
      } else {
        target_info.values.forEach(v => {
          auto_complete.push(`${this.inputVal} ${v}`)

        })
      }

    })

    this.matches = auto_complete
  }
}
customElements.define('filter-suggestions', Filter)
