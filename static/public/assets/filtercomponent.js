import { LitElement, html, ref, createRef } from 'https://cdn.jsdelivr.net/gh/lit/dist@3/all/lit-all.min.js';
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
    this.showFilterSearch = false
    this.addEventListener('add-filter', this.handleChildEvent)
    this.addEventListener('close-search', () => { this.showFilterSearch = false })
    const body = document.querySelector('body')
    this.filters = []
    window.setQueryBuilderFromParams = () => {
      const urlSearchParams = new URLSearchParams(window.location.search);
      const query = urlSearchParams.get("query")
      if (query) {
        const fls = query.split(/\s+AND\s+|\s+OR\s+/i).flatMap((element, index, array) => {
          return index < array.length - 1 ? [element, 'AND'] : [element];
        });
        this.upadteFilters(fls)
      }
    }

    body.addEventListener('click', () => {
      this.showFilterSearch = false
    })
  }

  setBuilderValue = (value) => {
    const fls = value.split(/\s+AND\s+|\s+OR\s+/i).flatMap((element, index, array) => {
      return index < array.length - 1 ? [element, 'AND'] : [element];
    });
    this.upadteFilters(fls)
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
    let joiner = "AND"
    const [newField, newOperator, value] = this.getFieldAndValue(event.detail.filter)
    for (let filter of this.filters) {
      const [field, operator, val] = this.getFieldAndValue(filter)
      if (newField == field) {
        joiner = "OR"
      }
      if (newField === field && newOperator === operator && value === val) {
        this.showFilterSearch = false
        return
      }
    }

    if (this.filters.length === 0) {
      this.upadteFilters([event.detail.filter])
    } else {
      this.upadteFilters([...this.filters, joiner, event.detail.filter])
    }
    this.showFilterSearch = false
  }

  upadteFilters(newVal) {
    this.filters = newVal
    const val = newVal.join(" ")
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
    this.upadteFilters(this.filters.filter(f => f != filter))
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

  toggleJoinOperator(index) {
    if (this.filters[index] === "AND") {
      this.filters[index] = "OR"
    } else {
      this.filters[index] = "AND"
    }
    this.upadteFilters([...this.filters])
  }

  render() {
    return html`
  <div class="relative w-full" @click=${(e) => e.stopPropagation()}>
    <div class="flex items-center flex-wrap gap-2 m-2 border border-1 border-slate-400 px-4 py-2 w-ful rounded">
           <i class="fa-regular fa-filter h-4 w-4 text-gray-500"></i>
            <div class="flex flex-wrap gap-2">
             ${this.filters.map((filter, index) => html` ${filter === "AND" || filter === "OR" ?
      html`<button type="button" @click=${() => this.toggleJoinOperator(index)} class="text-gray-500 bg-gray-100  px-2 py-1 rounded-full">
        ${filter}
        <i class="fa-solid fa-sliders-simple"></i>
        </button>` :
      html` <button  type="button" class="bg-blue-50 shrink-0 text-sm font-bold px-2 text-blue-500 rounded-lg py-1">
                              ${filter} 
                              ${html`<span class="ml-2 text-xs hover:bg-blue-200 p-1 rounded-full" @click=${() => this.removeFilter(filter)}>
                                     <i class="fa-sharp fa-xmark"></i>
                              </span>`}
                              </button>`
      } `
    )}
         ${this.showFilterSearch ? html`<filter-suggestions></filter-suggestions>` : this.filters.length == 0 ?
        html`<button type="button" @click=${() => this.showFilterSearch = !this.showFilterSearch} class="text-gray-500" >Click to add filter...</button>`
        :
        html`<button type="button" @click=${() => this.showFilterSearch = !this.showFilterSearch} class="px-2 py-1 border rounded text-gray-500 hover:bg-gray-100"><i class="fa-solid fa-plus"></i></button>`
      }
      
         </div>
    </div>
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
    "method", "status_code", "url_path", "duration_ns", "request_body", "request_header", "response_body", "response_header",
    "host", "raw_url", "referer", "query_param", "path_param", "request_type"
  ]

  string_operators = ["=", "!="]
  number_operators = ["=", "!=", ">=", "<="]

  filterAutoComplete = {
    method: {
      type: "string",
      operators: this.string_operators,
      values: ["GET", "POST", "PUT", "DELETE", "PATCH", "HEAD"]
    },
    status_code: {
      type: "number",
      operators: this.number_operators,
      values: [200, 201, 400, 404, 500, 300, 100]
    },
    request_type: {
      type: "string",
      operators: this.string_operators,
      values: ["Outgoing", "Incoming"]
    },
    duration_ns: { operators: this.number_operators, type: "number", values: [] },
    url_path: { operators: this.string_operators, values: [] },
    raw_url: { operators: this.string_operators, values: [] }
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
    this.matches = this.fields
    this.projectId = window.location.pathname.split("/")[2]
    const builderContainer = document.getElementById("queryBuilder")
    if (builderContainer) {
      const url_paths = JSON.parse(builderContainer.dataset.url_paths)
      this.filterAutoComplete.url_path.values = (url_paths || []).sort()
      this.filterAutoComplete.raw_url.values = (JSON.parse(builderContainer.dataset.raw_urls) || []).sort()
    }

  }

  inputRef = createRef();

  render() {
    return html`
        <div class="relative  text-gray-500">
         <input type="text" autofocus
               ${ref(this.inputRef)}
               class="border px-2 py-1 w-max rounded-xl focus:ring-1"
              @input=${(event) => {
        this.handleChange(event.target.value)
        this.adjustInputWidthAndFocus()
      }} 
              .value = ${this.inputVal}
             @keydown=${(e) => {
        if (e.key === "Enter") {
          this.triggerCustomEvent(e.target.value)
        }
      }
      }
/>
  <div class="flex flex-col border overflow-auto p-4 gap-2 text-left shadow bg-white w-96 left-1/2 -translate-x-1/2 absolute z-50 -bottom-3" style="max-height:500px">
    ${this.matches.map(
        (match) => html`
                   <button type="button"  class="match_buttons px-4 py-1 text-base text-left hover:bg-gray-100" 
                   @click=${(e) => {
            this.autoCompleteInput(match)
            this.adjustInputWidthAndFocus()
          }}
                    @keydown=${(e) => {
            if (e.key === "Enter") {
              this.autoCompleteInput(match)
              this.adjustInputWidthAndFocus()
            }
            if (e.key === 'Tab') {
              e.preventDefault();
              const buttons = document.querySelectorAll('.match_buttons');
              const activeButton = document.activeElement;
              const index = Array.from(buttons).indexOf(activeButton);

              if (index !== -1) {
                const nextIndex = (index + 1) % buttons.length;
                buttons[nextIndex].focus();
              }
            }
          }
          }
          >${match}</button>
                 `
      )}
  </div>
          </div >
        </div >
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

  getValue(filter) {
    const parts = filter.replace(" ", "").trim().split(/\s*([=<>!]+)\s*/);
    if (parts.length !== 3) {
      return ""
    }
    const [field, operator, value] = parts;
    return value

  }

  needsAutoComplete(filter) {
    const rootEnd = filter.indexOf(".")
    if (rootEnd == -1) return { result: false, field: null, prefix: null }

    const field = filter.substring(0, rootEnd)
    const prefix = filter.substring(rootEnd + 1)
    if (["request_body", "response_body", "request_header", "response_header", "query_param", "path_param"].includes(field)) {
      return {
        result: true,
        field: field,
        prefix: prefix
      }
    }
    if (rootEnd == -1) return { result: false, field: null, prefix: null }
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
      let target_info = this.filterAutoComplete[target]
      if (!target_info) {
        const key = this.needsAutoComplete(this.inputVal)
        if (key.result) {
          fetch(`/p/${this.projectId}/query_builder/autocomplete?category=${key.field}&prefix=.${key.prefix}`).then(res => res.json()).then(data => {
            this.matches = data.map(d => key.field + d)
          })
          return
        } else {
          target_info = { operators: this.string_operators, values: [] }
        }
      }

      if (filter == this.inputVal.trim() || filter.length > this.inputVal.length) {
        for (let op of target_info.operators) {
          auto_complete.push(`${filter} ${op} `)
        }
      } else {
        target_info.values.forEach(v => {
          const valTyped = this.getValue(this.inputVal)
          if (String(v).startsWith(valTyped) || String(`"${v}`).startsWith(valTyped) || String(`"${v} "`).startsWith(valTyped)) {
            if (target_info.type === "number") {
              auto_complete.push(`${this.inputVal.replace(valTyped, "")} ${v}`)
            } else {
              auto_complete.push(`${this.inputVal.replace(valTyped, "")} "${v}"`)
            }
          }
        })
      }
    })

    this.matches = auto_complete
  }
}
customElements.define('filter-suggestions', Filter)
