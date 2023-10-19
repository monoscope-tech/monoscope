import { LitElement, html, css } from 'https://cdn.jsdelivr.net/gh/lit/dist@3/core/lit-core.min.js';
export class MyElement extends LitElement {

  static properties = {
    greeting: {},
    planet: {},
    filters: {},
    showFilterSearch: {}
  };

  static styles = css`
    .filter_content_container {
      width: 80%;
      display: flex;
      flex-direction: column;
      border: 1px solid lightgrey;
      height: 500px;
      margine-inline: auto;
    }

    .filter_content_container > input {
      padding: 8px 10px;
      border: 1px solid black
    }

    .component_container {
      position: relative;
      width: 800px
    }

    .filter_container {
      border: 1px solid grey;
      display: flex;
      align-items: center;
      padding: 5px 8px;
    }

    .planet {
      color: var(--planet-color, blue);
    }
    
    .filter_list {
      display: flex;
      flex-gap: 5px;
      align-items: center
    }
  `;

  constructor() {
    super();
    this.greeting = 'Hello';
    this.planet = 'World';
    this.filters = []
    this.showFilterSearch = false
    this.addEventListener('add-filter', this.handleChildEvent)
  }

  handleChildEvent(event) {
    this.filters = [...this.filters, event.detail.filter]
    this.showFilterSearch = false
  }

  render() {
    return html`
  <div class="relative w-full">
    <div class="flex gap-4 border border-1 border-slate-400 px-4 py-2 w-ful rounded">
            <div class="flex gap-2">
             ${this.filters.map(
      (filter) => html`
                   <button  type="button" class="bg-green-100 px-2 rounded-lg py-1">${filter} ${html`<span>remove</span>`}</button>
                 `
    )}
         </div>
     ${this.filters.length == 0 ?
        html`<button type="button" @click=${() => this.showFilterSearch = true} class="text-gray-500" >Click to add filter...</button>`
        :
        html`<button type="button" @click=${() => this.showFilterSearch = true}>Add</button>`
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
  filters = [
    "method", "request_body", "request_headers", "response_body", "response_headers",
    "host", "url_path", "raw_url", "referer", "status_code", "query_params", "path_params"
  ]

  string_operators = ["=", "!="]
  number_operators = ["=", "!=", ">=", "<="]

  filterAutoComplete = {
    "method": {
      operators: this.string_operators,
      values: [`"GET"`, `"POST"`, `"PUT"`, `"DELETE"`, `"PATCH"`, `"HEAD"`]
    },
    "status_code": {
      operators: this.number_operators,
      values: [200, 201, 400, 404, 500, 300, 100]
    },
    "others": { operators: this.string_operators, values: [""] }
  }

  static properties = {
    matches: {},
    inputVal: {},
  }

  constructor() {
    super()
    this.inputVal = ''
    this.matches = []
  }

  render() {
    return html`
        <div class="z-10 h-[500px] p-4 flex flex-col gap-2 shadow bg-white w-1/2 absolute left-0 -bottom-2 text-gray-500">
          <input type="text" class="border px-4 py-2 rounded focus:ring-1" @input=${this.handleChange} .value=${this.inputVal} @change=${(e) => this.triggerCustomEvent(e.target.value)} />
          <div>
            <div class="flex flex-col gap-2 text-left">
             ${this.matches.map(
      (match) => html`
                   <button type="button" class="" @click=${() => this.autoCompleteInput(match)}>${match}</button>
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
    this.inputVal = val + " "
    this.triggerCustomEvent(val)
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

  handleChange(event) {
    if (event.key === "Enter") {
      this.triggerCustomEvent(event.target.value)
      return
    }
    this.inputVal = event.target.value.trim()
    let filters = this.filters.filter(v => v.startsWith(this.inputVal) || this.inputVal.startsWith(v))
    let auto_complete = []
    filters.forEach(filter => {
      let target = filter
      if (target !== "method" && target !== "status_code") {
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
