import {
  LitElement,
  html,
  ref,
  createRef,
} from 'https://cdn.jsdelivr.net/gh/lit/dist@3/all/lit-all.min.js';

export class Testkit extends LitElement {
  static properties = {
    filters: {},
    showFilterSearch: {},
  };

  constructor() {
    super();
    this.showFilterSearch = false;
    this.addEventListener('add-filter', this.handleChildEvent);
    this.addEventListener('close-search', () => {
      this.showFilterSearch = false;
    });
    const body = document.querySelector('body');
    this.filters = [];
    window.setQueryBuilderFromParams = () => {
      const urlSearchParams = new URLSearchParams(window.location.search);
      const query = urlSearchParams.get('query');
      if (query) {
        const fls = query
          .split(/\s+AND\s+|\s+OR\s+/i)
          .flatMap((element, index, array) => {
            return index < array.length - 1 ? [element, 'AND'] : [element];
          });
        this.upadteFilters(fls);
      }
    };

    body.addEventListener('click', () => {
      this.showFilterSearch = false;
    });
  }

  setBuilderValue = (value) => {
    const fls = value
      .split(/\s+AND\s+|\s+OR\s+/i)
      .flatMap((element, index, array) => {
        return index < array.length - 1 ? [element, 'AND'] : [element];
      });
    this.upadteFilters(fls);
  };

  getFieldAndValue(filter) {
    const parts = filter.trim().split(/\s*([=<>!]+)\s*/);
    if (parts.length !== 3) {
      return [];
    }
    const [field, operator, value] = parts;
    return [field, operator, value];
  }

  handleChildEvent(event) {
    let joiner = 'AND';
    const [newField, newOperator, value] = this.getFieldAndValue(
      event.detail.filter
    );
    for (let filter of this.filters) {
      const [field, operator, val] = this.getFieldAndValue(filter);
      if (newField == field) {
        joiner = 'OR';
      }
      if (newField === field && newOperator === operator && value === val) {
        this.showFilterSearch = false;
        return;
      }
    }

    if (this.filters.length === 0) {
      this.upadteFilters([event.detail.filter]);
    } else {
      this.upadteFilters([...this.filters, joiner, event.detail.filter]);
    }
    this.showFilterSearch = false;
  }

  upadteFilters(newVal) {
    this.filters = newVal;
    const val = newVal.join(' ');
    window.queryBuilderValue = val;
    if (window.editor) {
      window.editor.setValue(val);
    }
  }

  removeFilter(filter) {
    const index = this.filters.indexOf(filter);
    if (index > 0) {
      this.filters[index - 1] = filter;
    } else {
      if (this.filters.length > 0) {
        this.filters[index + 1] = filter;
      }
    }
    this.upadteFilters(this.filters.filter((f) => f != filter));
  }

  isValidFilter(filter) {
    const parts = filter.trim().split(/\s*([=<>!]+)\s*/);
    if (parts.length !== 3) {
      return false;
    }
    const [field, operator, value] = parts;
    if (!field || !value) {
      return false;
    }
    return true;
  }

  toggleJoinOperator(index) {
    if (this.filters[index] === 'AND') {
      this.filters[index] = 'OR';
    } else {
      this.filters[index] = 'AND';
    }
    this.upadteFilters([...this.filters]);
  }

  render() {
    return html`
      <div class="relative w-full" @click=${(e) => e.stopPropagation()}>
        <div
          class="flex items-center flex-wrap gap-2 m-2 border border-1 border-slate-400 px-4 py-2 w-ful rounded"
        >
          <i class="fa-regular fa-filter h-4 w-4 text-gray-500"></i>
          <div class="flex flex-wrap gap-2">
            ${this.filters.map(
              (filter, index) =>
                html`
                  ${filter === 'AND' || filter === 'OR'
                    ? html`<button
                        type="button"
                        @click=${() => this.toggleJoinOperator(index)}
                        class="text-gray-500 bg-gray-100  px-2 py-1 rounded-full"
                      >
                        ${filter}
                        <i class="fa-solid fa-sliders-simple"></i>
                      </button>`
                    : html` <button
                        type="button"
                        class="bg-blue-50 shrink-0 text-sm font-bold px-2 text-blue-500 rounded-lg py-1"
                      >
                        ${filter}
                        ${html`<span
                          class="ml-2 text-xs hover:bg-blue-200 p-1 rounded-full"
                          @click=${() => this.removeFilter(filter)}
                        >
                          <i class="fa-sharp fa-xmark"></i>
                        </span>`}
                      </button>`}
                `
            )}
          </div>
        </div>
      </div>
    `;
  }
  createRenderRoot() {
    return this;
  }
}

customElements.define('testkit-element', Testkit);
