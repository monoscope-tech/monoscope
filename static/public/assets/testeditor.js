import { LitElement, html, ref, createRef } from './js/thirdparty/lit.js';

export class Collection extends LitElement {
  static properties = {
    collection: {},
    showNewStepModal: {},
    showCode: {},
  };
  collections = [];
  pid = '';
  cold_id = '';

  constructor() {
    super();
    const segs = window.location.pathname.split('/');
    this.pid = segs[2];
    this.col_id = segs[4];
    const dataStore = document.getElementById('test-data').dataset.collection;
    this.collection = JSON.parse(dataStore);
    this.showCode = false;
    this.addEventListener('add-step', this.handleAddStep);
    this.addEventListener('close-modal', () => {
      this.showNewStepModal = false;
    });

    this.addEventListener('edit-step', async (event) => {
      const { step, ind } = event.detail.data;
      this.collection.steps[ind] = step;
      await this.saveSteps();
    });

    this.addEventListener('delete-step', async (event) => {
      const step_index = event.detail.step;
      const steps = this.collection.steps;
      this.collection.steps = steps.filter((s, ind) => step_index !== ind);
      await this.saveSteps();
    });
  }

  async handleAddStep(event) {
    const step = event.detail.data;
    this.collection.steps.push(step);
    await this.saveSteps();
  }

  async saveSteps() {
    const response = await fetch(
      `/p/${this.pid}/testing/${this.col_id}/update_steps`,
      {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(this.collection.steps),
      }
    );

    if (!response.ok) {
      alert('Something went wrong');
      return;
    }
    this.collection = { ...this.collection };
    this.showNewStepModal = false;
    const event = new CustomEvent('successToast', {
      detail: {
        value: ['Steps updated successfully'],
      },
    });
    document.querySelector('body').dispatchEvent(event);
  }

  render() {
    return html`<div class="w-full px-16">
      ${this.showNewStepModal ? html`<step-modal></step-modal>` : null}
      <div class="flex flex-col gap-2 h-48 border-b pt-8">
        <h4 class="text-3xl font-medium text-gray-800">
          ${this.collection.title}
        </h4>
        <p class="text-gray-500 max-w-xl">${this.collection.description}</p>
        <div class="flex justify-between w-1/2 items-center mt-auto pr-3">
          <h6 class="font-medium text-2xl text-gray-600">Steps</h6>
          <div class="flex gap-8 items-center">
            <button title="run all" class="text-blue-500 text-3xl">
              <i class="fa fa-play" aria-hidden="true"></i>
            </button>

            <label class="relative inline-flex items-center cursor-pointer" x>
              <input
                type="checkbox"
                value=""
                class="sr-only peer"
                @click=${() => {
                  if (!this.showCode) {
                    const yamlData = jsyaml.dump(this.collection.steps, {
                      indent: 2,
                    });
                    setTimeout(() => {
                      const editor = CodeMirror(
                        document.getElementById('test-editor'),
                        {
                          value: yamlData,
                          mode: 'yaml',
                          lineNumbers: true,
                          theme: 'dracula',
                        }
                      );
                      window.testEditor = editor;
                    });
                    this.showCode = true;
                  } else {
                    if (window.testEditor) {
                      const val = window.testEditor.getValue();
                      const data = jsyaml.load(val);
                      data.map((step) => {
                        if (step.json) {
                          step.json =
                            typeof step.json === 'string'
                              ? step.json
                              : JSON.stringify(step.json);
                        }
                      });
                      this.collection.steps = data;
                    }
                    this.showCode = false;
                    this.saveSteps();
                  }
                }}
              />
              <div
                class="w-9 h-3 bg-gray-200 peer-focus:outline-none peer-focus:ring-blue-300 dark:peer-focus:ring-blue-800 rounded-full peer peer-checked:after:translate-x-full rtl:peer-checked:after:-translate-x-full after:content-[''] after:absolute after:top-[0] after:start-[0] after:bg-white after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all peer-checked:bg-blue-600"
              ></div>
              <span
                class="ms-3 text-sm font-medium text-gray-900 dark:text-gray-300"
                >Code</span
              >
            </label>
          </div>
        </div>
      </div>
      <div class="w-full grid grid-cols-2 h-[calc(100vh-220px)]">
        ${this.showCode
          ? html`<div
              id="test-editor"
              class="w-full h-full overflow-y-hidden border-r"
            ></div>`
          : html` <div
              class="flex flex-col overflow-y-auto border-r pr-3 h-full gap-4 py-3"
            >
              ${this.collection.steps?.map(
                (step, ind) =>
                  html`<step-element .data=${step} ind=${ind}></step-element>`
              )}
              ${this.collection.steps?.length === 0
                ? html`<div
                    class="self-center text-center text-lg max-w-lg mt-10 font-medium text-gray-700"
                  >
                    This collection has no test steps, click the plus button to
                    add test steps
                  </div>`
                : null}
              <button
                class="bg-blue-500 px-2 py-1 self-center h-10 w-10 text-2xl rounded-full text-white active:ring-1"
                @click=${() => (this.showNewStepModal = true)}
              >
                +
              </button>
            </div>`}
        <div class="h-full p-3 overflow-y-scroll">
          <h3 class="mt-10 w-full text-center">
            Run test to see the results appear here
          </h3>
        </div>
      </div>
    </div>`;
  }
  createRenderRoot() {
    return this;
  }
}

customElements.define('test-editor', Collection);

class Step extends LitElement {
  static properties = {
    data: {},
    ind: {},
    showDetails: {},
    editModal: {},
  };
  method = '';
  constructor() {
    super();
    this.showDetails = false;
    this.editModal = false;
    this.ind = 0;
    this.addEventListener('close-modal', () => {
      this.editModal = false;
    });
    // For editing step
    this.addEventListener('add-step', (event) => {
      event.stopPropagation();
      const step = event.detail.data;
      const e = new CustomEvent('edit-step', {
        detail: {
          data: { step: step, ind: this.ind },
        },
        bubbles: true,
        composed: true,
      });
      this.dispatchEvent(e);
      this.editModal = false;
    });
  }

  getEntries(val) {
    if (!val) {
      return [['', '']];
    }
    const arr = Object.entries(val);
    if (arr.length === 0) {
      return [['', '']];
    }
    return arr;
  }

  getAssertEntries(val) {
    if (!val) {
      return [['', '']];
    }
    const arr = val.map((v) => Object.entries(v)[0]);
    if (arr.length === 0) {
      return [['', '']];
    }
    return arr;
  }
  getMethodUrl(data) {
    const methods = [
      'GET',
      'POST',
      'PATCH',
      'PUT',
      'DELETE',
      'HEAD',
      'OPTIONS',
    ];
    for (let key in data) {
      if (methods.includes(key.toUpperCase())) {
        return [key.toUpperCase(), data[key]];
      }
    }
    return ['GET', ''];
  }
  getMethodColor(color) {
    switch (this.getMethodUrl(this.data)[0]) {
      case 'POST':
        return 'text-blue-500';
      case 'PATCH':
        return 'text-purple-500';
      case 'DELETE':
        return 'text-red-500';
      case 'PUT':
        return 'text-orange-500';
      case 'GET':
        return 'text-green-500';
      default:
        return 'text-gray-500';
    }
  }
  render() {
    return html`<article class="border rounded-lg overflow-hidden relative">
      ${this.editModal
        ? html`<step-modal
            title=${this.data.title}
            url=${this.getMethodUrl(this.data)[1]}
            method=${this.getMethodUrl(this.data)[0]}
            .asserts=${this.getAssertEntries(this.data.asserts)}
            .headers=${this.getEntries(this.data.headers)}
            .exports=${this.getEntries(this.data.exports)}
            .params=${this.getEntries(this.data.params)}
            .body=${this.data.json
              ? { current: 'json', json: this.data.json }
              : { current: 'form-data', url: this.getEntries(this.data.body) }}
          ></step-modal>`
        : null}
      <div
        class="absolute text-gray-600 bg-gray-50 px-4  flex items-center gap-3 right-2 translate-y-1/2"
      >
        <button class="text-blue-500 text-xl">
          <i class="fa fa-play" aria-hidden="true"></i>
        </button>
        <button @click=${() => (this.editModal = true)}>
          <i class="fa fa-regular fa-edit" aria-hidden="true"></i>
        </button>
        <button class="text-red-500">
          <i class="fa fa-trash" aria-hidden="true"></i>
        </button>
      </div>
      <button
        class="text-left bg-gray-50 p-3 w-full flex flex-col"
        @click=${(e) => (this.showDetails = !this.showDetails)}
      >
        <div class="flex items-center gap-3">
          <span class="text-gray-600 font-medium">${this.data.title}</span>
          <!-- <div class="flex gap-1 items-center text-xs text-gray-500">
            <span class="font-bold text-green-500"
              >${this.getMethodUrl(this.data)[0]}</span
            >
            <span>${this.getMethodUrl(this.data)[1]}</span>
          </div> -->
        </div>
        <span class="text-green-500 text-xs">passed</span>
      </button>
      ${this.showDetails
        ? html`<div class="bg-white p-3 w-full flex flex-col gap-4">
            <div class="flex gap-1 items-center mt-2 text-gray-500">
              <span class=${'font-semibold ' + this.getMethodColor()}
                >${this.getMethodUrl(this.data)[0]}</span
              >
              <span>${this.getMethodUrl(this.data)[1]}</span>
            </div>
            ${this.data.json
              ? html`
                  <div class="w-full">
                    <h6 class="mb-1 font-medium text-gray-800">Body (json)</h6>
                    <div class="font-mono text-sm bg-gray-100 p-3 rounded-lg">
                      ${this.data.json}
                    </div>
                  </div>
                `
              : null}
            ${this.data.body
              ? html`
                  <key-val
                    .data=${this.data.body}
                    stitle="Body (url-encoded)"
                  ></key-val>
                `
              : null}
            ${this.data.headers
              ? html`<key-val
                  .data=${this.data.headers}
                  stitle="Headers"
                ></key-val>`
              : null}
            ${this.data.params
              ? html`<key-val
                  .data=${this.data.params}
                  stitle="Params"
                ></key-val>`
              : null}
            ${this.data.asserts
              ? html`<asserts-val
                  .data=${this.data.asserts}
                  stitle="Asserts"
                ></asserts-val>`
              : null}
            ${this.data.exports
              ? html`<key-val
                  .data=${this.data.exports}
                  stitle="Exports"
                ></key-val>`
              : null}
          </div>`
        : null}
    </article>`;
  }
  createRenderRoot() {
    return this;
  }
}
customElements.define('step-element', Step);

class KeyVal extends LitElement {
  static properties = {
    stitle: { type: String },
    data: {},
  };
  constructor() {
    super();
    this.data = {};
    this.stitle = '';
  }

  render() {
    return html`
      <div class="w-full">
        <h6 class="mb-1 font-medium text-gray-800">${this.stitle}</h6>
        <div class="w-full flex flex-col gap-1">
          ${Object.entries(this.data).map(
            (kv) => html`<div class="flex gap-4 items-center w-full">
              <span
                class="text-sm w-full border border-dashed text-gray-700 px-2 p-1 rounded-lg"
                >${kv[0]}</span
              >
              <span
                class="text-sm w-full border border-dashed px-2 py-0.5 text-gray-500 rounded-lg"
                >${kv[1]}</span
              >
            </div>`
          )}
        </div>
      </div>
    `;
  }
  createRenderRoot() {
    return this;
  }
}
customElements.define('key-val', KeyVal);

class AssertsVal extends LitElement {
  static properties = {
    stitle: { type: String },
    data: {},
  };
  constructor() {
    super();
    this.data = {};
    this.stitle = '';
  }

  render() {
    return html`
      <div class="w-full">
        <h6 class="mb-1 font-medium text-gray-800">${this.stitle}</h6>
        <div class="w-full flex flex-col gap-1">
          ${this.data.map((as) => {
            const kv = Object.entries(as);
            return html`<div class="flex gap-4 items-center w-full">
              <span
                class="text-sm w-full border border-dashed text-gray-700 px-2 p-1 rounded-lg"
                >${kv[0][0]}</span
              >
              <span
                class="text-sm w-full border border-dashed px-2 py-0.5 text-gray-500 rounded-lg"
                >${kv[0][1]}</span
              >
            </div>`;
          })}
        </div>
      </div>
    `;
  }
  createRenderRoot() {
    return this;
  }
}
customElements.define('asserts-val', AssertsVal);

class NewStepModal extends LitElement {
  static properties = {
    method: {},
    title: {},
    url: {},
    methods: {},
    showMethods: {},
    params: {},
    headers: {},
    body: {},
    asserts: {},
    currentTab: {},
    exports: {},
    errors: {},
  };
  staticMethods = ['GET', 'POST', 'PATCH', 'PUT', 'DELETE', 'HEAD', 'OPTIONS'];
  constructor() {
    super();
    this.methods = this.staticMethods;
    this.method = '';
    this.url = '';
    this.errors = [];
    this.title = '';
    this.params = [['', '']];
    this.headers = [['', '']];
    this.asserts = [['', '']];
    this.body = { current: 'json', json: '', url: [['', '']] };
    this.exports = [['', '']];
    this.showMethods = false;
    this.currentTab = 'Params';
    document.addEventListener('click', () => {
      this.showMethods = false;
    });
    this.addEventListener('update-headers', (event) => {
      const data = event.detail.data;
      const last = data[data.length - 1];
      if (last[0] && last[1]) {
        data.push(['', '']);
      }
      this.headers = [...data];
    });
    this.addEventListener('update-params', (event) => {
      const data = event.detail.data;
      const last = data[data.length - 1];
      if (last[0] && last[1]) {
        data.push(['', '']);
      }
      this.params = [...data];
    });
    this.addEventListener('update-body', (event) => {
      const data = event.detail.data;
      if (data.current !== 'json') {
        const last = data.url[data.url.length - 1];
        if (!last || (last[0] && last[1])) {
          data.url.push(['', '']);
        }
      }
      this.body = { ...data };
    });
    this.addEventListener('update-asserts', (event) => {
      const data = event.detail.data;
      const last = data[data.length - 1];
      if (last[0] && last[1]) {
        data.push(['', '']);
      }
      this.asserts = [...data];
    });
    this.addEventListener('update-exports', (event) => {
      const data = event.detail.data;
      const last = data[data.length - 1];
      if (last[0] && last[1]) {
        data.push(['', '']);
      }
      this.exports = [...data];
    });
  }
  validateStep(asserts, method, url, title) {
    let errors = [];
    if (title.trim() === '') {
      errors.push("Title can't not be empty");
    }
    if (!method || !this.staticMethods.includes(method.toUpperCase())) {
      errors.push('Invalid method');
    }
    if (!url || url.trim() === '') {
      errors.push('Invalid url');
    }

    const allAsserts = [
      'exists',
      'number',
      'string',
      'boolean',
      'ok',
      'empty',
      'notEmpty',
    ];
    if (asserts) {
      asserts = asserts.filter((kv) => kv[0] && kv[1]);
      asserts.forEach((assert) => {
        if (!allAsserts.includes(assert[0])) {
          errors.push(`${assert[0]}: is not a valid assertion`);
        }
      });
    }
    return errors;
  }

  sanitize(step) {
    delete step['method'];
    delete step['url'];
  }

  buildStep() {
    const asserts = this.asserts
      .filter((a) => a[0] && a[1])
      .map((assert) => {
        return { [assert[0]]: assert[1] };
      });
    const headers = this.twoDtoObject(this.headers);
    const exports = this.twoDtoObject(this.exports);
    const params = this.twoDtoObject(this.params);
    const body = this.twoDtoObject(this.body.url);
    const stepObject = {
      method: this.method,
      url: this.url,
      [this.method]: this.url,
      title: this.title,
      asserts: asserts.length > 0 ? asserts : undefined,
      exports,
      headers,
      params,
      json: this.body.current === 'json' ? this.body.json : undefined,
      body: this.body.current != 'json' ? body : undefined,
    };
    let errs = this.validateStep(
      this.asserts,
      this.method,
      this.url,
      this.title
    );
    if (errs.length > 0) {
      this.errors = errs;
      return;
    }
    this.sanitize(stepObject);
    const event = new CustomEvent('add-step', {
      detail: {
        data: stepObject,
      },
      bubbles: true,
      composed: true,
    });
    this.dispatchEvent(event);
  }

  twoDtoObject(array2D) {
    if (!array2D) return undefined;
    array2D = array2D.filter((kv) => kv[0] && kv[1]);
    if (array2D.length === 0) return undefined;
    const resultObject = array2D.reduce((obj, [key, val]) => {
      obj[key] = val;
      return obj;
    }, {});
    return resultObject;
  }

  render() {
    return html`
      <div
        class="fixed inset-0 z-50 w-screen overflow-y-auto bg-gray-300 bg-opacity-50"
        id="modal-bg"
        @click=${(e) => {
          const event = new CustomEvent('close-modal', {
            detail: {},
            bubbles: true,
            composed: true,
          });
          this.dispatchEvent(event);
        }}
      >
        <div
          class="flex min-h-full items-end justify-center p-4 text-center sm:items-center sm:p-0"
        >
          <div
            class="relative transform overflow-hidden rounded-lg bg-white text-left transition-all  w-full max-w-2xl"
            @click=${(e) => {
              e.stopPropagation();
              this.showMethods = false;
            }}
          >
            <div class="bg-white">
              <h3
                class="text-lg  w-full px-6 py-4 border-b font-semibold leading-6 text-gray-700"
                id="modal-title"
              >
                Step details
              </h3>
              <div
                class="px-6 py-4 items-start flex flex-col gap-5 text-gray-700 h-[60vh] overflow-y-auto"
              >
                ${this.errors.length > 0
                  ? html`<div>
                      <button
                        @click=${() => (this.errors = [])}
                        class="text-red-700"
                      >
                        x
                      </button>
                      ${this.errors.map(
                        (err) =>
                          html`<p class="text-red-500 text-sm">${err}</p>`
                      )}
                    </div>`
                  : null}
                <div class="flex flex-col gap-1 w-full">
                  <label for="title" class="text-sm font-medium leading-none"
                    >Title</label
                  >
                  <input
                    .value=${this.title}
                    class="flex h-9 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm transition-colors placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
                    @keyup=${(e) => (this.title = e.target.value)}
                    placeholder="Test Profile edit"
                  />
                </div>
                <div class="flex  mt-2 gap-2 w-full">
                  <div class="flex flex-col gap-1 relative">
                    <label for="title" class="text-sm font-medium leading-none"
                      >Method</label
                    >
                    <div class="relative" @click=${(e) => e.stopPropagation()}>
                      <input
                        placeholder="method"
                        .value=${this.method}
                        @focus=${() => (this.showMethods = true)}
                        @keyup=${(e) => {
                          this.method = e.target.value;
                          let matched = this.methods.filter((method) =>
                            method
                              .toLowerCase()
                              .startsWith(e.target.value.toLowerCase())
                          );
                          if (matched.length > 0 && e.target.value.length > 0) {
                            this.methods = matched;
                          } else {
                            this.methods = this.staticMethods;
                          }
                        }}
                        class="flex h-9 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm transition-colors placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
                      />
                      <span class="rotate-90 absolute right-2 top-2">></span>
                      ${this.showMethods
                        ? html` <div
                            class="w-full flex flex-col left-0 shadow-md rounded-lg bg-white z-10 absolute top-[100%]"
                          >
                            ${this.methods.map(
                              (method) =>
                                html`<button
                                  class="px-4 py-2 text-left w-full hover:bg-gray-200 text-sm"
                                  @click=${(e) => {
                                    this.method = method;
                                    this.showMethods = false;
                                  }}
                                >
                                  ${method}
                                </button>`
                            )}
                          </div>`
                        : null}
                    </div>
                  </div>
                  <div class="flex flex-col gap-1 relative w-full">
                    <label for="title" class="text-sm font-medium leading-none"
                      >URL</label
                    >
                    <input
                      placeholder="Request URL"
                      .value=${this.url}
                      @input=${(e) => (this.url = e.target.value)}
                      class="flex h-9 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm transition-colors placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
                    />
                  </div>
                </div>
                <div class="w-full mt-2">
                  <nav class="w-full">
                    <ul
                      class="flex gap-10 text-sm font-medium border-b w-full items-center text-gray-500"
                    >
                      <li>
                        <button
                          @click=${() => (this.currentTab = 'Params')}
                          class="py-2 ${this.currentTab === 'Params'
                            ? 'border-b border-b-blue-500 text-blue-500 font-medium'
                            : ''}"
                        >
                          Params
                        </button>
                      </li>
                      <li>
                        <button
                          @click=${() => (this.currentTab = 'Headers')}
                          class="py-2 ${this.currentTab === 'Headers'
                            ? 'border-b border-b-blue-500 text-blue-500 font-medium'
                            : ''}"
                        >
                          Headers
                        </button>
                      </li>
                      ${this.method !== 'GET'
                        ? html`<li>
                            <button
                              @click=${() => (this.currentTab = 'Body')}
                              class="py-2 ${this.currentTab === 'Body'
                                ? 'border-b border-b-blue-500 text-blue-500 font-medium'
                                : ''}"
                            >
                              Body
                            </button>
                          </li>`
                        : null}
                    </ul>
                  </nav>
                  <div class="py-3">
                    ${this.currentTab === 'Params'
                      ? html`<params-element
                          .params=${this.params}
                          eventName="update-params"
                        ></params-element>`
                      : this.currentTab === 'Headers'
                      ? html`<headers-element
                          .headers=${this.headers}
                        ></headers-element>`
                      : html`<body-element .body=${this.body}></body-element>`}
                  </div>
                </div>
                <div class="w-full">
                  <assert-element .asserts=${this.asserts}></assert-element>
                </div>
                <div class="w-full">
                  <h6
                    class="text-sm font-medium mb-2 w-full items-center text-gray-500"
                  >
                    Exports
                  </h6>
                  <params-element
                    .params=${this.exports}
                    eventName="update-exports"
                  ></params-element>
                </div>
              </div>
            </div>

            <div
              class=" px-4 py-3 sm:flex sm:flex-row-reverse sm:px-6 border-t"
            >
              <button
                type="button"
                @click=${(e) => {
                  this.buildStep();
                }}
                class="inline-flex w-full justify-center rounded-md bg-blue-500 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-blue-600 sm:ml-3 sm:w-[100px]"
              >
                Save
              </button>
              <button
                type="button"
                @click=${() => {
                  const event = new CustomEvent('close-modal', {
                    detail: {},
                    bubbles: true,
                    composed: true,
                  });
                  this.dispatchEvent(event);
                }}
                class="mt-3 inline-flex w-full justify-center rounded-md bg-white px-3 py-2 text-sm font-semibold text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 hover:bg-gray-50 sm:mt-0 sm:w-[100px]"
              >
                Cancel
              </button>
            </div>
          </div>
        </div>
      </div>
    `;
  }

  createRenderRoot() {
    return this;
  }
}

customElements.define('step-modal', NewStepModal);

class ParamsElement extends LitElement {
  static properties = {
    params: {},
    eventName: {},
  };
  constructor() {
    super();
    this.params = [['', '']];
    this.eventName = 'update-params';
  }
  sendEvent(data) {
    const event = new CustomEvent(this.eventName, {
      detail: { data: data },
      bubbles: true,
      composed: true,
    });
    this.dispatchEvent(event);
  }
  render() {
    return html`<div class="flex flex-col gap-2">
      ${this.params.map((p, ind) => {
        const [k, v] = p;
        return html`<div class="flex gap-2" key=${ind}>
          <input
            class="flex h-7 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
            placeholder="key"
            value=${k}
            @keyup=${(e) => {
              this.params[ind][0] = e.target.value;
              this.sendEvent(this.params);
            }}
          />
          <input
            class="flex h-7 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
            placeholder="value"
            value=${v}
            @keyup=${(e) => {
              this.params[ind][1] = e.target.value;
              this.sendEvent(this.params);
            }}
          />
          <button
            class="text-red-500 rounded shrink-0"
            @click=${(e) => {
              if (this.params.length === 1) return;
              this.params = this.params.filter((v, i) => i != ind);
              this.sendEvent(this.params);
            }}
          >
            <i class="fa fa-trash" aria-hidden="true"></i>
          </button>
        </div>`;
      })}
    </div>`;
  }
  createRenderRoot() {
    return this;
  }
}

customElements.define('params-element', ParamsElement);

class Headers extends LitElement {
  static properties = {
    headers: {},
    showHeaders: {},
    headerComp: {},
  };
  allHeaders = [
    'Authorization',
    'Content-Type',
    'Allowed-Methods',
    'Host',
    'Referer',
    'Origin',
  ];
  constructor() {
    super();
    this.headers = [['', '']];
    this.headersComp = this.allHeaders;
    this.addEventListener('update-header', (event) => {
      const { val, target } = event.detail;
      this.headers[target][0] = val;
      this.sendEvent(this.headers);
    });
  }
  sendEvent(data) {
    const event = new CustomEvent('update-headers', {
      detail: { data: data },
      bubbles: true,
      composed: true,
    });
    this.dispatchEvent(event);
  }
  render() {
    return html`<div class="flex flex-col gap-2">
      ${this.headers.map(
        (p, ind) => html`<div class="flex gap-2">
          <custom-select
            .val=${p[0]}
            .target=${ind}
            .options=${this.allHeaders}
            placeholder=${'header'}
            eventName=${'update-header'}
          ></custom-select>
          <input
            class="flex h-7 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
            placeholder="value"
            .value=${p[1]}
            @keyup=${(e) => {
              this.headers[ind][1] = e.target.value;
              this.sendEvent(this.headers);
            }}
          />
          <button
            class="text-red-500 rounded shrink-0"
            @click=${(e) => {
              if (this.headers.length === 1) return;
              this.headers = this.headers.filter((v, i) => i != ind);
              this.sendEvent(this.headers);
            }}
          >
            <i class="fa fa-trash" aria-hidden="true"></i>
          </button>
        </div>`
      )}
    </div>`;
  }
  createRenderRoot() {
    return this;
  }
}

customElements.define('headers-element', Headers);

class BodyElement extends LitElement {
  static properties = {
    body: {},
  };

  constructor() {
    super();
    this.body = { current: 'json', json: '', url: [['', '']] };
  }

  sendEvent(data) {
    const event = new CustomEvent('update-body', {
      detail: { data: data },
      bubbles: true,
      composed: true,
    });
    this.dispatchEvent(event);
  }

  render() {
    return html`<div>
      <select
        .value=${this.body.current}
        @change=${(e) => {
          this.body.current = e.target.value;
          this.sendEvent(this.body);
        }}
      >
        <option value="json">json</option>
        <option value="url">url-encoded</option>
      </select>
      <div class="mt-2 flex flex-col gap-2">
        ${this.body.current === 'json'
          ? html`<textarea
              .value=${this.body.json}
              @keyup=${(e) => {
                this.body.json = e.target.value;
                this.sendEvent(this.body);
              }}
              class="flex h-36 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm transition-colors placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
              placeholder="json request body"
            ></textarea>`
          : this.body.url.map(
              (p, ind) => html`<div class="flex gap-2">
                <input
                  class="flex h-7 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
                  placeholder="key"
                  .value=${p[0]}
                  @keyup=${(e) => {
                    this.body.url[ind][0] = e.target.value;
                    this.sendEvent(this.body);
                  }}
                />
                <input
                  class="flex h-7 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
                  placeholder="value"
                  .value=${p[1]}
                  @keyup=${(e) => {
                    this.body.url[ind][1] = e.target.value;
                    this.sendEvent(this.body);
                  }}
                />
              </div>`
            )}
      </div>
    </div>`;
  }
  createRenderRoot() {
    return this;
  }
}
customElements.define('body-element', BodyElement);

class AssertsElement extends LitElement {
  static properties = {
    asserts: {},
    showAsserts: { state: true },
    assertComp: {},
  };
  allAsserts = [
    'exists',
    'number',
    'string',
    'boolean',
    'ok',
    'empty',
    'notEmpty',
  ];
  constructor() {
    super();
    this.asserts = [['', '']];
    this.showAsserts = false;
    this.assertsComp = this.allAsserts;
    this.addEventListener('update-assert', (event) => {
      const { val, target } = event.detail;
      this.asserts[target][0] = val;
      this.sendEvent(this.asserts);
    });
  }
  sendEvent(data) {
    const event = new CustomEvent('update-asserts', {
      detail: { data: data },
      bubbles: true,
      composed: true,
    });
    this.dispatchEvent(event);
  }

  render() {
    return html`<div class="py-3 w-full">
      <h6 class="text-sm font-medium mb-2 w-full items-center text-gray-500">
        Assertions
      </h6>
      <div class="flex flex-col gap-2">
        ${this.asserts.map(
          (p, ind) => html`<div class="flex gap-2 w-full">
            <custom-select
              .val=${p[0]}
              .target=${ind}
              .options=${this.allAsserts}
              placeholder=${'assert'}
              eventName=${'update-assert'}
            ></custom-select>
            <input
              class="flex h-7 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
              placeholder="jsonpath expression"
              .value=${p[1]}
              @keyup=${(e) => {
                this.asserts[ind][1] = e.target.value;
                this.sendEvent(this.asserts);
              }}
            />
            <button
              class="text-red-500 rounded shrink-0"
              @click=${(e) => {
                if (this.asserts.length === 1) return;
                this.asserts = this.asserts.filter((v, i) => i != ind);
                this.sendEvent(this.asserts);
              }}
            >
              <i class="fa fa-trash" aria-hidden="true"></i>
            </button>
          </div>`
        )}
      </div>
    </div>`;
  }

  createRenderRoot() {
    return this;
  }
}
customElements.define('assert-element', AssertsElement);

class CustomSelect extends LitElement {
  static properties = {
    showOptions: {},
    options: {},
    target: {},
    val: {},
    eventName: {},
    fOptions: { state: true },
    placeholder: {},
  };
  constructor() {
    super();
    this.options = [''];
    this.fOptions = [];
    this.target = 0;
    this.val = '';
    this.placeholder = 'key';
  }
  sendEvent(data) {
    const event = new CustomEvent(this.eventName, {
      detail: { val: data, target: this.target },
      bubbles: true,
      composed: true,
    });
    this.dispatchEvent(event);
  }

  render() {
    return html`
      <div class="relative w-full">
        <input
          placeholder=${this.placeholder}
          .value=${this.val}
          @focus=${() => (this.showOptions = true)}
          @keyup=${(e) => {
            this.sendEvent(e.target.value);
            let matched = this.options.filter((header) =>
              header.toLowerCase().startsWith(e.target.value.toLowerCase())
            );
            if (matched.length > 0 && e.target.value.length > 0) {
              this.fOptions = matched;
            } else if (e.target.value.length === 0) {
              this.fOptions = this.options;
            } else {
              this.showOptions = false;
            }
          }}
          class="flex h-7 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
        />
        <span class="rotate-90 absolute right-2 top-1">></span>
        ${this.showOptions
          ? html` <div
              class="w-full flex flex-col left-0 z-10 shadow-md rounded-lg bg-white absolute top-[100%] max-h-96 overflow-y-auto"
            >
              ${this.fOptions.map(
                (option) =>
                  html`<button
                    class="px-2 py-1 text-left w-full hover:bg-gray-200 text-xs"
                    @click=${(e) => {
                      this.sendEvent(option);
                      this.showOptions = false;
                    }}
                  >
                    ${option}
                  </button>`
              )}
            </div>`
          : null}
      </div>
    `;
  }
  createRenderRoot() {
    return this;
  }
}
customElements.define('custom-select', CustomSelect);
