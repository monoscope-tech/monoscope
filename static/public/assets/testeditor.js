import { LitElement, html, ref, createRef } from './js/thirdparty/lit.js';
import {
  METHODS,
  PostConfig,
  getEvent,
  triggerToastEvent,
  ASSERTS,
  validateYaml,
  getDeletedUpdatedAndNewSteps,
} from './testeditor-utils.js';

export class Collection extends LitElement {
  static properties = {
    collection: {},
    showNewStepModal: {},
    showCode: {},
    config: {},
    codeHasChanges: { type: Boolean },
    showSettings: {},
    runningAllTests: { type: Boolean },
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
    const steps = JSON.parse(
      document.getElementById('test-data').dataset.steps
    );

    this.collection = JSON.parse(dataStore);
    this.collection.steps = steps.map((step) => {
      return { id: step.id, lastRun: step.lastRun, ...step.stepData };
    });
    this.showCode = false;
    this.showSettings = false;
    this.runningAllTests = false;

    this.addEventListener('add-step', this.handleAddStep);
    this.addEventListener('close-modal', () => {
      this.showNewStepModal = false;
    });

    this.addEventListener('edit-step', async (event) => {
      const { step, ind } = event.detail.data;
      const target = this.collection.steps[ind];
      const targetId = target.id;
      const lastRun = target.lastRun;
      const errorEvent = getEvent('errorToast', {
        value: ['Something went wrong'],
      });
      try {
        const response = await fetch(
          `/p/${this.pid}/testing/step/${targetId}`,
          { ...PostConfig, body: JSON.stringify(step) }
        );
        if (!response.ok) {
          document.querySelector('body').dispatchEvent(errorEvent);
          return;
        }

        this.collection.steps[ind] = { id: targetId, lastRun, ...step };
        this.collection = { ...this.collection };
        this.showNewStepModal = false;
        const event = getEvent('successToast', {
          value: ['Step edited successfully'],
        });
        triggerToastEvent(event);
      } catch (err) {
        triggerToastEvent(errorEvent);
      }
    });

    this.addEventListener('delete-step', async (event) => {
      const step_index = event.detail.step;
      const target = this.collection.steps[step_index];
      const targetId = target.id;
      const errorEvent = getEvent('errorToast', {
        value: ['Something went wrong'],
      });

      try {
        const response = await fetch(
          `/p/${this.pid}/testing/step/${targetId}`,
          { method: 'DELETE' }
        );
        if (!response.ok) {
          document.querySelector('body').dispatchEvent(errorEvent);
          return;
        }
        this.collection.steps = this.collection.steps.filter(
          (s) => s.id !== targetId
        );
        this.collection = { ...this.collection };
        this.showNewStepModal = false;
        const event = getEvent('successToast', {
          value: ['Step edited successfully'],
        });
        triggerToastEvent(event);
      } catch (err) {
        triggerToastEvent(errorEvent);
      }
    });

    this.addEventListener('update-config', (event) => {
      this.collection.config = event.detail.config;
      this.updateCollection('config', this.collection.config);
    });
    this.addEventListener('update-schedule', (event) => {
      this.collection.schedule = event.detail.schedule;
      this.collection.isScheduled = event.detail.isScheduled;
      this.updateCollection('schedule', {
        schedule: this.collection.schedule,
        isScheduled: this.collection.isScheduled,
      });
    });
    this.addEventListener('close-settings', () => {
      this.showSettings = false;
    });
  }

  async handleAddStep(event) {
    const step = event.detail.data;
    const errorEvent = getEvent('errorToast', {
      value: ['Something went wrong'],
    });
    try {
      const response = await fetch(
        `/p/${this.pid}/testing/add_step/${this.col_id}`,
        { ...PostConfig, body: JSON.stringify(step) }
      );
      if (!response.ok) {
        document.querySelector('body').dispatchEvent(errorEvent);
        return;
      }
      this.collection = {
        ...this.collection,
        steps: [...this.collection.steps, step],
      };
      this.showNewStepModal = false;
      const event = getEvent('successToast', {
        value: ['Step added successfully'],
      });
      triggerToastEvent(event);
    } catch (err) {
      triggerToastEvent(errorEvent);
    }
  }

  async updateCollection(action, value) {
    const errorEvent = getEvent('errorToast', {
      value: ['Something went wrong'],
    });
    try {
      const response = await fetch(
        `/p/${this.pid}/testing/${this.col_id}/update_${action}`,
        { ...PostConfig, body: JSON.stringify(value) }
      );
      if (!response.ok) {
        document.querySelector('body').dispatchEvent(errorEvent);
        return;
      }
      this.collection = { ...this.collection };
      this.showNewStepModal = false;
      const event = getEvent('successToast', {
        value: [`${action} updated successfully`],
      });
      triggerToastEvent(event);
    } catch (err) {
      triggerToastEvent(errorEvent);
    }
  }

  toggleCode() {
    if (!this.showCode) {
      const yamlData = jsyaml.dump(this.collection.steps, {
        indent: 2,
      });
      setTimeout(() => {
        const editor = CodeMirror(document.getElementById('test-editor'), {
          value: yamlData,
          mode: 'yaml',
          lineNumbers: true,
          theme: 'dracula',
        });
        window.testEditor = editor;
      });
      this.showCode = true;
    } else {
      if (window.testEditor) {
        const val = window.testEditor.getValue();
        const data = validateYaml(val);
        if (data) {
          const current = this.collection.steps;
          this.collection.steps = data;
        }
      }
      this.showCode = false;
    }
  }

  async saveCode() {
    if (!this.codeHasChanges) return;
    if (window.testEditor) {
      const val = window.testEditor.getValue();
      const data = validateYaml(val);
      if (data) {
        const operations = getDeletedUpdatedAndNewSteps(
          this.collection.steps,
          data
        );
        // TODO: save updates (DELETE, UPDATED, AND NEW)
        this.collection.steps = data;
        this.codeHasChanges = false;
      }
    }
  }

  async runAllTests() {
    this.runningAllTests = true;
    try {
      const response = await fetch(
        `/p/${this.pid}/testing/${this.col_id}/run_all`,
        { method: 'POST' }
      );
      if (response.ok) {
        const json = await response.json();
      }
    } catch (error) {
    } finally {
      this.runningAllTests = false;
    }
  }

  render() {
    return html` ${this.showNewStepModal
        ? html`<step-modal></step-modal>`
        : null}
      <div class="w-full grid grid-cols-11 h-[calc(100vh-50px)]">
        <div class="flex flex-col gap-3 col-span-2 h-full px-4 pt-8 border-r">
          <h4 class="text-3xl font-medium text-gray-800">
            ${this.collection.title}
          </h4>
          <p class="text-gray-500 max-w-xl">${this.collection.description}</p>
          <div class="text-gray-700 text-sm">
            <div class="flex gap-2 items-center">
              <span>Created:</span>
              <span class="font-semibold"
                >${new Date(this.collection.createdAt).toLocaleString()}</span
              >
            </div>
            <div class="flex gap-2 items-center">
              <span>Last modified:</span>
              <span class="font-semibold"
                >${new Date(this.collection.updatedAt).toLocaleString()}</span
              >
            </div>
          </div>
          <div>
            <button
              class="self-center text-blue-500  text-xl font-bold"
              @click=${() => (this.showSettings = true)}
            >
              <i class="fa-solid fa-gear"></i>
              settings
            </button>
          </div>
          ${this.showSettings
            ? html`<settings-modal
                .config=${this.collection.config}
                .schedule=${this.collection.schedule}
                .isScheduled=${this.collection.isScheduled}
              ></settings-modal>`
            : null}
        </div>
        <div
          class="flex flex-col col-span-5 gap-4 h-full overflow-y-hidden border-r"
        >
          <div class="flex justify-between items-center w-full pt-3 px-4">
            <h3 class="text-gray-700 font-medium text-2xl">Steps</h3>
            <div class="flex gap-2">
              <button
                title="run all"
                class="bg-blue-500 text-white gap-2 flex items-center rounded px-3 py-1"
                @click=${() => runAllTests()}
              >
                Run all
                <i class="fa fa-play" aria-hidden="true"></i>
              </button>
              <label class="relative inline-flex items-center cursor-pointer">
                <input
                  type="checkbox"
                  value=""
                  class="sr-only peer"
                  @click=${() => this.toggleCode()}
                />
                <div
                  class="w-9 h-3 bg-gray-200 peer-focus:outline-none peer-focus:ring-blue-300 dark:peer-focus:ring-blue-800 rounded-full peer peer-checked:after:translate-x-full rtl:peer-checked:after:-translate-x-full after:content-[''] after:absolute after:top-[5px] after:start-[0] after:bg-white after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all peer-checked:bg-blue-600"
                ></div>
                <span
                  class="ms-3 text-sm font-medium text-gray-900 dark:text-gray-300"
                >
                  Code</span
                >
              </label>
            </div>
          </div>
          ${this.showCode
            ? html`<div
                id="test-editor"
                class="w-full h-full overflow-y-hidden border-t"
              ></div>`
            : html` <div
                class="flex flex-col overflow-y-auto px-2 h-full gap-4 py-3 border-t"
              >
                ${this.collection.steps?.map(
                  (step, ind) =>
                    html`<step-element .data=${step} ind=${ind}></step-element>`
                )}
                ${this.collection.steps?.length === 0
                  ? html`<div
                      class="self-center text-center flex flex-col gap-2 items-center max-w-md my-24 mb-10 text-gray-700"
                    >
                      <i class="text-5xl fa-solid fa-empty-set"></i>
                      This collection has no test steps, click the plus button
                      to add test steps
                    </div>`
                  : null}
                <button
                  class="bg-blue-500 px-2 py-1 self-center h-10 w-10 text-2xl rounded-full text-white active:ring-1"
                  @click=${() => (this.showNewStepModal = true)}
                >
                  +
                </button>
              </div>`}
        </div>
        <div class="h-full p-3 col-span-4 overflow-y-scroll">
          <div class="mt-24 max-w-md  mx-auto flex flex-col gap-2 text-center">
            <i class="fal fa-list-alt text-5xl"></i>
            <span class="text-gray-700"
              >Run test to see the results appear here</span
            >
          </div>
        </div>
      </div>`;
  }
  createRenderRoot() {
    return this;
  }
}

customElements.define('test-editor', Collection);

class SettingsModal extends LitElement {
  static properties = {
    config: {},
    schedule: { type: String },
    isScheduled: { type: Boolean },
    changed: { type: Boolean },
  };
  constructor() {
    super();
    this.config = {};
    this.schedule = '*/30 * * * *';
    this.isScheduled = true;
    this.changed = false;
  }

  closeModal() {
    this.dispatchEvent(
      new CustomEvent('close-settings', { bubbles: true, composed: true })
    );
  }

  updateColl() {
    if (!this.changed) return;
    this.dispatchEvent(
      new CustomEvent('update-schedule', {
        detail: {
          schedule: this.schedule,
          isScheduled: this.isScheduled,
        },
        bubbles: true,
        composed: true,
      })
    );
    this.changed = false;
  }

  handleScheduleChange(e) {
    this.schedule = e.target.value;
    this.changed = true;
  }

  render() {
    return html` <div
      class="fixed inset-0 z-50 w-screen overflow-y-auto bg-gray-300 bg-opacity-50"
      id="modal-bg"
      @click=${(e) => {
        this.closeModal();
      }}
    >
      <div
        class="flex min-h-full items-end justify-center p-4 text-center sm:items-center sm:p-0"
      >
        <div
          class="relative transform overflow-hidden rounded-lg bg-white shadow-sm text-left transition-all  w-full max-w-2xl"
          @click=${(e) => {
            e.stopPropagation();
          }}
        >
          <div class="w-full">
            <h3
              class="text-lg  w-full px-6 py-4 border-b font-semibold leading-6 text-gray-700"
              id="modal-title"
            >
              Settings
            </h3>
            <div
              class="flex min-h-[30vh] max-h-[70vh] overflow-y-auto flex-col gap-4 p-6"
            >
              <div class="flex flex-col gap-10">
                <config-element .config=${this.config || {}}></config-element>
                <div class="rounded-lg border flex flex-col text-gray-700">
                  <div
                    class="w-full flex p-2 border-b items-center justify-between  bg-gray-100"
                  >
                    <h6 class="font-semibold">Scheduling</h6>
                  </div>
                  <div class="p-3 w-full flex flex-col gap-3">
                    <label
                      class="relative inline-flex items-center cursor-pointer w-max"
                    >
                      <input
                        type="checkbox"
                        value=""
                        class="sr-only peer"
                        .checked=${this.isScheduled}
                        @click=${(e) => {
                          this.isScheduled = e.target.checked;
                          this.changed = true;
                        }}
                      />
                      <div
                        class="w-9 h-3 bg-gray-200 peer-focus:outline-none peer-focus:ring-blue-300 dark:peer-focus:ring-blue-800 rounded-full peer peer-checked:after:translate-x-full rtl:peer-checked:after:-translate-x-full after:content-[''] after:absolute after:top-0 after:start-[0] after:bg-white after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all peer-checked:bg-blue-600"
                      ></div>
                      <span
                        class="ms-3 text-sm font-medium text-gray-900 dark:text-gray-300"
                      >
                        on</span
                      >
                    </label>
                    <div class="flex items-center gap-4">
                      <select
                        class="px-2 py-2 w-full rounded"
                        .value=${this.schedule}
                        @change=${this.handleScheduleChange}
                      >
                        <option value="*/5 * * * *">Every 5 minutes</option>
                        <option value="*/10 * * * *">Every 10 minutes</option>
                        <option value="*/15 * * * *">Every 15 minutes</option>
                        <option value="*/30 * * * *">Every 30 minutes</option>
                        <option value="0 * * * *">Every hour</option>
                        <option value="0 0 * * *">Every day</option>
                      </select>
                    </div>

                    <button
                      @click=${() => this.updateColl()}
                      class=${'px-4 py-1 text-white rounded ' +
                      `${
                        this.changed
                          ? 'bg-blue-500 hover:bg-blue-600 active:ring-1'
                          : 'bg-blue-200'
                      }`}
                    >
                      Save
                    </button>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>`;
  }
  createRenderRoot() {
    return this;
  }
}

customElements.define('settings-modal', SettingsModal);

class Config extends LitElement {
  static properties = {
    showConfigModal: {},
    config: {},
  };
  constructor() {
    super();
    this.config = {};
    this.addEventListener('close-modal', (e) => {
      e.stopPropagation();
      this.showConfigModal = false;
    });
  }
  render() {
    return html` <div class="rounded-lg border flex flex-col text-gray-700">
      <h6 class="p-2 font-semibold border-b bg-gray-100">Configurations</h6>
      <div class="p-2 flex flex-col gap-2">
        <div class="w-full flex flex-col gap-1">
          ${Object.entries(this.config).map(
            (kv) =>
              html`<div class="flex gap-2 items-center w-full">
                <span
                  class="text-sm w-full border border-dashed text-gray-700 px-2 p-1 rounded-lg"
                  >${kv[0]}</span
                >
                <span
                  class="text-sm w-full border border-dashed px-2 py-0.5 text-gray-500 rounded-lg"
                  >${kv[1]}</span
                >
                <button
                  class="text-sm text-red-500 font-medium"
                  @click=${() => {
                    const conf = { ...this.config };
                    delete conf[kv[0]];
                    this.dispatchEvent(
                      new CustomEvent('update-config', {
                        detail: { config: conf },
                        bubbles: true,
                        composed: true,
                      })
                    );
                  }}
                >
                  <i class="fa fa-trash"></i>
                </button>
              </div>`
          )}
        </div>
        <button
          class="self-center text-blue-500 font-medium"
          @click=${() => (this.showConfigModal = true)}
        >
          <i class="fa fa-plus"></i>
        </button>
      </div>
      ${this.showConfigModal
        ? html` <config-modal .config=${this.config}></config-modal> `
        : null}
    </div>`;
  }
  createRenderRoot() {
    return this;
  }
}

customElements.define('config-element', Config);

class ConfigModal extends LitElement {
  static properties = {
    name: {},
    value: {},
    config: {},
  };

  constructor() {
    super();
    this.name = '';
    this.value = '';
    this.config = {};
  }

  closeModal() {
    const event = new CustomEvent('close-modal', {
      bubbles: true,
      composed: true,
    });
    this.dispatchEvent(event);
  }

  saveConfig() {
    this.config = {
      ...this.config,
      [this.name]: this.value,
    };
    const event = new CustomEvent('update-config', {
      detail: { config: this.config },
      bubbles: true,
      composed: true,
    });
    this.dispatchEvent(event);
    this.closeModal();
  }

  render() {
    return html`
      <div
        class="fixed inset-0 z-50 w-full px-6 overflow-y-auto bg-gray-300 bg-opacity-30"
        id="modal-bg"
        @click=${(e) => this.closeModal()}
      >
        <div
          class="flex min-h-full items-end justify-center p-4 text-center sm:items-center sm:p-0"
        >
          <div
            class="relative transform overflow-hidden rounded-lg bg-white shadow-sm text-left transition-all  w-full max-w-2xl"
            @click=${(e) => {
              e.stopPropagation();
            }}
          >
            <div class="w-full">
              <h3
                class="text-lg  w-full px-6 py-4 border-b font-semibold leading-6 text-gray-700"
                id="modal-title"
              >
                Add config
              </h3>
              <div class="flex flex-col gap-4 p-6">
                <div class="flex flex-col gap-1 relative">
                  <label for="var-name" class="font-medium leading-none"
                    >Variable name</label
                  >
                  <input
                    class="flex h-9 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
                    name="var-name"
                    .value=${this.name}
                    @keyup=${(e) => (this.name = e.target.value)}
                    placeholder="Variable name"
                  />
                </div>
                <div class="w-full flex flex-col gap-1">
                  <label for="var-value" class="font-medium leading-none"
                    >Variable value</label
                  >
                  <input
                    class="flex h-9 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
                    name="var-value"
                    .value=${this.value}
                    @keyup=${(e) => (this.value = e.target.value)}
                    placeholder="Variable value"
                  />
                </div>
              </div>
              <div class="flex w-full justify-end gap-4 px-6 py-4 border-t">
                <button
                  class="border rounded px-4 py-1"
                  @click=${() => this.closeModal()}
                >
                  Close
                </button>
                <button
                  class="bg-blue-500 px-5 py-1 self-center rounded text-white active:ring-1"
                  @click=${() => this.saveConfig()}
                >
                  Save
                </button>
              </div>
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

customElements.define('config-modal', ConfigModal);

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
    for (let key in data) {
      if (METHODS.includes(key.toUpperCase())) {
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
    return html`<article
      class="border rounded-lg group overflow-hidden relative"
    >
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
        class="absolute text-gray-600 bg-gray-50 px-4  hidden  group-hover:flex items-center gap-3 right-2 translate-y-1/2"
      >
        <button>View results</button>
        <button class="text-blue-500 text-lg">
          <i class="fa fa-play" aria-hidden="true"></i>
        </button>
        <button @click=${() => (this.editModal = true)}>
          <i class="fa fa-regular fa-edit" aria-hidden="true"></i>
        </button>
        <button
          class="text-red-500"
          @click=${() => {
            if (confirm('Are you sure you want to delete this step')) {
              const e = new CustomEvent('delete-step', {
                detail: {
                  step: this.ind,
                },
                bubbles: true,
                composed: true,
              });
              this.dispatchEvent(e);
            }
          }}
        >
          <i class="fa fa-trash" aria-hidden="true"></i>
        </button>
      </div>
      <button
        class="text-left bg-gray-50 p-3 w-full flex gap-3"
        @click=${(e) => (this.showDetails = !this.showDetails)}
      >
        <span
          class="bg-green-500 mt-[6px] h-3 w-3 rounded-full"
          title="passed"
        ></span>
        <div class="flex flex-col gap-1">
          <span class="text-gray-600 font-medium">${this.data.title}</span>
          <div class="flex gap-1 text-xs text-gray-500">
            <span class=${'font-bold ' + this.getMethodColor()}
              >${this.getMethodUrl(this.data)[0]}</span
            >
            <span>${this.getMethodUrl(this.data)[1]}</span>
          </div>
        </div>
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
    sTitle: { type: String },
    data: {},
  };
  constructor() {
    super();
    this.data = {};
    this.sTitle = '';
  }

  render() {
    return html`
      <div class="w-full">
        <h6 class="mb-1 font-medium text-gray-800">${this.sTitle}</h6>
        <div class="w-full flex flex-col gap-1">
          ${Object.entries(this.data).map(
            (kv) =>
              html`<div class="flex gap-4 items-center w-full">
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
          ${this.data.map((as, i) => {
            const kv = Object.entries(as);
            return html`<div class="flex gap-2 items-center w-full">
              ${i % 2 === 0
                ? html`
                    <span class="text-green-500 font-bold">
                      <i class="fa-solid fa-circle-check"></i>
                    </span>
                  `
                : html`
                    <span class="text-red-500 font-bold">
                      <i class="fa-solid fa-circle-xmark"></i>
                    </span>
                  `}
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
  staticMethods = METHODS;
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

    if (asserts) {
      asserts = asserts.filter((kv) => kv[0] && kv[1]);
      asserts.forEach((assert) => {
        if (!ASSERTS.includes(assert[0])) {
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
                  ? html`<div
                      class="flex flex-col gap-1 bg-red-50 w-full p-4 rounded-lg"
                    >
                      ${this.errors.map(
                        (err) => html`<p class="text-red-500">${err}</p>`
                      )}
                      <button
                        @click=${() => (this.errors = [])}
                        class="text-red-700"
                      >
                        <i class="fa fa-close"></i>
                      </button>
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
                      <span class="absolute right-2 top-2">
                        <i class="fa fa-chevron-down"></i>
                      </span>
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
                        : html`<body-element
                            .body=${this.body}
                          ></body-element>`}
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
                Close
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
        (p, ind) =>
          html`<div class="flex gap-2">
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
              (p, ind) =>
                html`<div class="flex gap-2">
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
  constructor() {
    super();
    this.asserts = [['', '']];
    this.showAsserts = false;
    this.assertsComp = ASSERTS;
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
          (p, ind) =>
            html`<div class="flex gap-2 w-full">
              <custom-select
                .val=${p[0]}
                .target=${ind}
                .options=${ASSERTS}
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
    const event = getEvent(this.eventName, { val: data, target: this.target });
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
        <span class="absolute right-2 top-1">
          <i class="fa fa-chevron-down"></i>
        </span>
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
