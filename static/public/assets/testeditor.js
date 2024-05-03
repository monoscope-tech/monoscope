import { LitElement, html, ref, createRef } from "./js/thirdparty/lit.js";
import {
  METHODS,
  PostConfig,
  getEvent,
  triggerToastEvent,
  ASSERTS,
  validateYaml,
  getDeletedUpdatedAndNewSteps,
} from "./testeditor-utils.js";

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
    this.schedule = "*/30 * * * *";
    this.isScheduled = true;
    this.changed = false;
  }

  closeModal() {
    this.dispatchEvent(
      new CustomEvent("close-settings", { bubbles: true, composed: true })
    );
  }

  updateColl() {
    if (!this.changed) return;
    this.dispatchEvent(
      new CustomEvent("update-schedule", {
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
    return html`<dialog id="settings_modal" class="modal">
      <div class="modal-box">
        <h3
          class="text-lg w-full py-3 mb-4 border-b font-semibold leading-6 text-gray-700"
        >
          Settings
        </h3>
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
                  class="toggle "
                  .checked=${this.isScheduled}
                  @click=${(e) => {
                    this.isScheduled = e.target.checked;
                    this.changed = true;
                  }}
                />
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
                class=${"btn btn-sm " +
                `${this.changed ? "btn-success" : "btn-disabled"}`}
              >
                Save
              </button>
            </div>
          </div>
        </div>
        <div class="modal-action">
          <form method="dialog">
            <button class="btn">Close</button>
          </form>
        </div>
      </div>
      <form method="dialog" class="modal-backdrop">
        <button>Close</button>
      </form>
    </dialog> `;
  }
  createRenderRoot() {
    return this;
  }
}

customElements.define("settings-modal", SettingsModal);

class Config extends LitElement {
  static properties = {
    showConfigModal: {},
    config: {},
  };
  constructor() {
    super();
    this.config = {};
    this.addEventListener("close-modal", (e) => {
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
                      new CustomEvent("update-config", {
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

customElements.define("config-element", Config);

class ConfigModal extends LitElement {
  static properties = {
    name: {},
    value: {},
    config: {},
  };

  constructor() {
    super();
    this.name = "";
    this.value = "";
    this.config = {};
  }

  closeModal() {
    const event = new CustomEvent("close-modal", {
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
    const event = new CustomEvent("update-config", {
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
                  class="btn btn-sm btn-outline"
                  @click=${() => this.closeModal()}
                >
                  Close
                </button>
                <button
                  class="btn btn-sm btn-success"
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

customElements.define("config-modal", ConfigModal);

