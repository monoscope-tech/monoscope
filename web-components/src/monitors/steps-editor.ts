import { LitElement, TemplateResult, html, nothing } from 'lit';
import { unsafeHTML } from 'lit/directives/unsafe-html.js';
import jsyaml from 'js-yaml';
import { renderAssertionBuilder } from './steps-assertions.js';
import { makeRequestAndProcessResponse, generateRequestPreviewFromObject, renderJsonWithIndentation } from './steps-executer';
import { customElement, query, state } from 'lit/decorators.js';
import * as monaco from 'monaco-editor';
import { convertTestkitToCollectionSteps, convertCollectionStepsToTestkitFormat, faSprite_ } from './test-editor-utils';
import { Assertion, AssertionBuilderProps, AssertionResult, Result, Step } from '../types/types';

const validMethods = ['GET', 'POST', 'PUT', 'DELETE', 'PATCH', 'HEAD', 'OPTIONS', 'TRACE', 'CONNECT'];

@customElement('steps-editor')
export class StepsEditor extends LitElement {
  @state() private collectionSteps: Step[] = [];
  @state() private collectionResults: any[] = [];
  @state() private saveErrors: Record<string, any>[] = [];
  @state() private isSendingRequest: boolean[] = [];
  @state() private sendRequestErrors: (string | null)[] = [];
  @state() private isOnboarding = false;

  private editor: any = null;
  private response: any = {};
  @query('#steps-codeEditor') private editorContainer!: HTMLElement;
  createRenderRoot = () => this;
  constructor() {
    super();
    this.collectionSteps = convertTestkitToCollectionSteps(window.collectionSteps) || [];
    this.collectionResults = window.collectionResults || [];
    this.isSendingRequest = this.collectionSteps.map(() => false);
    this.sendRequestErrors = this.collectionSteps.map(() => null);

    // Ensure there's at least one step
    if (this.collectionSteps.length === 0) {
      this.collectionSteps = [
        {
          _expanded: true,
          _method: 'GET',
          assertions: [{ equal: ['$.resp.status', 200] }],
          _assertions: [{ type: 'statusCode', operation: 'equals', value: 200, status: 'PASSED' }],
        },
      ];
    } else if (this.collectionSteps.length == 1) {
      this.collectionSteps[0]._expanded = true;
    }
    window.updateStepAssertions = (assertion, expression, step) => {
      const stepData = this.collectionSteps[step];
      const asserts = stepData.asserts || [];
      asserts.push({ [assertion]: expression });
      stepData.asserts = asserts;
      this.collectionSteps[step] = stepData;
      window.collectionSteps = this.collectionSteps;
      this.requestUpdate();
    };

    window.updateCollectionResults = (results) => {
      if (results && Array.isArray(results)) {
        this.collectionResults = results;
      }
      this.requestUpdate();
    };

    window.updateStepsWithErrors = (errors) => {
      if (errors && Array.isArray(errors)) {
        this.saveErrors = errors;
      }
      this.requestUpdate();
    };
    window.updateEditorVal = () => {
      // this.updateEditorContent()
    };
    window.addCollectionStep = () => {
      this.addStep();
    };
    document.addEventListener('DOMContentLoaded', () => {
      this.initializeEditor(monaco);
    });
  }
  addStep() {
    this.collectionSteps = [
      ...this.collectionSteps,
      {
        _expanded: false,
        _method: 'GET',
        _url: '',
        assertions: [{ equal: ['$.resp.status', 200] }],
        _assertions: [
          {
            type: 'statusCode',
            operation: 'equals',
            value: 200,
            status: 'PASSED',
          },
        ],
      },
    ];
  }
  initializeEditor(monaco: typeof import('monaco-editor')) {
    monaco.editor.defineTheme('nightOwl', {
      base: 'vs-dark',
      inherit: true,
      rules: [
        { token: 'comment', foreground: '#6A9955' },
        { token: 'keyword', foreground: '#C586C0' },
        { token: 'number', foreground: '#B5CEA8' },
        { token: 'string', foreground: '#CE9178' },
        { token: 'operator', foreground: '#D4D4D4' },
        { token: 'identifier', foreground: '#D4D4D4' },
        { token: 'type', foreground: '#4EC9B0' },
        { token: 'delimiter', foreground: '#D4D4D4' },
        { token: 'punctuation', foreground: '#D4D4D4' },
        { token: 'namespace', foreground: '#9CDCFE' },
        { token: 'function', foreground: '#DCDCAA' },
        { token: 'class', foreground: '#4EC9B0' },
        { token: 'variable', foreground: '#D4D4D4' },
      ],
      colors: {
        'editor.foreground': '#D4D4D4',
        'editor.background': '#011627',
        'editor.selectionBackground': '#2D3643',
        'editor.lineHighlightBackground': '#202B33',
        'editorCursor.foreground': '#D4D4D4',
        'editorWhitespace.foreground': '#404040',
      },
    });

    console.log(this.editorContainer);
    this.editor = monaco.editor.create(this.editorContainer, {
      value: jsyaml.dump(convertCollectionStepsToTestkitFormat(this.collectionSteps), { indent: 2 }),
      language: 'yaml',
      theme: 'nightOwl',
      fontSize: 14,
      lineHeight: 20,
      lineNumbersMinChars: 3,
      automaticLayout: true,
      minimap: { enabled: false },
      fontFamily: 'JetBrains Mono, monospace',
      fontLigatures: true,
      fontWeight: '400',
    });

    const model = this.editor.getModel();
    model.onDidChangeContent(() => {
      try {
        const newCollectionSteps = jsyaml.load(model.getValue());
        const toggler = document.querySelector('#test-code-toggle') as HTMLInputElement;
        if (this.collectionSteps != convertTestkitToCollectionSteps(newCollectionSteps)) {
          if (toggler && toggler.checked) {
            this.collectionSteps = convertTestkitToCollectionSteps(newCollectionSteps);
          }
        }
      } catch (e) {
        console.error('Invalid YAML input', e);
      }
    });
  }

  updateEditorContent() {
    const testkitContent = convertCollectionStepsToTestkitFormat(this.collectionSteps);
    const editorContent = jsyaml.dump(testkitContent, { indent: 2 });
    if (this.editor) {
      this.editor.getModel().setValue(editorContent);
    }
  }

  _onDragOver(event: any) {
    event.preventDefault();
    const items = document.querySelectorAll('.draggable');
    let closestItem: Element | null = null;
    let smallestDistance = Number.MAX_SAFE_INTEGER;

    items.forEach((item) => {
      const box = item.getBoundingClientRect();
      const midpoint = box.top + box.height / 2;
      const distance = Math.abs(event.clientY - midpoint);
      if (distance < smallestDistance) {
        closestItem = item;
        smallestDistance = distance;
      }
    });

    items.forEach((item) => {
      if (item === closestItem) {
        item.classList.add('active-drop-target');
      } else {
        item.classList.remove('active-drop-target');
      }
    });
  }

  _onDragEnter(event: any) {
    if (event.target.hasAttribute('data-index')) {
      event.preventDefault(); // Necessary to allow dropping
      event.target.classList.add('over'); // Highlight the drop target only if it has data-index
    }
  }

  _onDragLeave(event: any) {
    if (event.target.hasAttribute('data-index')) {
      event.target.classList.remove('over');
      event.target.classList.remove('active-drop-target'); // Remove additional highlight
    }
  }

  _onDrop(event: any) {
    const activeTarget = document.querySelector('.active-drop-target') as HTMLElement;
    if (activeTarget) {
      event.preventDefault();
      activeTarget.classList.remove('over');
      activeTarget.classList.remove('active-drop-target');
      const originIndex = parseInt(event.dataTransfer.getData('text/plain'));
      const targetIndex = parseInt(activeTarget.dataset.index || '0');
      if (targetIndex !== originIndex) {
        const movedItems = [...this.collectionSteps];
        const item = movedItems.splice(originIndex, 1)[0];
        movedItems.splice(targetIndex, 0, item);
        this.collectionSteps = [...movedItems];
        this.requestUpdate();
      }
    }
  }

  toggleExpanded(idx: number) {
    this.collectionSteps[idx]._expanded = !this.collectionSteps[idx]._expanded;
    this.requestUpdate();
  }

  async sendStepRequest(e: any, idx: number) {
    e.preventDefault();
    this.isSendingRequest[idx] = true;
    this.requestUpdate();
    this.sendRequestErrors[idx] = null;
    try {
      const resp = await makeRequestAndProcessResponse(this.collectionSteps[idx]);
      const stepResult = this.collectionResults[idx];
      if (stepResult) {
        this.collectionResults[idx] = { ...stepResult, ...resp };
      } else {
        this.collectionResults[idx] = { ...resp };
      }
    } catch (error: any) {
      let msg = error.message;
      if (msg === 'Failed to fetch') {
        if (!navigator.onLine) {
          msg += ': Please check your network connection';
        } else {
          msg += ': This could be due to a CORS restriction, DNS failure, server being unreachable.';
        }
      }
      this.sendRequestErrors[idx] = 'Send request error: \n' + msg;
    } finally {
      this.isSendingRequest[idx] = false;
      this.requestUpdate();
    }
  }

  renderCollectionStep(stepData: Step, idx: number, result: Result | null, saveError: { method?: string; url?: string }) {
    const stepResult = this.collectionResults[idx];
    const hasResults = !!result;
    const hasFailingAssertions = result?.assert_results?.some((a) => a.ok === undefined || a.ok === false) || false;
    const svErr = saveError !== undefined;
    const failed = !stepData.disabled && (hasFailingAssertions || svErr);
    const passed = !stepData.disabled && hasResults && !hasFailingAssertions && !svErr;
    saveError = saveError ? saveError : {};
    const configuredOptions = {
      'request-options': (stepData.headers ? Object.keys(stepData.headers) : []).length,
      'query-params': (stepData.params ? Object.keys(stepData.params) : []).length,
      'request-body': stepData.json || stepData.raw || stepData._requestBody ? 1 : 0,
      ignoreSSLErrors: stepData.ignoreSSLErrors ? 1 : 0,
      followRedirects: stepData.followRedirects ? 1 : 0,
      timeout: stepData.timeout !== 60 ? 1 : 0,
    };
    const activeTab = this.collectionSteps[idx].activeTab || 'request-options';
    const setActiveTab = (tab: string) => {
      this.collectionSteps[idx].activeTab = tab;
      this.requestUpdate();
    };
    const totalConfigured = Object.values(configuredOptions).reduce((a, b) => a + b, 0);

    return html`
      <div
        class="rounded-2xl overflow-hidden group/item  bg-fillWeak collectionStep border draggable  ${
          failed ? 'border-strokeError-strong' : passed ? 'border-strokeSuccess-strong' : 'border-strokeWeak'
        }"
        data-index="${idx}"
      >
        <div class="flex flex-row items-center">
          <div class="h-full shrink p-3 cursor-move"
            draggable="true"
            @dragstart="${(e: any) => e.dataTransfer.setData('text/plain', e.target.dataset.index)}"
          >${faSprite_('grip-dots-vertical', 'solid', 'h-4 w-4')}</div>
          <div class="flex-1 flex flex-row items-center gap-1 pr-5 py-3" @pointerdown="${() => this.toggleExpanded(idx)}">
            <label
             for="stepState-${idx}" class="flex items-center whitespace-nowrap gap-1 py-1 w-max text-xs bg-fillStrong badge text-textInverse-strong">Step ${
               idx + 1
             }</label>
            <div class="w-full space-y-1 shrink" @pointerdown="${(e: any) => e.stopPropagation()}">
              <input
              class="text-lg w-full pl-2 bg-transparent outline-hidden focus:outline-hidden" placeholder="Give your step a name*"
               .value="${stepData.title || ''}" id="title-${idx}" @change=${(e: any) => this.updateValue(e, idx, null, null, 'title')} />
            </div>
            <div class="items-center w-max shrink-0 gap-3 text-xs text-textWeak flex">
                <input
                  @pointerdown="${(e: any) => e.stopPropagation()}"
                  @change="${(e: any) => {
                    this.collectionSteps[idx].disabled = !e.target.checked;
                    this.requestUpdate();
                  }}"
                  ?checked="${stepData.disabled === undefined ? true : stepData.disabled ? false : true}"
                  type="checkbox"
                  class="toggle toggle-sm  ${stepData.disabled ? 'border-strokeError-strong  text-textError' : 'border-strokeSuccess-strong text-textSuccess'}"
                   />
                <button class="text-textSuccess cursor-pointer" @pointerdown="${(e: any) => {
                  e.preventDefault();
                  e.stopPropagation();
                  this.collectionSteps = this.collectionSteps.filter((_, i) => i != idx);
                  this.collectionResults = this.collectionResults.filter((_, i) => i != idx);
                }}">
                  ${faSprite_('trash', 'regular', 'w-4 h-4 stroke-iconError')}
                </button>
                <button type="button" class="text-textWeak transition-all ${stepData._expanded ? 'rotate-90' : ''}">
                  ${faSprite_('f-chevron-right', 'solid', 'w-4 h-4')}
                </button>
              </div>
          </div>
        </div>
        <div class="p-4 pt-0 bg-fillWeaker ${stepData._expanded ? 'block' : 'hidden'} ">
        <div class="rounded-xl p-4 bg-fillWeaker">
          <div>
            <div class="p-0 m-0s">
              <div class="">
                <div class="text-sm text-textStrong"><div>URL<span class="text-error">*</span></div></div>
                <div class="relative flex flex-row gap-2 items-center">
                  <label for="actions-list-input-${idx}" class="w-28 shrink text-sm font-medium form-control">
                    <select id="actions-list-input-${idx}" class="select select-sm shadow-none w-full" @change=${(e: any) =>
                      this.updateValue(e, idx, null, null, '_method')}>
                      ${validMethods.map((methodItem) => html`<option ?selected=${methodItem == stepData._method}>${methodItem}</option>`)}
                    </select>
                    ${saveError.method ? html`<span class="text-textError text-xs">${saveError.method}</span>` : ''}
                  </label>
                  <label for="actions-data-${idx}" class="flex-1 text-sm font-medium form-control w-full flex flex-row items-center gap-1">
                    <input
                      placeholder="https://example.com/api/users"
                      type="text" id="actions-data-${idx}" .value=${
                        stepData._url || ''
                      } class="input input-sm shadow-none w-full" @change=${(e: any) => this.updateValue(e, idx, null, null, '_url')}
                    />
                    ${saveError.url ? html`<span class="text-textError text-xs">${saveError.url}</span>` : ''}
                  </label>
                </div>
              </div>
              <details class="mt-4">
                <summary class="cursor-pointer text-sm text-textStrong font-medium">Advanced Options (${totalConfigured} configured)</summary
                 <div>
                  <div class="mt-4 pb-3 border rounded-xl">
                    <div role="tablist" class="tabs tabs-bordered pt-1">
                      <a role="tab" class="tab  ${
                        activeTab === 'request-options' ? 'tab-active [--bc:var(--brand-color)] text-textBrand font-bold' : ''
                      }" @pointerdown=${() => setActiveTab('request-options')}>
                        Request Options ${
                          configuredOptions['request-options'] > 0
                            ? html`<span class="badge badge-sm badge-ghost">${configuredOptions['request-options']}</span>`
                            : ''
                        }
                      </a>
                      <a role="tab" class="tab ${
                        activeTab === 'query-params' ? 'tab-active [--bc:var(--brand-color)] text-textBrand font-bold' : ''
                      }" @pointerdown=${() => setActiveTab('query-params')}>
                        Query Params ${
                          configuredOptions['query-params'] > 0
                            ? html`<span class="badge badge-sm badge-ghost">${configuredOptions['query-params']}</span>`
                            : ''
                        }
                      </a>
                      <a role="tab" class="tab ${
                        activeTab === 'request-body' ? 'tab-active [--bc:var(--brand-color)] text-textBrand font-bold' : ''
                      }" @pointerdown=${() => setActiveTab('request-body')}>
                        Request Body ${
                          configuredOptions['request-body'] > 0
                            ? html`<span class="badge badge-sm badge-ghost">${configuredOptions['request-body']}</span>`
                            : ''
                        }
                      </a>
                    </div>
                    <div class="p-4 space-y-3">
                      ${
                        activeTab === 'request-options'
                          ? html`
                              <div class="form-control w-full">
                                <div class="label"><span class="label-text">HTTP Version</span></div>
                                <select
                                  class="select select-sm max-w-xs shadow-none"
                                  .value=${this.collectionSteps[idx].httpVersion}
                                  @change=${(e: any) => {
                                    this.collectionSteps[idx].httpVersion = e.target.value;
                                  }}
                                >
                                  <option value="http2-http1">HTTP/2 fallback to HTTP/1.1</option>
                                  <option value="http2">HTTP/2 Only</option>
                                  <option value="http1">HTTP/1.1</option>
                                </select>
                              </div>
                              <div class="flex gap-4 items-center">
                                <div class="form-control">
                                  <label class="label cursor-pointer justify-start gap-3">
                                    <input
                                      type="checkbox"
                                      class="checkbox checkbox-sm"
                                      ?checked=${stepData.followRedirects}
                                      @change=${(e: any) => (this.collectionSteps[idx].followRedirects = e.target.value == 'on')}
                                    />
                                    <span class="text-textStrong font-medium">Follow redirects</span>
                                  </label>
                                </div>
                                <div class="form-control">
                                  <label class="label cursor-pointer justify-start gap-3">
                                    <input
                                      type="checkbox"
                                      class="checkbox checkbox-sm"
                                      ?checked=${stepData.ignoreSSLErrors}
                                      @change=${(e: any) => (this.collectionSteps[idx].ignoreSSLErrors = e.target.value == 'on')}
                                    />
                                    <span class="text-textStrong font-medium">Ignore server certificate error</span>
                                  </label>
                                </div>
                              </div>

                              <div class="flex items-center gap-2 text-textStrong font-medium">
                                <span class="">Time out after</span>
                                <input
                                  type="number"
                                  value=${stepData.timeout || 60}
                                  class="input input-sm w-20 shadow-none"
                                  @change=${(e: any) => (this.collectionSteps[idx].timeout = parseInt(e.target.value))}
                                />
                                <span>seconds</span>
                              </div>
                              <div class="form-control w-full">
                                <div class="label"><span class="label-text">Request Headers</span></div>
                                <div class="space-y-2 paramRows" id="[${idx}][headers]">
                                  ${this.renderParamsRows(stepData, idx, 'headers')}
                                </div>
                              </div>
                              <div class="form-control w-full">
                                <div class="label"><span class="label-text">Cookies</span></div>
                                <textarea
                                  class="textarea"
                                  placeholder="cookie-name-1=value; cookie-name-2=value"
                                  @change=${(e: any) => ((this.collectionSteps[idx].headers ??= {}).Cookie = e.target.value)}
                                >
${stepData?.headers?.Cookie || ''}</textarea
                                >
                              </div>
                            `
                          : nothing
                      }
                      ${
                        activeTab === 'query-params'
                          ? html`
                              <div class="form-control">
                                <div class="label flex-col items-start gap-2">
                                  <p class="label-text">Parameters to encode</p>
                                  <p class="label-text text-xs">
                                    Add all parameters that require encoding to the below fields. Query parameters that do not require
                                    encoding can be added to the URL field directly.
                                  </p>
                                </div>
                                <div class="space-y-2 paramRows" id="[${idx}][params]">
                                  ${this.renderParamsRows(stepData, idx, 'params')}
                                </div>
                              </div>
                            `
                          : nothing
                      }
                      ${
                        activeTab === 'request-body'
                          ? html`
                              <div class="form-control">
                                <div class="label items-start gap-2"><span class="label-text">Body Type</span></div>
                                <select
                                  class="select select-sm max-w-xs"
                                  .value=${stepData._requestType || 'application/json'}
                                  @change=${(e: any) => {
                                    this.collectionSteps[idx]._requestType = e.target.value;
                                    this.requestUpdate();
                                  }}
                                >
                                  <option>application/json</option>
                                  <option>application/x-www-form-urlencoded</option>
                                  <option>text/html</option>
                                  <option>raw</option>
                                </select>
                              </div>

                              <div class="form-control">
                                <div class="label flex-col items-start gap-2">
                                  <span class="label-text">Request Body</span>
                                  <p class="label-text text-xs">Insert variables using the syntax {{variableName}}</p>
                                </div>
                                ${this.collectionSteps[idx]._requestType === 'application/x-www-form-urlencoded'
                                  ? html`<div class="flex flex-col gap-1">${this.renderParamsRows(stepData, idx, '_requestBody')}</div>`
                                  : html` <textarea
                                      class="w-full border border-strokeWeak textarea"
                                      name="[${idx}][json]"
                                      @change=${(e: any) => {
                                        this.collectionSteps[idx]._json = e.target.value;
                                      }}
                                    >
${stepData._json}</textarea
                                    >`}
                              </div>
                            `
                          : nothing
                      }
                    </div>
                  </div>
                </details>
              </div>
              <button class="mt-5 btn btn-sm btn-primary px-2 py-1" @pointerdown=${(e: any) => this.sendStepRequest(e, idx)}>
              ${this.isSendingRequest[idx] ? html`<span class="loading loading-dots loading-sm"></span>` : 'Send request'}
              </button>
              ${
                this.sendRequestErrors[idx] !== null ? html`<div class="mt-2 text-textError">${this.sendRequestErrors[idx]}</div>` : nothing
              }
              ${
                stepResult && stepResult.resp
                  ? html`
                      <h3 class=" text-textStrong text-lg font-medium py-2 mt-10">
                        Request Preview
                        <span class="font-normal  text-textWeak"
                          >(took <strong>${stepResult.resp.duration_ms}ms</strong> with status
                          <strong>${stepResult.resp.status}</strong>)</span
                        >
                      </h3>
                      <div class="rounded-xl border border-weak p-4  text-textStrong flex flex-col gap-1">
                        ${unsafeHTML(generateRequestPreviewFromObject(this.collectionSteps[idx]))}
                      </div>
                      <div role="tablist" class="tabs tabs-bordered max-h-96 overflow-y-auto border border-strokeWeak rounded-xl mt-6">
                        <input
                          type="radio"
                          name="resp-items"
                          role="tab"
                          class="tab checked:[--bc:var(--brand-color)] checked:text-[var(--brand-color)] checked:font-bold"
                          aria-label="Response Headers"
                          checked
                        />
                        <div role="tabpanel" class="tab-content p-4">
                          <div class="flex rounded-sm  bg-fillWeak px-2 py-1 mb-2 items-center gap-2">
                            ${faSprite_('circle-info', 'regular', 'w-4 h-4 fill-none stroke-strokeWeak')}
                            <span class=" text-textWeak">Click below to add field as an assertion</span>
                          </div>
                          ${Object.entries(stepResult.resp.headers).map(([key, value]) => {
                            let assertionObj: Assertion = {
                              type: 'header',
                              operation: 'equals',
                              headerName: key,
                              value: value,
                              status: 'PASSED',
                            };
                            return html`
                              <div class="flex items-center gap-2">
                                <span class=" text-textStrong">${key}:</span>
                                <span class=" text-textWeak">${value}</span>
                                <button
                                  data-tippy-content="Add as an assertion"
                                  class="rounded-full border fill-textDisabled shadow-[0px_4px_4px_0px_rgba(0,0,0,0.06)] border-strokeWeak shadown-sm p-1.5 bg-bgBase"
                                  @pointerdown="${(e: any) => this.addAssertion(e, idx, assertionObj)}"
                                >
                                  ${faSprite_('plus', 'regular', 'w-3 h-3')}
                                </button>
                              </div>
                            `;
                          })}
                        </div>

                        <input
                          type="radio"
                          name="resp-items"
                          role="tab"
                          class="tab checked:[--bc:var(--brand-color)] checked:text-[var(--brand-color)] checked:font-bold"
                          aria-label="Response Body"
                        />
                        <div role="tabpanel" class="tab-content p-4">
                          <div>{</div>
                          <div class="pl-3">
                            ${renderJsonWithIndentation(
                              stepResult.resp.json,
                              (e, assertionObj) => this.addAssertion(e, idx, assertionObj),
                              '$'
                            )}
                          </div>
                          <div>}</div>
                        </div>

                        <input
                          type="radio"
                          name="resp-items"
                          role="tab"
                          class="tab checked:[--bc:var(--brand-color)] checked:text-[var(--brand-color)] checked:font-bold"
                          aria-label="Response Status Code"
                        />
                        <div role="tabpanel" class="tab-content p-4">${stepResult.resp.status}</div>
                      </div>
                    `
                  : nothing
              }
            </div>

            <details class="mt-10" ?open=${stepResult && stepResult.resp}>
              <summary class="label-text text-lg mb-2 cursor-pointer">
                <div class="inline-flex text-lg gap-2 items-center cursor-pointer  text-textStrong font-medium">
                  Add Assertions <span class="font-normal  text-textWeak">(optional)</span>
                  <a href="https://apitoolkit.io/docs/dashboard/dashboard-pages/api-tests/#test-definition-syntax" class="" target="_blank">
                    ${faSprite_('circle-info', 'regular', 'w-3 h-3 text-iconNeutral')}
                  </a>
                </div>
              </summary>
              <div class=" text-textStrong space-y-2 px-2 paramRows [&_.assertIndicator]:inline-block" id="[${idx}][asserts]">
                <p class="">Your step is successful;</p>
          ${renderAssertionBuilder({
            assertions: this.collectionSteps[idx]._assertions || [],
            result: this.collectionResults[idx],
            updateAssertion: (index, updates) => this.updateAssertion(idx, index, updates),
            addAssertion: (e: any) => this.addAssertion(e, idx, { type: 'body', operation: 'equals', value: '' }),
            removeAssertion: (index) => (e: any) => this.removeAssertion(idx, index),
          })}
              </div>
            </details>
            <details class="mt-10">
              <summary class="label-text text-lg mb-2 cursor-pointer">
                <div class="inline-flex gap-2 items-center cursor-pointer text-lg  text-textStrong font-medium">Extract variables from the response <span class="font-normal  text-textWeak">(optional)</span></div></summary>
              <div class="text-sm space-y-2 px-2 paramRows" id="[${idx}][exports]">
                <p class=" text-textStrong">Variables consist of a variable name and a json path pointing to the variable in the response.</p>
                ${this.renderParamsRows(stepData, idx, 'exports')}
                <button class="flex items-center gap-1 mt-4" type="button" @pointerdown=${() => {}}>
                ${faSprite_('plus', 'regular', 'w-4 h-4  text-textWeak')}
                <span class="underline  text-textWeak font-semibold">New variable<span>
                </button>
              </div>
            </div>
          </div>
        </div>
        </div>
      </div>
    `;
  }

  renderAssertResult(result: AssertionResult | undefined) {
    let hasPassed = result?.ok === true || false;
    let notRun = !result;
    let error = result?.err?.advice || '';

    if (hasPassed) {
      return html` <svg class="icon w-3 h-3 text-textSuccess"><use href="/public/assets/svgs/fa-sprites/solid.svg#check"></use></svg>`;
    }
    if (!hasPassed && !notRun) {
      return html`<span title="${error}"
        ><svg class="icon w-3 h-3 text-textError"><use href="/public/assets/svgs/fa-sprites/regular.svg#trash"></use></svg><span></span
      ></span>`;
    }
    return html`<span title="${error}" class="opacity-0"
      ><svg class="icon w-3 h-3 text-textError"><use href="/public/assets/svgs/fa-sprites/regular.svg#trash"></use></svg><span></span
    ></span>`;
  }

  renderParamRow(
    key: string,
    value: string,
    type: string,
    idx: number,
    aidx: number,
    category: string,
    result: AssertionResult | undefined = undefined,
    saveError: Record<string, any> | null = null
  ) {
    let error = result?.err?.advice || '';
    let keyError = '';
    if (saveError) {
      error = saveError.value ? saveError.value : error;
      keyError = saveError.key ? saveError.key : '';
    }
    const options = [
      {
        value: 'body',
        label: 'Body',
      },
      {
        value: 'header',
        label: 'Header',
      },
      {
        value: 'status',
        label: 'Status Code',
      },
      {
        value: 'responseTime',
        label: 'Response Time',
      },
    ];
    const noValue = category == 'status' || category == 'responseTime';
    return html`
      <div class="flex flex-row gap-2 w-full paramRow">
        <span class="shrink hidden assertIndicator"> ${this.renderAssertResult(result)} </span>
        <div class="flex flex-col w-1/3">
          <input
            class="input input-sm shadow-none w-full"
            list="${type}DataList"
            placeholder="Key"
            .value="${key}"
            @change=${(e: any) => this.updateKey(e, idx, type, aidx)}
          />
          <span class="text-xs text-textError w-full">${keyError}</span>
        </div>
        ${type === 'exports'
          ? html`
              <div class="flex flex-col w-1/3">
                <select class="select select-sm max-w-xs shadow-none" @change=${(e: any) => this.updateExportCategory(e, idx, type, aidx)}>
                  ${options.map(
                    (option) => html` <option value=${option.value} ?selected=${option.value === category}>${option.label}</option> `
                  )}
                </select>
              </div>
            `
          : nothing}
        ${type != 'exports' || !noValue
          ? html`<div class="shrink w-full flex flex-col">
          <input
            list="${type === 'asserts' ? 'assertAutocomplete-' + idx : ''}"
            class="input shadow-none ${error ? 'input-error' : ''} input-sm w-full"
            placeholder="Value"
            .value="${value}"
            @input=${(e: any) => this.updateValue(e, idx, type, aidx, key)}
          />
          <span class="text-xs text-textError">${error}</span>
        </div>
        <a class="cursor-pointer text-iconNeutral" @pointerdown=${(e: any) => this.deleteKey(e, idx, type, aidx, key)}>
          <svg class="inline-block icon w-5 h-5 p-1 rounded-full shadow-sm border stroke-strokeError-strong"><use href="/public/assets/svgs/fa-sprites/regular.svg#trash"></use></svg>
        </a>
      </div>`
          : nothing}
      </div>
    `;
  }

  renderParamsRows(stepData: Step, idx: number, type: string, results?: AssertionResult[]) {
    let rows: TemplateResult<1>[] = [];
    const errors = this.saveErrors[idx] ? this.saveErrors[idx][type] || [] : [];
    if (type === 'asserts') {
      let matches = [];
      let fieldPathValues = new Set();
      let resultContainer = document.querySelector('#res-container-' + idx);
      let elements: NodeListOf<Element>;
      if (resultContainer) {
        elements = resultContainer.querySelectorAll('[data-field-path]');
        elements.forEach((element) => {
          let path = element.getAttribute('data-field-path');
          if (path) {
            fieldPathValues.add(('$.resp.json.' + path.replace(/\.?(\d+)\.?/g, '.[$1].')).replace('..', '.'));
          }
        });
      }

      matches = Array.from(fieldPathValues);

      const data = stepData[type] || [];
      data.forEach((assertObj, aidx) =>
        Object.entries(assertObj).map(([key, value]) =>
          rows.push(this.renderParamRow(key, value, type, idx, aidx, '', results ? results[aidx] : undefined, errors[aidx]))
        )
      );
      if (rows.length === 0 || !data.some((d) => d.key.trim() === '' && d.value.trim() === '')) {
        rows.push(this.renderParamRow('', '', type, idx, rows.length, ''));
      }
      rows.push(html`
        <datalist id=${'assertAutocomplete-' + idx}>
          ${matches.map((fieldPath) => {
            return html`<option class="w-full  text-left text-xs px-3 py-1 hover:bg-fillWeak">${fieldPath}</option>`;
          })}
        </datalist>
      `);
    } else if (type === 'exports') {
      const data = stepData[type] || [];
      rows = data.map((d, ind) => {
        return this.renderParamRow(d.key, d.value, type, idx, ind, d.category, undefined, errors[ind]);
      });
      if (rows.length === 0 || data.some((d) => d.key.trim() === '' && d.value.trim() === '')) {
        rows.push(this.renderParamRow('', '', type, idx, rows.length, 'body', undefined, errors[rows.length - 1]));
      }
    } else {
      const data = stepData[type] || {};
      rows = Object.entries(data)
        .filter(([key, _]) => key != 'Cookie')
        .map(([key, value], ind) => this.renderParamRow(key, value as string, type, idx, ind, '', undefined, errors[ind]));
      if (rows.length === 0 || !Object.entries(data).some(([k, v]) => k.trim() === '' && (v as string).trim() === '')) {
        rows.push(this.renderParamRow('', '', type, idx, rows.length, '', undefined, null));
      }
    }
    return html`${rows}`;
  }

  updateKey(event: any, idx: number, type: string, aidx: number | null) {
    const newKey = event.target.value;
    const oldKey = event.target.defaultValue;
    const stepData = this.collectionSteps[idx];

    const updateObject = (obj: Record<string, any>, oldKey: string, newKey: string) => {
      const oldValue = obj[oldKey];
      delete obj[oldKey];
      obj[newKey] = oldValue || '';
    };

    if (type == null) {
      updateObject(stepData, oldKey, newKey);
    } else {
      stepData[type] = stepData[type] || (aidx === null ? {} : []);
      if (aidx != null) {
        if (type === 'exports') {
          if (this.collectionSteps[idx][type]) {
            const val = this.collectionSteps[idx][type][aidx] || { key: '', value: '', category: '' };
            this.collectionSteps[idx][type][aidx] = { ...val, key: newKey };
          }
        } else {
          const arrayItem = stepData[type][aidx] || {};
          const values = Object.entries(arrayItem);
          if (values.length > 0) {
            const val = values[0][1];
            stepData[type][aidx] = {
              [newKey]: val,
            };
          } else {
            stepData[type][aidx] = {
              [newKey]: '',
            };
          }
        }
      } else {
        updateObject(stepData[type], oldKey, newKey);
      }
    }
    this.requestUpdate();
  }

  deleteKey(_event: any, idx: number, type: string, aidx: number | null, oldKey: string) {
    const stepData = this.collectionSteps[idx];
    stepData[type] = stepData[type] || (aidx != null ? [] : {});

    if (aidx != null) {
      stepData[type].splice(aidx, 1);
    } else {
      delete stepData[type][oldKey];
    }
    this.requestUpdate();
  }

  updateExportCategory(event: any, idx: number, type: string, aidx: number) {
    if (type !== 'exports') return;
    const value = event.target.value;
    const stepData = this.collectionSteps[idx];
    stepData[type] = stepData[type] || [];
    stepData[type][aidx] = stepData[type][aidx] || {};
    stepData[type][aidx]['category'] = value;
    this.requestUpdate();
  }

  updateValue(event: any, idx: number, type: string | null, aidx: number | null, key: string) {
    const value = event.target.value;
    if (type == null) {
      this.collectionSteps[idx][key] = value;
      this.requestUpdate();
      return;
    }
    if (aidx != null) {
      if (key === '') {
        if (type === 'asserts') {
          const val = this.collectionSteps[idx][type];
          if (val) {
            val[aidx] = { ok: value };
          }
        } else if (type === 'exports') {
          if (this.collectionSteps[idx][type]) {
            const val = this.collectionSteps[idx][type][aidx];
            this.collectionSteps[idx][type][aidx] = { ...val, value: value };
          }
        } else {
          if (type === 'exports') {
            if (this.collectionSteps[idx][type]) {
              const val = this.collectionSteps[idx][type][aidx];
              this.collectionSteps[idx][type][aidx] = { ...val, value: value };
            }
          } else {
            this.collectionSteps[idx][type][aidx][key] = value;
          }
        }
      } else {
        this.collectionSteps[idx][type][key] = value;
      }
      this.requestUpdate();
    }
  }

  render() {
    const toggler = document.querySelector('#test-code-toggle')! as HTMLInputElement;
    if (toggler && !toggler.checked) {
      this.updateEditorContent();
    }
    return html`
      <style>
        .draggable {
          transition: transform 0.4s ease;
        }
        .over {
          border: 2px solid blue;
        }
        .active-drop-target {
          background-color: lightblue !important;
          border: 2px solid blue;
          transform: translateY(-20px);
        }
      </style>
      <div id="collectionStepsContainer" class="overflow-y-auto">
        <div id="steps-codeEditor" class="min-h-[28rem] max-h-screen hidden group-has-[.editormode:checked]/colform:block"></div>
        <div class="group-has-[.editormode:checked]/colform:hidden">
          <div
            id="collectionStepsContainer"
            class="collectionSteps draggable-container space-y-4"
            @dragover="${this._onDragOver}"
            @drop="${this._onDrop}"
            @dragenter="${this._onDragEnter}"
            @dragleave="${this._onDragLeave}"
          >
            ${this.collectionSteps.map(
              (stepData, idx) => this.renderCollectionStep(stepData, idx, this.collectionResults[idx], this.saveErrors[idx]) || undefined
            )}
          </div>
          ${this.isOnboarding
            ? nothing
            : html`<div class="p-4 pt-4">
                <a
                  class="btn btn-sm blue-outline-btn bg-transparent border-[var(--brand-color)] items-center cursor-pointer"
                  @pointerdown=${() => this.addStep()}
                >
                  <svg class="inline-block icon w-3 h-3"><use href="/public/assets/svgs/fa-sprites/solid.svg#plus"></use></svg>
                  Add new step
                </a>
              </div>`}
        </div>
      </div>
    `;
  }

  // Assume that stepIndex is available in your component
  updateAssertion(stepIdx: number, index: number, updates: Partial<Assertion>) {
    if (this.collectionSteps[stepIdx]._assertions === undefined) {
      this.collectionSteps[stepIdx]._assertions = [];
    }
    this.collectionSteps[stepIdx]._assertions = this.collectionSteps[stepIdx]._assertions.map((assertion, i) =>
      i === index ? { ...assertion, ...updates } : assertion
    );
    // Optionally re-evaluate the assertion after the update
    const updatedAssertion = this.collectionSteps[stepIdx]._assertions[index];
    const status = this.evaluateAssertion(updatedAssertion) ? 'PASSED' : 'FAILED';
    this.collectionSteps[stepIdx]._assertions = this.collectionSteps[stepIdx]._assertions.map((assertion, i) =>
      i === index ? { ...assertion, status } : assertion
    );
    this.requestUpdate();
  }

  addAssertion(e: any, idx: number, assertion_obj: Assertion) {
    e.preventDefault();
    this.collectionSteps[idx]._assertions = [...(this.collectionSteps[idx]._assertions || []), assertion_obj];
    this.requestUpdate();
  }

  removeAssertion(idx: number, index: number) {
    if (this.collectionSteps[idx]._assertions === undefined) {
      this.collectionSteps[idx]._assertions = [];
    }
    this.collectionSteps[idx]._assertions.splice(index, 1);
    if (this.collectionResults && this.collectionResults[idx]) {
      this.collectionResults[idx].assert_results?.splice(index, 1);
    }
    this.requestUpdate();
  }

  evaluateAssertion(assertion: Assertion): boolean {
    // Implement your assertion evaluation logic here
    if (assertion.type === 'body' && assertion.operation === 'contains') {
      return this.response.body.includes(assertion.value);
    }
    // Handle other types and operations
    return false; // Default to false if not matched
  }
}
