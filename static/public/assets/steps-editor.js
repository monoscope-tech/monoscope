import { LitElement, html, ref, createRef, nothing, unsafeHTML } from './js/thirdparty/lit.js'
import { renderAssertionBuilder } from './steps-assertions.js'
import { makeRequestAndProcessResponse, generateRequestPreviewFromObject, renderJsonWithIndentation } from './steps-executor.js'

const validMethods = ['GET', 'POST', 'PUT', 'DELETE', 'PATCH', 'HEAD', 'OPTIONS', 'TRACE', 'CONNECT']

export class StepsEditor extends LitElement {
  static properties = {
    collectionSteps: [],
    collectionResults: [],
    saveErrors: [],
    isSendingRequest: false,
  }

  constructor() {
    super()
    this.collectionSteps = window.collectionSteps || []
    this.collectionResults = window.collectionResults || []
    this.saveErrors = []
    this.isSendingRequest = false

    // Ensure there's at least one step
    if (this.collectionSteps.length === 0) {
      this.collectionSteps = [{ _expanded: true }]
    } else if (this.collectionSteps.length == 1) {
      this.collectionSteps[0]._expanded = true
    }
    this.collectionSteps = [
      {
        _expanded: true,
        assertions: [{ equal: ['$.resp.status', 200] }],
        _assertions: [{ type: 'statusCode', operation: 'equals', value: 200, status: 'PASSED' }],
      },
    ]

    require.config({ paths: { vs: '/public/assets/js/monaco/vs' } })
    require.config({ paths: { vs: 'https://unpkg.com/monaco-editor/min/vs' } })
    require(['vs/editor/editor.main'], () => {
      this.initializeEditor(monaco)
    })

    window.updateStepAssertions = (assertion, expression, step) => {
      const stepData = this.collectionSteps[step]
      const asserts = stepData.asserts || []
      asserts.push({ [assertion]: expression })
      stepData.asserts = asserts
      this.collectionSteps[step] = stepData
      window.collectionSteps = this.collectionSteps
      this.requestUpdate()
    }

    window.updateCollectionResults = (results) => {
      if (results && Array.isArray(results)) {
        this.collectionResults = results
      }
      this.requestUpdate()
    }

    window.updateStepsWithErrors = (errors) => {
      if (errors && Array.isArray(errors)) {
        this.saveErrors = errors
      }
      this.requestUpdate()
    }
    window.updateEditorVal = () => {
      this.updateEditorContent()
    }
  }

  initializeEditor(monaco) {
    const editorContainer = this.querySelector('#steps-codeEditor')

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
    })

    this.editor = monaco.editor.create(editorContainer, {
      value: jsyaml.dump(this.collectionSteps, { ident: 2 }),
      language: 'yaml',
      theme: 'nightOwl',
      fontSize: 14,
      lineHeight: 20,
      lineNumbersMinChars: 3,
      automaticLayout: true,
      minimap: { enabled: false },
    })

    const model = this.editor.getModel()
    model.onDidChangeContent(() => {
      try {
        const newCollectionSteps = jsyaml.load(model.getValue())
        if (this.collectionSteps != newCollectionSteps) {
          this.collectionSteps = newCollectionSteps
          this.requestUpdate()
        }
      } catch (e) {
        console.error('Invalid YAML input', e)
      }
    })
  }

  updateEditorContent() {
    const editorContent = jsyaml.dump(this.collectionSteps, { ident: 2 })
    if (this.editor && this.editor.getModel().getValue() != editorContent) {
      this.editor.getModel().setValue(editorContent)
    }
  }

  createRenderRoot() {
    return this
  }

  _onDragOver(event) {
    event.preventDefault()
    const items = document.querySelectorAll('.draggable')
    let closestItem = null
    let smallestDistance = Number.MAX_SAFE_INTEGER

    items.forEach((item) => {
      const box = item.getBoundingClientRect()
      const midpoint = box.top + box.height / 2
      const distance = Math.abs(event.clientY - midpoint)
      if (distance < smallestDistance) {
        closestItem = item
        smallestDistance = distance
      }
    })

    items.forEach((item) => {
      if (item === closestItem) {
        item.classList.add('active-drop-target')
      } else {
        item.classList.remove('active-drop-target')
      }
    })
  }

  _onDragEnter(event) {
    if (event.target.hasAttribute('data-index')) {
      event.preventDefault() // Necessary to allow dropping
      event.target.classList.add('over') // Highlight the drop target only if it has data-index
    }
  }

  _onDragLeave(event) {
    if (event.target.hasAttribute('data-index')) {
      event.target.classList.remove('over')
      event.target.classList.remove('active-drop-target') // Remove additional highlight
    }
  }

  _onDrop(event) {
    const activeTarget = document.querySelector('.active-drop-target')
    if (activeTarget) {
      event.preventDefault()
      activeTarget.classList.remove('over')
      activeTarget.classList.remove('active-drop-target')
      const originIndex = parseInt(event.dataTransfer.getData('text/plain'))
      const targetIndex = parseInt(activeTarget.dataset.index)
      if (targetIndex !== originIndex) {
        const movedItems = [...this.collectionSteps]
        const item = movedItems.splice(originIndex, 1)[0]
        movedItems.splice(targetIndex, 0, item)
        this.collectionSteps = [...movedItems]
        this.requestUpdate()
      }
    }
  }

  toggleExpanded(idx) {
    this.collectionSteps[idx]._expanded = !this.collectionSteps[idx]._expanded
    this.requestUpdate()
  }

  sendStepRequest(e, idx) {
    e.preventDefault()
    this.isSendingRequest = true
    makeRequestAndProcessResponse(this.collectionSteps[idx]).then((resp) => {
      this.isSendingRequest = false
      this.collectionResults[idx] = resp
      this.requestUpdate()
    })
  }

  renderCollectionStep(stepData, idx, result, saveError) {
    const stepResult = this.collectionResults[idx]
    const hasResults = !!result
    const hasFailingAssertions = result?.assert_results?.some((a) => !a.ok || a.ok === false) || false
    const svErr = saveError !== undefined
    saveError = saveError ? saveError : {}
    const configuredOptions = {
      'request-options': (stepData.headers || []).length,
      'query-params': (stepData.params || []).length,
      'request-body': stepData.json || stepData.raw || stepData._requestBody ? 1 : 0,
    }
    const activeTab = this.collectionSteps[idx].activeTab || 'request-options'
    const setActiveTab = (tab) => {
      this.collectionSteps[idx].activeTab = tab
      this.requestUpdate()
    }

    const totalConfigured = Object.values(configuredOptions).reduce((a, b) => a + b, 0)
    return html`
      <div
        class="rounded-lg overflow-hidden border group/item collectionStep bg-base-100 draggable shadow-md ${
          hasFailingAssertions || svErr ? 'border-red-500' : hasResults ? 'border-green-500' : 'border-slate-200'
        }"
        data-index="${idx}"
      >
        <div class="flex flex-row items-center bg-gray-50">
          <div class="h-full shrink bg-gray-50 p-3 border-r border-r-slate-200 cursor-move "
            draggable="true"
            @dragstart="${(e) => e.dataTransfer.setData('text/plain', e.target.dataset.index)}"
          >${faSprite_('grip-dots-vertical', 'solid', 'h-4 w-4')}</div>
          <div class="flex-1 flex flex-row items-center gap-1 bg-base-100 pr-5 py-3">
            <label for="stepState-${idx}" class="p-3 cursor-pointer text-xs text-slate-700" @click="${() => this.toggleExpanded(idx)}">${idx + 1}</label>
            <label for="stepState-${idx}" class="p-3 cursor-pointer" @click="${() => this.toggleExpanded(idx)}">
              ${faSprite_('chevron-right', 'solid', 'h-4 w-3 ' + (stepData._expanded ? 'rotate-90' : ''))}
            </label>
            <div class="w-full space-y-1 relative">
              <div class="absolute right-0 items-center gap-3 text-xs text-gray-600 hidden group-hover/item:flex">
                <a class="text-red-700 cursor-pointer" @click="${() => (this.collectionSteps = this.collectionSteps.filter((_, i) => i != idx))}">
                  ${faSprite_('trash', 'regular', 'w-2 h-3')}
                </a>
              </div>
              <input class="text-lg w-full pl-2" placeholder="Give your step a name*" .value="${stepData.title || ''}" id="title-${idx}" @change=${(e) =>
      this.updateValue(e, idx, null, null, 'title')} />
            </div>
          </div>
        </div>
        <div class="border-t border-t-slate-200  p-3 ${stepData._expanded ? 'block' : 'hidden'} ">
          <div>
            <div class="space-y-2 bg-base-200 p-4">
              <div class="form-control">
                <div class="label label-text"><div>URL<span class="text-error">*</span></div></div>
                <div class="relative flex flex-row gap-2 items-center">
                  <label for="actions-list-input-${idx}" class="w-28 shrink text-sm font-medium form-control">
                    <select id="actions-list-input-${idx}" class="select select-sm select-bordered w-full" @change=${(e) => this.updateValue(e, idx, null, null, '_method')}>
                      ${validMethods.map((methodItem) => html`<option ?selected=${methodItem == stepData._method}>${methodItem}</option>`)}
                    </select>
                    ${saveError.method ? html`<span class="text-red-700 text-xs">${saveError.method}</span>` : ''}
                  </label>
                  <label for="actions-data" class="flex-1 text-sm font-medium form-control w-full flex flex-row items-center gap-1">
                    <input
                      type="text" id="actions-data-${idx}" class="input input-sm input-bordered w-full" @change=${(e) => this.updateValue(e, idx, null, null, '_url')}
                    />
                    ${saveError.url ? html`<span class="text-red-700 text-xs">${saveError.url}</span>` : ''}
                  </label>
                </div>
              </div>
              <details>
                <summary class="cursor-pointer text-sm">Advanced Options (${totalConfigured ? totalConfigured : 0} configured)</summary
                <div>
                  <div class="mt-4 border-l-2 border-l-slate-300 pl-4 pb-3 bg-base-100">
                    <div role="tablist" class="tabs tabs-bordered pt-1">
                      <a role="tab" class="tab ${activeTab === 'request-options' ? 'tab-active' : ''}" @click=${() => setActiveTab('request-options')}>
                        Request Options ${configuredOptions['request-options'] > 0 ? html`<span class="badge badge-sm badge-ghost">${configuredOptions['request-options']}</span>` : ''}
                      </a>
                      <a role="tab" class="tab ${activeTab === 'query-params' ? 'tab-active' : ''}" @click=${() => setActiveTab('query-params')}>
                        Query Params ${configuredOptions['query-params'] > 0 ? html`<span class="badge badge-sm badge-ghost">${configuredOptions['query-params']}</span>` : ''}
                      </a>
                      <a role="tab" class="tab ${activeTab === 'request-body' ? 'tab-active' : ''}" @click=${() => setActiveTab('request-body')}>
                        Request Body ${configuredOptions['request-body'] > 0 ? html`<span class="badge badge-sm badge-ghost">${configuredOptions['request-body']}</span>` : ''}
                      </a>
                    </div>
                    <div class="border-b border-x border-base-300 p-4 rounded-b-lg space-y-3">
                      ${
                        activeTab === 'request-options'
                          ? html`
                              <div class="form-control w-full">
                                <div class="label"><span class="label-text">HTTP Version</span></div>
                                <select class="select select-sm select-bordered max-w-xs" @change=${(e) => this.updateValue(e, idx, null, null, 'httpVersion')}>
                                  <option value="http2-http1">HTTP/2 fallback to HTTP/1.1</option>
                                  <option value="http2">HTTP/2 Only</option>
                                  <option value="http1">HTTP/1.1</option>
                                </select>
                              </div>
                              <div>
                                <div class="form-control">
                                  <label class="label cursor-pointer justify-start gap-3">
                                    <input
                                      type="checkbox"
                                      class="checkbox checkbox-sm"
                                      ?checked=${stepData.followRedirects}
                                      @change=${(e) => (this.collectionSteps[idx].followRedirects = e.target.value == 'on')}
                                    />
                                    <span class="label-text">Follow redirects</span>
                                  </label>
                                </div>
                                <div class="form-control">
                                  <label class="label cursor-pointer justify-start gap-3">
                                    <input
                                      type="checkbox"
                                      class="checkbox checkbox-sm"
                                      ?checked=${stepData.ignoreCertificateError}
                                      @change=${(e) => (this.collectionSteps[idx].ignoreCertificateError = e.target.value == 'on')}
                                    />
                                    <span class="label-text">Ignore server certificate error</span>
                                  </label>
                                </div>
                              </div>

                              <div class="flex items-center gap-3 label-text">
                                <span class="">Time out after</span>
                                <input
                                  type="number"
                                  value=${stepData.timeout || 60}
                                  class="input input-bordered input-sm w-20"
                                  @change=${(e) => (this.collectionSteps[idx].timeout = parseInt(e.target.value))}
                                />
                                <span class="text-sm">seconds</span>
                              </div>
                              <div class="form-control w-full">
                                <div class="label"><span class="label-text">Request Headers</span></div>
                                <div class="space-y-2 paramRows" id="[${idx}][headers]">${this.renderParamsRows(stepData, idx, 'headers')}</div>
                              </div>
                              <div class="form-control w-full">
                                <div class="label"><span class="label-text">Cookies</span></div>
                                <textarea
                                  class="textarea textarea-bordered"
                                  placeholder="cookie-name-1=value; cookie-name-2=value"
                                  @change=${(e) => ((this.collectionSteps[idx].headers ??= {}).Cookie = e.target.value)}
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
                                    Add all parameters that require encoding to the below fields. Query parameters that do not require encoding can be added to the URL field directly.
                                  </p>
                                </div>
                                <div class="space-y-2 paramRows" id="[${idx}][params]">${this.renderParamsRows(stepData, idx, 'params')}</div>
                              </div>
                            `
                          : nothing
                      }
                      ${
                        activeTab === 'request-body'
                          ? html`
                              <div class="form-control">
                                <div class="label items-start gap-2"><span class="label-text">Body Type</span></div>
                                <select class="select select-sm select-bordered max-w-xs" @change=${(e) => (this.collectionSteps[idx]._requestType = e.target.value)}>
                                  <option selected>application/json</option>
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
                                <textarea class="w-full border border-slate-200" name="[${idx}][json]" @change=${(e) => (this.collectionSteps[idx]._requestBody = e.target.value)}>
${stepData._requestBody}</textarea
                                >
                              </div>
                            `
                          : nothing
                      }
                    </div>
                  </div>
                </details>
              </div>
              <button class="btn btn-sm btn-primary mt-5" ?disabled=${!stepData._url} @click=${(e) => this.sendStepRequest(e, idx)}>
              ${this.isSendingRequest ? html`<span class="loading loading-dots loading-sm"></span>` : 'Send'}
              </button>
              <br/>
              ${
                stepResult && stepResult.resp
                  ? html`
                      <div class="py-3">The request responded with a status of <strong>${stepResult.resp.status}</strong>and took <strong>${stepResult.resp.duration_ms}</strong> ms</div>
                      <h3 class="text-xl py-3">Request Preview</h3>
                      <div class="bg-base-200 p-3">${unsafeHTML(generateRequestPreviewFromObject(this.collectionSteps[idx]))}</div>
                    `
                  : nothing
              }
            </div>

            <details class="mt-8" ?open=${stepResult && stepResult.resp}>
              <summary class="label-text text-lg mb-2 cursor-pointer">
                <div class="inline-flex gap-2 items-center">
                  Add Assertions (optional)
                  <a href="https://apitoolkit.io/docs/dashboard/dashboard-pages/api-tests/#test-definition-syntax" class="" target="_blank">
                    ${faSprite_('circle-info', 'regular', 'w-3 h-3 text-slate-700')}
                  </a>
                </div>
              </summary>
              <div class="text-sm space-y-2 px-2 paramRows [&_.assertIndicator]:inline-block" id="[${idx}][asserts]">
                ${
                  stepResult && stepResult.resp
                    ? html`
                        <div role="tablist" class="tabs tabs-boxed max-h-96 overflow-y-auto">
                          <input type="radio" name="resp-items" role="tab" class="tab" aria-label="Response Headers" checked />
                          <div role="tabpanel" class="tab-content bg-base-100 p-3">
                            ${Object.entries(stepResult.resp.headers).map(
                              ([key, value]) => html` <span class="hover:bg-yellow-200 cursor-pointer" @click="${(e) => addAssertion(e, 'header', value)}"> ${key}: ${value} </span><br /> `
                            )}
                          </div>

                          <input type="radio" name="resp-items" role="tab" class="tab" aria-label="Response Body" />
                          <div role="tabpanel" class="tab-content bg-base-100 p-3">
                            <div>{</div>
                            <div class="pl-3">${renderJsonWithIndentation(stepResult.resp.json, '$')}</div>
                            <div>}</div>
                          </div>

                          <input type="radio" name="resp-items" role="tab" class="tab" aria-label="Response Status Code" />
                          <div role="tabpanel" class="tab-content bg-base-100 p-3">${stepResult.resp.status}</div>
                        </div>
                      `
                    : nothing
                }
                <p class="mt-5">Your step is successful:</p>
          ${renderAssertionBuilder({
            assertions: this.collectionSteps[idx]._assertions || [],
            result: this.collectionResults[idx],
            updateAssertion: (index, updates) => this.updateAssertion(idx, index, updates),
            addAssertion: (e) => this.addAssertion(e, idx),
          })}
              </div>
            </details>
            <details class="mt-5">
              <summary class="label-text text-lg mb-2 cursor-pointer"><div class="inline-flex gap-2 items-center ">Extract variables from the response (optional)</div></summary>
              <div class="text-sm space-y-2 px-2 paramRows" id="[${idx}][exports]">
                <p>Variables consist of a variable name and a json path pointing to the variable in the response.</p>
                ${this.renderParamsRows(stepData, idx, 'exports')}
              </div>
            </div>
          </div>
        </div>
      </div>
    `
  }

  renderAssertResult(result) {
    let hasPassed = result?.ok === true || false
    let notRun = !result
    let error = result?.err?.advice || ''

    if (hasPassed) {
      return html` <svg class="icon w-3 h-3 text-green-500"><use href="/public/assets/svgs/fa-sprites/solid.svg#check"></use></svg>`
    }
    if (!hasPassed && !notRun) {
      return html`<span title="${error}"
        ><svg class="icon w-3 h-3 text-red-500"><use href="/public/assets/svgs/fa-sprites/regular.svg#trash"></use></svg><span></span
      ></span>`
    }
    return html`<span title="${error}" class="opacity-0"
      ><svg class="icon w-3 h-3 text-red-500"><use href="/public/assets/svgs/fa-sprites/regular.svg#trash"></use></svg><span></span
    ></span>`
  }

  renderParamRow(key, value, type, idx, aidx, result, saveError) {
    let error = result?.err?.advice || ''
    let keyError = ''
    if (saveError) {
      error = saveError.value ? saveError.value : error
      keyError = saveError.key ? saveError.key : ''
    }

    return html`
      <div class="flex flex-row gap-2 w-full paramRow">
        <span class="shrink hidden assertIndicator"> ${this.renderAssertResult(result)} </span>
        <div class="flex flex-col w-1/3">
          <input class="input input-bordered input-xs w-full" list="${type}DataList" placeholder="Key" .value="${key}" @change=${(e) => this.updateKey(e, idx, type, aidx)} />
          <span class="text-xs text-red-500 w-full">${keyError}</span>
        </div>
        <div class="shrink w-full flex flex-col">
          <input
            list="${type === 'asserts' ? 'assertAutocomplete-' + idx : ''}"
            class="input input-bordered ${error ? 'input-error' : ''} input-xs w-full"
            placeholder="Value"
            .value="${value}"
            @input=${(e) => this.updateValue(e, idx, type, aidx, key)}
          />
          <span class="text-xs text-red-500">${error}</span>
        </div>
        <a class="cursor-pointer text-slate-600" @click=${(e) => this.deleteKey(e, idx, type, aidx, key)}>
          <svg class="inline-block icon w-3 h-3 "><use href="/public/assets/svgs/fa-sprites/regular.svg#trash"></use></svg>
        </a>
      </div>
    `
  }

  renderParamsRows(stepData, idx, type, results) {
    let rows
    const errors = this.saveErrors[idx] ? this.saveErrors[idx][type] || [] : []
    if (type === 'asserts') {
      let matches = []
      let fieldPathValues = new Set()
      let resultContainer = document.querySelector('#res-container-' + idx)
      let elements = []
      if (resultContainer) {
        elements = resultContainer.querySelectorAll('[data-field-path]')
      }
      elements.forEach((element) => {
        let path = element.getAttribute('data-field-path')
        if (path) {
          fieldPathValues.add(('$.resp.json.' + path.replace(/\.?(\d+)\.?/g, '.[$1].')).replace('..', '.'))
        }
      })

      matches = Array.from(fieldPathValues)

      const data = stepData[type] || []
      rows = data.map((assertObj, aidx) => Object.entries(assertObj).map(([key, value]) => this.renderParamRow(key, value, type, idx, aidx, results[aidx], errors[aidx])))
      if (rows.length === 0 || !Object.entries(data).some(([k, v]) => k.trim() === '' && v.trim() === '')) {
        rows.push(this.renderParamRow('', '', type, idx, rows.length))
      }
      rows.push(html`
        <datalist id=${'assertAutocomplete-' + idx}>
          ${matches.map((fieldPath) => {
            return html`<option class="w-full  text-left text-xs px-3 py-1 hover:bg-gray-200">${fieldPath}</option>`
          })}
        </datalist>
      `)
    } else {
      const data = stepData[type] || {}
      rows = Object.entries(data)
        .filter(([key, _]) => key != 'Cookie')
        .map(([key, value], ind) => this.renderParamRow(key, value, type, idx, null, undefined, errors[ind]))
      if (rows.length === 0 || !Object.entries(data).some(([k, v]) => k.trim() === '' && v.trim() === '')) {
        rows.push(this.renderParamRow('', '', type, idx))
      }
    }
    return html`${rows}`
  }

  updateKey(event, idx, type, aidx) {
    const newKey = event.target.value
    const oldKey = event.target.defaultValue
    const stepData = this.collectionSteps[idx]

    const updateObject = (obj, oldKey, newKey) => {
      const oldValue = obj[oldKey]
      delete obj[oldKey]
      obj[newKey] = oldValue || ''
    }

    if (type == null) {
      updateObject(stepData, oldKey, newKey)
      this.requestUpdate()
      console.log('updateKey resp: ids=>${idx}, type=>${type}, aidx=>${aidx} => this.collectionSteps[${idx}] => ', this.collectionSteps[idx])
      return
    }

    stepData[type] = stepData[type] || (aidx != null ? [] : {})

    if (aidx != null) {
      const arrayItem = stepData[type][aidx] || {}
      const values = Object.entries(arrayItem)
      if (values.length > 0) {
        const val = values[0][1]
        stepData[type][aidx] = {
          [newKey]: val,
        }
      } else {
        stepData[type][aidx] = {
          [newKey]: '',
        }
      }
    } else {
      updateObject(stepData[type], oldKey, newKey)
    }
    this.requestUpdate()
    console.log(`updateKey resp: ids=>${idx}, type=>${type}, aidx=>${aidx} => this.collectionSteps[${idx}] => `, this.collectionSteps[idx])
  }

  deleteKey(_event, idx, type, aidx, oldKey) {
    const stepData = this.collectionSteps[idx]
    stepData[type] = stepData[type] || (aidx != null ? [] : {})

    if (aidx != null) {
      stepData[type].splice(aidx, 1)
    } else {
      delete stepData[type][oldKey]
    }
    this.requestUpdate()
  }

  updateValue(event, idx, type, aidx, key) {
    const value = event.target.value
    if (type == null) {
      this.collectionSteps[idx][key] = value
      this.requestUpdate()
      console.log(`updateValue resp: ids=>${idx}, type=>${type}, aidx=>${aidx}, key=${key}  this.collectionSteps[${idx}] => `, this.collectionSteps[idx])
      return
    }
    if (aidx != null) {
      if (key === '') {
        this.collectionSteps[idx][type][aidx] = { ok: value }
      } else {
        this.collectionSteps[idx][type][aidx][key] = value
      }
    } else {
      this.collectionSteps[idx][type][key] = value
    }
    this.requestUpdate()
    console.log(`updateValue resp: ids=>${idx}, type=>${type}, aidx=>${aidx}, key=${key}  this.collectionSteps[${idx}] => `, this.collectionSteps[idx])
  }

  render() {
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
            class="collectionSteps draggable-container pl-4 space-y-4"
            @dragover="${this._onDragOver}"
            @drop="${this._onDrop}"
            @dragenter="${this._onDragEnter}"
            @dragleave="${this._onDragLeave}"
          >
            ${this.collectionSteps.map((stepData, idx) => this.renderCollectionStep(stepData, idx, this.collectionResults[idx], this.saveErrors[idx]) || undefined)}
          </div>
          <div class="p-4 pt-2">
            <a class="btn btn-outline btn-neutral btn-sm items-center cursor-pointer" @click=${() => (this.collectionSteps = [...this.collectionSteps, {}])}>
              <svg class="inline-block icon w-3 h-3"><use href="/public/assets/svgs/fa-sprites/solid.svg#plus"></use></svg>
              Add a step to test
            </a>
          </div>
        </div>
      </div>
    `
  }

  // Assume that stepIndex is available in your component
  updateAssertion(stepIdx, index, updates) {
    this.collectionSteps[stepIdx]._assertions = this.collectionSteps[stepIdx]._assertions.map((assertion, i) => (i === index ? { ...assertion, ...updates } : assertion))
    // Optionally re-evaluate the assertion after the update
    const updatedAssertion = this.collectionSteps[stepIdx]._assertions[index]
    const status = this.evaluateAssertion(updatedAssertion) ? 'PASSED' : 'FAILED'
    this.collectionSteps[stepIdx]._assertions = this.collectionSteps[stepIdx]._assertions.map((assertion, i) => (i === index ? { ...assertion, status } : assertion))
    this.requestUpdate()
  }

  addAssertion(e, idx) {
    e.preventDefault()
    this.collectionSteps[idx]._assertions = [
      ...this.collectionSteps[idx]._assertions,
      {
        type: 'body',
        operation: 'equals',
        value: '',
        status: 'PASSED',
      },
    ]
    this.requestUpdate()
  }

  evaluateAssertion(assertion) {
    // Implement your assertion evaluation logic here
    if (assertion.type === 'body' && assertion.operation === 'contains') {
      return this.response.body.includes(assertion.value)
    }
    // Handle other types and operations
    return false // Default to false if not matched
  }
}

function faSprite_(iconName, kind, classes) {
  return html`<svg class="${classes}"><use href="/public/assets/svgs/fa-sprites/${kind}.svg#${iconName}"></use></svg>`
}

customElements.define('steps-editor', StepsEditor)
