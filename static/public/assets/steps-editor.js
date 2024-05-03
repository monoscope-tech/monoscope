import { LitElement, html, ref, createRef } from './js/thirdparty/lit.js'

export class StepsEditor extends LitElement {
  static properties = {
    collectionSteps: [],
  }

  constructor() {
    super()
    this.collectionSteps = window.collectionSteps || [{}]

    require.config({ paths: { vs: '/assets/js/monaco/vs' } })
    require.config({ paths: { vs: 'https://unpkg.com/monaco-editor/min/vs' } })
    require(['vs/editor/editor.main'], () => {
      this.initializeEditor(monaco)
    })
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

  updated(_changedProperties) {
    this.updateEditorContent()
  }

  createRenderRoot() {
    return this
  }

  methodAndUrl(obj) {
    const validMethods = ['GET', 'POST', 'PUT', 'DELETE', 'PATCH', 'HEAD', 'OPTIONS', 'TRACE', 'CONNECT']
    const validEntries = Object.entries(obj).filter(([method, url]) => validMethods.includes(method) && url !== null)
    const found = validEntries.length > 0 ? validEntries[0] : null
    return found ? { method: found[0], url: found[1] } : { method: '', url: '' }
  }


  _onDragOver(event) {
    event.preventDefault();
    const items = document.querySelectorAll('.draggable');
    let closestItem = null;
    let smallestDistance = Number.MAX_SAFE_INTEGER;

    items.forEach(item => {
      const box = item.getBoundingClientRect();
      const midpoint = box.top + box.height / 2;
      const distance = Math.abs(event.clientY - midpoint);
      if (distance < smallestDistance) {
        closestItem = item;
        smallestDistance = distance;
      }
    });

    items.forEach(item => {
      if (item === closestItem) {
        item.classList.add('active-drop-target');
      } else { item.classList.remove('active-drop-target') }
    });
  }

  _onDragEnter(event) {
    if (event.target.hasAttribute('data-index')) {
      event.preventDefault();  // Necessary to allow dropping
      event.target.classList.add('over');  // Highlight the drop target only if it has data-index
    }
  }

  _onDragLeave(event) {
    if (event.target.hasAttribute('data-index')) {
      event.target.classList.remove('over');
      event.target.classList.remove('active-drop-target');  // Remove additional highlight
    }
  }

  _onDrop(event) {
    const activeTarget = document.querySelector('.active-drop-target');
    if (activeTarget) {
      event.preventDefault();
      activeTarget.classList.remove('over');
      activeTarget.classList.remove('active-drop-target');
      const originIndex = parseInt(event.dataTransfer.getData('text/plain'));
      const targetIndex = parseInt(activeTarget.dataset.index);
      if (targetIndex !== originIndex) {
        const movedItems = [...this.collectionSteps];
        const item = movedItems.splice(originIndex, 1)[0];
        movedItems.splice(targetIndex, 0, item);
        this.collectionSteps = [...movedItems];
        this.requestUpdate();
      }
    }
  }


  renderCollectionStep(stepData, idx) {
    const { method, url } = this.methodAndUrl(stepData)
    return html`
      <div
        class="rounded-lg overflow-hidden border border-slate-200 group/item collectionStep bg-white draggable"
        draggable="true"
        @dragstart="${(e) => e.dataTransfer.setData('text/plain', e.target.dataset.index)}"
        @dragover="${this._onDragOver}"
        @drop="${this._onDrop}"
        @dragenter="${this._onDragEnter}"
        @dragleave="${this._onDragLeave}"
        data-index="${idx}"
      >
        <input type="checkbox" id="stepState-${idx}" class="hidden stepState" />
        <div class="flex flex-row items-center bg-gray-50">
          <div class="h-full shrink bg-gray-50 p-3 border-r border-r-slate-200">
            <svg class="h-4 w-4"><use href="/assets/svgs/fa-sprites/solid.svg#grip-dots-vertical"></use></svg>
          </div>
          <div class="flex-1 flex flex-row items-center gap-1 bg-white pr-5 py-3">
            <label for="stepState-${idx}" class="p-3 cursor-pointer text-xs text-slate-700">${idx + 1}</label>
            <label for="stepState-${idx}" class="p-3 cursor-pointer">
              <svg class="h-4 w-3 group-has-[.stepState:checked]/item:rotate-90"><use href="/assets/svgs/fa-sprites/solid.svg#chevron-right"></use></svg>
            </label>
            <div class="w-full space-y-1 relative">
              <div class="absolute right-0 items-center gap-3 text-xs text-gray-600 hidden group-hover/item:flex">
                <button>View results</button>
                <button class="text-blue-600">
                  <svg class="w-2 h-3"><use href="/assets/svgs/fa-sprites/solid.svg#play"></use></svg>
                </button>
                <a class="text-red-700" @click="${() => (this.collectionSteps = this.collectionSteps.filter((_, i) => i != idx))}">
                  <svg class="w-2 h-3"><use href="/assets/svgs/fa-sprites/solid.svg#xmark"></use></svg>
                </a>
              </div>
              <input class="text-lg w-full" placeholder="Untitled" .value="${stepData.title || ''}" id="title-${idx}" @change=${(e) => this.updateValue(e, idx, null, null, 'title')} />
              <div class="relative flex flex-row gap-2 items-center">
                <label for="actions-list-input-${idx}" class="w-28 shrink text-sm font-medium form-control">
                  <input
                    list="actions-list"
                    id="actions-list-input-${idx}"
                    class="input input-sm input-bordered w-full"
                    placeholder="method"
                    value="${method}"
                    @change=${(e) => this.updateKey(e, idx, null, null)}
                  />
                </label>
                <label for="actions-data" class="flex-1 text-sm font-medium form-control w-full flex flex-row items-center gap-1">
                  <input
                    type="text"
                    id="actions-data-${idx}"
                    class="input input-sm input-bordered w-full"
                    placeholder="Request URI"
                    .value="${url}"
                    @change=${(e) => this.updateValue(e, idx, null, null, method)}
                  />
                </label>
              </div>
            </div>
          </div>
        </div>
        <div class="border-t border-t-slate-200 space-y-3 p-3 hidden group-has-[.stepState:checked]/item:block">
          <div role="tablist" class="tabs tabs-bordered pt-1">
            <input type="radio" name="_httpOptions-${idx}" role="tab" class="tab" aria-label="Params" checked />
            <div role="tabpanel" class="tab-content px-2 py-4 space-y-2 paramRows" id="[${idx}][params]">${this.renderParamsRows(stepData, idx, 'params')}</div>

            <input type="radio" name="_httpOptions-${idx}" role="tab" class="tab" aria-label="Headers" />
            <div role="tabpanel" class="tab-content px-2 py-4 space-y-2 paramRows" id="[${idx}][headers]">${this.renderParamsRows(stepData, idx, 'headers')}</div>

            <input type="radio" name="_httpOptions-${idx}" role="tab" class="tab" aria-label="Body" />
            <div role="tabpanel" class="tab-content px-2 py-4">
              <select class="peer select select-sm select-bordered" data-chosen="json" @change="${(e) => e.target.setAttribute('data-chosen', e.target.value)}">
                <option selected>json</option>
                <option>raw</option>
              </select>
              <div class="hidden peer-data-[chosen=json]:block">
                <textarea class="w-full border border-slate-200" name="[${idx}][json]" @change=${(e) => this.updateValue(e, idx, null, null, 'json')}>${JSON.stringify(stepData.json)}</textarea>
              </div>
              <div class="hidden peer-data-[chosen=raw]:block">
                <textarea class="w-full border border-slate-200" name="[${idx}][raw]" @change=${(e) => this.updateValue(e, idx, null, null, 'raw')}>${stepData.raw}</textarea>
              </div>
            </div>
          </div>
          <div>
            <h5 class="label-text p-1 mb-2">Assertions</h5>
            <div class="text-sm space-y-2 px-2 paramRows [&_.assertIndicator]:inline-block" id="[${idx}][asserts]">${this.renderParamsRows(stepData, idx, 'asserts')}</div>
          </div>
          <div>
            <h5 class="label-text p-1 mb-2">Exports</h5>
            <div class="text-sm space-y-2 px-2 paramRows" id="[${idx}][exports]">${this.renderParamsRows(stepData, idx, 'exports')}</div>
          </div>
        </div>
      </div>
    `
  }

  renderParamRow(key, value, type, idx, aidx) {
    return html`
      <div class="flex flex-row items-center gap-2  paramRow">
        <span class="shrink hidden assertIndicator">✅</span>
        <input class="input input-bordered input-xs w-1/3" placeholder="Key" .value="${key}" @change=${(e) => this.updateKey(e, idx, type, aidx)} />
        <input class="input input-bordered input-xs w-full" placeholder="Value" .value="${value}" @input=${(e) => this.updateValue(e, idx, type, aidx, key)} />
        <a class="cursor-pointer text-red-700" @click=${(e) => this.deleteKey(e, idx, type, aidx, key)}
          ><svg class="inline-block icon w-3 h-3 "><use href="/assets/svgs/fa-sprites/solid.svg#xmark"></use></svg
        ></a>
      </div>
    `
  }

  renderParamsRows(stepData, idx, type) {
    let rows
    if (type === 'asserts') {
      const data = stepData[type] || []
      rows = data.map((assertObj, aidx) => Object.entries(assertObj).map(([key, value]) => this.renderParamRow(key, value, type, idx, aidx)))
      if (rows.length === 0 || !Object.entries(data).some(([k, v]) => k.trim() === '' && v.trim() === '')) {
        rows.push(this.renderParamRow('', '', type, idx, rows.length))
      }
    } else {
      const data = stepData[type] || {}
      rows = Object.entries(data).map(([key, value]) => this.renderParamRow(key, value, type, idx, null))
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
      return
    }

    stepData[type] = stepData[type] || (aidx != null ? [] : {})

    if (aidx != null) {
      const arrayItem = stepData[type][aidx] || {}
      updateObject(arrayItem, oldKey, newKey)
      stepData[type][aidx] = arrayItem
    } else {
      updateObject(stepData[type], oldKey, newKey)
    }

    this.requestUpdate()
  }

  deleteKey(_event, idx, type, aidx, oldKey) {
    const stepData = this.collectionSteps[idx]
    stepData[type] = stepData[type] || (aidx != null ? [] : {})

    if (aidx != null) {
      const arrayItem = stepData[type][aidx] || {}
      delete arrayItem[oldKey]
      stepData[type][aidx] = arrayItem
    } else {
      delete stepData[type][oldKey]
    }
    this.requestUpdate() // Trigger a re-render
  }

  updateValue(event, idx, type, aidx, key) {
    const value = event.target.value
    if (type == null) {
      this.collectionSteps[idx][key] = value
      this.requestUpdate()
      return
    }
    if (aidx != null) {
      this.collectionSteps[idx][type][aidx][key] = value
    } else {
      this.collectionSteps[idx][type][key] = value
    }
    this.requestUpdate()
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
      <div id="collectionStepsContainer" class="h-full">
        <div id="steps-codeEditor" class="h-full max-h-screen hidden group-has-[.editorMode:checked]/colForm:block"></div>
        <div class="h-full overflow-y-scroll group-has-[.editorMode:checked]/colForm:hidden">
          <div id="collectionStepsContainer" class=" p-4 space-y-4 collectionSteps">${this.collectionSteps.map((stepData, idx) => this.renderCollectionStep(stepData, idx))}</div>
          <div class="p-4 pt-2">
            <a class="btn btn-outline btn-neutral btn-sm items-center cursor-pointer" @click=${() => (this.collectionSteps = [...this.collectionSteps, {}])}>
              <svg class="inline-block icon w-3 h-3"><use href="/assets/svgs/fa-sprites/solid.svg#plus"></use></svg>
              Add Another Step
            </a>
          </div>
        </div>
      </div>
    `
  }
}
customElements.define('steps-editor', StepsEditor)
