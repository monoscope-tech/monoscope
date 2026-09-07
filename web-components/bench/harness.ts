import { schemaManager } from '../src/query-editor/schema-manager';
import type { SchemaData } from '../src/query-editor/schema-index';
const params = new URLSearchParams(location.search);
const engine = params.get('engine') || 'codemirror';
const host = document.getElementById('host')!;
const count = Number(params.get('fields') || 1000),
  shape = params.get('shape') || 'flat';
const fields: SchemaData['fields'] = {
  status_code: { type: 'string', examples: ['OK', 'ERROR'] },
  attributes: { type: 'object' },
  level: { type: 'string' },
};
for (let i = 0; i < count; i++)
  fields[(shape === 'nested' ? 'attributes.' : shape === 'deep' ? 'attributes.a.b.c.d.e.f.g.h.' : '') + `field_${i}`] = { type: 'string' };
const schema = { fields };
let element: any,
  editor: any,
  getValue: () => string,
  setValue: (value: string) => void,
  focus: () => void,
  complete: () => Promise<unknown>;
const start = performance.now();
if (engine === 'baseline') {
  const module = await import('virtual:baseline-editor');
  (await import('virtual:baseline-config')).initializeDefaultSchema();
  element = document.createElement('query-editor-baseline');
  host.append(element);
  await element.updateComplete;
  module.schemaManager.setSchemaData('spans', schema);
  editor = element.editor;
  getValue = () => editor.getValue();
  setValue = (value) => {
    editor.setValue(value);
    const model = editor.getModel();
    editor.setPosition({ lineNumber: model.getLineCount(), column: model.getLineMaxColumn(model.getLineCount()) });
  };
  focus = () => editor.focus();
  complete = () => element.refreshSuggestions();
} else if (engine === 'monaco') {
  await import('virtual:optimized-editor');
  schemaManager.acquire(1);
  schemaManager.setSchemaData('spans', schema);
  element = document.createElement('query-editor-monaco');
  host.append(element);
  await element.updateComplete;
  editor = element.editor;
  getValue = () => editor.getValue();
  setValue = (value) => {
    editor.setValue(value);
    const model = editor.getModel();
    editor.setPosition({ lineNumber: model.getLineCount(), column: model.getLineMaxColumn(model.getLineCount()) });
  };
  focus = () => editor.focus();
  complete = () => element.refreshSuggestions();
} else if (engine === 'textarea') {
  element = document.createElement('textarea');
  host.append(element);
  getValue = () => element.value;
  setValue = (value) => {
    element.value = value;
  };
  focus = () => element.focus();
  complete = async () => [];
} else {
  const { startCompletion, currentCompletions } = await import('@codemirror/autocomplete');
  await import('../src/query-editor/query-editor');
  element = document.createElement('query-editor');
  host.append(element);
  await element.updateComplete;
  schemaManager.setSchemaData('spans', schema);
  getValue = () => element.getValue();
  setValue = (value) => element.setValue(value);
  focus = () => element.focusEditor();
  complete = async () => {
    startCompletion(element.view);
    for (let i = 0; i < 200; i++) {
      await new Promise((r) => setTimeout(r, 10));
      if (currentCompletions(element.view.state).length) return currentCompletions(element.view.state).map((c) => c.label);
    }
    return [];
  };
}
const startupMs = performance.now() - start;
const longTasks: number[] = [];
if (PerformanceObserver.supportedEntryTypes.includes('longtask'))
  new PerformanceObserver((list) => longTasks.push(...list.getEntries().map((e) => e.duration))).observe({ type: 'longtask' });
Object.assign(window, {
  bench: {
    engine,
    startupMs,
    getValue,
    setValue,
    focus,
    complete,
    schemaManager,
    element,
    longTasks,
    async backend(text: string) {
      if (engine === 'textarea') return [];
      if (engine === 'baseline') {
        setValue(text);
        return element.refreshSuggestions();
      }
      return schemaManager.complete(text, 'spans', '', 1);
    },
    async cycle(n: number) {
      for (let i = 0; i < n; i++) {
        const el = document.createElement('query-editor');
        host.append(el);
        await (el as any).updateComplete;
        el.remove();
      }
    },
  },
});
