import { EditorView, ViewPlugin } from '@codemirror/view';
import { completionStatus, type Completion, type CompletionSection } from '@codemirror/autocomplete';
import type { Suggestion } from './completion';

const priorityFields = new Set(['status_code', 'level', 'method', 'name', 'duration', 'service', 'path', 'http_status', 'resource', 'attributes']);
const commonOperators = new Set(['==', '!=', '>', '<', 'contains', 'in']);
const section = (name: string, rank: number): CompletionSection => ({ name, rank });
const moreFields = section('More Fields', 1);
const common = section('Common', 0);
const moreOperators = section('More Operators', 1);
const librarySections = new Map(['Saved Views', 'Recent Searches', 'Popular Searches'].map((name, i) => [name.toLowerCase(), section(name, i + 2)]));

export function completionSection(item: Suggestion, items: Suggestion[]): CompletionSection | undefined {
  if (item.section) return librarySections.get(item.section.toLowerCase()) || section(item.section, 2);
  const grammar = items.filter(s => !s.section);
  if (grammar.length > 8 && grammar.every(s => s.kind === 'field'))
    return priorityFields.has(item.label) ? undefined : moreFields;
  if (grammar.length > 6 && grammar.every(s => s.kind === 'operator'))
    return commonOperators.has(item.label) ? common : moreOperators;
  return undefined;
}

export function completionIcon(completion: Completion): HTMLElement {
  const icon = document.createElement('span');
  icon.className = 'query-completion-icon';
  icon.setAttribute('aria-hidden', 'true');
  icon.textContent = completion.type === 'property' ? 'F' : completion.type === 'operator' ? '=' : completion.type === 'snippet' ? '↗' : 'V';
  return icon;
}

// CodeMirror keeps ownership of options, selection, scrolling and ARIA. Add the
// existing product chrome once per popup; no schema work or extra option tree.
export const completionChrome = ViewPlugin.fromClass(class {
  private queued = false;
  private destroyed = false;
  constructor(private view: EditorView) {}
  update() {
    if (this.queued || !completionStatus(this.view.state)) return;
    this.queued = true;
    queueMicrotask(() => {
      this.queued = false;
      if (this.destroyed) return;
      const popup = this.view.dom.querySelector<HTMLElement>('.query-completion-dropdown');
      if (!popup || popup.querySelector('.query-completion-help')) return;
      const hint = document.createElement('div');
      hint.className = 'query-completion-hint';
      hint.append('Type a field name, then an operator and value — e.g. ');
      const example = document.createElement('code');
      example.textContent = 'status_code == "ERROR"';
      hint.append(example);
      const footer = document.createElement('div');
      footer.className = 'query-completion-help';
      const keys = document.createElement('span');
      for (const [labels, text] of [[['↑', '↓', 'Tab'], ' to navigate  •  '], [['Enter'], ' to select  •  '], [['Esc'], ' to close']] as const) {
        for (const label of labels) {
          const key = document.createElement('kbd');
          key.textContent = label;
          keys.append(key, ' ');
        }
        keys.append(text);
      }
      const guide = document.createElement('a');
      guide.href = 'https://monoscope.tech/docs/dashboard/dashboard-pages/api-log-explorer/';
      guide.target = '_blank';
      guide.rel = 'noopener';
      guide.textContent = 'Syntax guide ↗';
      footer.append(keys, guide);
      popup.prepend(hint);
      popup.append(footer);
      this.view.requestMeasure();
    });
  }
  destroy() { this.destroyed = true; }
});

export const completionTheme = EditorView.theme({
  '.query-completion-dropdown.cm-tooltip': {
    position: 'absolute !important', top: '100% !important', left: '0 !important',
    width: '100%', maxWidth: 'none', marginTop: '4px', boxSizing: 'border-box',
    display: 'flex', flexDirection: 'column', maxHeight: '80dvh !important',
    borderRadius: '6px', overflow: 'hidden', fontFamily: 'var(--font-sans, system-ui, sans-serif)', fontSize: '12px',
    backgroundColor: 'var(--color-bgRaised, Canvas)', color: 'var(--color-textStrong, CanvasText)',
    border: '1px solid var(--color-strokeWeak, GrayText)', boxShadow: 'var(--shadow-lg, 0 8px 25px -5px rgb(0 0 0 / 0.1))', zIndex: '100',
  },
  '.query-completion-dropdown.cm-tooltip-autocomplete ul': { order: '0', flex: '1 1 auto', minHeight: '0', width: '100%', maxWidth: 'none', maxHeight: 'none !important', fontFamily: 'inherit', padding: '0', overscrollBehavior: 'contain' },
  '.query-completion-dropdown.cm-tooltip-autocomplete ul li[role=option]': { display: 'flex', alignItems: 'center', gap: '8px', padding: '4px 12px', minHeight: '28px', boxSizing: 'border-box', borderBottom: '1px solid var(--color-strokeWeak, GrayText)' },
  '.query-completion-dropdown.cm-tooltip-autocomplete ul li[role=option]:hover, .query-completion-dropdown.cm-tooltip-autocomplete ul li[aria-selected=true]': { backgroundColor: 'var(--color-fillBrand-weak, Highlight)', color: 'var(--color-textStrong, HighlightText)' },
  '.query-completion-dropdown.cm-tooltip-autocomplete .cm-completionLabel': { flex: '1', overflow: 'hidden', textOverflow: 'ellipsis' },
  '.query-completion-dropdown.cm-tooltip-autocomplete .cm-completionDetail': { marginLeft: '8px', fontStyle: 'normal', opacity: '1', color: 'var(--query-type-color, inherit)' },
  '.query-completion-dropdown.cm-tooltip-autocomplete .cm-completionMatchedText': { textDecoration: 'none', fontWeight: '600' },
  '.query-completion-dropdown.cm-tooltip-autocomplete ul completion-section': { padding: '6px 12px', fontWeight: '600', textTransform: 'uppercase', backgroundColor: 'var(--color-fillWeaker, #f4f5f8)', borderBottom: '1px solid var(--color-strokeWeak, GrayText)' },
  '.query-completion-icon': { display: 'inline-flex', alignItems: 'center', justifyContent: 'center', width: '20px', height: '20px', flexShrink: '0', borderRadius: '4px', border: '1px solid color-mix(in srgb, var(--color-sky-400, #38bdf8) 30%, transparent)', backgroundColor: 'color-mix(in srgb, var(--color-sky-400, #38bdf8) 15%, transparent)', color: 'var(--color-sky-400, #38bdf8)', fontWeight: '600' },
  '.query-completion-hint': { display: 'none', order: '-1', padding: '6px 12px', color: 'var(--color-textWeak, inherit)', backgroundColor: 'var(--color-fillWeaker, #f4f5f8)', borderBottom: '1px solid var(--color-strokeWeak, GrayText)' },
  '.query-completion-empty .query-completion-hint': { display: 'block' },
  '.query-completion-help': { order: '1', display: 'flex', flexWrap: 'wrap', justifyContent: 'space-between', gap: '8px', padding: '6px 12px', color: 'var(--color-textWeak, inherit)', borderTop: '1px solid var(--color-strokeWeak, GrayText)' },
  '.query-completion-help kbd, .query-completion-hint code': { padding: '0 3px', borderRadius: '3px', backgroundColor: 'var(--color-fillWeak, #f4f5f8)' },
  '.query-completion-help kbd': { border: '1px solid var(--color-strokeWeak, GrayText)' },
  '.query-completion-help a': { color: 'var(--color-textBrand, LinkText)' },
  '.query-type-string': { '--query-type-color': '#38bdf8' },
  '.query-type-number, .query-type-int': { '--query-type-color': '#34d399' },
  '.query-type-object': { '--query-type-color': '#a78bfa' },
  '.query-type-array': { '--query-type-color': '#2dd4bf' },
  '.query-type-duration': { '--query-type-color': '#fbbf24' },
  '.query-type-boolean': { '--query-type-color': '#fb923c' },
  '.query-type-bytes': { '--query-type-color': '#fb7185' },
});
