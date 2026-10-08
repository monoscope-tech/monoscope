import { EditorState } from '@codemirror/state';
import { EditorView, lineNumbers } from '@codemirror/view';
import { defaultHighlightStyle, syntaxHighlighting } from '@codemirror/language';
import { PostgreSQL, sql } from '@codemirror/lang-sql';
import { darkHighlightStyle, language } from './query-editor/language';

class QueryPreview extends HTMLElement {
  private view?: EditorView;
  private lifecycle?: AbortController;

  connectedCallback() {
    this.lifecycle = new AbortController();
    const panel = this.closest('[popover]');
    const show = () => {
      if (this.view) return;
      const dark = document.body.getAttribute('data-theme') === 'dark';
      const code = this.querySelector('pre');
      this.view = new EditorView({
        parent: this,
        state: EditorState.create({
          doc: code?.textContent ?? '',
          extensions: [
            this.getAttribute('language') === 'sql' ? sql({ dialect: PostgreSQL }) : language,
            syntaxHighlighting(dark ? darkHighlightStyle : defaultHighlightStyle),
            EditorState.readOnly.of(true), EditorView.editable.of(false),
            lineNumbers(), EditorView.lineWrapping,
            EditorView.contentAttributes.of({ 'aria-label': 'Widget query', tabindex: '0' }),
            EditorView.theme({
              '&': { backgroundColor: 'transparent', color: 'inherit', fontSize: '12px' },
              '.cm-scroller': { fontFamily: 'var(--font-mono, monospace)', lineHeight: '1.7' },
              '.cm-gutters': { backgroundColor: 'transparent', border: 'none', color: 'var(--color-textWeak)' },
              '.cm-content': { padding: '8px 0' },
              '&.cm-focused': { outline: 'none' },
            }, { dark }),
          ],
        }),
      });
      if (code) code.hidden = true;
    };
    panel?.addEventListener('beforetoggle', event => {
      if ((event as ToggleEvent).newState === 'open') show();
    }, { signal: this.lifecycle.signal });
    if (panel?.matches(':popover-open')) show();
  }

  disconnectedCallback() {
    this.lifecycle?.abort();
    this.view?.destroy();
    this.view = undefined;
  }
}
customElements.define('query-preview', QueryPreview);
