import { Editor, rootCtx, defaultValueCtx, editorViewCtx, commandsCtx } from '@milkdown/kit/core';
import { commonmark } from '@milkdown/kit/preset/commonmark';
import { gfm } from '@milkdown/kit/preset/gfm';
import { history } from '@milkdown/kit/plugin/history';
import { listener, listenerCtx } from '@milkdown/kit/plugin/listener';
import { clipboard } from '@milkdown/kit/plugin/clipboard';
import { indent } from '@milkdown/kit/plugin/indent';
import { trailing } from '@milkdown/kit/plugin/trailing';
import { getMarkdown, replaceAll } from '@milkdown/kit/utils';
import '@milkdown/crepe/theme/common/prosemirror.css';
import '@milkdown/crepe/theme/common/reset.css';
import '@milkdown/crepe/theme/frame.css';
import './wiki-editor.css';

class WikiEditor extends HTMLElement {
  private lifetime?: AbortController;
  private editor?: Editor;

  connectedCallback() {
    const lifetime = this.lifetime = new AbortController();
    void this.mount(lifetime);
  }

  private async mount(lifetime: AbortController) {
    const source = this.querySelector<HTMLTextAreaElement>('textarea')!;
    const root = this.querySelector<HTMLElement>('[data-rich-editor]')!;
    const mode = this.querySelector<HTMLSelectElement>('[data-editor-mode]')!;
    const controls = this.querySelector<HTMLElement>('[data-editor-controls]')!;
    const status = this.querySelector<HTMLElement>('[data-editor-status]')!;
    const editor = Editor.make().config(ctx => {
      ctx.set(rootCtx, root);
      ctx.set(defaultValueCtx, source.value);
    }).use(commonmark).use(gfm).use(history).use(listener).use(clipboard).use(indent).use(trailing);
    try {
      await editor.create();
      if (lifetime.signal.aborted) { await editor.destroy(); return; }
      this.editor = editor;
      // Adopt typing that happened while the widget was loading.
      editor.action(replaceAll(source.value));
      const document = () => editor.action(ctx => ctx.get(editorViewCtx).state.doc);
      let original = document();
      let originalMarkdown = source.value;
      const sync = () => {
        if (this.dataset.mode === 'rich') source.value = document().eq(original) ? originalMarkdown : editor.action(getMarkdown());
      };
      editor.action(ctx => ctx.get(listenerCtx).markdownUpdated(sync));
      controls.addEventListener('click', event => {
        const button = (event.target as Element).closest<HTMLButtonElement>('[data-editor-command]');
        if (!button) return;
        editor.action(ctx => {
          ctx.get(commandsCtx).call(button.dataset.editorCommand!, button.dataset.editorParameter ? Number(button.dataset.editorParameter) : undefined);
          ctx.get(editorViewCtx).focus();
        });
      }, { signal: lifetime.signal });
      source.form!.addEventListener('submit', sync, { capture: true, signal: lifetime.signal });
      mode.addEventListener('change', () => {
        sync();
        if (mode.value === 'rich') {
          editor.action(replaceAll(source.value));
          original = document();
          originalMarkdown = source.value;
        }
        this.dataset.mode = mode.value;
        source.required = mode.value === 'source';
      }, { signal: lifetime.signal });
      editor.action(ctx => {
        const view = ctx.get(editorViewCtx);
        view.dom.setAttribute('role', 'textbox');
        view.dom.setAttribute('aria-multiline', 'true');
        view.dom.setAttribute('aria-required', 'true');
        view.dom.setAttribute('aria-labelledby', 'memory-body-label');
        if (source.hasAttribute('aria-describedby')) view.dom.setAttribute('aria-describedby', source.getAttribute('aria-describedby')!);
      });
      root.hidden = controls.hidden = false;
      source.required = false;
      const wasFocused = source.matches(':focus');
      mode.value = this.dataset.mode = 'rich';
      if (wasFocused) editor.action(ctx => ctx.get(editorViewCtx).focus());
    } catch {
      if (!lifetime.signal.aborted) status.textContent = 'Rich-text editing could not load. You can still edit and save Markdown below.';
      lifetime.abort();
      this.editor = undefined;
      this.dataset.mode = 'source';
      source.required = true;
      root.hidden = controls.hidden = true;
      await editor.destroy();
    }
  }

  disconnectedCallback() {
    this.lifetime?.abort();
    void this.editor?.destroy();
    this.editor = undefined;
    this.dataset.mode = 'source';
  }
}

customElements.define('wiki-editor', WikiEditor);
