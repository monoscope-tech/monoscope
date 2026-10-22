import { CrepeBuilder } from '@milkdown/crepe/builder';
import { topBar } from '@milkdown/crepe/feature/top-bar';
import { linkTooltip } from '@milkdown/crepe/feature/link-tooltip';
import { listItem } from '@milkdown/crepe/feature/list-item';
import { table } from '@milkdown/crepe/feature/table';
import { placeholder } from '@milkdown/crepe/feature/placeholder';
import { cursor } from '@milkdown/crepe/feature/cursor';
import { editorViewCtx } from '@milkdown/kit/core';
import { uploadConfig } from '@milkdown/kit/plugin/upload';
import { replaceAll } from '@milkdown/kit/utils';
import '@milkdown/crepe/theme/common/prosemirror.css';
import '@milkdown/crepe/theme/common/reset.css';
import '@milkdown/crepe/theme/common/top-bar.css';
import '@milkdown/crepe/theme/common/link-tooltip.css';
import '@milkdown/crepe/theme/common/list-item.css';
import '@milkdown/crepe/theme/common/table.css';
import '@milkdown/crepe/theme/common/placeholder.css';
import '@milkdown/crepe/theme/common/cursor.css';
import '@milkdown/crepe/theme/frame.css';
import './wiki-editor.css';

class WikiEditor extends HTMLElement {
  private lifetime?: AbortController;
  private editor?: CrepeBuilder;

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
    const editor = new CrepeBuilder({ root, defaultValue: source.value })
      .addFeature(topBar).addFeature(linkTooltip).addFeature(listItem)
      .addFeature(table).addFeature(placeholder).addFeature(cursor);
    editor.editor.config(ctx => ctx.update(uploadConfig.key, config => ({ ...config, uploader: async () => [] })));
    try {
      await editor.create();
      if (lifetime.signal.aborted) { await editor.destroy(); return; }
      this.editor = editor;
      // Adopt typing that happened while the widget was loading.
      editor.editor.action(replaceAll(source.value));
      const document = () => editor.editor.action(ctx => ctx.get(editorViewCtx).state.doc);
      let original = document();
      let originalMarkdown = source.value;
      const sync = () => {
        if (this.dataset.mode === 'rich') source.value = document().eq(original) ? originalMarkdown : editor.getMarkdown();
      };
      editor.on(listener => listener.markdownUpdated(sync));
      source.form!.addEventListener('submit', sync, { capture: true, signal: lifetime.signal });
      mode.addEventListener('change', () => {
        sync();
        if (mode.value === 'rich') {
          editor.editor.action(replaceAll(source.value));
          original = document();
          originalMarkdown = source.value;
        }
        this.dataset.mode = mode.value;
        source.required = mode.value === 'source';
      }, { signal: lifetime.signal });
      editor.editor.action(ctx => {
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
      if (wasFocused) editor.editor.action(ctx => ctx.get(editorViewCtx).focus());
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
