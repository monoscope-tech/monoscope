import { CrepeBuilder } from '@milkdown/crepe/builder';
import { blockEdit } from '@milkdown/crepe/feature/block-edit';
import { codeMirror } from '@milkdown/crepe/feature/code-mirror';
import { cursor } from '@milkdown/crepe/feature/cursor';
import { linkTooltip } from '@milkdown/crepe/feature/link-tooltip';
import { listItem } from '@milkdown/crepe/feature/list-item';
import { placeholder } from '@milkdown/crepe/feature/placeholder';
import { table } from '@milkdown/crepe/feature/table';
import { toolbar } from '@milkdown/crepe/feature/toolbar';
import { editorViewCtx, remarkStringifyOptionsCtx } from '@milkdown/kit/core';
import { listenerCtx } from '@milkdown/kit/plugin/listener';
import { replaceAll } from '@milkdown/kit/utils';
import { languages } from '@codemirror/language-data';
import type { Handle } from 'mdast-util-to-markdown';
import '@milkdown/crepe/theme/common/prosemirror.css';
import '@milkdown/crepe/theme/common/reset.css';
import '@milkdown/crepe/theme/common/block-edit.css';
import '@milkdown/crepe/theme/common/code-mirror.css';
import '@milkdown/crepe/theme/common/cursor.css';
import '@milkdown/crepe/theme/common/link-tooltip.css';
import '@milkdown/crepe/theme/common/list-item.css';
import '@milkdown/crepe/theme/common/placeholder.css';
import '@milkdown/crepe/theme/common/table.css';
import '@milkdown/crepe/theme/common/toolbar.css';
import '@milkdown/crepe/theme/frame.css';
import './wiki-editor.css';

// remark escapes `[` in text, which would save `[[path]]` wikilinks as `\[\[path]]`.
const wikilinkText: Handle = (node: { value: string }, _parent, state, info) => {
  const parts = node.value.split(/(\[\[[^\]\n]+\]\])/);
  return parts.map((part, i) => i % 2 ? part : state.safe(part, { ...info, before: i ? parts[i - 1].slice(-1) : info.before, after: i < parts.length - 1 ? parts[i + 1][0] : info.after })).join('');
};

class WikiEditor extends HTMLElement {
  private lifetime?: AbortController;
  private crepe?: CrepeBuilder;

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
    const crepe = new CrepeBuilder({ root, defaultValue: source.value })
      .addFeature(cursor).addFeature(listItem).addFeature(linkTooltip).addFeature(toolbar).addFeature(blockEdit).addFeature(table)
      .addFeature(placeholder, { text: 'Type / for blocks, or [[path]] to link another page' })
      .addFeature(codeMirror, { languages });
    crepe.editor.config(ctx => ctx.update(remarkStringifyOptionsCtx, options => ({ ...options, handlers: { ...options.handlers, text: wikilinkText } })));
    const { editor } = crepe;
    try {
      await crepe.create();
      if (lifetime.signal.aborted) { await crepe.destroy(); return; }
      this.crepe = crepe;
      // Adopt typing that happened while the widget was loading.
      editor.action(replaceAll(source.value));
      const document = () => editor.action(ctx => ctx.get(editorViewCtx).state.doc);
      let original = document();
      let originalMarkdown = source.value;
      const sync = () => {
        if (this.dataset.mode === 'rich') source.value = document().eq(original) ? originalMarkdown : crepe.getMarkdown();
      };
      editor.action(ctx => ctx.get(listenerCtx).markdownUpdated(sync));
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
      this.crepe = undefined;
      this.dataset.mode = 'source';
      source.required = true;
      root.hidden = controls.hidden = true;
      await crepe.destroy();
    }
  }

  disconnectedCallback() {
    this.lifetime?.abort();
    void this.crepe?.destroy();
    this.crepe = undefined;
    this.dataset.mode = 'source';
  }
}

customElements.define('wiki-editor', WikiEditor);
