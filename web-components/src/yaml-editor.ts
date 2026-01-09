import { LitElement, html } from 'lit';
import { customElement, query } from 'lit/decorators.js';
import * as monaco from 'monaco-editor/esm/vs/editor/editor.api';

declare global {
  interface Window {
    yamlEditor: YamlEditorComponent | null;
  }
}

@customElement('yaml-editor')
export class YamlEditorComponent extends LitElement {
  // Disable Shadow DOM so Monaco styles work correctly
  protected createRenderRoot = () => this;

  @query('.yaml-editor-container') private container!: HTMLDivElement;
  private editor: monaco.editor.IStandaloneCodeEditor | null = null;
  private themeObserver: MutationObserver | null = null;

  getValue(): string {
    return this.editor?.getValue() ?? '';
  }

  setValue(value: string): void {
    this.editor?.setValue(value);
  }

  protected firstUpdated(): void {
    // Small delay to ensure container is in DOM and has dimensions
    requestAnimationFrame(() => this.initEditor());
  }

  private initEditor(): void {
    if (!this.container || this.editor) return;

    const isDark = document.body.getAttribute('data-theme') === 'dark';
    const initialValue = this.getAttribute('data-initial-value') || '';

    this.editor = monaco.editor.create(this.container, {
      value: initialValue,
      language: 'yaml',
      theme: isDark ? 'vs-dark' : 'vs',
      automaticLayout: true,
      minimap: { enabled: false },
      lineNumbers: 'on',
      wordWrap: 'on',
      scrollBeyondLastLine: false,
      fontSize: 13,
      tabSize: 2,
      renderLineHighlight: 'line',
      folding: true,
      scrollbar: {
        verticalScrollbarSize: 10,
        horizontalScrollbarSize: 10,
      },
      padding: { top: 12, bottom: 12 },
      bracketPairColorization: { enabled: true },
      guides: { indentation: true },
    });

    // Watch for theme changes
    this.themeObserver = new MutationObserver((mutations) => {
      mutations.forEach((m) => {
        if (m.attributeName === 'data-theme') {
          const dark = document.body.getAttribute('data-theme') === 'dark';
          this.editor?.updateOptions({ theme: dark ? 'vs-dark' : 'vs' });
        }
      });
    });
    this.themeObserver.observe(document.body, { attributes: true, attributeFilter: ['data-theme'] });

    // Expose globally for HTMX integration
    window.yamlEditor = this;
  }

  disconnectedCallback(): void {
    super.disconnectedCallback();
    this.themeObserver?.disconnect();
    this.editor?.dispose();
    this.editor = null;
    if (window.yamlEditor === this) window.yamlEditor = null;
  }

  protected render() {
    return html`<div class="yaml-editor-container" style="width: 100%; height: 100%;"></div>`;
  }
}

// Export/Import helpers
export function yamlEditorExport(filename = 'dashboard.yaml'): void {
  const yaml = window.yamlEditor?.getValue();
  if (!yaml) return;
  const blob = new Blob([yaml], { type: 'application/x-yaml' });
  const a = document.createElement('a');
  a.href = URL.createObjectURL(blob);
  a.download = filename;
  a.click();
  URL.revokeObjectURL(a.href);
}

export async function yamlEditorImport(file: File): Promise<void> {
  if (!file) return;
  const text = await file.text();
  window.yamlEditor?.setValue(text);
}

// Expose on window for inline hyperscript usage
(window as any).yamlEditorExport = yamlEditorExport;
(window as any).yamlEditorImport = yamlEditorImport;
