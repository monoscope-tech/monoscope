import { LitElement, html } from 'lit';
import { customElement, query } from 'lit/decorators.js';
import { EditorState, Prec, Transaction, Compartment } from '@codemirror/state';
import { EditorView, keymap, placeholder } from '@codemirror/view';
import { history, historyKeymap, defaultKeymap, insertNewline } from '@codemirror/commands';
import {
  autocompletion,
  closeBrackets,
  closeBracketsKeymap,
  startCompletion,
  closeCompletion,
  acceptCompletion,
  moveCompletionSelection,
  selectedCompletion,
  type CompletionContext,
  type Completion,
} from '@codemirror/autocomplete';
import { setDiagnostics } from '@codemirror/lint';
import { StreamLanguage, syntaxHighlighting, defaultHighlightStyle, HighlightStyle } from '@codemirror/language';
import { tags } from '@lezer/highlight';
import { schemaManager, type SchemaData, type FieldInfo } from './schema-manager';
import { wordAtCursor, DATA_SOURCES, AGGREGATION_COMMANDS, STATS_FUNCTIONS } from './completion';
import { completionChrome, completionTheme, completionIcon, completionSection } from './completion-dropdown';
import { unclosedQuote, verdictToError, type Verdict, type QueryError } from './validation';
export { schemaManager, unclosedQuote, verdictToError, type Verdict, type SchemaData };

const keywords = new Set([...DATA_SOURCES, ...AGGREGATION_COMMANDS, ...STATS_FUNCTIONS, 'and', 'or', 'not', 'by', 'as']);
const language = StreamLanguage.define<{ quote: string; comment: boolean }>({
  startState: () => ({ quote: '', comment: false }),
  token(stream, state) {
    if (!state.quote && (state.comment || stream.match('/_'))) {
      state.comment = true;
      while (!stream.eol()) { if (stream.match('_/')) { state.comment = false; break; } stream.next(); }
      return 'comment';
    }
    if (stream.eatSpace()) return null;
    if (!state.quote && stream.match('//')) {
      stream.skipToEnd();
      return 'comment';
    }
    if (!state.quote && (stream.peek() === '"' || stream.peek() === "'")) state.quote = stream.next()!;
    if (state.quote) {
      let escaped = false,
        c;
      while ((c = stream.next()) != null) {
        if (!escaped && c === state.quote) {
          state.quote = '';
          break;
        }
        if (!escaped && c === '\\') escaped = true;
        else escaped = false;
      }
      return 'string';
    }
    if (stream.match(/^\d+(?:\.\d+)?(?:[eE][+-]?\d+)?(?:ns|µs|us|ms|s|m|h|d|w)?/)) return 'number';
    if (stream.match(/^[a-zA-Z_][\w.]*/)) return keywords.has(stream.current().toLowerCase()) ? 'keyword' : 'variableName';
    if (stream.match(/^[=><!~|]+/)) return 'operator';
    stream.next();
    return null;
  },
});
const REGEX_PATTERNS = {
  hasSummarize: /summarize\s+/i,
  hasBinFunction: /summarize.*by\s+.*bin(_auto)?\s*\(\s*\w+\s*[,)].*$/i,
  summarizeClause: /\|\s*summarize\s+[^|]+/i,
  summarizeByClause: /(\s*summarize\s+[^|]*?by\s+)([^|]*?)(?=\||$)/i,
};
interface LibraryItem {
  query: string;
  label: string;
  section: string;
  search: string;
}
interface QueryLibItem {
  id: string;
  queryType: 'history' | 'saved';
  queryText: string;
  title?: string;
  updatedAt: string;
  byMe: boolean;
}
let nextClient = 0;
const darkHighlightStyle = HighlightStyle.define([
  { tag: tags.keyword, color: '#569cd6' },
  { tag: tags.string, color: '#ce9178' },
  { tag: tags.number, color: '#b5cea8' },
  { tag: tags.comment, color: '#6a9955' },
]);
const editorTheme = EditorView.theme({
  '&': { width: '100%', color: 'inherit', backgroundColor: 'transparent', fontSize: '14px' },
  '&.cm-focused': { outline: 'none' },
  '.cm-scroller': {
    fontFamily: 'var(--font-mono, monospace)',
    lineHeight: '20px',
    maxHeight: '240px',
    overflow: 'auto',
  },
  '.cm-content': { padding: '5px 0', minHeight: '30px', caretColor: 'currentColor' },
  '.cm-line': { padding: '0' },
  '.cm-placeholder': { color: 'inherit', opacity: '0.6' },
  '.cm-tooltip': {
    backgroundColor: 'var(--color-bgRaised, Canvas)',
    color: 'var(--color-textStrong, CanvasText)',
    border: '1px solid var(--color-strokeWeak, GrayText)',
    zIndex: '100',
  },
  '.cm-tooltip-autocomplete': { maxWidth: 'min(600px, 95vw)' },
  '.cm-tooltip-autocomplete ul': { maxHeight: 'min(360px, 60vh)', fontFamily: 'inherit' },
  '.cm-tooltip-autocomplete ul li': { padding: '4px 8px' },
  '.cm-tooltip-autocomplete ul li[aria-selected]': {
    backgroundColor: 'var(--color-fillBrand, Highlight)',
    color: 'var(--color-textStrong, HighlightText)',
  },
  '.cm-completionDetail': { opacity: '0.7' },
  '.cm-cursor': { borderLeftColor: 'currentColor' },
  '@media (forced-colors: active)': { '&.cm-focused': { outline: '2px solid Highlight' } },
});

@customElement('query-editor')
export class QueryEditorComponent extends LitElement {
  @query('.editor-container') private container!: HTMLElement;
  private view: EditorView | null = null;
  private value = '';
  private client = ++nextClient;
  private lifecycle: AbortController | null = null;
  private unsubscribe?: () => void;
  private validationAbort?: AbortController;
  private validationTimer?: ReturnType<typeof setTimeout>;
  private updateTimer?: ReturnType<typeof setTimeout>;
  private validationSeq = 0;
  private validationRequest?: { key: string; promise: Promise<void> };
  private documentVersion = 0;
  private verdicts = new Map<string, Verdict>();
  private lastEmitted = '';
  private library: LibraryItem[] = [];
  private popular: LibraryItem[] = [];
  private programmatic = false;
  private theme = new Compartment();
  private themeObserver?: MutationObserver;
  private themeExtensions() {
    const dark = document.body.getAttribute('data-theme') === 'dark';
    return [EditorView.theme({}, { dark }), syntaxHighlighting(dark ? darkHighlightStyle : defaultHighlightStyle)];
  }
  private ready = false;
  private initialFocus = false;
  private initialSelection: { anchor: number; head: number } | undefined;

  protected createRenderRoot() {
    const input = this.querySelector<HTMLTextAreaElement>('textarea[data-query-input]');
    this.value = input?.value ?? this.getAttribute('default-value') ?? '';
    if (input) this.initialSelection = { anchor: input.selectionStart, head: input.selectionEnd };
    this.initialFocus = !!input && document.activeElement === input;
    this.replaceChildren();
    return this;
  }
  connectedCallback() {
    super.connectedCallback();
    if (this.ready) void this.updateComplete.then(() => this.mountEditor());
  }
  protected firstUpdated() {
    this.ready = true;
    this.mountEditor();
  }
  private project() {
    return this.getAttribute('project-id') || (window as any).PROJECT_ID || '';
  }
  private table() {
    return this.getAttribute('query-source') || schemaManager.getDefaultSchema();
  }
  public getValue(): string {
    return this.view?.state.doc.toString() ?? this.value;
  }
  public setValue(value: string): void {
    this.value = value;
    if (this.view && this.view.state.doc.toString() !== value)
      this.view.dispatch({ changes: { from: 0, to: this.view.state.doc.length, insert: value }, selection: { anchor: value.length } });
  }
  public focusEditor(): void {
    void this.updateComplete.then(() => this.view?.focus());
  }
  public refreshLayout(): void {
    this.view?.requestMeasure();
  }
  public setSchema(schema: Partial<SchemaData>): void {
    if (schema.fields) schemaManager.setSchemaData(this.table(), { fields: schema.fields }, this.project());
  }
  public setDynamicResolver(fn: (path: string[]) => Promise<{ name: string; info: FieldInfo }[]>): void {
    schemaManager.setDynamicResolver(fn);
  }
  public setQueryLibrary(items: QueryLibItem[]): void {
    this.library = items.map((item) => ({
      query: item.queryText,
      label: item.title || item.queryText,
      section: item.queryType === 'saved' ? 'Saved views' : 'Recent searches',
      search: `${item.title || ''} ${item.queryText}`.toLowerCase(),
    }));
    schemaManager.setLibrary(this.client, [...this.library, ...this.popular]);
  }
  public setPopularSearches(items: { query: string; description?: string }[]): void {
    this.popular = items.map((item) => ({
      query: item.query,
      label: item.query,
      section: 'Popular searches',
      search: `${item.description || ''} ${item.query}`.toLowerCase(),
    }));
    schemaManager.setLibrary(this.client, [...this.library, ...this.popular]);
  }
  private complete = async (context: CompletionContext) => {
    const table = this.table(),
      project = this.project();
    const version = this.documentVersion,
      revision = schemaManager.getRevision();
    const text = context.state.sliceDoc(0, context.pos);
    let suggestions;
    try {
      suggestions = await schemaManager.complete(text, table, project, this.client);
    } catch {
      return null;
    } // A failed worker cannot disable the text input.
    if (
      context.aborted ||
      !this.isConnected ||
      version !== this.documentVersion ||
      revision !== schemaManager.getRevision() ||
      table !== this.table() ||
      project !== this.project()
    )
      return null;
    const options: Completion[] = suggestions.map((s) => ({
      label: s.label,
      detail: s.detail,
      type: s.kind === 'field' ? 'property' : s.kind === 'table' ? 'namespace' : s.kind,
      section: completionSection(s, suggestions),
      apply: (view, _completion, from, to) => {
        if (s.fullQuery !== undefined) {
          from = 0;
          to = view.state.doc.length;
        }
        view.dispatch({
          changes: { from, to, insert: s.insertText },
          selection: { anchor: from + s.insertText.length },
          annotations: Transaction.userEvent.of('input.complete'),
        });
        queueMicrotask(() => {
          if (this.isConnected && view.hasFocus) startCompletion(view);
        });
      },
    }));
    return { from: context.pos - wordAtCursor(text).length, options, filter: false };
  };
  private mountEditor() {
    if (!this.isConnected || this.view || !this.container) return;
    this.lifecycle = new AbortController();
    schemaManager.acquire(this.client);
    schemaManager.setLibrary(this.client, [...this.library, ...this.popular]);
    const submit = () => {
      this.emitQuery(true);
      return true;
    };
    this.view = new EditorView({
      parent: this.container,
      state: EditorState.create({
        doc: this.value,
        selection: this.initialSelection,
        extensions: [
          editorTheme,
          completionTheme,
          completionChrome,
          EditorView.lineWrapping,
          history(),
          closeBrackets(),
          language,
          language.data.of({ closeBrackets: { brackets: ['(', '[', '"'] } }),
          this.theme.of(this.themeExtensions()),
          placeholder('level == "ERROR"'),
          EditorView.contentAttributes.of({ 'aria-label': 'Query', 'data-query-input': '', class: 'no-focus-ring' }),
          autocompletion({
            override: [this.complete],
            activateOnTypingDelay: 40,
            selectOnOpen: false,
            interactionDelay: 0,
            defaultKeymap: false,
            maxRenderedOptions: 35,
            icons: false,
            tooltipClass: state => `query-completion-dropdown${state.doc.length ? '' : ' query-completion-empty'}`,
            optionClass: completion => `query-type-${(completion.detail || '').replace(/[^a-z]/g, '')}`,
            addToOptions: [{ render: completionIcon, position: 20 }],
          }),
          Prec.highest(
            keymap.of([
              { key: 'Mod-Enter', run: insertNewline },
              { key: 'Ctrl-Space', run: startCompletion },
              { key: 'ArrowDown', run: moveCompletionSelection(true) },
              { key: 'ArrowUp', run: moveCompletionSelection(false) },
              { key: 'Tab', run: moveCompletionSelection(true) },
              { key: 'Shift-Tab', run: moveCompletionSelection(false) },
              { key: 'Escape', run: closeCompletion },
              {
                key: 'Enter',
                run: (view) => (selectedCompletion(view.state) ? acceptCompletion(view) : (closeCompletion(view), submit())),
              },
            ])
          ),
          keymap.of([...closeBracketsKeymap, ...historyKeymap, ...defaultKeymap]),
          EditorView.domEventHandlers({
            focus: (_event, view) => {
              startCompletion(view);
            },
            click: (_event, view) => {
              // A click must also reopen a popup dismissed while the editor kept focus.
              if (view.state.selection.main.empty) startCompletion(view);
            },
          }),
          EditorView.updateListener.of((update) => {
            if (!update.docChanged) return;
            this.documentVersion++;
            this.invalidateValidation();
            if (this.programmatic || update.view.composing) return;
            clearTimeout(this.validationTimer);
            clearTimeout(this.updateTimer);
            this.validationTimer = setTimeout(() => void this.validateAndMark(this.getValue()), 400);
            this.updateTimer = setTimeout(() => this.emitQuery(), 500);
          }),
          EditorView.domEventHandlers({
            compositionend: () => {
              clearTimeout(this.validationTimer);
              clearTimeout(this.updateTimer);
              this.validationTimer = setTimeout(() => void this.validateAndMark(this.getValue()), 400);
              this.updateTimer = setTimeout(() => this.emitQuery(), 500);
            },
          }),
        ],
      }),
    });
    if (this.project() && typeof Worker !== 'undefined')
      void schemaManager.load('spans', `/p/${this.project()}/log_explorer/schema`, this.project()).catch(() => {});
    this.themeObserver = new MutationObserver(() => this.view?.dispatch({ effects: this.theme.reconfigure(this.themeExtensions()) }));
    this.themeObserver.observe(document.body, { attributes: true, attributeFilter: ['data-theme'] });
    this.initialSelection = undefined;
    this.unsubscribe = schemaManager.subscribe(() => {
      if (this.view?.hasFocus) {
        closeCompletion(this.view);
        startCompletion(this.view);
      }
      this.verdicts.clear();
      this.invalidateValidation();
      clearTimeout(this.validationTimer);
      this.validationTimer = setTimeout(() => void this.validateAndMark(this.getValue()), 400);
    });
    document.addEventListener(
      'keydown',
      (event) => {
        const target = event.target as HTMLElement;
        if (
          event.key !== '/' ||
          event.ctrlKey ||
          event.metaKey ||
          event.altKey ||
          target.closest('input, textarea, [contenteditable="true"]')
        )
          return;
        if (document.querySelector('query-editor') !== this) return;
        event.preventDefault();
        this.view?.focus();
      },
      { signal: this.lifecycle.signal }
    );
    if (this.initialFocus) {
      this.initialFocus = false;
      this.view.focus();
    }
  }
  private invalidateValidation() {
    this.validationRequest = undefined;
    this.validationSeq++;
    this.validationAbort?.abort();
  }
  private showError(error: QueryError | null) {
    if (!this.view) return;
    const doc = this.view.state.doc;
    const line = doc.line(Math.min(error?.line || 1, doc.lines));
    const from = Math.min(line.to, line.from + (error?.startColumn || 1) - 1);
    this.view.dispatch(
      setDiagnostics(
        this.view.state,
        error
          ? [{ from, to: Math.min(doc.length, Math.max(from, line.from + error.endColumn - 1)), severity: 'error', message: error.message }]
          : []
      )
    );
    this.view.contentDOM.setAttribute('aria-invalid', String(!!error));
    if (error) document.body.dispatchEvent(new CustomEvent('showParseError', { detail: error.message, bubbles: true, composed: true }));
    else (window as any).clearQueryParseError?.();
  }
  private validateAndMark(query: string): Promise<void> {
    const key = JSON.stringify([this.project(), this.table(), query]);
    if (this.validationRequest?.key === key) return this.validationRequest.promise;
    this.invalidateValidation();
    const promise = this.requestValidation(query);
    this.validationRequest = { key, promise };
    void promise.finally(() => {
      if (this.validationRequest?.promise === promise) this.validationRequest = undefined;
    });
    return promise;
  }
  private async requestValidation(query: string): Promise<void> {
    const seq = this.validationSeq;
    const local = unclosedQuote(query);
    if (local) {
      this.showError(local);
      return;
    }
    const pid = this.project(),
      source = this.table();
    if (!pid) return;
    const key = JSON.stringify([pid, source, query]);
    let verdict = this.verdicts.get(key);
    if (!verdict) {
      this.validationAbort = new AbortController();
      try {
        const params = new URLSearchParams({ query, source });
        const response = await fetch(`/p/${pid}/log_explorer/validate?${params}`, {
          credentials: 'include',
          headers: { Accept: 'application/json' },
          signal: this.validationAbort.signal,
        });
        if (!response.ok) return;
        verdict = (await response.json()) as Verdict;
      } catch {
        return;
      }
      if (seq !== this.validationSeq || !this.isConnected || pid !== this.project() || source !== this.table()) return;
      if (this.verdicts.size >= 50) this.verdicts.delete(this.verdicts.keys().next().value!);
      this.verdicts.set(key, verdict);
    }
    if (seq === this.validationSeq && this.isConnected) this.showError(verdictToError(verdict));
  }
  private emitQuery(force = false) {
    clearTimeout(this.updateTimer);
    const value = this.getValue();
    if (!force && value === this.lastEmitted) return;
    this.lastEmitted = value;
    if (this.getAttribute('widget-editor') !== 'true') {
      const url = new URL(location.href);
      if (value.trim()) url.searchParams.set('query', value);
      else url.searchParams.delete('query');
      historyReplace(url);
    }
    const target = this.getAttribute('target-widget-preview');
    if (target) document.getElementById(target)?.dispatchEvent(new CustomEvent('update-widget-query', { detail: { value } }));
    else {
      this.dispatchEvent(new CustomEvent('update-query', { detail: { value }, bubbles: true }));
      // The bubbling event already reaches window; emit once to avoid duplicate refetches.
    }
    void this.validateAndMark(value);
  }
  public handleAddQuery(fragment: string, replace = false): void {
    const current = this.getValue().trim();
    let value = fragment;
    if (!replace && current) {
      const where = current.toLowerCase().indexOf('| where '),
        pipe = current.indexOf('|');
      value =
        where >= 0
          ? `${current.slice(0, where + 8)}(${fragment}) and ${current.slice(where + 8)}`
          : pipe > 0
            ? `${current.slice(0, pipe)} and ${fragment} ${current.slice(pipe)}`
            : `${current} and ${fragment}`;
    }
    this.programmatic = true;
    try {
      this.setValue(value);
      if (this.view) closeCompletion(this.view);
    } finally {
      this.programmatic = false;
    }
    this.emitQuery();
  }
  // Toggle a subquery - add if not present, remove if present
  public toggleSubQuery(queryFragment: string): void {
    const currentValue = this.getValue().trim();

    if (currentValue.includes(queryFragment)) {
      // Remove the fragment if it exists
      const escFragment = queryFragment.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
      let newQuery = currentValue;

      // Handle different position cases
      if (new RegExp(`^${escFragment}$`).test(currentValue)) {
        newQuery = ''; // It's the only query
      } else if (new RegExp(`^${escFragment} and `, 'i').test(currentValue)) {
        newQuery = currentValue.replace(new RegExp(`^${escFragment} and `, 'i'), ''); // At start
      } else if (new RegExp(` and ${escFragment}$`, 'i').test(currentValue)) {
        newQuery = currentValue.replace(new RegExp(` and ${escFragment}$`, 'i'), ''); // At end
      } else {
        newQuery = currentValue.replace(new RegExp(` and ${escFragment}`, 'i'), ''); // In middle
      }

      // Clean up
      newQuery = newQuery.replace(/^and /i, '').replace(/ and$/i, '').trim();
      this.handleAddQuery(newQuery, true);
    } else {
      // Add the fragment if it doesn't exist
      this.handleAddQuery(queryFragment, currentValue ? false : true);
    }
  }

  public handleVisualizationChange(visualizationType: string): void {
    const currentQuery = this.getValue().trim();

    // Check if the query contains a summarize clause
    const hasSummarize = REGEX_PATTERNS.hasSummarize.test(currentQuery);

    // Check if summarize includes bin_auto or bin with any field
    const hasBinFunction = REGEX_PATTERNS.hasBinFunction.test(currentQuery);

    let newQuery = '';
    switch (visualizationType) {
      case 'timeseries': // Bar chart
      case 'timeseries_line': // Line chart
        // If query already has the correct format for timeseries, don't change it
        if (hasSummarize && hasBinFunction) {
          return;
        }

        if (hasSummarize && !hasBinFunction) {
          // Query has summarize but no bin_auto for timestamp, add bin_auto(timestamp) to the by clause
          newQuery = currentQuery.replace(REGEX_PATTERNS.summarizeByClause, (_match: string, summarizePrefix: string, byClause: string) => {
            // Add bin_auto(timestamp) to the beginning of the by clause
            const updatedBy = byClause.trim()
              ? `${summarizePrefix}bin_auto(timestamp), ${byClause.trim()}`
              : `${summarizePrefix}bin_auto(timestamp)`;
            return updatedBy;
          });
        } else if (!hasSummarize) {
          // No summarize clause, add one with bin_auto(timestamp)
          newQuery = `${currentQuery ? currentQuery + ' ' : ''}| summarize count(*) by bin_auto(timestamp), status_code`;
        }
        break;
      case 'table':
      case 'top-list':
      case 'distribution':
      case 'query-value':
        // We don't modify queries for these visualization types
        return;
      case 'logs':
      default:
        // For logs or default case (which is interpreted as logs), remove any summarize part
        if (hasSummarize) {
          newQuery = currentQuery.replace(REGEX_PATTERNS.summarizeClause, '');
        } else {
          return; // No summarize to remove
        }
        break;
    }

    this.handleAddQuery(newQuery, true);
  }

  disconnectedCallback() {
    this.value = this.getValue();
    this.themeObserver?.disconnect();
    this.lifecycle?.abort();
    this.unsubscribe?.();
    this.unsubscribe = undefined;
    clearTimeout(this.validationTimer);
    clearTimeout(this.updateTimer);
    this.invalidateValidation();
    schemaManager.release(this.client);
    this.view?.destroy();
    this.view = null;
    super.disconnectedCallback();
  }
  protected render() {
    return html`<div
      class="relative w-full flex ${this.hasAttribute('standalone-ai-search') ? 'min-h-8 pl-2 border rounded-md border-strokeStrong focus-within:border-strokeBrand-strong' : ''}"
    >
      <div class="editor-container w-full min-w-0 flex-1"></div>
      ${this.hasAttribute('standalone-ai-search') ? html`<div class="p-1"><button type="button" class="px-2 py-0.5 inline-flex gap-1.5 items-center rounded-sm text-textWeak hover:bg-fillWeak focus-visible:outline-2" aria-label="Ask AI" @click=${() => this.dispatchEvent(new CustomEvent('open-ai-search', { bubbles: true }))}>Ask AI</button></div>` : null}
    </div>`;
  }
}
const historyReplace = (url: URL) => window.history.replaceState({}, '', url.toString());
