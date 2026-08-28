import { LitElement, html, css, TemplateResult } from 'lit';
import { customElement, state, query } from 'lit/decorators.js';
import { repeat } from 'lit/directives/repeat.js';
import 'monaco-editor/esm/vs/editor/contrib/suggest/browser/suggestController.js';
import * as monaco from 'monaco-editor/esm/vs/editor/editor.api.js';
import {
  AGGREGATION_COMMANDS,
  DATA_SOURCES,
  LOGICAL_OPERATORS,
  OPERATOR_DETAILS,
  STATS_FUNCTIONS,
  SUGGESTION_OPERATORS,
  computeSuggestions,
  filterSuggestions,
  wordAtCursor,
  type SchemaAccess,
  type Suggestion,
  type SuggestionKind as CompletionKind,
} from './completion';
import EditorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker';
import { conf as yamlConf, language as yamlLanguage } from 'monaco-editor/esm/vs/basic-languages/yaml/yaml.js';
import { groupBy, pick } from 'lodash';

// Configure Monaco workers (we only use the base editor, no language services).
// Without this, Monaco logs a warning and falls back to running worker code on the main thread.
(self as any).MonacoEnvironment = (self as any).MonacoEnvironment || {
  getWorker: () => new EditorWorker(),
};

// Make monaco available globally for tests
globalThis.monaco = monaco;

// Types
type SuggestionKind = 'completion' | 'recentSearch' | 'savedView' | 'popularSearch';
type QueryLibType = 'history' | 'saved';
type FieldType = 'string' | 'number' | 'boolean' | 'duration' | 'array' | 'object';

interface BaseSuggestion {
  readonly kind: SuggestionKind;
}

interface CompletionItem extends BaseSuggestion {
  readonly kind: 'completion';
  label: string;
  insertText: string;
  kindCategory: CompletionKind;
  detail?: string;
  score?: number;
  originalItem?: any;
  isContextSpecific?: boolean;
  parentPath?: string;
  partialField?: string;
}

interface RecentSearch extends BaseSuggestion {
  readonly kind: 'recentSearch';
  query: string;
  timestamp: string;
}

interface SavedView extends BaseSuggestion {
  readonly kind: 'savedView';
  name: string;
  query: string;
  owner?: {
    name: string;
    icon?: string;
  };
}

interface PopularSearch extends BaseSuggestion {
  readonly kind: 'popularSearch';
  query: string;
  description?: string;
}

interface QueryLibItem {
  id: string;
  projectId: string;
  createdAt: string;
  updatedAt: string;
  userId: string;
  queryType: QueryLibType;
  queryText: string;
  queryAst: any;
  title?: string;
  byMe: boolean;
}

// Schema types
type FieldValue = string | number | boolean;

type FieldInfo = {
  type?: FieldType;
  field_type?: FieldType;
  description?: string;
  enum?: FieldValue[];
  fields?: Record<string, FieldInfo>;
  properties?: Record<string, FieldInfo>;
  items?: FieldInfo;
  examples?: FieldValue[];
};

type Schema = {
  fields: Record<string, FieldInfo>;
  operators?: Record<string, string[]>;
  dynamicResolver?: (path: string[]) => Promise<FieldInfo[]>;
};

type SchemaData = {
  fields: Record<string, FieldInfo>;
  properties?: Record<string, SchemaData>;
  operators?: Record<string, string[]>;
};

type SuggestionItem = CompletionItem | RecentSearch | SavedView | PopularSearch;

interface SchemaField {
  name: string;
  type: string;
  examples?: FieldValue[];
  fields?: Record<string, any>;
}

// Define constants to avoid duplication
// Operators categorized by type
const COMPARISON_OPERATORS = ['==', '!=', '>', '<', '>=', '<=', '=~'];
const SET_OPERATORS = ['in', '!in', 'has', '!has', 'has_any', 'has_all'];
const STRING_OPERATORS = ['contains', '!contains', 'startswith', '!startswith', 'endswith', '!endswith', 'matches'];
const PIPE_OPERATOR = ['|'];

// Combine all operators for easy access
const ALL_OPERATORS = [...COMPARISON_OPERATORS, ...SET_OPERATORS, ...STRING_OPERATORS, ...PIPE_OPERATOR];

// Operator descriptions for suggestion dropdown

// Common operators shown first, then advanced ones
const COMMON_OPERATORS = ['==', '!=', '>', '<', 'contains', 'in'];
const ADVANCED_OPERATORS = ['>=', '<=', '=~', '!in', 'has', '!has', 'has_any', 'has_all', '!contains', 'startswith', '!startswith', 'endswith', '!endswith', 'matches', ...LOGICAL_OPERATORS.filter((op) => op !== '!exists')];
const operatorSortText = (op: string, i: number) => {
  const group = COMMON_OPERATORS.includes(op) ? '0' : '1';
  return `${group}_${String(i).padStart(3, '0')}_${op}`;
};

// Performance constants
const IDLE_CALLBACK_TIMEOUT = 50;
const MAX_CACHE_SIZE = 100;

// Common filter fields get higher priority (lower sortText = shown first)
const PRIORITY_FIELDS = new Set(['status_code', 'level', 'method', 'name', 'duration', 'service', 'path', 'http_status', 'resource', 'attributes']);
const fieldSortText = (name: string) => PRIORITY_FIELDS.has(name) ? `0_${name}` : `1_${name}`;

// Sources and keywords
const AGGREGATION_MODIFIERS = ['by', 'as', 'limit'];

// Combine all keywords
const KEYWORDS = [...DATA_SOURCES, ...AGGREGATION_COMMANDS, ...AGGREGATION_MODIFIERS, ...LOGICAL_OPERATORS, ...STATS_FUNCTIONS];

// Precompiled regexes for performance - created once, reused many times
const REGEX_PATTERNS = {
  // Field dot notation patterns
  dotMatchEnd: /([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\.$/,
  dotMatchPartial: /([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\.([a-zA-Z0-9_]*)$/,

  // Operator patterns
  operatorMatch:
    /([\w\.]+)\s*(==|!=|>=|<=|>|<|=~|!in|in|has_any|has_all|!has|has|!contains|contains|!startswith|startswith|!endswith|endswith|matches)\s*$/,

  // Value patterns
  afterQuotedValue: /".*"\s*$/,
  afterNumericValue: /\d+\s*$/,

  // Field space patterns
  fieldSpace: /([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\s+$/,

  // Logical operators (precompiled from filtered list)
  logicalOperator: new RegExp(`\\b(and|or|not)\\s+$`, 'i'),

  // Stats/aggregation patterns
  statsOrTimechart: /stats\s|timechart\s/i,
  byKeyword: /\bby\s*$/i,
  timechartKeyword: /timechart/i,

  // Visualization patterns
  hasSummarize: /summarize\s+/i,
  hasBinFunction: /summarize.*by\s+.*bin(_auto)?\s*\(\s*\w+\s*[,)].*$/i,
  summarizeClause: /\|\s*summarize\s+[^|]+/i,
  summarizeByClause: /(\s*summarize\s+[^|]*?by\s+)([^|]*?)(?=\||$)/i,

  // Character tests (precompiled for hot path)
  digitTest: /\d/,
  whitespaceTest: /\s/,
  wordBoundaryTest: /[^\w\d_=<>!&|+\-*/%^.:]/,
};

// Schema Manager class for better encapsulation
class SchemaManager {
  private schemas: string[] = DATA_SOURCES;
  private defaultSchema = DATA_SOURCES[0];
  private schemaData: Record<string, SchemaData> = {};

  // Caches for schema resolution to avoid redundant computation
  private nestedCache = new Map<string, SchemaField[]>();
  private valueCache = new Map<string, string[]>();

  private nestedResolver: (schema: string, prefix: string) => Promise<SchemaField[]> = async (schema, prefix) => {
    const currentSchema = this.schemaData[schema] || this.schemaData[this.defaultSchema];
    if (!currentSchema?.fields) return [];

    const fields = Object.entries(currentSchema.fields);

    if (!prefix) {
      // Top-level fields
      return fields
        .filter(([name]) => !name.includes('.'))
        .map(([name, info]) => ({
          name,
          type: info.type || info.field_type || 'string',
          examples: info.examples || info.enum || [],
          fields: fields.some(([k]) => k.startsWith(`${name}.`)) ? {} : undefined,
        }));
    }

    // Nested fields
    const prefixWithDot = `${prefix}.`;
    const nestedFields = fields
      .filter(([name]) => name.startsWith(prefixWithDot))
      .reduce((acc, [name, info]) => {
        const childName = name.substring(prefixWithDot.length).split('.')[0];
        if (!acc.has(childName)) {
          acc.set(childName, {
            name: childName,
            type: info.type || info.field_type || 'string',
            examples: info.examples || info.enum || [],
            fields: fields.some(([k]) => k.startsWith(`${prefixWithDot}${childName}.`)) ? {} : undefined,
          });
        }
        return acc;
      }, new Map<string, SchemaField>());

    return Array.from(nestedFields.values());
  };

  private valueResolver: (schema: string, field: string) => Promise<string[]> = async (schema, field) => {
    const fieldInfo = this.schemaData[schema]?.fields?.[field] || this.schemaData[this.defaultSchema]?.fields?.[field];
    return (fieldInfo?.enum || fieldInfo?.examples || []).map(String);
  };

  setSchemas = (list: string[]) => {
    this.schemas = list;
  };
  setDefaultSchema = (schema: string) => {
    if (this.schemas.includes(schema)) this.defaultSchema = schema;
  };
  setSchemaData = (schema: string, data: SchemaData) => {
    this.schemaData[schema] = data;
    // Clear caches when schema changes
    this.nestedCache.clear();
    this.valueCache.clear();
    // The schema arrives lazily (fetched on first focus), so a query typed
    // before it lands validated against an empty field set. Tell mounted
    // editors to re-check what's already in the box.
    document.body.dispatchEvent(new CustomEvent('schema-loaded', { detail: schema }));
  };
  getSchemaData = (schema: string) => this.schemaData[schema];
  setNestedResolver = (fn: typeof this.nestedResolver) => {
    this.nestedResolver = fn;
    this.nestedCache.clear();
  };
  setValueResolver = (fn: typeof this.valueResolver) => {
    this.valueResolver = fn;
    this.valueCache.clear();
  };
  getSchemas = () => this.schemas;
  getDefaultSchema = () => this.defaultSchema;
  // Synchronous root-field names of the active schema — the field validator runs
  // on every keystroke and can't await the (async, nested) resolver.
  getFieldRoots = (): Set<string> => {
    const fields = this.schemaData[this.defaultSchema]?.fields;
    return new Set(Object.keys(fields || {}).map((f) => f.split('.')[0]));
  };
  private setCacheWithLimit<K, V>(cache: Map<K, V>, key: K, value: V): void {
    if (cache.size >= MAX_CACHE_SIZE) {
      const firstKey = cache.keys().next().value;
      if (firstKey !== undefined) cache.delete(firstKey);
    }
    cache.set(key, value);
  }

  resolveNested = async (schema: string, prefix: string): Promise<SchemaField[]> => {
    const cacheKey = `${schema}:${prefix}`;
    const cached = this.nestedCache.get(cacheKey);
    if (cached) return cached;
    const result = await this.nestedResolver(schema, prefix);
    // An empty result means the schema hasn't arrived yet — caching it would
    // pin "no fields" for this prefix until something else clears the cache.
    if (result.length) this.setCacheWithLimit(this.nestedCache, cacheKey, result);
    return result;
  };
  resolveValues = async (schema: string, field: string): Promise<string[]> => {
    const cacheKey = `${schema}:${field}`;
    const cached = this.valueCache.get(cacheKey);
    if (cached) return cached;
    const result = await this.valueResolver(schema, field);
    this.setCacheWithLimit(this.valueCache, cacheKey, result);
    return result;
  };
  getFieldSuggestions = async (schema = this.getDefaultSchema()): Promise<{ name: string; type: string; description?: string }[]> =>
    (await this.resolveNested(schema, '')).map((field) => ({
      name: field.name,
      type: field.type,
      description: field.examples?.join(', '),
    }));

  // Legacy compatibility methods
  getRootFields = async (): Promise<{ name: string; info: FieldInfo }[]> => {
    const fields = await this.nestedResolver(this.defaultSchema, '');
    return fields.map((f) => ({
      name: f.name,
      info: {
        type: f.type as FieldType,
        examples: f.examples,
        enum: f.examples,
        fields: f.fields,
      },
    }));
  };

  resolveNestedFields = async (path: string[]): Promise<{ name: string; info: FieldInfo }[]> => {
    const fields = await this.nestedResolver(this.defaultSchema, path.join('.'));
    return fields.map((f) => ({
      name: f.name,
      info: {
        type: f.type as FieldType,
        examples: f.examples,
        enum: f.examples,
        fields: f.fields,
      },
    }));
  };

  setSchema = (cfg: Partial<Schema>) => {};

  setDynamicResolver = (fn: (path: string[]) => Promise<any[]>) => {
    this.nestedResolver = async (schema: string, prefix: string) => {
      const path = prefix ? prefix.split('.') : [];
      const fields = await fn(path);
      return fields.map((f) => ({
        name: f.name,
        type: f.info.type,
        examples: f.info.examples,
        fields: f.info.fields,
      }));
    };
    this.nestedCache.clear();
  };
}

// Create schema manager instance
const schemaManager = new SchemaManager();

// Monarch configuration for AQL
export const conf = {
  comments: { lineComment: '//', blockComment: ['/_', '_/'] },
  brackets: [
    ['(', ')'],
    ['[', ']'],
  ],
  autoClosingPairs: [
    { open: '"', close: '"' },
    { open: '[', close: ']' },
    { open: '(', close: ')' },
  ],
  surroundingPairs: [
    { open: '"', close: '"' },
    { open: '[', close: ']' },
    { open: '(', close: ')' },
  ],
  ignoreCase: true,
};

export const language = {
  defaultToken: '',
  tokenPostfix: '.aql',
  ignoreCase: true,
  keywords: KEYWORDS,
  operators: ALL_OPERATORS,
  tokenizer: {
    root: [
      [/\[[0-9]+(?:\.[0-9]+)?(?:s|m|h|d|w)\]/, 'number.timespan'],
      [/[0-9]+(?:\.[0-9]+)?(?:ns|µs|us|ms|s|m|h|d|w)/, 'number.duration'],
      [/[0-9]+(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?/, 'number'],
      [/"([^"\\]|\\.)*"/, 'string'],
      [/\/(\\.|[^\\/])+\/[iIsS]*/, 'regexp'],
      [/[a-zA-Z_][\w]*/, { cases: { '@keywords': 'keyword', '@default': 'identifier' } }],
      [/[=><!~|]+/, { cases: { '@operators': 'operator', '@default': '' } }],
      [/[[\]()\.,|]/, 'delimiter'],
      { include: '@whitespace' },
    ],
    whitespace: [
      [/[ \t\r\n]+/, 'white'],
      [/\/\/.*$/, 'comment'],
      [/\/_/, 'comment', '@comment'],
    ],
    comment: [
      [/\_\//, 'comment', '@pop'],
      [/./, 'comment'],
    ],
  },
};

// Define transparent themes for light and dark modes
monaco.editor.defineTheme('transparent-theme-light', {
  base: 'vs',
  inherit: true,
  rules: [],
  colors: {
    'editor.background': '#00000000',
    'editor.lineHighlightBackground': '#00000000',
    'editorGutter.background': '#00000000',
  },
});

monaco.editor.defineTheme('transparent-theme-dark', {
  base: 'vs-dark',
  inherit: true,
  rules: [],
  colors: {
    'editor.background': '#00000000',
    'editor.lineHighlightBackground': '#00000000',
    'editorGutter.background': '#00000000',
  },
});

// Register AQL language
monaco.languages.register({ id: 'aql' });
monaco.languages.setMonarchTokensProvider('aql', language as monaco.languages.IMonarchLanguage);
monaco.languages.setLanguageConfiguration('aql', conf as unknown as monaco.languages.LanguageConfiguration);

// Register YAML language for yaml-editor component
monaco.languages.register({ id: 'yaml', extensions: ['.yaml', '.yml'], aliases: ['YAML', 'yaml'] });
monaco.languages.setMonarchTokensProvider('yaml', yamlLanguage as monaco.languages.IMonarchLanguage);
monaco.languages.setLanguageConfiguration('yaml', yamlConf as monaco.languages.LanguageConfiguration);

// Suggestions come from ./completion, called directly by the component. Monaco
// keeps syntax highlighting only: its own completion provider and suggest widget
// are unused, so there is exactly one list and one code path behind it.

// Query validation.
//
// The grammar lives in one place — Pkg.Parser.Stats, behind
// /log_explorer/validate — and its verdict carries the position to underline.
// The regex approximation that used to live here could not know what the parser
// knows (aliases introduced by `summarize`, which roots the server accepts) and
// produced squiggles on valid queries. What stays local is only what must be
// instant and cannot be wrong: an unterminated quote makes every later token
// ambiguous, so it is reported before a round trip is worth making.

interface QueryError {
  message: string;
  startColumn: number;
  endColumn: number;
  line: number;
}

export function unclosedQuote(query: string): QueryError | null {
  let inQuote = false;
  let quoteChar = '';
  let quoteStart = 0;
  for (let i = 0; i < query.length; i++) {
    const c = query[i];
    if (!inQuote && (c === '"' || c === "'")) {
      inQuote = true;
      quoteChar = c;
      quoteStart = i;
    } else if (inQuote && c === quoteChar && query[i - 1] !== '\\') {
      inQuote = false;
    }
  }
  return inQuote
    ? { message: `Unclosed ${quoteChar === '"' ? 'double' : 'single'} quote`, startColumn: quoteStart + 1, endColumn: query.length + 1, line: 1 }
    : null;
}

/** Shape of the /log_explorer/validate response. */
export interface Verdict {
  valid: boolean;
  message?: string;
  column?: number;
  width?: number;
}

export const verdictToError = (v: Verdict): QueryError | null =>
  v.valid || !v.message
    ? null
    : { message: v.message, startColumn: v.column ?? 1, endColumn: (v.column ?? 1) + (v.width ?? 1), line: 1 };

@customElement('query-editor')
export class QueryEditorComponent extends LitElement {
  // Light DOM, so the markup picks up the page's Tailwind classes. Lit *appends* its
  // parts to the render root instead of clearing it, so the server-rendered skeleton
  // (queryEditorSkeleton_ in LogQueryBox.hs, shown while Monaco loads on idle) has to be
  // dropped here — otherwise it stays on screen beside the real editor.
  protected createRenderRoot = () => {
    this.replaceChildren();
    return this;
  };

  @query('#editor-container') private _editorContainer!: HTMLElement;
  @query('.placeholder-overlay') private _placeholderElement!: HTMLElement;

  @state() private completionItems: CompletionItem[] = [];
  @state() public recentSearches: RecentSearch[] = [];
  @state() public savedViews: SavedView[] = [];
  @state() public popularSearches: PopularSearch[] = [];
  @state() private showSuggestions = false;
  @state() private currentQuery = '';
  @state() private selectedIndex = -1;
  @state() private defaultValue = '';
  @state() private updateURLParams = true;

  private editor: monaco.editor.IStandaloneCodeEditor | null = null;
  private suggestionListeners: (() => void)[] = [];
  private isProgrammaticUpdate = false;
  private updateHandlers: Array<monaco.IDisposable> = [];
  private resizeObserver: ResizeObserver | null = null;
  private themeObserver: MutationObserver | null = null;
  // Single abort signal scoping every global listener to this connection lifecycle.
  // disconnectedCallback aborts it, so any in-flight callback that fires after
  // teardown is dropped by the platform — no manual remove needed per listener.
  private lifecycleAbort: AbortController | null = null;

  // Bound handlers for cleanup
  private resizeHandler = () => {
    this.adjustEditorHeight();
    this.refreshLayoutThrottled();
  };
  // Called by the deferred loader in index.ts once Monaco lands, so a click that
  // arrived while the module was still in flight still ends with a focused editor.
  focusEditor(): void {
    void this.updateComplete.then(() => this.editor?.focus());
  }

  private keydownHandler = (e: KeyboardEvent) => {
    if (e.key === '/' && !e.ctrlKey && !e.metaKey && !e.altKey) {
      const target = e.target as HTMLElement;
      if (target.tagName !== 'INPUT' && target.tagName !== 'TEXTAREA' && target.contentEditable !== 'true') {
        e.preventDefault();
        this.editor?.focus();
      }
    }
  };

  // Memoization cache for getMatches
  private _matchesCache: { query: string; result: any } | null = null;

  // Track last render state to prevent unnecessary re-renders
  private _lastRenderState: {
    showSuggestions: boolean;
    currentQuery: string;
    selectedIndex: number;
  } | null = null;

  // Prevent unnecessary re-renders by checking if suggestion-related state actually changed
  // Screen readers need the input to announce itself as a combobox and to say
  // which option is active; Monaco owns the inner textarea, so the attributes are
  // applied to it after each render rather than in a template.
  private ariaState = '';
  protected updated(): void {
    // Monaco owns this textarea; write to it only when the announced state
    // actually changed rather than on every render.
    const next = `${this.showSuggestions}:${this.selectedIndex}`;
    if (next === this.ariaState) return;
    const input = this.editor?.getDomNode()?.querySelector('textarea');
    if (!input) return;
    this.ariaState = next;
    input.setAttribute('role', 'combobox');
    input.setAttribute('aria-expanded', String(this.showSuggestions));
    input.setAttribute('aria-controls', 'query-suggestions');
    input.setAttribute('aria-autocomplete', 'list');
    if (this.showSuggestions && this.selectedIndex >= 0) input.setAttribute('aria-activedescendant', `query-suggestion-${this.selectedIndex}`);
    else input.removeAttribute('aria-activedescendant');
  }

  shouldUpdate(changedProperties: Map<string, any>): boolean {
    // Check if any of the properties that affect the suggestions dropdown changed
    const suggestionStateChanged =
      changedProperties.has('showSuggestions') ||
      changedProperties.has('currentQuery') ||
      changedProperties.has('selectedIndex') ||
      changedProperties.has('completionItems') ||
      changedProperties.has('recentSearches') ||
      changedProperties.has('savedViews') ||
      changedProperties.has('popularSearches');

    // If suggestion state didn't change, skip render
    if (!suggestionStateChanged) {
      return false;
    }

    return true;
  }

  // Color-coded type badges for dropdown items, keyed by Monaco CompletionItemKind
  private readonly KIND_BADGES: Record<CompletionKind, { label: string; cls: string }> = {
    field: { label: 'F', cls: 'text-sky-400 bg-sky-400/15 border-sky-400/30' },
    operator: { label: 'Op', cls: 'text-amber-400 bg-amber-400/15 border-amber-400/30' },
    value: { label: 'V', cls: 'text-emerald-400 bg-emerald-400/15 border-emerald-400/30' },
    table: { label: 'M', cls: 'text-violet-400 bg-violet-400/15 border-violet-400/30' },
    keyword: { label: 'K', cls: 'text-rose-400 bg-rose-400/15 border-rose-400/30' },
    function: { label: 'fn', cls: 'text-orange-400 bg-orange-400/15 border-orange-400/30' },
    snippet: { label: '{}', cls: 'text-teal-400 bg-teal-400/15 border-teal-400/30' },
  };



  public setPopularSearches(items: { query: string; description?: string }[]): void {
    if (!items?.length) return;
    this.popularSearches = items.map((item) => ({
      kind: 'popularSearch' as const,
      query: item.query,
      description: item.description || '',
    }));
    this._matchesCache = null; // Invalidate cache
  }

  // Public method to refresh editor layout
  public refreshLayout(): void {
    if (this.editor) {
      this.editor.layout();
    }
  }

  private layoutRefreshPending = false;
  private refreshLayoutThrottled = () => {
    if (!this.layoutRefreshPending) {
      this.layoutRefreshPending = true;
      requestAnimationFrame(() => {
        this.layoutRefreshPending = false;
        if (!this.isConnected) return;
        this.refreshLayout();
      });
    }
  };

  private suggestionSeq = 0;
  private triggerSuggestions = () => {
    this.showSuggestions = true;
    void this.refreshSuggestions();
  };

  /** Cache of verdicts by query text — retyping a query costs no round trip. */
  private verdicts = new Map<string, Verdict>();
  private validateSeq = 0;

  /** Ask the server whether this query is valid. Errors are treated as "no verdict". */
  private async fetchVerdict(query: string): Promise<Verdict | null> {
    const cached = this.verdicts.get(query);
    if (cached) return cached;
    const pid = this.getAttribute('project-id') || (window as any).PROJECT_ID;
    if (!pid) return null;
    try {
      // The source decides which table's columns exist, so the verdict has to be asked for
      // under the same one the query runs against — otherwise the Metrics page underlines
      // `metric_name`, a real column there, on a query the server accepts.
      const source = this.getAttribute('query-source');
      const url = `/p/${pid}/log_explorer/validate?query=${encodeURIComponent(query)}${source ? `&source=${encodeURIComponent(source)}` : ''}`;
      const res = await fetch(url, {
        headers: { Accept: 'application/json' },
        credentials: 'include',
      });
      if (!res.ok) return null;
      const verdict: Verdict = await res.json();
      if (this.verdicts.size > 50) this.verdicts.clear();
      this.verdicts.set(query, verdict);
      return verdict;
    } catch {
      // Offline or a failed round trip must not paint the query as invalid.
      return null;
    }
  }

  private showError(model: monaco.editor.ITextModel, error: QueryError | null): void {
    monaco.editor.setModelMarkers(
      model,
      'query-validator',
      error
        ? [
            {
              severity: monaco.MarkerSeverity.Error,
              message: error.message,
              startLineNumber: error.line,
              startColumn: error.startColumn,
              endLineNumber: error.line,
              endColumn: error.endColumn,
            },
          ]
        : []
    );
    // Through the `showParseError` event — the same front door the server's
    // HX-Trigger and log-list use — rather than the window global it forwards to.
    if (error) {
      document.body.dispatchEvent(new CustomEvent('showParseError', { detail: error.message, bubbles: true, composed: true }));
    } else {
      (window as any).clearQueryParseError?.();
    }
  }

  private async validateAndMark(query: string, model: monaco.editor.ITextModel): Promise<void> {
    // Instant and never wrong: report it without waiting for a round trip.
    const local = unclosedQuote(query);
    if (local) return this.showError(model, local);

    const seq = ++this.validateSeq;
    const verdict = await this.fetchVerdict(query.trim());
    // A newer keystroke already asked; its answer wins.
    if (seq !== this.validateSeq || !this.isConnected) return;
    if (verdict) this.showError(model, verdictToError(verdict));
  }

  // Fires a little ahead of the refetch debounce below, so the message lands
  // before the data it explains.
  private validateTimeout: number | null = null;
  private validateDebounced = (queryValue: string) => {
    if (this.validateTimeout !== null) clearTimeout(this.validateTimeout);
    this.validateTimeout = window.setTimeout(() => {
      this.validateTimeout = null;
      if (!this.isConnected) return;
      const model = this.editor?.getModel();
      if (model) void this.validateAndMark(queryValue, model);
    }, 400);
  };

  // Debounced version - waits 300ms after user stops typing before firing
  private updateQueryTimeout: number | null = null;
  private lastQueryValue = '';
  private lastEmittedQueryValue = ''; // Track the last value we actually emitted
  private updateQueryDebounced = (queryValue: string) => {
    this.lastQueryValue = queryValue;

    // Clear existing timeout
    if (this.updateQueryTimeout !== null) clearTimeout(this.updateQueryTimeout);

    // Set new timeout - only fires after user stops typing for 300ms
    this.updateQueryTimeout = window.setTimeout(() => {
      this.updateQueryTimeout = null;
      if (!this.isConnected) return;
      // Only call updateQuery if the value actually changed
      if (this.lastQueryValue !== this.lastEmittedQueryValue) {
        this.updateQuery(this.lastQueryValue);
        this.lastEmittedQueryValue = this.lastQueryValue;
      }
    }, 500); // wait 300ms after last keypress
  };


  private updateQuery = (queryValue: string) => {
    if (this.updateURLParams) {
      const url = new URL(window.location.href);
      if (queryValue.trim()) {
        url.searchParams.set('query', queryValue);
      } else {
        url.searchParams.delete('query');
      }
      window.history.replaceState({}, '', url.toString());
    }

    const widgetPreviewId = this.getAttribute('target-widget-preview');
    if (widgetPreviewId) {
      document.getElementById(widgetPreviewId)?.dispatchEvent(
        new CustomEvent('update-widget-query', {
          detail: { value: queryValue },
        })
      );
    } else {
      this.dispatchEvent(
        new CustomEvent('update-query', {
          detail: { value: queryValue },
          bubbles: true,
        })
      );
      // Also dispatch to window for listeners that don't use bubbling (e.g., chart widgets)
      window.dispatchEvent(
        new CustomEvent('update-query', {
          detail: { value: queryValue },
        })
      );
    }

    // The update-query listeners above clear the inline message; re-assert it in
    // the same task so the error can't blink out between the clear and a repaint.
    const model = this.editor?.getModel();
    if (model) void this.validateAndMark(queryValue, model);
  };

  async firstUpdated(): Promise<void> {
    // Bail if disconnected before we got here (fast nav before render commits).
    if (!this.isConnected || !this._editorContainer) return;

    this.lifecycleAbort = new AbortController();
    const { signal } = this.lifecycleAbort;

    this.defaultValue = this.getAttribute('default-value') || '';
    this.updateURLParams = this.getAttribute('widget-editor') !== 'true';
    this.createMonacoEditor();
    this.setupSuggestions();

    this.addEventListener('keydown', (e: KeyboardEvent) => {
      if (e.key === 'Escape' && this.showSuggestions) {
        this.showSuggestions = false;
      }
    }, { signal });

    window.addEventListener('resize', this.resizeHandler, { signal });

    // A query restored from the URL is validated before the (lazily fetched)
    // schema exists, so re-check once it lands — otherwise an unknown field
    // stays unmarked until the next keystroke.
    document.body.addEventListener(
      'schema-loaded',
      () => {
        const model = this.editor?.getModel();
        if (model) void this.validateAndMark(model.getValue(), model);
        // Any completion list built before this point had no fields in it.
        if (this.editor?.hasTextFocus()) this.triggerSuggestions();
      },
      { signal }
    );

    // Set up ResizeObserver to handle container size changes
    if (this._editorContainer && window.ResizeObserver) {
      this.resizeObserver = new ResizeObserver(() => {
        this.refreshLayoutThrottled();
      });
      this.resizeObserver.observe(this._editorContainer);
    }

    // Focus editor when "/" is pressed
    document.addEventListener('keydown', this.keydownHandler, { signal });

    // Watch for theme changes
    this.themeObserver = new MutationObserver((mutations) => {
      if (!this.isConnected || !this.editor) return;
      mutations.forEach((mutation) => {
        if (mutation.type === 'attributes' && mutation.attributeName === 'data-theme') {
          const isDarkMode = document.body.getAttribute('data-theme') === 'dark';
          const theme = isDarkMode ? 'transparent-theme-dark' : 'transparent-theme-light';
          this.editor?.updateOptions({ theme });
        }
      });
    });

    this.themeObserver.observe(document.body, {
      attributes: true,
      attributeFilter: ['data-theme'],
    });
  }

  disconnectedCallback(): void {
    // Single abort tears down every listener registered with our signal —
    // works even if firstUpdated was mid-flight when we got disconnected.
    this.lifecycleAbort?.abort();
    this.lifecycleAbort = null;
    this.suggestionListeners.forEach((dispose) => dispose());
    this.suggestionListeners = [];
    this.updateHandlers.forEach((handler) => handler.dispose());
    this.updateHandlers = [];
    this.resizeObserver?.disconnect();
    this.resizeObserver = null;
    this.themeObserver?.disconnect();
    this.themeObserver = null;
    // Clear any pending debounced update
    if (this.updateQueryTimeout !== null) {
      clearTimeout(this.updateQueryTimeout);
      this.updateQueryTimeout = null;
    }
    // Reset tracking variables
    this.lastEmittedQueryValue = '';
    this.lastQueryValue = '';
    this.editor?.dispose();
    this.editor = null;
    super.disconnectedCallback();
  }

  public setSchema(schema: Partial<Schema>): void {
    schemaManager.setSchema(schema);
  }

  public setDynamicResolver(fn: (path: string[]) => Promise<{ name: string; info: FieldInfo }[]>): void {
    schemaManager.setDynamicResolver(fn);
  }

  public setQueryLibrary(items: QueryLibItem[]): void {
    if (!items?.length) return;

    const stripPrefix = (text: string) => text;
    const grouped = groupBy(items, 'queryType');

    this.recentSearches = (grouped.history || []).map((item) => ({
      kind: 'recentSearch' as const,
      query: stripPrefix(item.queryText),
      timestamp: this.formatRelativeTime(new Date(item.updatedAt)),
    }));

    this.savedViews = (grouped.saved || []).map((item) => ({
      kind: 'savedView' as const,
      name: item.title || `Query ${item.id.substring(0, 8)}`,
      query: stripPrefix(item.queryText),
      owner: { name: item.byMe ? 'You' : 'Other', icon: item.byMe ? '👤' : '👥' },
    }));

    this._matchesCache = null; // Invalidate cache
  }

  // Toggle a subquery - add if not present, remove if present
  public toggleSubQuery(queryFragment: string): void {
    if (!this.editor) return;
    const currentValue = this.editor.getValue().trim();

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
    if (!this.editor) return;

    const currentQuery = this.editor.getValue().trim();

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

  public handleAddQuery(queryFragment: string, replace: boolean = false): void {
    if (!this.editor) return;

    const editor = this.editor;
    const previouslyFocusedElement = document.activeElement as HTMLElement;
    const hadFocus = editor.hasTextFocus();

    this.isProgrammaticUpdate = true;

    const originalFocus = this.editor.focus;
    const originalTrigger = this.editor.trigger;
    const editorDomNode = this.editor.getDomNode();

    this.editor.focus = () => {};
    this.editor.trigger = () => {};

    const preventFocus = (e: FocusEvent) => {
      if (e.target === editorDomNode || editorDomNode?.contains(e.target as Node)) {
        e.preventDefault();
        e.stopPropagation();
        e.stopImmediatePropagation();
      }
    };

    document.addEventListener('focus', preventFocus, true);
    document.addEventListener('focusin', preventFocus, true);

    try {
      const currentValue = this.editor.getValue().trim();
      let newValue;

      if (replace || !currentValue) {
        newValue = queryFragment;
      } else {
        // Find where to insert the new condition
        const pipeIndex = currentValue.indexOf('|');
        const whereIndex = currentValue.toLowerCase().indexOf('| where ');

        if (whereIndex >= 0) {
          // Has explicit where clause - insert after "where"
          const wherePos = whereIndex + 8; // "| where ".length
          newValue = `${currentValue.substring(0, wherePos)}(${queryFragment}) and ${currentValue.substring(wherePos)}`;
        } else if (pipeIndex > 0) {
          // Has pipe but no where - insert at first segment
          newValue = `${currentValue.substring(0, pipeIndex)} and ${queryFragment} ${currentValue.substring(pipeIndex)}`;
        } else {
          // Simple query - just append
          newValue = `${currentValue} and ${queryFragment}`;
        }
      }

      this.editor.setValue(newValue);

      const model = this.editor.getModel();
      if (model) {
        const lastLine = model.getLineCount();
        const lastColumn = model.getLineMaxColumn(lastLine);
        this.editor.setPosition({ lineNumber: lastLine, column: lastColumn });
      }

      this.showSuggestions = false;
      this.selectedIndex = -1;

      this.updateQuery(newValue);

      // Update placeholder immediately
      this.updatePlaceholder();
    } finally {
      setTimeout(() => {
        editor.focus = originalFocus.bind(editor);
        editor.trigger = originalTrigger.bind(editor);

        document.removeEventListener('focus', preventFocus, true);
        document.removeEventListener('focusin', preventFocus, true);

        this.isProgrammaticUpdate = false;

        if (
          !hadFocus &&
          previouslyFocusedElement &&
          previouslyFocusedElement !== editorDomNode &&
          !editorDomNode?.contains(previouslyFocusedElement)
        ) {
          previouslyFocusedElement.focus();

          setTimeout(() => {
            if (document.activeElement === editorDomNode || editorDomNode?.contains(document.activeElement)) {
              previouslyFocusedElement.focus();
            }
          }, 50);
        }
      }, 0);
    }
  }

  private formatRelativeTime(date: Date): string {
    const diffSec = Math.floor((Date.now() - date.getTime()) / 1000);
    const intervals = [
      { threshold: 60, unit: 'just now' },
      { threshold: 3600, unit: 'minutes ago', divisor: 60 },
      { threshold: 86400, unit: 'hours ago', divisor: 3600 },
      { threshold: Infinity, unit: 'days ago', divisor: 86400 },
    ];

    for (const { threshold, unit, divisor } of intervals) {
      if (diffSec < threshold) {
        return divisor ? `${Math.floor(diffSec / divisor)} ${unit}` : unit;
      }
    }
    return '';
  }

  private createMonacoEditor(): void {
    const isDarkMode = document.body.getAttribute('data-theme') === 'dark';
    const theme = isDarkMode ? 'transparent-theme-dark' : 'transparent-theme-light';

    this.editor = monaco.editor.create(this._editorContainer, {
      value: this.defaultValue,
      language: 'aql', // Keep AQL for syntax highlighting and context-aware completion
      theme: theme,
      automaticLayout: false,
      minimap: { enabled: false },
      scrollBeyondLastLine: false,
      lineNumbers: 'off',
      roundedSelection: false,
      readOnly: false,
      cursorStyle: 'line',
      fontLigatures: false,
      fontSize: 14,
      lineHeight: 20,
      'semanticHighlighting.enabled': false,
      // Completions are ours (see ./completion); Monaco contributes highlighting only.
      quickSuggestions: false,
      suggestOnTriggerCharacters: false,
      suggest: {
        showIcons: false,
        snippetsPreventQuickSuggestions: true,
        filterGraceful: false,
        showWords: false,
      } as any,
      wordWrap: 'on',
      wrappingStrategy: 'simple',
      wrappingIndent: 'none',
      wordWrapOverride1: 'on',
      wordWrapOverride2: 'on',
      glyphMargin: false,
      folding: false,
      padding: { top: 8, bottom: 8 },
      renderLineHighlight: 'none',
      overviewRulerBorder: false,
      overviewRulerLanes: 0,
      hideCursorInOverviewRuler: true,
      scrollbar: {
        vertical: 'hidden',
        horizontal: 'hidden',
        alwaysConsumeMouseWheel: false,
      },
      lineDecorationsWidth: 0,
      lineNumbersMinChars: 0,
      renderWhitespace: 'none',
      cursorBlinking: 'solid',
      smoothScrolling: false,
      // PERF: Aggressive optimizations for instant typing
      accessibilitySupport: 'auto',
      matchBrackets: 'never',
      links: false,
      contextmenu: false,
      occurrencesHighlight: 'off',
      selectionHighlight: false,
      renderControlCharacters: false,
      codeLens: false,
      lightbulb: { enabled: 'off' as any },
      hover: { enabled: false },
      parameterHints: { enabled: false },
      inlayHints: { enabled: 'off' as any },
      stickyScroll: { enabled: false },
      find: { addExtraSpaceOnTop: false, autoFindInSelection: 'never', seedSearchStringFromSelection: 'never' },
      colorDecorators: false,
      dropIntoEditor: { enabled: false },
      unicodeHighlight: { ambiguousCharacters: false, invisibleCharacters: false },
    });

    this.setupEditorEvents();
    this.adjustEditorHeight();
    setTimeout(() => this.updatePlaceholder(), 100);
  }

  private setupEditorEvents(): void {
    if (!this.editor) return;

    this.updateHandlers.forEach((handler) => handler.dispose());
    this.updateHandlers = [];

    this.editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter, () => {
      const position = this.editor?.getPosition();
      if (position) {
        this.editor?.executeEdits('insert-line', [
          {
            range: {
              startLineNumber: position.lineNumber,
              startColumn: position.column,
              endLineNumber: position.lineNumber,
              endColumn: position.column,
            },
            text: '\n',
          },
        ]);
      }
    });

    let clickedOnSuggestion = false;
    document.addEventListener('pointerdown', (e: Event) => {
      clickedOnSuggestion = !!this.querySelector('.suggestions-dropdown')?.contains(e.target as Node);
    }, { signal: this.lifecycleAbort?.signal });

    const handlers = [
      this.editor.onDidFocusEditorText(() => {
        if (!this.isProgrammaticUpdate) {
          this.showSuggestions = true;
          this.updatePlaceholder();
          setTimeout(() => {
            if (!this.isProgrammaticUpdate) {
              this.triggerSuggestions();
            }
          }, 10);
        }
      }),

      this.editor.onMouseDown(() => {
        if (!this.isProgrammaticUpdate) {
          setTimeout(() => {
            if (!this.isProgrammaticUpdate && this.editor?.hasTextFocus()) {
              this.showSuggestions = true;
            }
          }, 10);
        }
      }),

      this.editor.onDidBlurEditorText(() => {
        setTimeout(() => {
          if (!this.editor?.hasTextFocus() && !clickedOnSuggestion) {
            this.showSuggestions = false;
          }
          this.updatePlaceholder();
        }, 300);
      }),

      this.editor.onKeyDown(this.handleKeyboardNavigation),

      this.editor.onDidChangeModelContent(() => {
        if (this.isProgrammaticUpdate) return;

        const model = this.editor?.getModel();
        if (!model) return;

        const newValue = model.getValue();

        // Only update placeholder on empty/non-empty transitions
        const isEmpty = newValue.trim() === '';
        const wasEmpty = !this.currentQuery || this.currentQuery.trim() === '';
        if (isEmpty !== wasEmpty) {
          this.updatePlaceholder();
        }

        // Update internal state without triggering Lit re-render
        this.currentQuery = newValue;

        // Validation is debounced, not per-keystroke: a half-typed query passes in
        // and out of a valid shape ("attribut" -> no operator yet -> no error),
        // so marking on every character made the message flicker and, since it
        // sits in flow above the editor, shifted the page with it.
        this.validateDebounced(newValue);

        // Suggestions follow the cursor directly now — Monaco's own widget is off.
        if (this.showSuggestions) void this.refreshSuggestions();

        // Debounced URL/event update
        this.updateQueryDebounced(newValue);
      }),

      // OPTIMIZATION: Removed cursor position handler - dropdown position is already correct
      // The dropdown position is calculated in render() based on editor position
      // No need to update on every cursor movement - it just adds overhead

      this.editor.onDidContentSizeChange(() => this.adjustEditorHeight()),
    ];

    this.updateHandlers.push(...handlers);
  }

  private handleKeyboardNavigation = (e: monaco.IKeyboardEvent): void => {
    if (!this.showSuggestions) return;

    const totalItems = this.getTotalVisibleSuggestions();
    if (totalItems === 0) return;

    const preventAndStop = () => {
      e.preventDefault();
      e.stopPropagation();
    };

    const key = e.browserEvent.key;
    const keyActions: Record<string, () => void> = {
      ArrowDown: () => {
        preventAndStop();
        this.selectedIndex = (this.selectedIndex + 1) % totalItems;
        this.scrollSelectedIntoView();
      },
      ArrowUp: () => {
        preventAndStop();
        this.selectedIndex = this.selectedIndex <= 0 ? totalItems - 1 : this.selectedIndex - 1;
        this.scrollSelectedIntoView();
      },
      Tab: () => {
        if (!e.browserEvent.shiftKey) {
          preventAndStop();
          this.selectedIndex = (this.selectedIndex + 1) % totalItems;
          this.scrollSelectedIntoView();
        } else {
          preventAndStop();
          this.selectedIndex = this.selectedIndex <= 0 ? totalItems - 1 : this.selectedIndex - 1;
          this.scrollSelectedIntoView();
        }
      },
      Enter: () => {
        if (this.selectedIndex >= 0) {
          preventAndStop();
          const item = this.getItemAtIndex(this.selectedIndex);
          if (item) this.insertCompletion(item);
        } else {
          preventAndStop();
          this.showSuggestions = false;
          this.selectedIndex = -1;
          const model = this.editor?.getModel();
          if (model) {
            this.updateQuery(model.getValue());
          }
        }
      },
      Escape: () => {
        preventAndStop();
        this.showSuggestions = false;
        this.selectedIndex = -1;
      },
    };

    keyActions[key]?.();
  };

  private scrollSelectedIntoView(): void {
    // OPTIMIZATION: Use requestAnimationFrame but skip if not needed
    // Only scroll if the dropdown is actually visible
    if (!this.showSuggestions) return;

    requestAnimationFrame(() => {
      // OPTIMIZATION: Cache the dropdown element instead of querying for each item
      const dropdown = this.querySelector('.suggestions-dropdown');
      if (!dropdown) return;

      const item = dropdown.querySelector(`[data-index="${this.selectedIndex}"]`) as HTMLElement;
      item?.scrollIntoView({ block: 'nearest' });
    });
  }

  private updateDropdownPosition(): void {
    if (!this.editor || !this.showSuggestions) return;

    const dropdown = this.querySelector('.suggestions-dropdown') as HTMLElement;
    if (!dropdown) return;

    const position = this.editor.getPosition();
    const coords = position ? this.editor.getScrolledVisiblePosition(position) : null;

    if (coords) {
      dropdown.style.top = `${coords.top + 24}px`;
      dropdown.style.left = '10px';
      dropdown.style.right = '10px';
    }
  }

  private adjustEditorHeight(): void {
    if (!this.editor || !this._editorContainer) return;
    const minHeight = 24; // Minimum height in pixels (single line + padding)
    const height = Math.max(this.editor.getContentHeight(), minHeight);

    // OPTIMIZATION: Only update if height actually changed
    if (this._editorContainer.style.height === `${height}px`) return;

    this._editorContainer.style.height = `${height}px`;
    this.editor.layout();
  }

  private updatePlaceholder(): void {
    if (!this._placeholderElement || !this.editor) return;
    const model = this.editor.getModel();
    const isEmpty = !model || model.getValue().trim() === '';
    this._placeholderElement.style.display = isEmpty ? 'block' : 'none';
  }

  /** Schema reads for the completion module, backed by the shared manager. */
  private get schemaAccess(): SchemaAccess {
    return {
      tables: () => schemaManager.getSchemas(),
      defaultTable: () => schemaManager.getDefaultSchema(),
      fields: (table, prefix) => schemaManager.resolveNested(table, prefix),
      values: (table, field) => schemaManager.resolveValues(table, field),
    };
  }

  /**
   * Recompute the suggestion list for the current cursor. This is the only
   * producer of `completionItems` — an empty result clears the list rather than
   * leaving the previous one on screen.
   */
  private refreshSuggestions = async (): Promise<void> => {
    const position = this.editor?.getPosition();
    const model = this.editor?.getModel();
    if (!position || !model) return;

    const text = model.getValueInRange({ startLineNumber: 1, startColumn: 1, endLineNumber: position.lineNumber, endColumn: position.column });
    // Late results from a superseded keystroke must not overwrite a newer list.
    const seq = ++this.suggestionSeq;
    const computed = await computeSuggestions(text, this.schemaAccess);
    if (seq !== this.suggestionSeq || !this.isConnected) return;

    const parentPath = text.includes('.') ? (text.match(REGEX_PATTERNS.dotMatchEnd) || text.match(REGEX_PATTERNS.dotMatchPartial))?.[1] : undefined;

    // Sort only where an explicit hint exists; everything else keeps the order the
    // completion module produced (value examples are listed most-useful-first, and
    // re-alphabetising them buries the common ones).
    this.completionItems = filterSuggestions(computed, wordAtCursor(text))
      .map((c, i) => ({ c, i }))
      .sort((a, b) => (a.c.sortText ?? '\uffff').localeCompare(b.c.sortText ?? '\uffff') || a.i - b.i)
      .map(({ c }) => c)
      .slice(0, 20)
      .map((c: Suggestion) => ({
        kind: 'completion' as const,
        label: c.label,
        insertText: c.insertText,
        kindCategory: c.kind,
        detail: c.detail || c.documentation || '',
        parentPath,
      }));

    this.selectedIndex = -1;
  };

  private getTotalVisibleSuggestions(): number {
    const matches = this.getMatches();
    return this.completionItems.length + matches.recent.length + matches.saved.length + matches.popular.length;
  }

  private getItemAtIndex(index: number): SuggestionItem | null {
    const matches = this.getMatches();
    const allItems = [...this.completionItems, ...matches.recent, ...matches.saved, ...matches.popular];
    return allItems[index] || null;
  }

  private getMatches() {
    const query = this.currentQuery?.toLowerCase() || '';

    // Check cache - avoid recomputing if query hasn't changed
    if (this._matchesCache && this._matchesCache.query === query) {
      return this._matchesCache.result;
    }

    const searchTerm = query.split('|').pop()?.trim() || '';

    const filterAndSlice = (items: any[], prop: string = 'query') =>
      (searchTerm
        ? items.filter((item) =>
            (prop === 'query' ? item.query : item.name).toLowerCase().includes(searchTerm) ||
            item.query.toLowerCase().includes(searchTerm)
          )
        : items
      ).slice(0, 5);

    const result = {
      saved: filterAndSlice(this.savedViews, 'name'),
      recent: filterAndSlice(this.recentSearches),
      popular: filterAndSlice(this.popularSearches),
    };

    // Cache the result
    this._matchesCache = { query, result };

    return result;
  }

  private handleSuggestionClick(item: SuggestionItem, e: MouseEvent): void {
    e.preventDefault();
    e.stopPropagation();
    this.insertCompletion(item);
    this.editor?.focus();
  }

  private insertCompletion(item: SuggestionItem): void {
    if (!this.editor) return;

    const position = this.editor.getPosition();
    const model = this.editor.getModel();
    if (!position || !model) return;

    let textToInsert = '';
    let replaceRange: monaco.IRange;

    if (item.kind === 'recentSearch' || item.kind === 'savedView' || item.kind === 'popularSearch') {
      textToInsert = item.query;
      replaceRange = {
        startLineNumber: position.lineNumber,
        startColumn: 1,
        endLineNumber: position.lineNumber,
        endColumn: model.getLineMaxColumn(position.lineNumber),
      };
    } else {
      textToInsert = item.insertText || item.label;

      const currentLine = model.getLineContent(position.lineNumber);
      const lineText = currentLine.substring(0, position.column - 1);
      const wordEndPos = position.column - 1;
      let wordStartPos = wordEndPos;

      const dotMatch = lineText.includes('.') ? (lineText.match(REGEX_PATTERNS.dotMatchEnd) || lineText.match(REGEX_PATTERNS.dotMatchPartial)) : null;

      if (dotMatch) {
        const lastDotIndex = lineText.lastIndexOf('.');
        wordStartPos = lastDotIndex + 1;
      } else {
        while (wordStartPos > 0) {
          const c = currentLine.charAt(wordStartPos - 1);
          if (REGEX_PATTERNS.whitespaceTest.test(c) || REGEX_PATTERNS.wordBoundaryTest.test(c)) break;
          wordStartPos--;
        }
      }

      replaceRange = {
        startLineNumber: position.lineNumber,
        startColumn: wordStartPos + 1,
        endLineNumber: position.lineNumber,
        endColumn: position.column,
      };
    }

    const shouldMoveCursor = textToInsert.endsWith('(') && !textToInsert.includes(' ');

    try {
      this.editor.executeEdits('completion', [{ range: replaceRange, text: textToInsert }]);
    } catch (e) {
      console.error('Error executing edit:', e);
      model.pushEditOperations([], [{ range: replaceRange, text: textToInsert }], () => null);
    }

    this.selectedIndex = -1;

    if (shouldMoveCursor) {
      const newPosition = {
        lineNumber: replaceRange.startLineNumber,
        column: replaceRange.startColumn + textToInsert.length,
      };

      setTimeout(() => {
        this.editor?.setPosition(newPosition);
        this.editor?.focus();
      }, 10);
    } else {
      this.editor.focus();
    }

    const triggerDelay = textToInsert.endsWith('.') ? 0 : 100;
    setTimeout(() => this.triggerSuggestions(), triggerDelay);
  }

  private setupSuggestions(): void {
    if (!this.editor) return;
    this.editor.addCommand(monaco.KeyCode.Space | monaco.KeyMod.CtrlCmd, () => this.triggerSuggestions());
  }

  private getCompletionIcon(kind: CompletionKind): TemplateResult {
    const badge = this.KIND_BADGES[kind] || { label: '?', cls: 'text-textWeak bg-fillWeak border-strokeWeak' };
    return html`<span class="inline-flex items-center justify-center w-5 h-5 rounded text-2xs font-semibold border leading-none ${badge.cls}">${badge.label}</span>`;
  }

  // Generate unique key for suggestion items (for repeat directive performance)
  private getSuggestionKey(item: SuggestionItem): string {
    switch (item.kind) {
      case 'completion':
        return `completion-${item.label}-${item.parentPath || ''}`;
      case 'recentSearch':
        return `recent-${item.query}`;
      case 'savedView':
        return `saved-${item.name}`;
      case 'popularSearch':
        return `popular-${item.query}`;
      default:
        return `unknown-${Math.random()}`;
    }
  }

  private getSuggestionUIData(item: SuggestionItem): {
    icon: string | TemplateResult;
    primaryText: string | TemplateResult;
    secondaryText: string | TemplateResult | undefined;
  } {
    const uiData: Record<SuggestionKind, () => ReturnType<typeof this.getSuggestionUIData>> = {
      recentSearch: () => ({
        icon: html`<span class="inline-flex items-center justify-center w-5 h-5 rounded text-2xs font-semibold border leading-none text-textWeak bg-fillWeak border-strokeWeak">⏱</span>`,
        primaryText: (item as RecentSearch).query,
        secondaryText: (item as RecentSearch).timestamp,
      }),
      savedView: () => ({
        icon: html`<span class="inline-flex items-center justify-center w-5 h-5 rounded text-2xs font-semibold border leading-none text-amber-400 bg-amber-400/15 border-amber-400/30">★</span>`,
        primaryText: (item as SavedView).name,
        secondaryText: html`
          <span class="truncate text-textWeak mr-2" title="${(item as SavedView).query}">${(item as SavedView).query}</span>
          ${(item as SavedView).owner
            ? html`<span class="flex-shrink-0 rounded-full w-6 h-6 flex items-center justify-center text-xs"
                >${(item as SavedView).owner!.icon || ''}</span
              >`
            : ''}
        `,
      }),
      popularSearch: () => ({
        icon: html`<span class="inline-flex items-center justify-center w-5 h-5 rounded text-2xs font-semibold border leading-none text-sky-400 bg-sky-400/15 border-sky-400/30">↗</span>`,
        primaryText: (item as PopularSearch).query,
        secondaryText: (item as PopularSearch).description,
      }),
      completion: () => {
        const completion = item as CompletionItem;
        const typeColor: Record<string, string> = {
          string: 'text-sky-400', number: 'text-emerald-400', int: 'text-emerald-400',
          object: 'text-violet-400', array: 'text-teal-400', boolean: 'text-orange-400',
          duration: 'text-amber-400', bytes: 'text-rose-400',
        };
        const detailCls = typeColor[completion.detail || ''] || 'text-textWeak';
        const detailHtml = completion.detail ? html`<span class="${detailCls}">${completion.detail}</span>` : undefined;
        if (completion.parentPath) {
          return {
            icon: this.getCompletionIcon(completion.kindCategory),
            primaryText: html` <span class="text-textWeak">${completion.parentPath}.</span><span>${completion.label}</span> `,
            secondaryText: detailHtml,
          };
        }
        return {
          icon: this.getCompletionIcon(completion.kindCategory),
          primaryText: completion.label,
          secondaryText: detailHtml,
        };
      },
    };

    return uiData[item.kind]();
  }

  private renderSuggestionItem(item: SuggestionItem, itemIndex: number): TemplateResult {
    const isSelected = itemIndex === this.selectedIndex;
    const { icon, primaryText, secondaryText } = this.getSuggestionUIData(item);
    const selectedClass = isSelected ? 'bg-fillBrand-weak' : '';

    const displayTextForTooltip =
      item.kind === 'completion'
        ? ((item as CompletionItem).parentPath ? `${(item as CompletionItem).parentPath}.${item.label}` : item.label) || ''
        : item.kind === 'savedView'
          ? (item as SavedView).name || 'Saved View'
          : item.query || '';

    return html`
      <div
        class="flex items-center justify-between px-4 py-2 hover:bg-fillBrand-weak cursor-pointer border-b border-strokeWeak ${selectedClass}"
        @pointerdown=${(e: MouseEvent) => this.handleSuggestionClick(item, e)}
        @mouseover=${() => { if (this.selectedIndex !== itemIndex) this.selectedIndex = itemIndex; }}
        data-index="${itemIndex}"
        id="query-suggestion-${itemIndex}"
        role="option"
        aria-selected="${isSelected}"
      >
        <div class="flex items-center gap-2 overflow-hidden">
          <span class="shrink-0">${icon}</span>
          <span class="truncate ${isSelected ? 'font-medium text-textBrand' : ''}" title="${displayTextForTooltip}">${primaryText}</span>
        </div>
        ${secondaryText ? html`<span class="text-xs text-textWeak ml-2 flex-shrink-0 flex items-center">${secondaryText}</span>` : ''}
      </div>
    `;
  }

  private splitCompletionSubgroups(items: CompletionItem[]): { items: SuggestionItem[]; title: string | null }[] {
    if (!items.length) return [];
    const isAllOperators = items.every((i) => i.kindCategory === 'operator');
    const isAllFields = items.every((i) => i.kindCategory === 'field');
    const hasMixedTypes = !isAllOperators && !isAllFields && items.some((i) => i.kindCategory === 'operator');

    if (isAllOperators && items.length > 6) {
      const common = items.filter((i) => COMMON_OPERATORS.includes(i.label));
      const advanced = items.filter((i) => !COMMON_OPERATORS.includes(i.label));
      const sections: { items: SuggestionItem[]; title: string | null }[] = [];
      if (common.length) sections.push({ items: common, title: 'Common' });
      if (advanced.length) sections.push({ items: advanced, title: 'More Operators' });
      return sections;
    }

    if (isAllFields && items.length > 8) {
      const priority = items.filter((i) => PRIORITY_FIELDS.has(i.label));
      const other = items.filter((i) => !PRIORITY_FIELDS.has(i.label));
      const sections: { items: SuggestionItem[]; title: string | null }[] = [];
      if (priority.length) sections.push({ items: priority, title: null });
      if (other.length) sections.push({ items: other, title: 'More Fields' });
      return sections;
    }

    if (hasMixedTypes) {
      const operators = items.filter((i) => i.kindCategory === 'operator');
      const fields = items.filter((i) => i.kindCategory !== 'operator');
      const sections: { items: SuggestionItem[]; title: string | null }[] = [];
      // Operators first when mixed — they're the more contextually relevant suggestion
      if (operators.length) sections.push({ items: operators, title: 'Operators' });
      if (fields.length) sections.push({ items: fields, title: 'Fields' });
      return sections;
    }

    return [{ items, title: null }];
  }

  private renderSuggestionDropdown(): TemplateResult {
    if (!this.showSuggestions || !this.editor) return html``;

    const completionSubgroups = this.splitCompletionSubgroups(this.completionItems);

    // Only show saved/recent/popular in field position (not when suggesting operators or values)
    const isFieldPosition =
      !this.completionItems.length ||
      this.completionItems.some((i) => i.kindCategory === 'field');

    type SectionDef = { items: SuggestionItem[]; title: string | null };
    const sections: SectionDef[] = [...completionSubgroups];
    if (isFieldPosition) {
      const matches = this.getMatches();
      if (matches.saved.length) sections.push({ items: matches.saved as SuggestionItem[], title: 'Saved Views' });
      if (matches.recent.length) sections.push({ items: matches.recent as SuggestionItem[], title: 'Recent Searches' });
      if (matches.popular.length) sections.push({ items: matches.popular as SuggestionItem[], title: 'Popular Searches' });
    }

    // Never leave the offsets empty: an `absolute` box with no top/left/right
    // sits at its static position, which for this last-child-of-the-input-row is
    // directly ON the input. It then swallows the click meant for the editor, so
    // focus never lands and the box flickers open and shut. There is no cursor to
    // measure until focus has landed, which is exactly when this happens.
    const position = this.editor.getPosition();
    const coords = position ? this.editor.getScrolledVisiblePosition(position) : null;
    const positionStyle = `top: ${coords ? coords.top + 24 : 34}px; left: 10px; right: 10px;`;

    // Nothing to offer: render nothing rather than a box announcing as much,
    // which only covers the query the user is typing.
    if (!sections.length) return html``;

    let currentIndex = 0;
    const keyboardHelp = html`
      <div class="sticky bottom-0 bg-bgRaised z-50 border-t border-strokeWeak px-4 py-2 text-xs text-textWeak flex items-center justify-between">
        <div>
          <span class="mr-2">
            <kbd class="px-1 py-0.5 bg-fillWeak border border-strokeStrong rounded text-xs">↑</kbd>
            <kbd class="px-1 py-0.5 bg-fillWeak border border-strokeStrong rounded text-xs">↓</kbd>
            <kbd class="px-1 py-0.5 bg-fillWeak border border-strokeStrong rounded text-xs">Tab</kbd> to navigate
          </span>
          <span class="mr-2">• <kbd class="px-1 py-0.5 bg-fillWeak border border-strokeStrong rounded text-xs">Enter</kbd> to select</span>
          <span>• <kbd class="px-1 py-0.5 bg-fillWeak border border-strokeStrong rounded text-xs">Esc</kbd> to close</span>
        </div>
        <a href="https://monoscope.tech/docs/dashboard/dashboard-pages/api-log-explorer/" target="_blank" rel="noopener" class="text-textBrand hover:underline shrink-0 ml-2">Syntax guide ↗</a>
      </div>
    `;

    const isEmptyEditor = !this.editor.getModel()?.getValue().trim();
    const syntaxHint = isEmptyEditor
      ? html`<div class="px-4 py-2 text-xs text-textWeak bg-fillWeaker border-b border-strokeWeak">
          Type a field name, then an operator and value — e.g. <code class="text-textStrong bg-fillWeak px-1 rounded">status_code == "ERROR"</code>
        </div>`
      : '';

    return html`
      <div
        class="mt-1 suggestions-dropdown absolute bg-bgRaised border border-strokeWeak shadow-lg z-50 max-h-[80dvh] overflow-y-auto rounded-md text-xs flex flex-col"
        style="${positionStyle}"
        id="query-suggestions"
        role="listbox"
        aria-label="Query suggestions"
      >
        ${syntaxHint}
        <div class="overflow-y-auto flex-grow min-h-0">
          ${sections.map(
            (section) => html`
              ${section.title
                ? html`<div
                    class="text-xs font-semibold text-textWeak px-4 py-2 uppercase border-t border-b border-strokeWeak bg-fillWeaker"
                  >
                    ${section.title}
                  </div>`
                : ''}
              ${repeat(
                section.items,
                (item) => this.getSuggestionKey(item),
                (item) => this.renderSuggestionItem(item, currentIndex++)
              )}
            `
          )}
        </div>
        ${keyboardHelp}
      </div>
    `;
  }

  render(): TemplateResult {
    const dropdownTemplate = this.renderSuggestionDropdown();
    const aiSearchIcon = html`<svg class="inline-block icon h-4 w-4 text-iconBrand" viewBox="0 0 512 512" aria-hidden="true">
      <path fill="currentColor" d="M327.5 85.2c-4.5 1.7-7.5 6-7.5 10.8s3 9.1 7.5 10.8L384 128l21.2 56.5c1.7 4.5 6 7.5 10.8 7.5s9.1-3 10.8-7.5L448 128l56.5-21.2c4.5-1.7 7.5-6 7.5-10.8s-3-9.1-7.5-10.8L448 64 426.8 7.5C425.1 3 420.8 0 416 0s-9.1 3-10.8 7.5L384 64 327.5 85.2zM9.3 240C3.6 242.6 0 248.3 0 254.6s3.6 11.9 9.3 14.5l114 52.7 52.7 114c2.6 5.7 8.3 9.3 14.5 9.3s11.9-3.6 14.5-9.3l52.7-114 114-52.7c5.7-2.6 9.3-8.3 9.3-14.5s-3.6-11.9-9.3-14.5l-114-52.7-52.7-114c-2.6-5.7-8.3-9.3-14.5-9.3s-11.9 3.6-14.5 9.3l-52.7 114L9.3 240zm83 14.5 98.3-45.4 45.4 98.3-45.4 98.3-45.4-98.3-98.3-45.4zM384 384l-56.5 21.2c-4.5 1.7-7.5 6-7.5 10.8s3 9.1 7.5 10.8L384 448l21.2 56.5c1.7 4.5 6 7.5 10.8 7.5s9.1-3 10.8-7.5L448 448l56.5-21.2c4.5-1.7 7.5-6 7.5-10.8s-3-9.1-7.5-10.8L448 384l-21.2-56.5c-1.7-4.5-6-7.5-10.8-7.5s-9.1 3-10.8 7.5L384 384z" />
    </svg>`;

    return html`
      <div
        class="relative w-full min-h-[38px] pl-2 flex border rounded-md border-strokeStrong focus-within:border-strokeBrand-strong focus:outline-2 "
      >
        <div class="relative overflow-x-hidden w-full flex-1">
          <div id="editor-container" class="w-full"></div>
          <div
            class="placeholder-overlay absolute top-0 left-0 right-0 bottom-0 pointer-events-none z-[1] text-textWeak text-sm leading-5 py-2 pl-0 hidden cursor-text"
          >
            <span class="opacity-60">level == "ERROR"</span>
            <span class="mx-1 opacity-30">·</span>
            <span class="opacity-40"><kbd class="px-1 py-0.5 bg-fillWeak border border-strokeWeak rounded text-xs">/</kbd> to focus</span>
          </div>
        </div>
        <div class="p-1">
          ${this.hasAttribute('standalone-ai-search')
            ? html`<button
                type="button"
                class="px-3 py-0.5 h-full inline-flex gap-2 items-center cursor-pointer border border-strokeBrand-strong text-textBrand hover:border-strokeBrand-weak rounded-sm"
                data-tippy-content="Search in plain English — describe what you're looking for"
                aria-label="Open AI search"
                @click=${() => this.dispatchEvent(new CustomEvent('open-ai-search', { bubbles: true }))}
              >
                ${aiSearchIcon} AI search
              </button>`
            : html`<label
                class="px-3 py-0.5 h-full inline-flex gap-2 items-center cursor-pointer border border-strokeBrand-strong text-textBrand hover:border-strokeBrand-weak rounded-sm group-has-[.ai-search:checked]/fltr:hidden"
                data-tippy-content="Search in plain English — describe what you're looking for"
                for="ai-search-chkbox"
              >
                ${aiSearchIcon} AI search
              </label>`}
        </div>
        ${dropdownTemplate}
      </div>
    `;
  }
}

// Expose schemaManager globally for external configuration
(window as any).schemaManager = schemaManager;

export { monaco, schemaManager, type SchemaData };
