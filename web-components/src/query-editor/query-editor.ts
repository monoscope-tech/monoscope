import { LitElement, html, css, TemplateResult } from 'lit';
import { customElement, state, query } from 'lit/decorators.js';
import 'monaco-editor/esm/vs/editor/contrib/suggest/browser/suggestController.js';
import * as monaco from 'monaco-editor/esm/vs/editor/editor.api';
import { debounce, groupBy, pick } from 'lodash';

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
  kindCategory: number;
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
type FieldInfo = {
  type: FieldType;
  description?: string;
  enum?: (string | number | boolean)[];
  fields?: Record<string, FieldInfo>;
  examples?: string[];
};

type Schema = {
  fields: Record<string, FieldInfo>;
  operators?: Record<string, string[]>;
  dynamicResolver?: (path: string[]) => Promise<FieldInfo[]>;
};

type SchemaData = {
  fields: Record<string, FieldInfo>;
  operators?: Record<string, string[]>;
};

type SuggestionItem = CompletionItem | RecentSearch | SavedView | PopularSearch;

interface SuggestController {
  model?: any;
  _model?: any;
  widget?: any;
}

interface SchemaField {
  name: string;
  type: string;
  examples?: string[];
  fields?: Record<string, any>;
}

// Define constants to avoid duplication
// Operators categorized by type
const COMPARISON_OPERATORS = ['==', '!=', '>', '<', '>=', '<=', '=~'];
const SET_OPERATORS = ['in', '!in', 'has', '!has', 'has_any', 'has_all'];
const STRING_OPERATORS = ['contains', '!contains', 'startswith', '!startswith', 'endswith', '!endswith', 'matches'];
const LOGICAL_OPERATORS = ['and', 'or', 'not', 'exists', '!exists'];
const PIPE_OPERATOR = ['|'];

// Combine all operators for easy access
const ALL_OPERATORS = [...COMPARISON_OPERATORS, ...SET_OPERATORS, ...STRING_OPERATORS, ...PIPE_OPERATOR];

// Sources and keywords
const DATA_SOURCES = ['spans', 'metrics'];
const AGGREGATION_COMMANDS = ['stats', 'timechart'];
const AGGREGATION_MODIFIERS = ['by', 'as', 'limit'];
const STATS_FUNCTIONS = ['count', 'sum', 'avg', 'min', 'max', 'median', 'stdev', 'range', 'p50', 'p75', 'p90', 'p95', 'p99', 'p100'];

// Combine all keywords
const KEYWORDS = [...DATA_SOURCES, ...AGGREGATION_COMMANDS, ...AGGREGATION_MODIFIERS, ...LOGICAL_OPERATORS, ...STATS_FUNCTIONS];

// Schema Manager class for better encapsulation
class SchemaManager {
  private schemas: string[] = DATA_SOURCES;
  private defaultSchema = DATA_SOURCES[0];
  private schemaData: Record<string, SchemaData> = {};

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
          type: info.type || 'string',
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
            type: info.type || 'string',
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
  };
  getSchemaData = (schema: string) => this.schemaData[schema];
  setNestedResolver = (fn: typeof this.nestedResolver) => {
    this.nestedResolver = fn;
  };
  setValueResolver = (fn: typeof this.valueResolver) => {
    this.valueResolver = fn;
  };
  getSchemas = () => this.schemas;
  getDefaultSchema = () => this.defaultSchema;
  resolveNested = (schema: string, prefix: string) => this.nestedResolver(schema, prefix);
  resolveValues = (schema: string, field: string) => this.valueResolver(schema, field);

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
monaco.languages.setMonarchTokensProvider('aql', language);
monaco.languages.setLanguageConfiguration('aql', conf);

// Completion provider
monaco.languages.registerCompletionItemProvider('aql', {
  triggerCharacters: [' ', '|', '.', '[', ',', '"'],
  provideCompletionItems: async (model, position) => {
    const text = model.getValueInRange({
      startLineNumber: 1,
      startColumn: 1,
      endLineNumber: position.lineNumber,
      endColumn: position.column,
    });

    const currentLine = model.getLineContent(position.lineNumber);
    const segments = text.split(/\|/).map((s) => s.trim());
    const last = segments[segments.length - 1];

    const suggestions: monaco.languages.CompletionItem[] = [];
    const tables = schemaManager.getSchemas();
    const firstToken = text.trim().split(/\s+/)[0].toLowerCase();
    const currentSchema = tables.includes(firstToken) ? firstToken : schemaManager.getDefaultSchema();

    const createRange = (startCol = position.column, endCol = position.column) => ({
      startLineNumber: position.lineNumber,
      startColumn: startCol,
      endLineNumber: position.lineNumber,
      endColumn: endCol,
    });

    const lineText = currentLine.substring(0, position.column - 1);

    // First priority: Check for nested fields after dot
    const dotMatch =
      lineText.match(/([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\.$/) ||
      lineText.match(/([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\.[a-zA-Z0-9_]*$/);

    if (dotMatch) {
      const fieldPrefix = dotMatch[1];
      const nested = await schemaManager.resolveNested(currentSchema, fieldPrefix);

      nested.forEach((n) =>
        suggestions.push({
          label: n.name,
          kind: monaco.languages.CompletionItemKind.Field,
          insertText: n.type === 'object' ? `${n.name}.` : `${n.name} `,
          range: createRange(),
          detail: n.type,
          documentation: n.examples?.join(', '),
        })
      );

      return { suggestions };
    }

    // Check for operator pattern - show value suggestions
    const operatorMatch = lineText.match(
      /([\w\.]+)\s*(==|!=|>=|<=|>|<|=~|in|!in|has|!has|has_any|has_all|contains|!contains|startswith|!startswith|endswith|!endswith|matches)\s*$/
    );
    if (operatorMatch) {
      const fieldName = operatorMatch[1];
      const operator = operatorMatch[2];
      const values = await schemaManager.resolveValues(currentSchema, fieldName);

      // Special handling for 'in' and '!in' operators
      if (operator === 'in' || operator === '!in') {
        suggestions.push({
          label: '("value1", "value2")',
          kind: monaco.languages.CompletionItemKind.Snippet,
          insertText: '("value1", "value2") ',
          range: createRange(),
        });
      } else {
        values.forEach((v) =>
          suggestions.push({
            label: String(v),
            kind: monaco.languages.CompletionItemKind.Value,
            insertText: typeof v === 'string' ? `"${v}" ` : `${v} `,
            range: createRange(),
          })
        );
      }
      return { suggestions };
    }

    // Fourth priority: Check for complete field-operator-value pattern - suggest logical operators
    const afterValue = /".*"\s*$/.test(lineText) || /\d+\s*$/.test(lineText);
    if (afterValue) {
      [...LOGICAL_OPERATORS.filter((op) => op === 'and' || op === 'or'), PIPE_OPERATOR[0]].forEach((op) =>
        suggestions.push({
          label: op,
          kind: monaco.languages.CompletionItemKind.Operator,
          insertText: `${op} `,
          range: createRange(),
        })
      );
      return { suggestions };
    }

    // Fifth priority: Check for logical operators followed by space - suggest fields
    const logicalOperatorPattern = new RegExp(
      `\\b(${LOGICAL_OPERATORS.filter((op) => ['and', 'or', 'not'].includes(op)).join('|')})\\s+$`,
      'i'
    );
    const logicalOperatorMatch = lineText.match(logicalOperatorPattern);
    if (logicalOperatorMatch) {
      const fields = await schemaManager.resolveNested(currentSchema, '');
      fields.forEach((f) =>
        suggestions.push({
          label: f.name,
          kind: monaco.languages.CompletionItemKind.Field,
          detail: f.type,
          documentation: f.examples?.join(', '),
          insertText: f.type === 'object' ? `${f.name}.` : `${f.name} `,
          range: createRange(),
        })
      );
      return { suggestions };
    }

    // Check for field name followed by space
    const fieldSpaceMatch = lineText.match(/([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\s+$/);
    if (fieldSpaceMatch && !logicalOperatorMatch) {
      [
        '==',
        '!=',
        '>',
        '<',
        '>=',
        '<=',
        '=~',
        'in',
        '!in',
        'has',
        '!has',
        'has_any',
        'has_all',
        'contains',
        '!contains',
        'startswith',
        '!startswith',
        'endswith',
        '!endswith',
        'matches',
        'and',
        'or',
        'not',
        'exists',
      ].forEach((op) =>
        suggestions.push({
          label: op,
          kind: monaco.languages.CompletionItemKind.Operator,
          insertText: `${op} `,
          range: createRange(),
        })
      );
      return { suggestions };
    }

    // Empty or start
    if (segments.length === 1 && (last === '' || !tables.some((t) => last.toLowerCase().startsWith(t)))) {
      // Suggest data sources (tables)
      tables
        .filter((t) => last === '' || t.toLowerCase().startsWith(last.toLowerCase().trim()))
        .forEach((t) =>
          suggestions.push({
            label: t,
            kind: monaco.languages.CompletionItemKind.Module,
            insertText: `${t} `,
            range: createRange(),
          })
        );

      if (last === '' || !tables.some((t) => last.toLowerCase().trim().startsWith(t))) {
        const defaultSchema = schemaManager.getDefaultSchema();
        const fields = await schemaManager.resolveNested(defaultSchema, '');
        fields.forEach((f) =>
          suggestions.push({
            label: f.name,
            kind: monaco.languages.CompletionItemKind.Field,
            detail: f.type,
            documentation: f.examples?.join(', '),
            insertText: f.type === 'object' ? `${f.name}.` : `${f.name} `,
            range: createRange(),
          })
        );
      }
      return { suggestions };
    }

    // Schema name completion
    if (segments.length === 1 && tables.some((t) => t.startsWith(last.toLowerCase()))) {
      tables
        .filter((t) => t.startsWith(last.toLowerCase()))
        .forEach((t) =>
          suggestions.push({
            label: t,
            kind: monaco.languages.CompletionItemKind.Module,
            insertText: `${t} `,
            range: createRange(),
          })
        );
      return { suggestions };
    }

    // After schema
    if (segments.length === 1 && tables.includes(last.toLowerCase())) {
      // Suggest aggregation commands and modifiers
      [...AGGREGATION_COMMANDS, 'limit'].forEach((k) =>
        suggestions.push({
          label: k,
          kind: monaco.languages.CompletionItemKind.Keyword,
          insertText: `${k} `,
          range: createRange(),
        })
      );
      const fields = await schemaManager.resolveNested(currentSchema, '');
      fields.forEach((f) =>
        suggestions.push({
          label: f.name,
          kind: monaco.languages.CompletionItemKind.Field,
          insertText: f.type === 'object' ? `${f.name}.` : `${f.name} `,
          range: createRange(),
        })
      );
      return { suggestions };
    }

    // Search segment
    if (!/stats|timechart/i.test(last)) {
      [
        '==',
        '!=',
        '>',
        '<',
        '>=',
        '<=',
        '=~',
        'in',
        '!in',
        'has',
        '!has',
        'has_any',
        'has_all',
        'contains',
        '!contains',
        'startswith',
        '!startswith',
        'endswith',
        '!endswith',
        'matches',
        'and',
        'or',
        'not',
        'exists',
      ].forEach((op) =>
        suggestions.push({
          label: op,
          kind: monaco.languages.CompletionItemKind.Operator,
          insertText: `${op} `,
          range: createRange(),
        })
      );

      const fields = await schemaManager.resolveNested(currentSchema, '');
      fields.forEach((f) =>
        suggestions.push({
          label: f.name,
          kind: monaco.languages.CompletionItemKind.Field,
          detail: f.type,
          documentation: f.examples?.join(', '),
          insertText: f.type === 'object' ? `${f.name}.` : `${f.name} `,
          range: createRange(),
        })
      );
      return { suggestions };
    }

    // Stats/timechart segment
    if (/stats\s|timechart\s/i.test(last)) {
      // Use the STATS_FUNCTIONS constant directly
      STATS_FUNCTIONS.forEach((fn) =>
        suggestions.push({
          label: fn,
          kind: monaco.languages.CompletionItemKind.Function,
          insertText: `${fn}(`,
          range: createRange(),
        })
      );

      if (/\bby\s*$/i.test(last)) {
        const fields = await schemaManager.resolveNested(currentSchema, '');
        fields.forEach((f) =>
          suggestions.push({
            label: f.name,
            kind: monaco.languages.CompletionItemKind.Field,
            insertText: f.type === 'object' ? `${f.name}.` : `${f.name} `,
            range: createRange(),
          })
        );
      } else {
        suggestions.push({
          label: 'by',
          kind: monaco.languages.CompletionItemKind.Keyword,
          insertText: 'by ',
          range: createRange(),
        });
      }

      if (/timechart/i.test(last)) {
        ['[5m]', '[1h]'].forEach((iv) =>
          suggestions.push({
            label: iv,
            kind: monaco.languages.CompletionItemKind.Value,
            insertText: iv,
            range: createRange(),
          })
        );
      }
      return { suggestions };
    }

    return { suggestions };
  },
});

@customElement('query-editor')
export class QueryEditorComponent extends LitElement {
  protected createRenderRoot = () => this;

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

  private readonly KIND_ICONS = ['📄', '🔢', '🔍', '#', '📊', '📋', '#', '🔢', '✅', '❓'];

  public setPopularSearches(items: { query: string; description?: string }[]): void {
    if (!items?.length) return;
    this.popularSearches = items.map((item) => ({
      kind: 'popularSearch' as const,
      query: item.query,
      description: item.description || '',
    }));
  }

  // Public method to refresh editor layout
  public refreshLayout(): void {
    if (this.editor) {
      this.editor.layout();
    }
  }

  private debouncedTriggerSuggestions = debounce(() => this.editor?.trigger('auto', 'editor.action.triggerSuggest', {}), 50);
  private debouncedLayoutRefresh = debounce(() => this.refreshLayout(), 100);
  private debouncedUpdateQuery = debounce((queryValue: string) => {
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
    }

    // Update URL if needed
  }, 300);

  async firstUpdated(): Promise<void> {
    if (!this._editorContainer) return;

    this.defaultValue = this.getAttribute('default-value') || '';
    this.updateURLParams = this.getAttribute('widget-editor') !== 'true';
    this.createMonacoEditor();
    this.setupSuggestions();

    this.addEventListener('keydown', (e: KeyboardEvent) => {
      if (e.key === 'Escape' && this.showSuggestions) {
        this.showSuggestions = false;
      }
    });

    window.addEventListener('resize', () => {
      this.adjustEditorHeight();
      this.debouncedLayoutRefresh();
    });

    // Set up ResizeObserver to handle container size changes
    if (this._editorContainer && window.ResizeObserver) {
      this.resizeObserver = new ResizeObserver(() => {
        this.debouncedLayoutRefresh();
      });
      this.resizeObserver.observe(this._editorContainer);
    }

    // Focus editor when "/" is pressed
    document.addEventListener('keydown', (e: KeyboardEvent) => {
      if (e.key === '/' && !e.ctrlKey && !e.metaKey && !e.altKey) {
        const target = e.target as HTMLElement;
        if (target.tagName !== 'INPUT' && target.tagName !== 'TEXTAREA' && target.contentEditable !== 'true') {
          e.preventDefault();
          this.editor?.focus();
        }
      }
    });

    // Watch for theme changes
    this.themeObserver = new MutationObserver((mutations) => {
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
      attributeFilter: ['data-theme']
    });
  }

  disconnectedCallback(): void {
    this.suggestionListeners.forEach((dispose) => dispose());
    this.suggestionListeners = [];
    this.updateHandlers.forEach((handler) => handler.dispose());
    this.updateHandlers = [];
    this.resizeObserver?.disconnect();
    this.resizeObserver = null;
    this.themeObserver?.disconnect();
    this.themeObserver = null;
    this.editor?.dispose();
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
    const hasSummarize = /summarize\s+/i.test(currentQuery);

    // Check if summarize includes bin_auto or bin with any field
    const hasBinFunction = /summarize.*by\s+.*bin(_auto)?\s*\(\s*\w+\s*[,)].*$/i.test(currentQuery);

    let newQuery = '';
    switch (visualizationType) {
      case 'timeseries': // Bar chart
      case 'timeseries_line': // Line chart
        // If query already has the correct format for timeseries, don't change it
        if (hasSummarize && hasBinFunction) {
          console.log(`Query already has bin/bin_auto function, keeping as is: "${currentQuery}"`);
          return;
        }

        if (hasSummarize && !hasBinFunction) {
          // Query has summarize but no bin_auto for timestamp, add bin_auto(timestamp) to the by clause
          newQuery = currentQuery.replace(/(\s*summarize\s+[^|]*?by\s+)([^|]*?)(?=\||$)/i, (match, summarizePrefix, byClause) => {
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
          newQuery = currentQuery.replace(/\|\s*summarize\s+[^|]*?(?=\||$)/i, '');
        } else {
          return; // No summarize to remove
        }
        break;
    }

    this.handleAddQuery(newQuery, true);
  }

  public handleAddQuery(queryFragment: string, replace: boolean = false): void {
    if (!this.editor) return;

    const previouslyFocusedElement = document.activeElement as HTMLElement;
    const hadFocus = this.editor.hasTextFocus();

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

      this.debouncedUpdateQuery(newValue);

      // Update placeholder immediately
      this.updatePlaceholder();
    } finally {
      setTimeout(() => {
        this.editor.focus = originalFocus.bind(this.editor);
        this.editor.trigger = originalTrigger.bind(this.editor);

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
      language: 'aql',
      theme: theme,
      automaticLayout: true,
      minimap: { enabled: false },
      scrollBeyondLastLine: false,
      lineNumbers: 'off',
      roundedSelection: false,
      readOnly: false,
      cursorStyle: 'line',
      fontLigatures: true,
      fontSize: 14,
      'semanticHighlighting.enabled': true,
      quickSuggestions: {
        other: true,
        comments: false,
        strings: false,
      },
      suggestOnTriggerCharacters: true,
      suggest: {
        showIcons: false,
        snippetsPreventQuickSuggestions: false,
        filterGraceful: true,
        showWords: false,
      } as any,
      wordWrap: 'on',
      wrappingStrategy: 'advanced',
      wrappingIndent: 'indent',
      wordWrapOverride1: 'on',
      wordWrapOverride2: 'on',
      glyphMargin: false,
      folding: false,
      padding: { top: 8, bottom: 4 },
      renderLineHighlight: 'none',
      overviewRulerBorder: false,
      overviewRulerLanes: 0,
      hideCursorInOverviewRuler: true,
      scrollbar: {
        vertical: 'hidden',
        horizontal: 'hidden',
        alwaysConsumeMouseWheel: false,
      },
      automaticLayout: true,
      lineDecorationsWidth: 0,
      lineNumbersMinChars: 0,
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
    document.addEventListener('pointerdown', (e: MouseEvent) => {
      clickedOnSuggestion = !!this.querySelector('.suggestions-dropdown')?.contains(e.target as Node);
    });

    const handlers = [
      this.editor.onDidFocusEditorText(() => {
        if (!this.isProgrammaticUpdate) {
          this.showSuggestions = true;
          this.updatePlaceholder();
          setTimeout(() => {
            if (!this.isProgrammaticUpdate) {
              this.editor?.trigger('focus', 'editor.action.triggerSuggest', {});
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
        if (!this.isProgrammaticUpdate) {
          const model = this.editor?.getModel();
          const position = this.editor?.getPosition();
          if (!model || !position) return;

          this.currentQuery = model.getLineContent(position.lineNumber);
          this.showSuggestions = true;
          this.updatePlaceholder();

          this.debouncedUpdateQuery(model.getValue());
          this.debouncedTriggerSuggestions();
          this.requestUpdate(); // Trigger re-render to update dropdown position
        }
      }),

      this.editor.onDidChangeCursorPosition(() => {
        if (this.showSuggestions) {
          // Update dropdown position when cursor changes
          this.requestUpdate();
        }
      }),

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
            this.debouncedUpdateQuery(model.getValue());
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
    requestAnimationFrame(() => {
      const item = this.querySelector(`[data-index="${this.selectedIndex}"]`) as HTMLElement;
      item?.scrollIntoView({ block: 'nearest' });
    });
  }

  private adjustEditorHeight(): void {
    if (!this.editor) return;
    const minHeight = 34; // Minimum height in pixels (single line + padding)
    const height = Math.max(this.editor.getContentHeight(), minHeight);
    this._editorContainer.style.height = `${height}px`;
    this.editor.layout();
  }

  private updatePlaceholder(): void {
    if (!this._placeholderElement || !this.editor) return;
    const model = this.editor.getModel();
    const isEmpty = !model || model.getValue().trim() === '';
    this._placeholderElement.style.display = isEmpty ? 'block' : 'none';
  }

  private updateSuggestions(aqlItems: any[] = [], isContextSpecific: boolean = false): void {
    const position = this.editor?.getPosition();
    const model = this.editor?.getModel();
    let parentPath = '';

    if (position && model) {
      const lineText = model.getLineContent(position.lineNumber).substring(0, position.column - 1);
      const dotMatch =
        lineText.match(/([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\.$/) ||
        lineText.match(/([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\.[a-zA-Z0-9_]*$/);
      if (dotMatch) {
        parentPath = dotMatch[1];
      }
    }

    this.completionItems = aqlItems.slice(0, 20).map((item) => {
      const completionData = item.completion || item;
      return {
        kind: 'completion' as const,
        label: completionData.label || completionData.insertText || 'Unknown',
        insertText: completionData.insertText || completionData.label || '',
        kindCategory: completionData.kind || monaco.languages.CompletionItemKind.Field,
        detail: completionData.detail || completionData.documentation || '',
        score: item.score || 2000,
        isContextSpecific: isContextSpecific,
        parentPath: parentPath || undefined,
      };
    });

    this.selectedIndex = -1;
  }

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
    const searchTerm = query.split('|').pop()?.trim() || '';

    const filterAndSlice = (items: any[], prop: string = 'query') =>
      searchTerm
        ? items
            .filter(
              (item) =>
                (prop === 'query' ? item.query : item.name).toLowerCase().includes(searchTerm) ||
                item.query.toLowerCase().includes(searchTerm)
            )
            .slice(0, 5)
        : items.slice(0, 5);

    return {
      saved: filterAndSlice(this.savedViews, 'name'),
      recent: filterAndSlice(this.recentSearches),
      popular: filterAndSlice(this.popularSearches),
    };
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

      const dotMatch =
        lineText.match(/([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\.$/) ||
        lineText.match(/([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\.[a-zA-Z0-9_]*$/);

      if (dotMatch) {
        const lastDotIndex = lineText.lastIndexOf('.');
        wordStartPos = lastDotIndex + 1;
      } else {
        while (wordStartPos > 0) {
          const c = currentLine.charAt(wordStartPos - 1);
          if (/\s/.test(c) || /[^\w\d_=<>!&|+\-*/%^.:]/.test(c)) break;
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
    setTimeout(() => this.editor?.trigger('keyboard', 'editor.action.triggerSuggest', {}), triggerDelay);
  }

  private setupSuggestions(): void {
    if (!this.editor) return;

    this.editor.addCommand(monaco.KeyCode.Space | monaco.KeyMod.CtrlCmd, () =>
      this.editor?.trigger('keyboard', 'editor.action.triggerSuggest', {})
    );

    const suggestController = this.editor.getContribution('editor.contrib.suggestController') as SuggestController;
    const model = suggestController?.model || suggestController?._model;

    ['onDidSuggest', '_onDidSuggest', 'onDidTrigger'].forEach((event) => {
      const listener = (model as any)?.[event];
      if (listener && typeof listener === 'function') {
        const dispose = listener((e: any) => {
          const items = e?.suggestions || e?.completionModel?.items || e?.items || [];
          if (items.length > 0) {
            this.showSuggestions = true;
            this.updateSuggestions(items, true);
          }
        });
        if (dispose) this.suggestionListeners.push(dispose);
      }
    });
  }

  private getCompletionIcon(kind: number): string {
    const idx = Math.min(Math.max(0, kind % 10), this.KIND_ICONS.length - 1);
    return this.KIND_ICONS[idx];
  }

  private getSuggestionUIData(item: SuggestionItem): {
    icon: string;
    primaryText: string | TemplateResult;
    secondaryText: string | TemplateResult | undefined;
  } {
    const uiData: Record<SuggestionKind, () => ReturnType<typeof this.getSuggestionUIData>> = {
      recentSearch: () => ({
        icon: '⏱️',
        primaryText: item.query,
        secondaryText: (item as RecentSearch).timestamp,
      }),
      savedView: () => ({
        icon: '⭐',
        primaryText: (item as SavedView).name,
        secondaryText: html`
          <span class="truncate text-textWeak mr-2" title="${item.query}">${item.query}</span>
          ${(item as SavedView).owner
            ? html`<span class="flex-shrink-0 rounded-full w-6 h-6 flex items-center justify-center text-xs"
                >${(item as SavedView).owner!.icon || ''}</span
              >`
            : ''}
        `,
      }),
      popularSearch: () => ({
        icon: '🔍',
        primaryText: item.query,
        secondaryText: (item as PopularSearch).description,
      }),
      completion: () => {
        const completion = item as CompletionItem;
        if (completion.parentPath) {
          return {
            icon: this.getCompletionIcon(completion.kindCategory),
            primaryText: html` <span class="text-textWeak">${completion.parentPath}.</span><span>${completion.label}</span> `,
            secondaryText: completion.detail,
          };
        }
        return {
          icon: this.getCompletionIcon(completion.kindCategory),
          primaryText: completion.label,
          secondaryText: completion.detail,
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
        @mouseover=${() => (this.selectedIndex = itemIndex)}
        data-index="${itemIndex}"
      >
        <div class="flex items-center gap-2 overflow-hidden">
          <span class="text-base">${icon}</span>
          <span class="truncate ${isSelected ? 'font-medium text-textBrand' : ''}" title="${displayTextForTooltip}">${primaryText}</span>
        </div>
        ${secondaryText ? html`<span class="text-xs text-textWeak ml-2 flex-shrink-0 flex items-center">${secondaryText}</span>` : ''}
      </div>
    `;
  }

  private renderSuggestionDropdown(): TemplateResult {
    if (!this.showSuggestions || !this.editor) return html``;

    const matches = this.getMatches();
    const groups = {
      completion: this.completionItems,
      saved: matches.saved,
      recent: matches.recent,
      popular: matches.popular,
    };

    const groupTitles = {
      completion: null,
      saved: 'Saved Views',
      recent: 'Recent Searches',
      popular: 'Popular Searches',
    };

    const orderedCategories: Array<keyof typeof groups> = ['completion', 'saved', 'recent', 'popular'];
    const sections = orderedCategories
      .filter((category) => groups[category]?.length > 0)
      .map((category) => ({
        items: groups[category],
        title: groupTitles[category],
      }));

    const position = this.editor.getPosition();
    const coords = position ? this.editor.getScrolledVisiblePosition(position) : null;
    let positionStyle = '';

    if (coords) {
      positionStyle = `top: ${coords.top + 24}px; left: 10px; right: 10px;`;
    }

    if (!sections.length) {
      return html`
        <div
          class="mt-1 suggestions-dropdown absolute bg-bgRaised border border-strokeMedium shadow-lg z-10 overflow-y-auto rounded-md text-xs"
          style="${positionStyle}"
        >
          <div class="px-4 py-2 text-sm text-textWeak italic">No suggestions found</div>
        </div>
      `;
    }

    let currentIndex = 0;
    const keyboardHelp = html`
      <div class="sticky bottom-0 bg-bgRaised z-50 border-t border-strokeMedium px-4 py-2 text-xs text-textWeak">
        <span class="mr-2">
          <kbd class="px-1 py-0.5 bg-fillWeak border border-strokeStrong rounded text-xs">↑</kbd>
          <kbd class="px-1 py-0.5 bg-fillWeak border border-strokeStrong rounded text-xs">↓</kbd>
          <kbd class="px-1 py-0.5 bg-fillWeak border border-strokeStrong rounded text-xs">Tab</kbd> to navigate
        </span>
        <span class="mr-2">• <kbd class="px-1 py-0.5 bg-fillWeak border border-strokeStrong rounded text-xs">Enter</kbd> to select</span>
        <span>• <kbd class="px-1 py-0.5 bg-fillWeak border border-strokeStrong rounded text-xs">Esc</kbd> to close</span>
      </div>
    `;

    return html`
      <div
        class="mt-1 suggestions-dropdown absolute bg-bgRaised border border-strokeMedium shadow-lg z-50 max-h-[80dvh] overflow-y-auto rounded-md text-xs flex flex-col"
        style="${positionStyle}"
      >
        <div class="overflow-y-auto flex-grow min-h-0">
          ${sections.map(
            (section) => html`
              ${section.title
                ? html`<div class="text-xs font-semibold text-textWeak px-4 py-2 uppercase border-t border-b border-strokeWeak bg-fillWeaker">
                    ${section.title}
                  </div>`
                : ''}
              ${section.items.map((item) => this.renderSuggestionItem(item, currentIndex++))}
            `
          )}
        </div>
        ${keyboardHelp}
      </div>
    `;
  }

  render(): TemplateResult {
    return html`
      <style>
        .monaco-editor .suggest-widget {
          display: none !important;
          visibility: hidden !important;
        }
        .monaco-editor {
          border-radius: 0;
          outline: none !important;
          border: none !important;
        }
        .monaco-editor .overflow-guard {
          border: none !important;
          outline: none !important;
        }
        /* Ensure editor doesn't overflow container */
        #editor-container {
          max-width: 100%;
          overflow: hidden;
        }
      </style>
      <div
        class="relative w-full h-full pl-2 flex border rounded-md border-strokeStrong focus-within:border-strokeBrand-strong focus:outline-2 "
      >
        <div class="relative overflow-x-hidden w-full flex-1">
          <div id="editor-container" class="w-full"></div>
          <div
            class="placeholder-overlay absolute top-0 left-0 right-0 bottom-0 pointer-events-auto z-[1] text-textWeak f/nont-mono text-sm leading-[18px] pt-2 pl-0 hidden cursor-text"
            @pointerdown=${() => this.editor?.focus()}
          >
            Filter logs and events. Press <span class="kbd">/</span> to search or <span class="kbd">?</span>
            to ask in Natural language.
          </div>
        </div>
        <div class="p-1">
          <label
            class="px-3 py-0.5 inline-flex gap-2 items-center cursor-pointer border border-strokeBrand-strong text-textBrand hover:border-strokeBrand-weak rounded-sm group-has-[.ai-search:checked]/fltr:hidden"
            data-tippy-content="Write queries in natural language with APItoolkit AI"
            for="ai-search-chkbox"
          >
            <svg class="inline-block icon h-4 w-4 text-iconBrand ">
              <use href="/public/assets/svgs/fa-sprites/regular.svg#sparkles"></use>
            </svg>
            ask
          </label>
        </div>
        ${this.renderSuggestionDropdown()}
      </div>
    `;
  }
}

// Add a convenience method to get field suggestions directly from schemaManager
schemaManager.getFieldSuggestions = async (schema?: string): Promise<{ name: string; type: string; description?: string }[]> => {
  const schemaToUse = schema || schemaManager.getDefaultSchema();
  const fields = await schemaManager.resolveNested(schemaToUse, '');
  return fields.map((field) => ({
    name: field.name,
    type: field.type,
    description: field.examples?.join(', '),
  }));
};

// Expose schemaManager globally for external configuration
(window as any).schemaManager = schemaManager;

export { monaco, schemaManager, type SchemaData };
