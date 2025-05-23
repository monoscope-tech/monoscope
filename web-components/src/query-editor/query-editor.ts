import { LitElement, html, css, TemplateResult } from 'lit';
import { customElement, state, query } from 'lit/decorators.js';
import 'monaco-editor/esm/vs/editor/contrib/suggest/browser/suggestController.js';
import * as monaco from 'monaco-editor/esm/vs/editor/editor.api';
import { debounce } from 'lodash';

// Make monaco available globally for tests
globalThis.monaco = monaco;

// Types
type SuggestionKind = 'completion' | 'recentSearch' | 'savedView' | 'popularSearch';
type QueryLibType = 'history' | 'saved';
type FieldType = 'string' | 'number' | 'boolean' | 'duration' | 'array' | 'object';

interface BaseItem {
  readonly kind: SuggestionKind;
}

interface CompletionItem extends BaseItem {
  readonly kind: 'completion';
  label: string;
  insertText: string;
  kindCategory: number;
  detail?: string;
  score?: number;
  originalItem?: any;
  isContextSpecific?: boolean;
  // For nested field handling
  parentPath?: string;
  partialField?: string;
}

interface RecentSearch extends BaseItem {
  readonly kind: 'recentSearch';
  query: string;
  timestamp: string;
}

interface SavedView extends BaseItem {
  readonly kind: 'savedView';
  name: string;
  query: string;
  owner?: {
    name: string;
    icon?: string;
  };
}

interface PopularSearch extends BaseItem {
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

// Schema manager types
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
  model?: {
    onDidSuggest?: (callback: (event: any) => void) => void;
    _onDidSuggest?: (callback: (event: any) => void) => void;
    onDidTrigger?: (callback: (event: any) => void) => void;
  };
  _model?: {
    onDidSuggest?: (callback: (event: any) => void) => void;
    _onDidSuggest?: (callback: (event: any) => void) => void;
    onDidTrigger?: (callback: (event: any) => void) => void;
  };
  widget?: any;
}

// Schema Manager: handles multiple schemas with default and dynamic resolution
interface SchemaField {
  name: string;
  type: string;
  examples?: string[];
  fields?: Record<string, any>;
}

const schemaManager = (() => {
  let schemas: string[] = ['spans', 'metrics'];
  let defaultSchema = 'spans';
  let schemaData: Record<string, SchemaData> = {}; // Store schema data per schema name

  // Default nested resolver implementation that uses stored schema data
  let nestedResolver: (schema: string, prefix: string) => Promise<SchemaField[]> = async (schema: string, prefix: string) => {
    const currentSchema = schemaData[schema] || schemaData[defaultSchema];
    if (!currentSchema?.fields) return [];

    if (prefix) {
      // Handle nested field access (e.g., "attributes.http" -> return fields under attributes.http)
      const pathParts = prefix.split('.');
      let current = currentSchema.fields;

      for (const part of pathParts) {
        if (current[part]?.fields) {
          current = current[part].fields!;
        } else {
          return [];
        }
      }

      return Object.entries(current).map(([name, info]) => ({
        name,
        type: info.type || 'string',
        examples: info.examples || [],
        fields: info.fields,
      }));
    }

    // Return root fields
    return Object.entries(currentSchema.fields).map(([name, info]) => ({
      name,
      type: info.type || 'string',
      examples: info.examples || [],
      fields: info.fields,
    }));
  };

  // Default value resolver implementation that uses stored schema data
  let valueResolver: (schema: string, field: string) => Promise<string[]> = async (schema: string, field: string) => {
    const currentSchema = schemaData[schema] || schemaData[defaultSchema];
    if (!currentSchema?.fields) return [];

    // Direct field lookup
    if (currentSchema.fields[field]?.examples) {
      return currentSchema.fields[field].examples!;
    }

    // Handle nested field access
    if (field.includes('.')) {
      const pathParts = field.split('.');
      let current = currentSchema.fields;

      for (const part of pathParts) {
        if (current[part]) {
          if (current[part].fields) {
            current = current[part].fields!;
          } else if (current[part].examples) {
            return current[part].examples!;
          }
        } else {
          break;
        }
      }
    }

    return [];
  };

  return {
    setSchemas: (list: string[]) => {
      schemas = list;
    },
    setDefaultSchema: (schema: string) => {
      if (schemas.includes(schema)) defaultSchema = schema;
    },
    setSchemaData: (schema: string, data: SchemaData) => {
      schemaData[schema] = data;
    },
    getSchemaData: (schema: string) => schemaData[schema],
    setNestedResolver: (fn: typeof nestedResolver) => {
      nestedResolver = fn;
    },
    setValueResolver: (fn: typeof valueResolver) => {
      valueResolver = fn;
    },
    getSchemas: () => schemas,
    getDefaultSchema: () => defaultSchema,
    resolveNested: (schema: string, prefix: string) => nestedResolver(schema, prefix),
    resolveValues: (schema: string, field: string) => valueResolver(schema, field),

    // Compatibility with the old API
    getRootFields: async (): Promise<{ name: string; info: FieldInfo }[]> => {
      const fields = await nestedResolver(defaultSchema, '');
      return fields.map((f) => ({
        name: f.name,
        info: {
          type: f.type as FieldType,
          examples: f.examples,
          fields: f.fields,
        },
      }));
    },
    resolveNestedFields: async (path: string[]): Promise<{ name: string; info: FieldInfo }[]> => {
      const fields = await nestedResolver(defaultSchema, path.join('.'));
      return fields.map((f) => ({
        name: f.name,
        info: {
          type: f.type as FieldType,
          examples: f.examples,
          fields: f.fields,
        },
      }));
    },
    setSchema: (cfg: Partial<Schema>) => {
      // For backward compatibility
      console.log('Using legacy setSchema method, consider updating to new API');
    },
    setDynamicResolver: (fn: (path: string[]) => Promise<any[]>) => {
      // For backward compatibility
      nestedResolver = async (schema: string, prefix: string) => {
        const path = prefix ? prefix.split('.') : [];
        const fields = await fn(path);
        return fields.map((f) => ({
          name: f.name,
          type: f.info.type,
          examples: f.info.examples,
          fields: f.info.fields,
        }));
      };
    },
  };
})();

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
  keywords: [
    'spans',
    'metrics',
    'stats',
    'timechart',
    'by',
    'as',
    'limit',
    'exists',
    '!exists',
    'and',
    'or',
    'not',
    'count',
    'sum',
    'avg',
    'min',
    'max',
    'median',
    'stdev',
    'range',
    'p50',
    'p75',
    'p90',
    'p95',
    'p99',
    'p100',
  ],
  operators: ['==', '!=', '>', '<', '>=', '<=', '=~', '|'],
  tokenizer: {
    root: [
      [/\[[0-9]+(?:s|m|h|d|w)\]/, 'number.timespan'],
      [/[0-9]+(?:ns|µs|us|ms|s|m|h|d|w)/, 'number.duration'],
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

// Define a custom theme with transparent background
monaco.editor.defineTheme('transparent-theme', {
  base: 'vs', // or 'vs-dark' for dark theme
  inherit: true,
  rules: [],
  colors: {
    'editor.background': '#00000000', // Transparent background
    'editor.lineHighlightBackground': '#00000000', // Transparent line highlight
    'editorGutter.background': '#00000000', // Transparent gutter
  },
});

// Register AQL language
monaco.languages.register({ id: 'aql' });
monaco.languages.setMonarchTokensProvider('aql', language);
monaco.languages.setLanguageConfiguration('aql', conf);

// Completion provider using schemaManager
console.log('Registering completion provider for aql language');
monaco.languages.registerCompletionItemProvider('aql', {
  triggerCharacters: [' ', '|', '.', '[', ',', '"'],
  provideCompletionItems: async (model, position) => {
    console.log('Completion provider triggered at position:', position);
    const text = model.getValueInRange({
      startLineNumber: 1,
      startColumn: 1,
      endLineNumber: position.lineNumber,
      endColumn: position.column,
    });

    // Get the current line and cursor position for context analysis
    const currentLine = model.getLineContent(position.lineNumber);

    const segments = text.split(/\|/).map((s) => s.trim());
    const last = segments[segments.length - 1];
    console.log('segments:', segments, 'last:', JSON.stringify(last));

    const suggestions: monaco.languages.CompletionItem[] = [];
    const tables = schemaManager.getSchemas();
    const firstToken = text.trim().split(/\s+/)[0].toLowerCase();
    const currentSchema = tables.includes(firstToken) ? firstToken : schemaManager.getDefaultSchema();

    // Helper to create range for completion items
    const createRange = (startCol: number = position.column, endCol: number = position.column) => ({
      startLineNumber: position.lineNumber,
      startColumn: startCol,
      endLineNumber: position.lineNumber,
      endColumn: endCol,
    });

    // PRIORITY: Check for nested fields after dot FIRST
    const lineText = currentLine.substring(0, position.column - 1);
    console.log('lineText for analysis:', JSON.stringify(lineText));

    const dotMatch =
      lineText.match(/([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\.$/) ||
      lineText.match(/([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\.[a-zA-Z0-9_]*$/);

    console.log('dotMatch result:', dotMatch);
    if (dotMatch) {
      const fieldPrefix = dotMatch[1];
      const nested = await schemaManager.resolveNested(currentSchema, fieldPrefix);

      nested.forEach((n) =>
        suggestions.push({
          label: n.name,
          kind: monaco.languages.CompletionItemKind.Field,
          insertText: n.type === 'object' ? n.name + '.' : n.name + ' ',
          range: createRange(),
          // Add additional properties for nested field context
          detail: n.type,
          documentation: n.examples?.join(', '),
        })
      );

      // Always return for dot notation, even if empty
      // This prevents fallback to other suggestion types
      return { suggestions };
    }

    // PRIORITY: Check for operator pattern - show value suggestions (MOVED UP)
    const operatorMatch = lineText.match(/([\w\.]+)\s*(==|!=|>=|<=|>|<|=~)\s*$/);
    if (operatorMatch) {
      const fieldName = operatorMatch[1];
      const values = await schemaManager.resolveValues(currentSchema, fieldName);
      values.forEach((v) =>
        suggestions.push({
          label: String(v),
          kind: monaco.languages.CompletionItemKind.Value,
          insertText: typeof v === 'string' ? `"${v}" ` : String(v) + ' ',
          range: createRange(),
        })
      );
      return { suggestions };
    }

    // PRIORITY: Check for complete field-operator-value pattern - suggest logical/pipeline operators
    const afterQuotedString = /".*"\s*$/.test(lineText);
    const afterNumber = /\d+\s*$/.test(lineText);
    if (afterQuotedString || afterNumber) {
      ['and', 'or', '|'].forEach((op) =>
        suggestions.push({
          label: op,
          kind: monaco.languages.CompletionItemKind.Operator,
          insertText: op + ' ',
          range: createRange(),
        })
      );
      return { suggestions };
    }

    // Check for logical operators followed by space - suggest fields
    const logicalOperatorMatch = lineText.match(/\b(and|or)\s+$/i);
    if (logicalOperatorMatch) {
      const fields = await schemaManager.resolveNested(currentSchema, '');
      fields.forEach((f) =>
        suggestions.push({
          label: f.name,
          kind: monaco.languages.CompletionItemKind.Field,
          detail: f.type,
          documentation: f.examples?.join(', '),
          insertText: f.type === 'object' ? f.name + '.' : f.name + ' ',
          range: createRange(),
        })
      );
      return { suggestions };
    }

    // Check for field name followed by space - suggest only operators
    const fieldSpaceMatch = lineText.match(/([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\s+$/);
    if (fieldSpaceMatch && !logicalOperatorMatch) {
      ['==', '!=', '>', '<', '>=', '<=', '=~', 'and', 'or', 'not', 'exists'].forEach((op) =>
        suggestions.push({
          label: op,
          kind: monaco.languages.CompletionItemKind.Operator,
          insertText: op + ' ',
          range: createRange(),
        })
      );
      return { suggestions };
    }

    // 1. Empty or start: suggest schemas + default fields
    if (segments.length === 1 && (last === '' || !tables.some((t) => last.toLowerCase().startsWith(t)))) {
      // Always suggest available schemas that match current input
      tables
        .filter((t) => last === '' || t.toLowerCase().startsWith(last.toLowerCase().trim()))
        .forEach((t) =>
          suggestions.push({
            label: t,
            kind: monaco.languages.CompletionItemKind.Module,
            insertText: t + ' ',
            range: createRange(),
          })
        );

      // If no schema prefix is being typed, also suggest fields from the default schema
      if (last === '' || !tables.some((t) => last.toLowerCase().trim().startsWith(t))) {
        const defaultSchema = schemaManager.getDefaultSchema();
        const fields = await schemaManager.resolveNested(defaultSchema, '');
        fields.forEach((f) =>
          suggestions.push({
            label: f.name,
            kind: monaco.languages.CompletionItemKind.Field,
            detail: f.type,
            documentation: f.examples?.join(', '),
            insertText: f.type === 'object' ? f.name + '.' : f.name + ' ',
            range: createRange(),
          })
        );
      }
      return { suggestions };
    }

    // 2. Schema name completion
    if (segments.length === 1 && tables.some((t) => t.startsWith(last.toLowerCase()))) {
      tables
        .filter((t) => t.startsWith(last.toLowerCase()))
        .forEach((t) =>
          suggestions.push({
            label: t,
            kind: monaco.languages.CompletionItemKind.Module,
            insertText: t + ' ',
            range: createRange(),
          })
        );
      return { suggestions };
    }

    // 3. After schema: LIMIT, stats, timechart, or filters
    if (segments.length === 1 && tables.includes(last.toLowerCase())) {
      ['limit', 'stats', 'timechart'].forEach((k) =>
        suggestions.push({
          label: k,
          kind: monaco.languages.CompletionItemKind.Keyword,
          insertText: k + ' ',
          range: createRange(),
        })
      );
      const fields = await schemaManager.resolveNested(currentSchema, '');
      fields.forEach((f) =>
        suggestions.push({
          label: f.name,
          kind: monaco.languages.CompletionItemKind.Field,
          insertText: f.type === 'object' ? f.name + '.' : f.name + ' ',
          range: createRange(),
        })
      );
      return { suggestions };
    }

    // 5. Search segment (no stats/timechart)
    if (!/stats|timechart/i.test(last)) {
      ['==', '!=', '>', '<', '>=', '<=', '=~', 'and', 'or', 'not', 'exists'].forEach((op) =>
        suggestions.push({
          label: op,
          kind: monaco.languages.CompletionItemKind.Operator,
          insertText: op + ' ',
          range: createRange(),
        })
      );

      // For field suggestions, use the determined current schema or default
      const fields = await schemaManager.resolveNested(currentSchema, '');
      fields.forEach((f) =>
        suggestions.push({
          label: f.name,
          kind: monaco.languages.CompletionItemKind.Field,
          detail: f.type,
          documentation: f.examples?.join(', '),
          insertText: f.type === 'object' ? f.name + '.' : f.name + ' ',
          range: createRange(),
        })
      );
      return { suggestions };
    }

    // 6. Stats/timechart segment
    if (/stats\s|timechart\s/i.test(last)) {
      ['count', 'sum', 'avg', 'min', 'max', 'median', 'stdev', 'range', 'p50', 'p75', 'p90', 'p95', 'p99', 'p100'].forEach((fn) =>
        suggestions.push({
          label: fn,
          kind: monaco.languages.CompletionItemKind.Function,
          insertText: fn + '(',
          range: createRange(),
        })
      );
      if (/\bby\s*$/i.test(last)) {
        const fields = await schemaManager.resolveNested(currentSchema, '');
        fields.forEach((f) =>
          suggestions.push({
            label: f.name,
            kind: monaco.languages.CompletionItemKind.Field,
            insertText: f.type === 'object' ? f.name + '.' : f.name + ' ',
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

    // Removed duplicate dot notation check - now handled at the top

    return { suggestions };
  },
});

@customElement('query-editor')
export class QueryEditorComponent extends LitElement {
  // Skip shadow DOM
  protected createRenderRoot = () => this;

  // DOM refs
  @query('#editor-container') private _editorContainer!: HTMLElement;
  @query('.placeholder-overlay') private _placeholderElement!: HTMLElement;

  // Component state
  @state() private completionItems: CompletionItem[] = [];
  @state() public recentSearches: RecentSearch[] = [];
  @state() public savedViews: SavedView[] = [];
  @state() public popularSearches: PopularSearch[] = [];
  @state() private showSuggestions = false;
  @state() private currentQuery = '';
  @state() private selectedIndex = -1;
  @state() private defaultValue = '';

  // Internal state
  private editor: monaco.editor.IStandaloneCodeEditor | null = null;
  private suggestionListeners: (() => void)[] = [];

  // Icons for different suggestion types: document, function, operator, variable, keyword, etc.
  private readonly KIND_ICONS = ['📄', '🔢', '🔍', '#', '📊', '📋', '#', '🔢', '✅', '❓'];

  // Method to set popular searches externally
  public setPopularSearches(items: { query: string; description?: string }[]): void {
    if (!items?.length) return;

    this.popularSearches = items.map((item) => ({
      kind: 'popularSearch' as const,
      query: item.query,
      description: item.description || '',
    }));
  }

  // Simple debounced handlers
  private debouncedTriggerSuggestions = debounce(() => this.editor?.trigger('auto', 'editor.action.triggerSuggest', {}), 50);
  private debouncedUpdateQuery = debounce((queryValue: string) => {
    // Emit update-query event
    this.dispatchEvent(
      new CustomEvent('update-query', {
        detail: { value: queryValue },
        bubbles: true,
      })
    );

    // Update URL query parameter
    const url = new URL(window.location.href);
    if (queryValue.trim()) {
      url.searchParams.set('query', queryValue);
    } else {
      url.searchParams.delete('query');
    }
    window.history.replaceState({}, '', url.toString());
  }, 300);

  // Helper to get all available suggestions
  private get serviceSuggestions(): SuggestionItem[] {
    // Combine all suggestion types in the order: Monaco completions, saved, recent, popular
    return [...this.completionItems, ...this.savedViews, ...this.recentSearches, ...this.popularSearches];
  }

  // Lifecycle methods
  async firstUpdated(): Promise<void> {
    if (!this._editorContainer) return;

    // Get default value from attributes
    this.defaultValue = this.getAttribute('default-value') || '';

    this.createMonacoEditor();
    this.setupSuggestions();

    // Event listeners
    this.addEventListener('keydown', (e: KeyboardEvent) => {
      if (e.key === 'Escape' && this.showSuggestions) {
        this.showSuggestions = false;
      }
    });

    window.addEventListener('resize', () => this.editor?.layout());
  }

  disconnectedCallback(): void {
    this.suggestionListeners.forEach((dispose) => dispose());
    this.suggestionListeners = [];
    this.editor?.dispose();
    super.disconnectedCallback();
  }

  // Public methods
  public setSchema(schema: Partial<Schema>): void {
    schemaManager.setSchema(schema);
  }

  public setDynamicResolver(fn: (path: string[]) => Promise<{ name: string; info: FieldInfo }[]>): void {
    schemaManager.setDynamicResolver(fn);
  }

  public setQueryLibrary(items: QueryLibItem[]): void {
    if (!items?.length) return;

    const stripPrefix = (text: string) => text;

    this.recentSearches = items
      .filter((item) => item.queryType === 'history')
      .map((item) => ({
        kind: 'recentSearch' as const,
        query: stripPrefix(item.queryText),
        timestamp: this.formatRelativeTime(new Date(item.updatedAt)),
      }));

    this.savedViews = items
      .filter((item) => item.queryType === 'saved')
      .map((item) => ({
        kind: 'savedView' as const,
        name: item.title || `Query ${item.id.substring(0, 8)}`,
        query: stripPrefix(item.queryText),
        owner: { name: item.byMe ? 'You' : 'Other', icon: item.byMe ? '👤' : '👥' },
      }));
  }

  // Public API method for adding query fragments
  public handleAddQuery(queryFragment: string): void {
    if (!this.editor) return;

    const currentValue = this.editor.getValue().trim();
    let newValue: string;
    if (currentValue) {
      newValue = `${currentValue} and ${queryFragment}`;
    } else {
      newValue = queryFragment;
    }

    this.editor.setValue(newValue);

    // Move cursor to the end
    const model = this.editor.getModel();
    if (model) {
      const lastLine = model.getLineCount();
      const lastColumn = model.getLineMaxColumn(lastLine);
      this.editor.setPosition({ lineNumber: lastLine, column: lastColumn });
    }

    this.debouncedUpdateQuery(newValue);
    this.showSuggestions = false;
    this.selectedIndex = -1;
  }

  // Private methods
  private formatRelativeTime(date: Date): string {
    const diffSec = Math.floor((Date.now() - date.getTime()) / 1000);

    if (diffSec < 60) return 'just now';
    if (diffSec < 3600) return `${Math.floor(diffSec / 60)} minutes ago`;
    if (diffSec < 86400) return `${Math.floor(diffSec / 3600)} hours ago`;
    return `${Math.floor(diffSec / 86400)} days ago`;
  }

  private createMonacoEditor(): void {
    this.editor = monaco.editor.create(this._editorContainer, {
      value: this.defaultValue,
      language: 'aql',
      theme: 'transparent-theme',
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
        showWords: false, // Disable word-based completion
      } as any,
      wordWrap: 'on',
      wrappingStrategy: 'advanced',
      wrappingIndent: 'indent',
      glyphMargin: false,
      folding: false,
      padding: { top: 8, bottom: 4 },
      // Remove borders and make background transparent
      renderLineHighlight: 'none',
      overviewRulerBorder: false,
      overviewRulerLanes: 0,
      hideCursorInOverviewRuler: true,
      scrollbar: {
        vertical: 'hidden',
        horizontal: 'hidden',
      },
    });

    this.setupEditorEvents();
    this.adjustEditorHeight();

    // Initial placeholder update
    setTimeout(() => this.updatePlaceholder(), 100);
  }

  private setupEditorEvents(): void {
    if (!this.editor) return;

    // Add Cmd+Enter handler for new lines
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

    // Track suggestion panel clicks
    let clickedOnSuggestion = false;
    document.addEventListener('mousedown', (e: MouseEvent) => {
      clickedOnSuggestion = !!this.querySelector('.suggestions-dropdown')?.contains(e.target as Node);
    });

    // Set up focus and blur events
    this.editor.onDidFocusEditorText(() => {
      this.showSuggestions = true;
      this.updatePlaceholder();
      // Trigger Monaco suggestions on focus
      setTimeout(() => this.editor?.trigger('focus', 'editor.action.triggerSuggest', {}), 10);
    });
    this.editor.onMouseDown(() => setTimeout(() => this.editor?.hasTextFocus() && (this.showSuggestions = true), 10));
    this.editor.onDidBlurEditorText(() => {
      setTimeout(() => {
        if (!this.editor?.hasTextFocus() && !clickedOnSuggestion) {
          this.showSuggestions = false;
        }
        this.updatePlaceholder();
      }, 300);
    });

    this.editor.onKeyDown(this.handleKeyboardNavigation);

    // Update suggestions when typing
    this.editor.onDidChangeModelContent(() => {
      const model = this.editor?.getModel();
      const position = this.editor?.getPosition();
      if (!model || !position) return;

      this.currentQuery = model.getLineContent(position.lineNumber);
      this.showSuggestions = true;
      this.updatePlaceholder();

      // Debounced update of query event and URL
      this.debouncedUpdateQuery(model.getValue());

      // Trigger suggestions on every input change
      this.debouncedTriggerSuggestions();
    });

    // Auto-adjust editor height
    this.editor.onDidContentSizeChange((e) => {
      // this._editorContainer.style.height = `${e.contentHeight}px`;
      this.editor?.layout();
    });
  }

  private handleKeyboardNavigation = (e: monaco.IKeyboardEvent): void => {
    if (!this.showSuggestions) return;

    const totalItems = this.getTotalVisibleSuggestions();
    if (totalItems === 0) return;

    // Prevent default and stop propagation for navigation keys
    const preventAndStop = () => {
      e.preventDefault();
      e.stopPropagation();
    };

    const key = e.browserEvent.key;

    // Handle navigation
    if (key === 'ArrowDown' || (key === 'Tab' && !e.browserEvent.shiftKey)) {
      preventAndStop();
      this.selectedIndex = (this.selectedIndex + 1) % totalItems;
      this.scrollSelectedIntoView();
    } else if (key === 'ArrowUp' || (key === 'Tab' && e.browserEvent.shiftKey)) {
      preventAndStop();
      this.selectedIndex = this.selectedIndex <= 0 ? totalItems - 1 : this.selectedIndex - 1;
      this.scrollSelectedIntoView();
    } else if (key === 'Enter') {
      // Only select suggestion if we have actively selected one (selectedIndex >= 0)
      if (this.selectedIndex >= 0) {
        preventAndStop();
        const item = this.getItemAtIndex(this.selectedIndex);
        if (item) this.insertCompletion(item);
      } else {
        // No suggestion selected - close dropdown and trigger update-query event
        preventAndStop();
        this.showSuggestions = false;
        this.selectedIndex = -1;

        // Use existing debounced function to trigger update-query and URL update
        const model = this.editor?.getModel();
        if (model) {
          this.debouncedUpdateQuery(model.getValue());
        }
      }
    } else if (key === 'Escape') {
      preventAndStop();
      this.showSuggestions = false;
      this.selectedIndex = -1;
    }
  };

  private scrollSelectedIntoView(): void {
    // Use requestAnimationFrame to ensure DOM has updated
    requestAnimationFrame(() => {
      const item = this.querySelector(`[data-index="${this.selectedIndex}"]`) as HTMLElement;
      item?.scrollIntoView({ block: 'nearest' });
    });
  }

  private adjustEditorHeight(): void {
    if (!this.editor) return;
    // const initialHeight = this.editor.getContentHeight();
    // this._editorContainer.style.height = `${initialHeight}px`;
    this.editor.layout();
  }

  private updatePlaceholder(): void {
    if (!this._placeholderElement || !this.editor) return;

    const model = this.editor.getModel();
    const isEmpty = !model || model.getValue().trim() === '';
    this._placeholderElement.style.display = isEmpty ? 'block' : 'none';
  }

  private updateSuggestions(aqlItems: any[] = [], isContextSpecific: boolean = false): void {
    // Debug: log the first few items to understand structure
    console.log('updateSuggestions received:', aqlItems.slice(0, 3));

    // Get current context to determine if we're in a nested field context
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

    // Process items (limit to 20)
    this.completionItems = aqlItems.slice(0, 20).map((item, index) => {
      // Monaco completion items might have nested structure under 'completion' property
      const completionData = item.completion || item;

      return {
        kind: 'completion',
        label: completionData.label || completionData.insertText || 'Unknown',
        insertText: completionData.insertText || completionData.label || '',
        kindCategory: completionData.kind || monaco.languages.CompletionItemKind.Field,
        detail: completionData.detail || completionData.documentation || '',
        score: item.score || 2000,
        isContextSpecific: isContextSpecific,
        // Add parent path if we're in a nested field context
        parentPath: parentPath || undefined,
      };
    });

    this.selectedIndex = -1;
  }

  private getTotalVisibleSuggestions(): number {
    const matches = this.getMatches();
    // Total across all categories
    return this.completionItems.length + matches.recent.length + matches.saved.length + matches.popular.length;
  }

  private getItemAtIndex(index: number): SuggestionItem | null {
    const matches = this.getMatches();

    // Get all items from all categories
    const groups = {
      completion: this.completionItems,
      recent: matches.recent,
      saved: matches.saved,
      popular: matches.popular,
    };

    // Convert to a flat array of valid groups
    const allItems = Object.values(groups)
      .filter((items) => items && items.length > 0)
      .flat();

    return allItems[index] || null;
  }

  private getMatches() {
    // Return cached results if query hasn't changed
    const query = this.currentQuery?.toLowerCase() || '';
    const lastPart = query.split('|').pop()?.trim() || '';
    const searchTerm = lastPart;

    // Filter all suggestion types with the same logic
    return {
      saved:
        this.savedViews.length > 0
          ? searchTerm
            ? this.savedViews
                .filter((view) => view.name.toLowerCase().includes(searchTerm) || view.query.toLowerCase().includes(searchTerm))
                .slice(0, 5) // Show only 5 most relevant saved views
            : this.savedViews.slice(0, 5)
          : [],
      recent:
        this.recentSearches.length > 0
          ? searchTerm
            ? this.recentSearches.filter((item) => item.query.toLowerCase().includes(searchTerm)).slice(0, 5) // Show only 5 most relevant recent searches
            : this.recentSearches.slice(0, 5) // Show only 5 most recent searches
          : [],
      popular:
        this.popularSearches.length > 0
          ? searchTerm
            ? this.popularSearches.filter((item) => item.query.toLowerCase().includes(searchTerm)).slice(0, 5) // Show only 5 most relevant popular searches
            : this.popularSearches.slice(0, 5) // Show only 5 most popular searches
          : [],
    };
  }

  private handleSuggestionClick(item: SuggestionItem, e: MouseEvent): void {
    e.preventDefault();
    e.stopPropagation();

    console.log(item);

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

    // Determine text and range based on item type
    if (item.kind === 'recentSearch' || item.kind === 'savedView' || item.kind === 'popularSearch') {
      // Replace entire line with query, but keep the prefix
      const currentLine = model.getLineContent(position.lineNumber);
      textToInsert = item.query;

      // Replace the entire line
      replaceRange = {
        startLineNumber: position.lineNumber,
        startColumn: 1,
        endLineNumber: position.lineNumber,
        endColumn: model.getLineMaxColumn(position.lineNumber),
      };
    } else {
      // It's a CompletionItem
      textToInsert = item.insertText || item.label;

      // Get current token for replacement
      const currentLine = model.getLineContent(position.lineNumber);
      const lineText = currentLine.substring(0, position.column - 1);
      const wordEndPos = position.column - 1;
      let wordStartPos = wordEndPos;

      // Check if we're in dot notation context (e.g., "attributes.client" or "attributes.")
      const dotMatch =
        lineText.match(/([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\.$/) ||
        lineText.match(/([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\.[a-zA-Z0-9_]*$/);

      if (dotMatch) {
        // For dot notation, only replace after the last dot
        const lastDotIndex = lineText.lastIndexOf('.');
        wordStartPos = lastDotIndex + 1;
      } else {
        // Normal token replacement - find start of current token by going backwards
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

    // Handle function suggestions that end with '()'
    const shouldMoveCursor = textToInsert.endsWith('(') && !textToInsert.includes(' ');
    if (shouldMoveCursor) {
      textToInsert = textToInsert;
    }

    // Execute the edit
    try {
      this.editor.executeEdits('completion', [{ range: replaceRange, text: textToInsert }]);
    } catch (e) {
      console.error('Error executing edit:', e);
      // Fallback direct model edit
      model.pushEditOperations([], [{ range: replaceRange, text: textToInsert }], () => null);
    }

    this.selectedIndex = -1;

    // Handle cursor positioning
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

    // Special handling for field completions that end with a dot
    if (textToInsert.endsWith('.')) {
      // Trigger suggestions immediately for dot notation
      setTimeout(() => this.editor?.trigger('keyboard', 'editor.action.triggerSuggest', {}), 0);
    } else {
      // Standard delay for other completions
      console.log('Triggering suggestions after insertion of:', textToInsert);
      setTimeout(() => this.editor?.trigger('keyboard', 'editor.action.triggerSuggest', {}), 100);
    }
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
    // Map to appropriate icon - fix for � display issue
    const idx = Math.min(Math.max(0, kind % 10), this.KIND_ICONS.length - 1);
    return this.KIND_ICONS[idx];
  }

  private getSuggestionUIData(item: SuggestionItem): {
    icon: string;
    primaryText: string | TemplateResult;
    secondaryText: string | TemplateResult | undefined;
  } {
    switch (item.kind) {
      case 'recentSearch':
        return {
          icon: '⏱️',
          primaryText: item.query,
          secondaryText: item.timestamp,
        };

      case 'savedView':
        return {
          icon: '⭐',
          primaryText: item.name,
          secondaryText: html`
            <span class="truncate text-gray-500 mr-2" title="${item.query}">${item.query}</span>
            ${item.owner
              ? html`<span class="flex-shrink-0 rounded-full w-6 h-6 flex items-center justify-center text-xs"
                  >${item.owner.icon || ''}</span
                >`
              : ''}
          `,
        };

      case 'popularSearch':
        return {
          icon: '🔍',
          primaryText: item.query,
          secondaryText: item.description,
        };

      case 'completion':
        // Check if this is a nested field with parent path
        if (item.parentPath) {
          return {
            icon: this.getCompletionIcon(item.kindCategory),
            primaryText: html` <span class="text-gray-400">${item.parentPath}.</span><span>${item.label}</span> `,
            secondaryText: item.detail,
          };
        }
        return {
          icon: this.getCompletionIcon(item.kindCategory),
          primaryText: item.label,
          secondaryText: item.detail,
        };
    }
  }

  private renderSuggestionItem(item: SuggestionItem, itemIndex: number): TemplateResult {
    const isSelected = itemIndex === this.selectedIndex;
    const { icon, primaryText, secondaryText } = this.getSuggestionUIData(item);
    const selectedClass = isSelected ? 'bg-blue-50' : '';

    // Determine the best text to show for tooltip (when primaryText is a template)
    const displayTextForTooltip =
      item.kind === 'completion'
        ? (item.parentPath ? `${item.parentPath}.${item.label}` : item.label) || ''
        : item.kind === 'savedView'
          ? item.name || 'Saved View'
          : item.kind === 'recentSearch'
            ? item.query || 'Recent Search'
            : item.kind === 'popularSearch'
              ? item.query || 'Popular Search'
              : '';

    return html`
      <div
        class="flex items-center justify-between px-4 py-2 hover:bg-blue-50 cursor-pointer border-b border-gray-100 ${selectedClass}"
        @click=${(e: MouseEvent) => this.handleSuggestionClick(item, e)}
        @mouseover=${() => (this.selectedIndex = itemIndex)}
        data-index="${itemIndex}"
      >
        <div class="flex items-center gap-2 overflow-hidden">
          <span class="text-base">${icon}</span>
          <span class="truncate ${isSelected ? 'font-medium text-blue-600' : ''}" title="${displayTextForTooltip}">${primaryText}</span>
        </div>
        ${secondaryText ? html`<span class="text-xs text-gray-500 ml-2 flex-shrink-0 flex items-center">${secondaryText}</span>` : ''}
      </div>
    `;
  }

  private renderSuggestionDropdown(): TemplateResult {
    if (!this.showSuggestions) return html``;

    // Get all matching items
    const matches = this.getMatches();

    // Group all suggestion categories
    const groups = {
      completion: this.completionItems,
      saved: matches.saved,
      recent: matches.recent,
      popular: matches.popular,
    };

    // Generate group titles
    const groupTitles = {
      completion: null, // No title for main completions
      saved: 'Saved Views',
      recent: 'Recent Searches',
      popular: 'Popular Searches',
    };

    // Define the order for sections (Monaco completions first, then saved, recent, popular)
    const orderedCategories: Array<keyof typeof groups> = ['completion', 'saved', 'recent', 'popular'];

    // Filter out empty groups and create sections in specified order
    const sections = orderedCategories
      .filter((category) => groups[category] && groups[category].length > 0)
      .map((category) => ({
        items: groups[category],
        title: groupTitles[category],
      }));

    // Only render if we have suggestions
    if (!sections.length) {
      return html`
        <div
          class="suggestions-dropdown absolute top-10 left-0 right-0 bg-white border border-gray-200 shadow-lg z-10 overflow-y-auto rounded-md text-xs"
        >
          <div class="px-4 py-2 text-sm text-gray-400 italic">No suggestions found</div>
        </div>
      `;
    }

    // Track index for selection
    let currentIndex = 0;
    const keyboardHelp = html`
      <div class="sticky bottom-0 bg-white z-50 border-t border-gray-200 px-4 py-2 text-xs text-gray-500">
        <span class="mr-2">
          <kbd class="px-1 py-0.5 bg-gray-100 border border-gray-300 rounded text-xs">↑</kbd>
          <kbd class="px-1 py-0.5 bg-gray-100 border border-gray-300 rounded text-xs">↓</kbd>
          <kbd class="px-1 py-0.5 bg-gray-100 border border-gray-300 rounded text-xs">Tab</kbd> to navigate
        </span>
        <span class="mr-2">• <kbd class="px-1 py-0.5 bg-gray-100 border border-gray-300 rounded text-xs">Enter</kbd> to select</span>
        <span>• <kbd class="px-1 py-0.5 bg-gray-100 border border-gray-300 rounded text-xs">Esc</kbd> to close</span>
      </div>
    `;

    return html`
      <div
        class="suggestions-dropdown absolute top-10 left-0 right-0 bg-white border border-gray-200 shadow-lg z-50 max-h-[80dvh] overflow-y-auto rounded-md text-xs flex flex-col"
      >
        <div class="overflow-y-auto flex-grow min-h-0">
          ${sections.map(
            (section) => html`
              ${section.title
                ? html`<div class="text-xs font-semibold text-gray-500 px-4 py-2 uppercase border-t border-b border-gray-100 bg-gray-50">
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
          border-radius: 6px;
          outline-color: var(--color-strokeStrong);
        }
        .monaco-editor:focus-within {
          outline-color: var(--color-strokeBrand-strong);
        }
      </style>
      <div class="relative w-full h-full">
        <div class="relative w-full h-full">
          <div id="editor-container" class="h-full"></div>
          <div
            class="placeholder-overlay absolute top-0 left-0 right-0 bottom-0 pointer-events-auto z-[1] text-gray-400 f/nont-mono text-sm leading-[18px] pt-2 pl-2 hidden cursor-text"
            @click=${() => this.editor?.focus()}
          >
            Filter by your logs and events. Press <span class="kbd">Shift + Space</span> to search using natural langauge.
          </div>
        </div>
        ${this.renderSuggestionDropdown()}
      </div>
    `;
  }
}

// Export monaco and schemaManager
// Expose schemaManager globally for external configuration
(window as any).schemaManager = schemaManager;

export { monaco, schemaManager, type SchemaData };
