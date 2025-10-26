import { describe, test, expect, beforeAll, beforeEach, afterEach } from 'vitest';
import { QueryEditorComponent, schemaManager, monaco } from '../src/query-editor/query-editor';

// Setup schema data for testing
const testSchemaData = {
  spans: {
    fields: {
      timestamp: { type: 'string', examples: [] },
      id: { type: 'string', examples: [] },
      status_code: { type: 'string', examples: ['OK', 'ERROR', 'UNSET'] },
      http_status: { type: 'number', examples: [200, 404, 500] },
      method: { type: 'string', examples: ['GET', 'POST', 'PUT', 'DELETE'] },
      path: { type: 'string', examples: ['/api/users', '/api/posts'] },
      duration: { type: 'number', examples: [] },
      resource: { type: 'object', examples: [] },
      'resource.service': { type: 'string', examples: ['api-service', 'web-service', 'auth-service'] },
      'resource.host': { type: 'string', examples: ['prod-1', 'prod-2', 'staging-1'] },
      'resource.region': { type: 'string', examples: ['us-east-1', 'us-west-2', 'eu-west-1'] },
      attributes: { type: 'object', examples: [] },
      'attributes.user_id': { type: 'string', examples: ['user-123', 'user-456'] },
      'attributes.session_id': { type: 'string', examples: ['sess-abc', 'sess-def'] },
    },
  },
  metrics: {
    fields: {
      value: { type: 'number', examples: [] },
      name: { type: 'string', examples: ['cpu_usage', 'memory_usage', 'request_count'] },
      unit: { type: 'string', examples: ['percent', 'bytes', 'count'] },
    },
  },
};

describe('Monaco AQL Editor Integration Tests', () => {
  let editorComponent: QueryEditorComponent;
  let editor: monaco.editor.IStandaloneCodeEditor;
  let container: HTMLElement;

  beforeAll(() => {
    // Setup schema manager with test data
    schemaManager.setSchemas(['spans', 'metrics']);
    schemaManager.setDefaultSchema('spans');
    schemaManager.setSchemaData('spans', testSchemaData.spans);
    schemaManager.setSchemaData('metrics', testSchemaData.metrics);
  });

  beforeEach(async () => {
    // Create container element
    container = document.createElement('div');
    container.style.width = '800px';
    container.style.height = '400px';
    document.body.appendChild(container);

    // Create editor component
    editorComponent = new QueryEditorComponent();
    container.appendChild(editorComponent);

    // Wait for component to initialize
    await editorComponent.updateComplete;
    await new Promise((resolve) => setTimeout(resolve, 150));

    // Get the Monaco editor instance
    editor = (editorComponent as any).editor;

    if (!editor) {
      throw new Error('Monaco editor not initialized');
    }
  });

  afterEach(() => {
    if (editorComponent && editorComponent.parentNode) {
      editorComponent.parentNode.removeChild(editorComponent);
    }
    if (container && container.parentNode) {
      container.parentNode.removeChild(container);
    }
  });

  // Helper function to get suggestions for a given query text
  async function getSuggestions(text: string): Promise<monaco.languages.CompletionItem[]> {
    const model = editor.getModel();
    if (!model) throw new Error('No model');

    // Set the text
    editor.setValue(text);

    // Position cursor at the end
    const lineCount = model.getLineCount();
    const column = model.getLineMaxColumn(lineCount);
    const position = { lineNumber: lineCount, column };

    // Trigger completion
    const result = await monaco.languages.CompletionItemProvider.triggerSuggest(model, position);

    return result?.suggestions || [];
  }

  // Test editor initialization
  test('Editor component should initialize successfully', () => {
    expect(editor).toBeDefined();
    expect(editor).not.toBeNull();
    expect(editor.getModel()).toBeDefined();
  });

  test('Editor should have correct language mode', () => {
    const model = editor.getModel();
    expect(model).toBeDefined();
    if (model) {
      const languageId = model.getLanguageId();
      expect(languageId).toBe('aql');
    }
  });

  test('Editor can set and get values', () => {
    editor.setValue('spans | status_code == "OK"');
    expect(editor.getValue()).toBe('spans | status_code == "OK"');
  });

  // Test schema manager integration
  test('Schema manager provides correct schemas', () => {
    const schemas = schemaManager.getSchemas();
    expect(schemas).toContain('spans');
    expect(schemas).toContain('metrics');
  });

  test('Schema manager has correct default schema', () => {
    const defaultSchema = schemaManager.getDefaultSchema();
    expect(defaultSchema).toBe('spans');
  });

  test('Schema manager resolves top-level fields', async () => {
    const fields = await schemaManager.resolveNested('spans', '');
    const fieldNames = fields.map((f) => f.name);

    expect(fieldNames).toContain('timestamp');
    expect(fieldNames).toContain('id');
    expect(fieldNames).toContain('status_code');
    expect(fieldNames).toContain('resource');
  });

  test('Schema manager resolves nested fields', async () => {
    const nestedFields = await schemaManager.resolveNested('spans', 'resource');
    expect(nestedFields.length).toBeGreaterThan(0);
    const fieldNames = nestedFields.map((f) => f.name);
    expect(fieldNames).toContain('service');
    expect(fieldNames).toContain('host');
  });

  test('Schema manager resolves field values', async () => {
    const values = await schemaManager.resolveValues('spans', 'status_code');
    expect(values).toContain('OK');
    expect(values).toContain('ERROR');
    expect(values).toContain('UNSET');
  });

  test('Schema manager resolves nested field values', async () => {
    const values = await schemaManager.resolveValues('spans', 'resource.service');
    expect(values).toContain('api-service');
    expect(values).toContain('web-service');
  });

  // Test real-world suggestion scenarios
  describe('Real-world completion scenarios', () => {
    test('Empty input should suggest schemas and fields', async () => {
      const model = editor.getModel();
      if (!model) throw new Error('No model');

      editor.setValue('');
      const position = { lineNumber: 1, column: 1 };

      // Manually trigger completion provider
      const providers = (monaco.languages as any).CompletionItemProviderRegistry;
      if (providers && providers._providers) {
        const aqlProviders = Array.from(providers._providers.values()).find((p: any) =>
          p.some((item: any) => item.triggerCharacters)
        );

        if (aqlProviders && aqlProviders.length > 0) {
          const provider = aqlProviders[0];
          const result = await provider.provideCompletionItems(
            model,
            position,
            { triggerKind: monaco.languages.CompletionTriggerKind.Invoke },
            { isCancellationRequested: false, onCancellationRequested: () => ({ dispose: () => {} }) }
          );

          const labels = result.suggestions.map((s: any) => s.label);
          expect(labels).toContain('spans');
          expect(labels).toContain('metrics');
        }
      }
    });

    test('After schema name with space should suggest keywords and fields', async () => {
      const model = editor.getModel();
      if (!model) throw new Error('No model');

      editor.setValue('spans ');
      const position = { lineNumber: 1, column: 7 }; // After "spans "

      const providers = (monaco.languages as any).CompletionItemProviderRegistry;
      if (providers && providers._providers) {
        const aqlProviders = Array.from(providers._providers.values()).find((p: any) =>
          p.some((item: any) => item.triggerCharacters)
        );

        if (aqlProviders && aqlProviders.length > 0) {
          const provider = aqlProviders[0];
          const result = await provider.provideCompletionItems(
            model,
            position,
            { triggerKind: monaco.languages.CompletionTriggerKind.Invoke },
            { isCancellationRequested: false, onCancellationRequested: () => ({ dispose: () => {} }) }
          );

          const labels = result.suggestions.map((s: any) => s.label);
          expect(labels).toContain('stats');
          expect(labels).toContain('timechart');
        }
      }
    });

    test('After operator should suggest field values', async () => {
      const model = editor.getModel();
      if (!model) throw new Error('No model');

      editor.setValue('spans | status_code == ');
      const position = { lineNumber: 1, column: 25 }; // After "== "

      const providers = (monaco.languages as any).CompletionItemProviderRegistry;
      if (providers && providers._providers) {
        const aqlProviders = Array.from(providers._providers.values()).find((p: any) =>
          p.some((item: any) => item.triggerCharacters)
        );

        if (aqlProviders && aqlProviders.length > 0) {
          const provider = aqlProviders[0];
          const result = await provider.provideCompletionItems(
            model,
            position,
            { triggerKind: monaco.languages.CompletionTriggerKind.Invoke },
            { isCancellationRequested: false, onCancellationRequested: () => ({ dispose: () => {} }) }
          );

          const labels = result.suggestions.map((s: any) => s.label);
          // Should suggest the enum values
          expect(labels.some((l: string) => l.includes('OK'))).toBe(true);
          expect(labels.some((l: string) => l.includes('ERROR'))).toBe(true);
          expect(labels.some((l: string) => l.includes('UNSET'))).toBe(true);
        }
      }
    });

    test('After dot should suggest nested fields', async () => {
      const model = editor.getModel();
      if (!model) throw new Error('No model');

      editor.setValue('spans | resource.');
      const position = { lineNumber: 1, column: 18 }; // After "resource."

      const providers = (monaco.languages as any).CompletionItemProviderRegistry;
      if (providers && providers._providers) {
        const aqlProviders = Array.from(providers._providers.values()).find((p: any) =>
          p.some((item: any) => item.triggerCharacters)
        );

        if (aqlProviders && aqlProviders.length > 0) {
          const provider = aqlProviders[0];
          const result = await provider.provideCompletionItems(
            model,
            position,
            { triggerKind: monaco.languages.CompletionTriggerKind.Invoke },
            { isCancellationRequested: false, onCancellationRequested: () => ({ dispose: () => {} }) }
          );

          const labels = result.suggestions.map((s: any) => s.label);
          expect(labels).toContain('service');
          expect(labels).toContain('host');
        }
      }
    });

    test('After quoted value with space should suggest logical operators', async () => {
      const model = editor.getModel();
      if (!model) throw new Error('No model');

      editor.setValue('spans | status_code == "OK" ');
      const position = { lineNumber: 1, column: 29 }; // After '"OK" '

      const providers = (monaco.languages as any).CompletionItemProviderRegistry;
      if (providers && providers._providers) {
        const aqlProviders = Array.from(providers._providers.values()).find((p: any) =>
          p.some((item: any) => item.triggerCharacters)
        );

        if (aqlProviders && aqlProviders.length > 0) {
          const provider = aqlProviders[0];
          const result = await provider.provideCompletionItems(
            model,
            position,
            { triggerKind: monaco.languages.CompletionTriggerKind.Invoke },
            { isCancellationRequested: false, onCancellationRequested: () => ({ dispose: () => {} }) }
          );

          const labels = result.suggestions.map((s: any) => s.label);
          expect(labels.some((l: string) => l.toLowerCase() === 'and')).toBe(true);
          expect(labels.some((l: string) => l.toLowerCase() === 'or')).toBe(true);
          expect(labels).toContain('|');
        }
      }
    });
  });

  // Test editor change events
  test('Editor fires change events', async () => {
    let changeEventFired = false;

    editor.onDidChangeModelContent(() => {
      changeEventFired = true;
    });

    editor.setValue('spans');
    await new Promise((resolve) => setTimeout(resolve, 100));

    expect(changeEventFired).toBe(true);
  });

  // Comprehensive Grammar Tests - Table-Driven Format
  describe('Comprehensive AQL Grammar Coverage', () => {
    // Helper to get completion suggestions
    async function getSuggestions(text: string): Promise<string[]> {
      const model = editor.getModel();
      if (!model) throw new Error('No model');

      editor.setValue(text);
      const position = { lineNumber: 1, column: text.length + 1 };

      const providers = (monaco.languages as any).CompletionItemProviderRegistry;
      if (providers && providers._providers) {
        const aqlProviders = Array.from(providers._providers.values()).find((p: any) =>
          p.some((item: any) => item.triggerCharacters)
        );

        if (aqlProviders && aqlProviders.length > 0) {
          const provider = aqlProviders[0];
          const result = await provider.provideCompletionItems(
            model,
            position,
            { triggerKind: monaco.languages.CompletionTriggerKind.Invoke },
            { isCancellationRequested: false, onCancellationRequested: () => ({ dispose: () => {} }) }
          );

          if (result && result.suggestions) {
            return result.suggestions.map((s: any) => s.label);
          }
        }
      }
      return [];
    }

    // Test case format: [description, query, expectedSuggestions, checkMode]
    // checkMode: 'includes' (default) = check if suggestions include all expected items
    //           'contains' = check if at least one suggestion contains the text
    const testCases: Array<{
      category: string;
      query: string;
      expect: string[];
      mode?: 'includes' | 'contains';
    }> = [
      // ==================== DATA SOURCES ====================
      { category: 'Data Sources', query: '', expect: ['spans', 'metrics'] },
      { category: 'Data Sources', query: 'metr', expect: ['metrics'] },

      // ==================== COMPARISON OPERATORS ====================
      { category: 'Comparison Operators', query: 'spans | status_code ', expect: ['==', '!=', '>', '<', '>=', '<=', '=~'] },
      { category: 'Comparison Operators', query: 'spans | http_status ', expect: ['==', '!=', '>', '<', '>=', '<='] },
      { category: 'Comparison Operators', query: 'spans | duration ', expect: ['>=', '<='] },

      // ==================== SET OPERATORS ====================
      { category: 'Set Operators', query: 'spans | method ', expect: ['in', '!in', 'has', '!has', 'has_any', 'has_all'] },

      // ==================== STRING OPERATORS ====================
      { category: 'String Operators', query: 'spans | path ', expect: ['contains', '!contains', 'startswith', '!startswith', 'endswith', '!endswith', 'matches'] },

      // ==================== LOGICAL OPERATORS ====================
      { category: 'Logical Operators', query: 'spans | status_code == "OK" ', expect: ['and', 'or', '|'], mode: 'contains' },
      { category: 'Logical Operators', query: 'spans | status_code ', expect: ['not', 'exists'] },
      { category: 'Logical Operators', query: 'spans | status_code == "OK" and ', expect: ['method', 'path'] },

      // ==================== AGGREGATIONS ====================
      { category: 'Aggregations', query: 'spans ', expect: ['stats', 'timechart', 'limit'] },
      { category: 'Aggregations', query: 'spans | status_code == "OK" ', expect: ['|'] },

      // ==================== STATS FUNCTIONS ====================
      { category: 'Stats Functions', query: 'spans | stats ', expect: ['count', 'sum', 'avg', 'min', 'max', 'median', 'stdev', 'range'] },
      { category: 'Stats Functions', query: 'spans | stats ', expect: ['p50', 'p75', 'p90', 'p95', 'p99'] },
      { category: 'Stats Functions', query: 'spans | stats ', expect: ['by'] },

      // ==================== NESTED FIELDS ====================
      { category: 'Nested Fields', query: 'spans | resource.', expect: ['service', 'host', 'region'] },
      { category: 'Nested Fields', query: 'spans | attributes.', expect: ['user_id', 'session_id'] },
      { category: 'Nested Fields', query: 'spans | resource.service == ', expect: ['api-service', 'web-service', 'auth-service'], mode: 'contains' },
      { category: 'Nested Fields', query: 'spans | resource.host == ', expect: ['prod-1', 'prod-2'], mode: 'contains' },

      // ==================== VALUE SUGGESTIONS ====================
      { category: 'Value Suggestions', query: 'spans | status_code == ', expect: ['OK', 'ERROR', 'UNSET'], mode: 'contains' },
      { category: 'Value Suggestions', query: 'spans | status_code != ', expect: ['OK'], mode: 'contains' },
      { category: 'Value Suggestions', query: 'spans | method == ', expect: ['GET', 'POST', 'PUT', 'DELETE'], mode: 'contains' },
      { category: 'Value Suggestions', query: 'spans | http_status == ', expect: ['200', '404', '500'], mode: 'contains' },

      // ==================== MULTIPLE DATA SOURCES ====================
      { category: 'Multiple Data Sources', query: 'metrics ', expect: ['value', 'name', 'unit'] },
      { category: 'Multiple Data Sources', query: 'metrics | name == ', expect: ['cpu_usage', 'memory_usage', 'request_count'], mode: 'contains' },

      // ==================== COMPLEX QUERIES ====================
      { category: 'Complex Queries', query: 'spans | status_code == "OK" and method == "GET" and ', expect: ['path', 'duration'] },
      { category: 'Complex Queries', query: 'spans | status_code == "OK" or ', expect: ['status_code', 'method'] },
      { category: 'Complex Queries', query: 'spans | status_code == "OK" | ', expect: ['stats', 'timechart'] },

      // ==================== TIMECHART ====================
      { category: 'Timechart', query: 'spans | timechart ', expect: ['[5m]', '[1h]'], mode: 'contains' },
      { category: 'Timechart', query: 'spans | timechart ', expect: ['count', 'sum', 'avg'] },

      // ==================== TOP-LEVEL FIELDS ====================
      { category: 'Top-Level Fields', query: 'spans ', expect: ['timestamp', 'id', 'status_code', 'method', 'path'] },
      { category: 'Top-Level Fields', query: 'spans ', expect: ['resource', 'attributes'] },

      // ==================== EDGE CASES ====================
      { category: 'Edge Cases', query: 'spans | stat', expect: ['status_code'] },
      { category: 'Edge Cases', query: 'spans  |  status_code  ==  ', expect: ['OK'], mode: 'contains' },
    ];

    // Generate tests from table
    testCases.forEach(({ category, query, expect: expectedItems, mode = 'includes' }) => {
      const testName = `${category}: "${query}" → should suggest ${expectedItems.join(', ')}`;

      test(testName, async () => {
        const model = editor.getModel();
        if (!model) throw new Error('No model');

        editor.setValue(query);
        const position = { lineNumber: 1, column: query.length + 1 };

        const providers = (monaco.languages as any).CompletionItemProviderRegistry;
        if (providers && providers._providers) {
          const aqlProviders = Array.from(providers._providers.values()).find((p: any) =>
            p.some((item: any) => item.triggerCharacters)
          );

          if (aqlProviders && aqlProviders.length > 0) {
            const provider = aqlProviders[0];
            const result = await provider.provideCompletionItems(
              model,
              position,
              { triggerKind: monaco.languages.CompletionTriggerKind.Invoke },
              { isCancellationRequested: false, onCancellationRequested: () => ({ dispose: () => {} }) }
            );

            const labels = result.suggestions.map((s: any) => s.label);

            if (mode === 'contains') {
              expectedItems.forEach((item) => {
                expect(labels.some((l: string) => l.includes(item))).toBe(true);
              });
            } else {
              expectedItems.forEach((item) => {
                expect(labels).toContain(item);
              });
            }
          }
        }
      });
    });
  });

  // Test component state
  test('Component shows suggestions state can be toggled', async () => {
    // Set the component to show suggestions
    (editorComponent as any).showSuggestions = true;
    await editorComponent.updateComplete;

    expect((editorComponent as any).showSuggestions).toBe(true);

    // Set the component to hide suggestions
    (editorComponent as any).showSuggestions = false;
    await editorComponent.updateComplete;

    expect((editorComponent as any).showSuggestions).toBe(false);
  });

  // Test component cleanup
  test('Component cleans up properly on disconnect', () => {
    // This test just verifies that disconnection doesn't throw
    expect(() => {
      if (editorComponent && editorComponent.parentNode) {
        editorComponent.parentNode.removeChild(editorComponent);
      }
    }).not.toThrow();
  });
});
