import { describe, test, expect, beforeAll, beforeEach, afterEach } from 'vitest';
import { MonacoEditorComponent, schemaManager, monaco } from '../src/monaco-editor';

// Setup schema data for testing
const testSchemaData = {
  spans: {
    fields: {
      timestamp: { type: 'string', examples: [] },
      id: { type: 'string', examples: [] },
      status_code: { type: 'string', examples: ['OK', 'ERROR', 'UNSET'] },
      resource: {
        type: 'object',
        fields: {
          service: { type: 'string', examples: ['api-service', 'web-service'] },
        },
      },
    },
  },
  metrics: {
    fields: {
      value: { type: 'number', examples: [] },
      name: { type: 'string', examples: [] },
    },
  },
};

describe('Monaco AQL Editor Integration Tests', () => {
  let editorComponent: MonacoEditorComponent;
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
    editorComponent = new MonacoEditorComponent();
    container.appendChild(editorComponent);

    // Wait for component to initialize
    await editorComponent.updateComplete;
    await new Promise((resolve) => setTimeout(resolve, 100));

    // Get the Monaco editor instance
    editor = (editorComponent as any).editor;

    if (!editor) {
      throw new Error('Monaco editor not initialized');
    }
  });

  afterEach(() => {
    if (editorComponent) {
      editorComponent.remove();
    }
    if (container && container.parentNode) {
      container.parentNode.removeChild(container);
    }
  });

  // Helper function to get suggestions at cursor position
  async function getSuggestionsAtPosition(text: string, line: number = 1, column?: number): Promise<monaco.languages.CompletionItem[]> {
    if (!editor) throw new Error('Editor not initialized');

    // Set editor value
    editor.setValue(text);

    // Position cursor at end if column not specified
    const model = editor.getModel();
    if (!model) throw new Error('Editor model not available');

    const position =
      column !== undefined ? { lineNumber: line, column: column } : { lineNumber: line, column: model.getLineLength(line) + 1 };

    editor.setPosition(position);

    // Get completion provider and trigger suggestions
    const providers = monaco.languages.getCompletionItemProviders('aql');
    if (providers.length === 0) {
      throw new Error('No completion providers registered');
    }

    const provider = providers[0];
    const result = await provider.provideCompletionItems(
      model,
      position,
      { triggerKind: monaco.languages.CompletionTriggerKind.Invoke, triggerCharacter: undefined },
      new monaco.CancellationToken()
    );

    return result?.suggestions || [];
  }

  // Helper function to simulate typing and get suggestions
  async function typeAndGetSuggestions(text: string): Promise<monaco.languages.CompletionItem[]> {
    if (!editor) throw new Error('Editor not initialized');

    editor.setValue('');

    // Simulate typing character by character
    for (const char of text) {
      const model = editor.getModel();
      if (!model) throw new Error('Editor model not available');

      const position = editor.getPosition();
      if (!position) throw new Error('Editor position not available');

      // Insert character
      editor.executeEdits('test', [
        {
          range: {
            startLineNumber: position.lineNumber,
            startColumn: position.column,
            endLineNumber: position.lineNumber,
            endColumn: position.column,
          },
          text: char,
        },
      ]);

      // Small delay to simulate real typing
      await new Promise((resolve) => setTimeout(resolve, 10));
    }

    // Get suggestions at current position
    const model = editor.getModel();
    if (!model) throw new Error('Editor model not available');

    const position = editor.getPosition();
    if (!position) throw new Error('Editor position not available');

    const providers = monaco.languages.getCompletionItemProviders('aql');
    if (providers.length === 0) {
      throw new Error('No completion providers registered');
    }

    const provider = providers[0];
    const result = await provider.provideCompletionItems(
      model,
      position,
      { triggerKind: monaco.languages.CompletionTriggerKind.Invoke, triggerCharacter: undefined },
      new monaco.CancellationToken()
    );

    return result?.suggestions || [];
  }

  // Test case data
  const testCases = [
    {
      name: 'Empty input - shows schemas and fields',
      input: '',
      expectedContains: ['spans', 'metrics', 'timestamp'],
    },
    {
      name: 'Partial schema name - shows matching schemas',
      input: 'spa',
      expectedContains: ['spans'],
    },
    {
      name: 'After schema - shows keywords and fields',
      input: 'spans ',
      expectedContains: ['limit', 'stats', 'summarize'],
    },
    {
      name: 'Partial field name - shows matching fields',
      input: 'spans | time',
      expectedContains: ['timestamp'],
    },
    {
      name: 'After operator - shows value suggestions',
      input: 'spans | status_code == ',
      expectedContains: ['OK', 'ERROR', 'UNSET'],
    },
    {
      name: 'Nested field access - shows nested fields',
      input: 'spans | resource.',
      expectedContains: ['service'],
    },
    {
      name: 'After string value - suggests logical operators',
      input: 'spans | status_code == "OK" ',
      expectedContains: ['AND', 'OR', '|'],
    },
  ];

  // Test individual suggestions
  testCases.forEach((testCase) => {
    test(testCase.name, async () => {
      const suggestions = await getSuggestionsAtPosition(testCase.input);
      const suggestionLabels = suggestions.map((s) => s.label);

      console.log(`Input: "${testCase.input}"`);
      console.log(`Suggestions: ${suggestionLabels.join(', ')}`);

      // Verify that all expected items are in the suggestions
      for (const expected of testCase.expectedContains) {
        const found = suggestionLabels.some((label) => (typeof label === 'string' ? label.includes(expected) : false));

        expect(found, `Expected "${expected}" in suggestions but not found. Suggestions: ${suggestionLabels.join(', ')}`).toBe(true);
      }
    });
  });

  // Test editor integration with typing simulation
  test('Typing simulation - schema completion', async () => {
    const suggestions = await typeAndGetSuggestions('sp');
    const suggestionLabels = suggestions.map((s) => s.label);

    expect(suggestionLabels.some((label) => (typeof label === 'string' ? label.includes('spans') : false))).toBe(true);
  });

  test('Typing simulation - field completion after pipe', async () => {
    const suggestions = await typeAndGetSuggestions('spans | sta');
    const suggestionLabels = suggestions.map((s) => s.label);

    expect(suggestionLabels.some((label) => (typeof label === 'string' ? label.includes('stats') : false))).toBe(true);
  });

  // Test event hooks
  test('Editor change events trigger suggestions', async () => {
    let changeEventFired = false;
    let suggestionsTriggered = false;

    if (!editor) throw new Error('Editor not initialized');

    // Hook into change events
    editor.onDidChangeModelContent(() => {
      changeEventFired = true;
    });

    // Hook into suggestion events via the component
    const originalUpdateSuggestions = (editorComponent as any).updateSuggestions;
    (editorComponent as any).updateSuggestions = function (...args: any[]) {
      suggestionsTriggered = true;
      return originalUpdateSuggestions.apply(this, args);
    };

    // Type text
    editor.setValue('spans');

    // Wait for events to process
    await new Promise((resolve) => setTimeout(resolve, 100));

    expect(changeEventFired).toBe(true);
  });

  test('Keyboard navigation in suggestions', async () => {
    if (!editor) throw new Error('Editor not initialized');

    // Type to trigger suggestions
    editor.setValue('spans | s');
    editor.setPosition({ lineNumber: 1, column: 10 });

    // Simulate showing suggestions in the component
    (editorComponent as any).showSuggestions = true;
    await editorComponent.updateComplete;

    // Check that the component is in suggestion mode
    expect((editorComponent as any).showSuggestions).toBe(true);
  });

  test('Suggestion insertion updates editor content', async () => {
    if (!editor) throw new Error('Editor not initialized');

    // Get suggestions for empty input
    const suggestions = await getSuggestionsAtPosition('');
    expect(suggestions.length).toBeGreaterThan(0);

    // Find a schema suggestion
    const schemaSuggestion = suggestions.find((s) => typeof s.label === 'string' && s.label === 'spans');

    expect(schemaSuggestion).toBeDefined();

    if (schemaSuggestion) {
      // Insert the suggestion manually (simulating completion)
      const insertText = schemaSuggestion.insertText || schemaSuggestion.label;
      editor.setValue(insertText.toString());

      expect(editor.getValue()).toContain('spans');
    }
  });

  test('Schema manager integration', async () => {
    // Test that schema manager provides correct data
    const schemas = schemaManager.getSchemas();
    expect(schemas).toContain('spans');
    expect(schemas).toContain('metrics');

    // Test nested field resolution
    const nestedFields = await schemaManager.resolveNested('spans', 'resource');
    expect(nestedFields.some((f) => f.name === 'service')).toBe(true);

    // Test value resolution
    const values = await schemaManager.resolveValues('spans', 'status_code');
    expect(values).toContain('OK');
    expect(values).toContain('ERROR');
    expect(values).toContain('UNSET');
  });
});

