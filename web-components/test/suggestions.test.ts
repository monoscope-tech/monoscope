import { describe, test, expect, beforeAll, beforeEach, afterEach } from 'vitest';
import { QueryEditorComponent, schemaManager, monaco } from '../src/query-editor/query-editor';

// Setup schema data for testing
const testSchemaData = {
  spans: {
    fields: {
      timestamp: { type: 'string', examples: [] },
      id: { type: 'string', examples: [] },
      status_code: { type: 'string', examples: ['OK', 'ERROR', 'UNSET'] },
      resource: { type: 'object', examples: [] },
      'resource.service': { type: 'string', examples: ['api-service', 'web-service'] },
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
    await new Promise((resolve) => setTimeout(resolve, 100));

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
    const fieldNames = fields.map(f => f.name);

    expect(fieldNames).toContain('timestamp');
    expect(fieldNames).toContain('id');
    expect(fieldNames).toContain('status_code');
    expect(fieldNames).toContain('resource');
  });

  test('Schema manager resolves nested fields', async () => {
    const nestedFields = await schemaManager.resolveNested('spans', 'resource');
    expect(nestedFields.length).toBeGreaterThan(0);
    expect(nestedFields.some((f) => f.name === 'service')).toBe(true);
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
