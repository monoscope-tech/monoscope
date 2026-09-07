import { describe, test, expect, beforeAll, vi } from 'vitest';
import { schemaManager } from '../src/query-editor/schema-manager';
import { computeSuggestions, type SchemaAccess } from '../src/query-editor/completion';
vi.stubGlobal('Worker', undefined);
const tableSchema: SchemaAccess = {
  tables: schemaManager.getSchemas,
  defaultTable: schemaManager.getDefaultSchema,
  fields: schemaManager.resolveNested,
  values: schemaManager.resolveValues,
};
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

beforeAll(() => {
  schemaManager.setSchemas(['spans', 'metrics']);
  schemaManager.setDefaultSchema('spans');
  schemaManager.setSchemaData('spans', testSchemaData.spans);
  schemaManager.setSchemaData('metrics', testSchemaData.metrics);
});
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
  {
    category: 'String Operators',
    query: 'spans | path ',
    expect: ['contains', '!contains', 'startswith', '!startswith', 'endswith', '!endswith', 'matches'],
  },

  // ==================== LOGICAL OPERATORS ====================
  { category: 'Logical Operators', query: 'spans | status_code == "OK" ', expect: ['and', 'or', '|'], mode: 'contains' },
  // Only operators the KQL grammar actually parses (Pkg/Parser/Expr.hs) — it has no
  // `exists` and no postfix `not`, so suggesting those would build unparseable queries.
  { category: 'Logical Operators', query: 'spans | status_code ', expect: ['==', 'has', 'contains'] },
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
  {
    category: 'Nested Fields',
    query: 'spans | resource.service == ',
    expect: ['api-service', 'web-service', 'auth-service'],
    mode: 'contains',
  },
  { category: 'Nested Fields', query: 'spans | resource.host == ', expect: ['prod-1', 'prod-2'], mode: 'contains' },

  // ==================== VALUE SUGGESTIONS ====================
  { category: 'Value Suggestions', query: 'spans | status_code == ', expect: ['OK', 'ERROR', 'UNSET'], mode: 'contains' },
  { category: 'Value Suggestions', query: 'spans | status_code != ', expect: ['OK'], mode: 'contains' },
  { category: 'Value Suggestions', query: 'spans | method == ', expect: ['GET', 'POST', 'PUT', 'DELETE'], mode: 'contains' },
  { category: 'Value Suggestions', query: 'spans | http_status == ', expect: ['200', '404', '500'], mode: 'contains' },

  // ==================== MULTIPLE DATA SOURCES ====================
  { category: 'Multiple Data Sources', query: 'metrics ', expect: ['value', 'name', 'unit'] },
  {
    category: 'Multiple Data Sources',
    query: 'metrics | name == ',
    expect: ['cpu_usage', 'memory_usage', 'request_count'],
    mode: 'contains',
  },

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

  // ==================== BARE CONDITION QUERIES (no collection prefix) ====================
  { category: 'Bare Conditions', query: '', expect: ['status_code', 'method', 'duration', 'resource', 'attributes', 'timestamp'] },
  { category: 'Bare Conditions', query: 'status_code ', expect: ['==', '!=', '>', '<', '>=', '<=', '=~'] },
  { category: 'Bare Conditions', query: 'status_code == ', expect: ['OK', 'ERROR', 'UNSET'], mode: 'contains' },
  { category: 'Bare Conditions', query: 'status_code == "OK" ', expect: ['and', 'or', '|'], mode: 'contains' },
  { category: 'Bare Conditions', query: 'status_code == "OK" and ', expect: ['method', 'path'] },
  { category: 'Bare Conditions', query: 'status_code == "OK" and method ', expect: ['==', '!='] },
  { category: 'Bare Conditions', query: 'status_code == "OK" and method == ', expect: ['GET', 'POST'], mode: 'contains' },
  { category: 'Bare Conditions', query: 'resource.', expect: ['service', 'host', 'region'] },
  { category: 'Bare Conditions', query: 'resource.service == ', expect: ['api-service'], mode: 'contains' },
  { category: 'Bare Conditions', query: 'duration > ', expect: [] },
  { category: 'Bare Conditions', query: 'http_status == ', expect: ['200', '404', '500'], mode: 'contains' },

  // ==================== EDGE CASES ====================
  { category: 'Edge Cases', query: 'spans | stat', expect: ['status_code'] },
  { category: 'Edge Cases', query: 'spans  |  status_code  ==  ', expect: ['OK'], mode: 'contains' },
];

describe('query grammar with real schema resolution', () => {
  test.each(testCases)('$category: $query', async ({ query, expect: expected, mode }) => {
    const labels = (await computeSuggestions(query, tableSchema)).map((s) => s.label);
    if (!expected.length) expect(labels).toEqual([]);
    for (const item of expected) {
      if (mode === 'contains') expect(labels.some((label) => label.includes(item))).toBe(true);
      else expect(labels).toContain(item);
    }
  });
});
