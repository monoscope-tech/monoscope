// The resolvers behind every field and value suggestion.
//
// The schema arrives from Haskell flattened — `resource.service.name` is a key, not a tree —
// and these two resolvers are what turn that back into something the editor can walk one
// level at a time. They had no tests, which is how the suggestion suite came to assert
// against a Monaco internal for months without anyone noticing it asserted nothing.
import { describe, test, expect, beforeEach } from 'vitest';
import { initializeDefaultSchema } from '../src/query-editor/query-editor-config';
import { schemaManager } from '../src/query-editor/query-editor';

const SPANS = {
  fields: {
    timestamp: { type: 'string', examples: [] },
    status_code: { type: 'string', examples: [] },
    is_root: { type: 'boolean', examples: [] },
    method: { type: 'string', examples: ['GET', 'POST'] },
    count: { field_type: 'number', examples: [] },
    resource: { type: 'object', examples: [] },
    'resource.service.name': { type: 'string', examples: ['api'] },
    'resource.service.version': { type: 'string', examples: ['1.0'] },
    'resource.host': { type: 'string', examples: ['prod-1'] },
  },
};

const names = (fs: { name: string }[]) => fs.map(f => f.name).sort();

beforeEach(() => {
  initializeDefaultSchema();
  schemaManager.setSchemaData('spans', SPANS as any);
});

describe('resolving fields one level at a time', () => {
  test('the top level lists only undotted fields', async () => {
    expect(names(await schemaManager.resolveNested('spans', ''))).toEqual(
      ['count', 'is_root', 'method', 'resource', 'status_code', 'timestamp'],
    );
  });

  // `fields` is the flag the editor reads to decide whether selecting a suggestion inserts a
  // trailing dot. Getting it wrong makes an object look like a leaf, and the reader cannot
  // drill in at all.
  test('a field with children is marked as having them, a leaf is not', async () => {
    const top = await schemaManager.resolveNested('spans', '');
    expect(top.find(f => f.name === 'resource')?.fields).toEqual({});
    expect(top.find(f => f.name === 'timestamp')?.fields).toBeUndefined();
  });

  test('a prefix yields immediate children, not the whole subtree', async () => {
    // `service` appears twice in the flattened schema (.name and .version) and must collapse
    // to one child — the editor would otherwise offer the same name repeatedly.
    expect(names(await schemaManager.resolveNested('spans', 'resource'))).toEqual(['host', 'service']);
  });

  test('a child that still has children of its own is marked, and its leaves resolve', async () => {
    const under = await schemaManager.resolveNested('spans', 'resource');
    expect(under.find(f => f.name === 'service')?.fields).toEqual({});
    expect(under.find(f => f.name === 'host')?.fields).toBeUndefined();
    expect(names(await schemaManager.resolveNested('spans', 'resource.service'))).toEqual(['name', 'version']);
  });

  test('both `type` and the wire-side `field_type` are honoured', async () => {
    const top = await schemaManager.resolveNested('spans', '');
    expect(top.find(f => f.name === 'count')?.type).toBe('number');
    expect(top.find(f => f.name === 'timestamp')?.type).toBe('string');
  });

  test('an unknown schema falls back to spans rather than returning nothing', async () => {
    expect(names(await schemaManager.resolveNested('no-such-schema', ''))).toContain('status_code');
  });

  test('a prefix that matches nothing is empty, not an error', async () => {
    expect(await schemaManager.resolveNested('spans', 'nope')).toEqual([]);
  });
});

describe('resolving values for a field', () => {
  test('the project\'s own examples win', async () => {
    expect(await schemaManager.resolveValues('spans', 'method')).toEqual(['GET', 'POST']);
  });

  // These are spec constants, valid whether or not this project has emitted one yet — a new
  // project with no ERROR spans must still be able to complete `status_code == "ERROR"`.
  test('spec constants are offered even with no data behind them', async () => {
    expect(await schemaManager.resolveValues('spans', 'status_code')).toEqual(['OK', 'ERROR', 'UNSET']);
    expect(await schemaManager.resolveValues('spans', 'level')).toContain('FATAL');
    expect(await schemaManager.resolveValues('spans', 'severity.text')).toContain('DEBUG');
  });

  // Carried as `value|description`; completion.ts splits on the pipe for the detail column.
  test('timestamps offer relative-time helpers with descriptions', async () => {
    const vs = await schemaManager.resolveValues('spans', 'timestamp');
    expect(vs).toContain('ago(1h)|1 hour ago');
    expect(vs).toContain('now()|current time');
    expect(await schemaManager.resolveValues('spans', 'observed_timestamp')).toEqual(vs);
  });

  test('a boolean field offers true and false', async () => {
    expect(await schemaManager.resolveValues('spans', 'is_root')).toEqual(['true', 'false']);
  });

  test('a field with nothing to suggest returns empty rather than throwing', async () => {
    expect(await schemaManager.resolveValues('spans', 'count')).toEqual([]);
    expect(await schemaManager.resolveValues('spans', 'no_such_field')).toEqual([]);
  });

  test('examples are stringified so numeric ones still render', async () => {
    schemaManager.setSchemaData('spans', { fields: { http_status: { type: 'number', examples: [200, 404] } } } as any);
    expect(await schemaManager.resolveValues('spans', 'http_status')).toEqual(['200', '404']);
  });
});

describe('the defaults the editor starts from', () => {
  test('spans and metrics are registered, with spans as the default', () => {
    expect(schemaManager.getSchemas()).toEqual(['spans', 'metrics']);
    expect(schemaManager.getDefaultSchema()).toBe('spans');
  });
});
