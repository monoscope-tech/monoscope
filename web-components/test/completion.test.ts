import { describe, test, expect } from 'vitest';
import { computeSuggestions, filterSuggestions, wordAtCursor, type CompletionField, type SchemaAccess, type Suggestion } from '../src/query-editor/completion';

// A schema shaped like the real /log_explorer/schema response: bare columns, the
// SELECT aliases the parser accepts, and dotted attribute columns.
const FIELDS: Record<string, CompletionField[]> = {
  '': [
    { name: 'timestamp', type: 'string' },
    { name: 'kind', type: 'string', examples: ['span', 'log'] },
    { name: 'level', type: 'string', examples: ['INFO', 'ERROR'] },
    { name: 'status_code', type: 'string', examples: ['OK', 'ERROR'] },
    { name: 'duration', type: 'number' },
    { name: 'service', type: 'string' },
    { name: 'errors', type: 'object', fields: {} },
    { name: 'attributes', type: 'object', fields: {} },
    { name: 'resource', type: 'object', fields: {} },
  ],
  attributes: [
    { name: 'http', type: 'object', fields: {} },
    { name: 'db', type: 'object', fields: {} },
    { name: 'user_id', type: 'string' },
  ],
  'attributes.http': [{ name: 'request', type: 'object', fields: {} }],
  'attributes.http.request': [{ name: 'method', type: 'string', examples: ['GET', 'POST'] }],
};

const VALUES: Record<string, string[]> = {
  status_code: ['OK', 'ERROR|the span failed'],
  level: ['INFO', 'ERROR'],
  'attributes.http.request.method': ['GET', 'POST'],
};

const schema = (over: Partial<SchemaAccess> = {}): SchemaAccess => ({
  tables: () => ['spans', 'metrics'],
  defaultTable: () => 'spans',
  fields: async (_t, prefix) => FIELDS[prefix] ?? [],
  values: async (_t, field) => VALUES[field] ?? [],
  ...over,
});

const labels = async (text: string, s: SchemaAccess = schema()) => (await computeSuggestions(text, s)).map((x) => x.label);
const kinds = async (text: string, s: SchemaAccess = schema()) => [...new Set((await computeSuggestions(text, s)).map((x) => x.kind))].sort();
const find = async (text: string, label: string): Promise<Suggestion | undefined> => (await computeSuggestions(text, schema())).find((x) => x.label === label);

describe('what the cursor position asks for', () => {
  // Each row is (what's typed, what the user is obviously reaching for).
  const cases: [string, string, (l: string[]) => boolean][] = [
    ['', 'fields on an empty query', (l) => l.includes('kind') && l.includes('status_code')],
    ['attrib', 'fields while typing a field name', (l) => l.includes('attributes')],
    ['attributes.', "that field's children", (l) => l.join() === 'http,db,user_id'],
    ['attributes.htt', 'children, still filtered by the caller', (l) => l.join() === 'http,db,user_id'],
    ['attributes.http.', 'grandchildren', (l) => l.join() === 'request'],
    ['status_code ', 'operators once the field is named', (l) => l[0] === '==' && l.includes('contains')],
    ['status_code == ', 'that field\'s values', (l) => l.join() === 'OK,ERROR'],
    ['level == "ERROR" ', 'clause joiners', (l) => l.join() === 'and,or,|'],
    ['duration > 500 ', 'joiners after a bare number too', (l) => l.join() === 'and,or,|'],
    ['level == "ERROR" and ', 'a fresh field after a joiner', (l) => l.includes('kind') && !l.includes('==')],
    ['kind in ', 'a list snippet, not raw values', (l) => l.join() === '("...", "...")'],
    ['kind has_any ', 'an array snippet', (l) => l.join() === '["...", "..."]'],
    ['spans ', 'commands plus fields once a source is set', (l) => l.includes('summarize') && l.includes('kind')],
    ['spans | stats ', 'aggregation functions', (l) => l.includes('count') && l.includes('p95')],
    ['spans | stats count() by ', 'fields to group by', (l) => l.includes('kind') && !l.includes('count')],
    ['spans | timechart ', 'bin intervals alongside functions', (l) => l.includes('[5m]') && l.includes('count')],
  ];

  test.each(cases)('%j → %s', async (text, _desc, holds) => {
    expect(holds(await labels(text))).toBe(true);
  });
});

describe('the bug that started this: a bare field name', () => {
  // Typing `attributes` used to offer only the table names, which read as "you
  // must prefix the query with spans/metrics". Fields must come first, and the
  // tables must not crowd them out.
  test('offers fields, not just table names', async () => {
    const l = await labels('attributes');
    expect(l).toContain('attributes');
    expect(l.indexOf('attributes')).toBeLessThan(l.indexOf('spans') === -1 ? Infinity : l.indexOf('spans'));
  });

  test('still offers a table name when the word is actually a table prefix', async () => {
    expect(await labels('sp')).toContain('spans');
  });

  test('a chosen table is not re-offered as a field prefix', async () => {
    expect(await labels('spans ')).not.toContain('spans');
  });
});

describe('insert text is what makes the next keystroke work', () => {
  test('object fields complete to a trailing dot so children open next', async () => {
    expect((await find('', 'attributes'))?.insertText).toBe('attributes.');
    expect((await find('', 'errors'))?.insertText).toBe('errors.');
  });

  test('leaf fields complete to a trailing space so an operator comes next', async () => {
    expect((await find('', 'kind'))?.insertText).toBe('kind ');
  });

  test('values are quoted, and a description rides along', async () => {
    const err = await find('status_code == ', 'ERROR');
    expect(err?.insertText).toBe('"ERROR" ');
    expect(err?.detail).toBe('the span failed');
  });

  test('operators carry their meaning for the detail column', async () => {
    expect((await find('kind ', 'contains'))?.detail).toBe('contains substring');
  });
});

describe('ordering puts the likely choice first', () => {
  test('== leads the operator list', async () => {
    expect((await labels('kind '))[0]).toBe('==');
  });

  test('common filter fields sort ahead of the rest', async () => {
    const suggestions = await computeSuggestions('', schema());
    const sorted = [...suggestions].sort((a, b) => (a.sortText ?? '').localeCompare(b.sortText ?? ''));
    expect(sorted.slice(0, 3).map((s) => s.label)).toEqual(['status_code', 'level', 'kind']);
  });
});

describe('kinds are tagged for the badge column', () => {
  test.each([
    ['', ['field', 'table']],
    ['kind ', ['operator']],
    ['status_code == ', ['value']],
    ['spans | stats ', ['function', 'keyword']],
    ['kind in ', ['snippet']],
  ])('%j is tagged %j', async (text, expected) => {
    expect(await kinds(text)).toEqual(expected);
  });
});

describe('degrades safely', () => {
  test('an unloaded schema yields no field suggestions rather than throwing', async () => {
    const empty = schema({ fields: async () => [] });
    expect(await labels('attrib', empty)).toEqual(['spans', 'metrics'].filter((t) => t.startsWith('attrib')));
    expect(await labels('attributes.', empty)).toEqual([]);
  });

  test('an unknown field offers no invented values', async () => {
    expect(await labels('nosuchfield == ')).toEqual([]);
  });

  test('a metrics query resolves against the metrics table', async () => {
    const seen: string[] = [];
    await computeSuggestions('metrics | where ', schema({ fields: async (t) => (seen.push(t), []) }));
    expect(seen).toEqual(['metrics']);
  });

  test('never returns undefined entries, whatever the input', async () => {
    const inputs = ['', ' ', '|', '||', '.', '..', '"', 'a.', '== ', 'kind ==', 'kind == "', 'spans |', 'x'.repeat(500)];
    for (const text of inputs) {
      const out = await computeSuggestions(text, schema());
      expect(Array.isArray(out)).toBe(true);
      expect(out.every((s) => typeof s.label === 'string' && typeof s.insertText === 'string')).toBe(true);
    }
  });
});

describe('filterSuggestions', () => {
  const items: Suggestion[] = [
    { label: 'kind', kind: 'field', insertText: 'kind ' },
    { label: 'attributes', kind: 'field', insertText: 'attributes.' },
    { label: 'attributes.http.request.method', kind: 'field', insertText: 'x' },
    { label: 'status_code', kind: 'field', insertText: 'x' },
  ];

  test('empty word keeps everything', () => {
    expect(filterSuggestions(items, '')).toHaveLength(4);
  });

  test('prefix matches come before substring matches', () => {
    expect(filterSuggestions(items, 'http').map((s) => s.label)).toEqual(['attributes.http.request.method']);
    expect(filterSuggestions(items, 'attr').map((s) => s.label)).toEqual(['attributes', 'attributes.http.request.method']);
  });

  test('matching is case-insensitive', () => {
    expect(filterSuggestions(items, 'KIND').map((s) => s.label)).toEqual(['kind']);
  });

  test('no match yields nothing rather than everything', () => {
    expect(filterSuggestions(items, 'zzz')).toEqual([]);
  });
});

describe('wordAtCursor decides what a completion replaces', () => {
  test.each([
    ['', ''],
    ['kin', 'kin'],
    ['level == "ERROR" and kin', 'kin'],
    ['attributes.', ''],
    ['attributes.htt', 'htt'],
    ['attributes.http.req', 'req'],
    ['kind == ', ''],
    // Inside a value literal the partial value is the word, so the list filters by it.
    ['kind == "GE', 'GE'],
  ])('%j → %j', (text, expected) => {
    expect(wordAtCursor(text)).toBe(expected);
  });
});
