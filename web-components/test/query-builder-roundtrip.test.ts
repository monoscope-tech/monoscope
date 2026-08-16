// The visual query builder reads a KQL query into UI state and writes it back.
//
// Both halves are regex-driven string surgery on the user's own query text, and they are
// the only path by which opening a dropdown can silently rewrite a query someone typed by
// hand. The round-trip — parse, change nothing, re-serialize — must be the identity, and
// editing one clause must not disturb the others. None of it was tested.
import { describe, test, expect, beforeEach } from 'vitest';
import '../src/query-editor/query-builder';

// The builder talks to the editor through `document.querySelector(selector).editor`,
// so a minimal stand-in exercises the real read/write path.
const withQuery = async (initial: string) => {
  document.body.innerHTML = '';
  const editor = document.createElement('div');
  editor.id = 'filterElement';
  let value = initial;
  // `handleAddQuery(text, replace)` is how the builder writes back — the same entry
  // point the editor exposes to every other caller.
  (editor as any).editor = { getValue: () => value, setValue: (v: string) => (value = v) };
  (editor as any).handleAddQuery = (fragment: string, replace = false) => {
    value = replace ? fragment : `${value} ${fragment}`.trim();
  };
  document.body.appendChild(editor);

  const builder = document.createElement('query-builder') as any;
  document.body.appendChild(builder);
  await builder.updateComplete;
  return {
    builder,
    read: () => (editor as any).editor.getValue(),
    parse: () => builder.extractQueryParts(),
    write: () => builder.updateQuery(),
  };
};

// Whitespace around pipes is not meaningful; compare on the shape, not the spacing.
const normalize = (q: string) =>
  q
    .split('|')
    .map((s) => s.trim().replace(/\s+/g, ' '))
    .filter(Boolean)
    .join(' | ');

beforeEach(() => {
  document.body.innerHTML = '';
});

describe('reading a query into the builder', () => {
  test('picks up the aggregations and the fields they group by', async () => {
    const q = await withQuery('status_code == 500 | summarize count() by service_name');
    q.parse();

    expect(q.builder.aggregations).toEqual([{ function: 'count', field: '' }]);
    expect(q.builder.groupByFields).toEqual(['service_name']);
  });

  test('a group-by list splits on top-level commas only, not inside a function call', async () => {
    const q = await withQuery('* | summarize count() by bin(timestamp, 5m), service_name');
    q.parse();

    expect(q.builder.groupByFields).toEqual(['bin(timestamp, 5m)', 'service_name']);
  });

  test('reads sort direction and limit', async () => {
    const q = await withQuery('* | sort by duration desc | take 25');
    q.parse();

    expect(q.builder.sortFields).toEqual([{ field: 'duration', direction: 'desc' }]);
    expect(q.builder.limitValue).toBe(25);
  });

  test('a plain filter leaves every clause empty rather than inventing one', async () => {
    const q = await withQuery('service_name == "api"');
    q.parse();

    expect(q.builder.aggregations).toEqual([]);
    expect(q.builder.groupByFields).toEqual([]);
    expect(q.builder.sortFields).toEqual([]);
    expect(q.builder.limitValue).toBeNull();
  });
});

describe('round-trip: parsing then writing back changes nothing', () => {
  test.each([
    ['a bare filter', 'service_name == "api"'],
    ['an aggregation', '* | summarize count() by service_name'],
    ['several aggregations', '* | summarize count(), avg(duration) by service_name'],
    ['a binned timeseries', '* | summarize count() by bin(timestamp, 5m)'],
    ['auto binning', '* | summarize count() by bin_auto(timestamp)'],
    ['sort and take', '* | sort by duration desc | take 50'],
    ['filter, aggregate, sort and take', 'status_code >= 400 | summarize count() by service_name | sort by service_name asc | take 10'],
    ['a multi-field group by', '* | summarize count() by service_name, status_code'],
  ])('%s survives a round-trip unchanged', async (_label, query) => {
    const q = await withQuery(query);

    q.parse();
    q.write();

    expect(normalize(q.read())).toBe(normalize(query));
  });

  test('round-tripping twice is still stable', async () => {
    const query = 'status_code >= 400 | summarize count() by service_name | sort by service_name asc | take 10';
    const q = await withQuery(query);

    q.parse();
    q.write();
    const once = q.read();
    q.parse();
    q.write();

    expect(normalize(q.read())).toBe(normalize(once));
  });
});

describe('editing one clause leaves the rest of the query alone', () => {
  test('adding a limit keeps the filter and the aggregation', async () => {
    const q = await withQuery('status_code == 500 | summarize count() by service_name');
    q.parse();

    q.builder.limitValue = 100;
    q.write();

    const out = normalize(q.read());
    expect(out).toContain('status_code == 500');
    expect(out).toContain('summarize count() by service_name');
    expect(out).toContain('take 100');
  });

  test('clearing the aggregation drops the summarize and keeps the filter', async () => {
    const q = await withQuery('status_code == 500 | summarize count() by service_name');
    q.parse();

    q.builder.aggregations = [];
    q.builder.groupByFields = [];
    q.write();

    const out = normalize(q.read());
    expect(out).toBe('status_code == 500');
  });

  test('changing the sort replaces the existing clause instead of appending a second', async () => {
    const q = await withQuery('* | sort by duration desc');
    q.parse();

    q.builder.sortFields = [{ field: 'timestamp', direction: 'asc' }];
    q.write();

    const out = q.read();
    expect(out).toContain('sort by timestamp asc');
    expect(out).not.toContain('duration desc');
    expect(out.match(/sort by/gi) ?? []).toHaveLength(1);
  });

  test('a rewritten query never leaves an empty pipe segment behind', async () => {
    const q = await withQuery('service_name == "api" | summarize count() by service_name | sort by service_name asc | take 5');
    q.parse();

    q.builder.aggregations = [];
    q.builder.groupByFields = [];
    q.builder.sortFields = [];
    q.builder.limitValue = null;
    q.write();

    const out = q.read();
    expect(out).not.toMatch(/\|\s*\|/);
    expect(out.trim()).not.toMatch(/\|\s*$/);
  });
});
