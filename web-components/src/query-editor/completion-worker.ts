import { SchemaIndex, type SchemaData } from './schema-index';
import { computeSuggestions, topSuggestions, wordAtCursor, librarySuggestions, type LibraryItem } from './completion';

export type Request =
  | { id: number; key: string; op: 'schema'; data: SchemaData }
  | { id: number; key: string; op: 'load'; url: string }
  | { id: number; key: string; op: 'fields' | 'values'; table: string; prefix: string }
  | { id: number; key: string; op: 'complete'; table: string; text: string; client: number }
  | { id: number; key: string; op: 'release'; client: number }
  | { id: number; key: string; op: 'library'; client: number; items: LibraryItem[] };
const libraries = new Map<number, LibraryItem[]>();
const versions = new Map<string, number>();
const schemas = new Map<string, SchemaIndex>();
const loads = new Map<string, Promise<SchemaData>>();
const pending = new Map<number, Extract<Request, { op: 'complete' }>>();
const reply = (id: number, value: unknown) => postMessage({ id, value });
async function run(request: Request) {
  const { id, key, op } = request;
  try {
    if (op === 'library') {
      libraries.set(request.client, request.items);
      reply(id, null);
      return;
    }
    if (op === 'schema') {
      versions.set(key, (versions.get(key) || 0) + 1);
      loads.delete(key);
      schemas.set(key, new SchemaIndex(request.data));
      reply(id, null);
      return;
    }
    if (op === 'load') {
      let load = loads.get(key);
      if (!load) {
        const version = versions.get(key) || 0;
        load = fetch(request.url, { credentials: 'include', headers: { Accept: 'application/json' } }).then(async (r) => {
          if (!r.ok) throw new Error(`Schema request failed: ${r.status}`);
          const data = (await r.json()) as SchemaData;
          if ((versions.get(key) || 0) !== version) return schemas.get(key)?.data || data;
          schemas.set(key, new SchemaIndex(data));
          return data;
        });
        loads.set(key, load);
      }
      try {
        reply(id, await load);
      } catch (e) {
        loads.delete(key);
        throw e;
      }
      return;
    }
    if (op === 'release') {
      libraries.delete(request.client);
      const old = pending.get(request.client);
      if (old) reply(old.id, []);
      pending.delete(request.client);
      reply(id, null);
      return;
    }
    const indexFor = (table: string) => schemas.get(`${key}:${table}`) || schemas.get(`${key}:spans`);
    if (op === 'fields') {
      reply(id, indexFor(request.table)?.fields(request.prefix) || []);
      return;
    }
    if (op === 'values') {
      reply(id, indexFor(request.table)?.values(request.prefix) || []);
      return;
    }
    if (request.op !== 'complete') return;
    const suggestions = await computeSuggestions(request.text, {
      tables: () => ['spans', 'metrics'],
      defaultTable: () => request.table,
      fields: async (table, prefix) => indexFor(table)?.fields(prefix) || [],
      values: async (table, field) => indexFor(table)?.values(field) || [],
    });
    const top = topSuggestions(suggestions, wordAtCursor(request.text));
    reply(
      id,
      !top.length || top.some((s) => s.kind === 'field')
        ? [...top, ...librarySuggestions(libraries.get(request.client) || [], request.text)]
        : top
    );
  } catch (e) {
    postMessage({ id, error: String(e) });
  }
}
onmessage = ({ data }: MessageEvent<Request>) => {
  if (data.op !== 'complete') {
    void run(data);
    return;
  }
  const previous = pending.get(data.client);
  if (previous) reply(previous.id, []);
  pending.set(data.client, data);
  if (!previous)
    setTimeout(() => {
      const next = pending.get(data.client);
      pending.delete(data.client);
      if (next) void run(next);
    }, 0);
};
