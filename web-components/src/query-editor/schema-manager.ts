import CompletionWorker from './completion-worker?worker';
import { SchemaIndex, type SchemaData, type FieldInfo } from './schema-index';
import {
  computeSuggestions,
  topSuggestions,
  wordAtCursor,
  type CompletionField,
  type Suggestion,
  librarySuggestions,
  type LibraryItem,
} from './completion';
import type { Request } from './completion-worker';
export type { SchemaData, FieldInfo } from './schema-index';
type WorkerRequest = Request extends infer R ? (R extends { id: number } ? Omit<R, 'id'> : never) : never;
type Resolver = (schema: string, prefix: string) => Promise<CompletionField[]>;

export class SchemaManager {
  private libraries = new Map<number, LibraryItem[]>();
  private versions = new Map<string, number>();
  private data = new Map<string, SchemaData>();
  private local = new Map<string, SchemaIndex>();
  private worker: Worker | null = null;
  private requests = new Map<number, { resolve: (value: any) => void; reject: (reason: Error) => void }>();
  private sequence = 0;
  private clients = new Set<number>();
  acquire = (client: number) => {
    this.clients.add(client);
  };
  private revision = 0;
  private schemas = ['spans', 'metrics'];
  private defaultSchema = 'spans';
  private nestedResolver?: Resolver;
  private valueResolver?: (schema: string, field: string) => Promise<string[]>;
  private listeners = new Set<() => void>();
  private loads = new Map<string, Promise<void>>();
  private key(table: string, project = '') {
    return `${project}:${table}`;
  }
  private rpc<T>(request: WorkerRequest): Promise<T> {
    if (!this.worker) {
      this.worker = new CompletionWorker();
      this.worker.onmessage = ({ data }) => {
        const pending = this.requests.get(data.id);
        this.requests.delete(data.id);
        if (data.error) pending?.reject(new Error(data.error));
        else pending?.resolve(data.value);
      };
      this.worker.onerror = () => {
        for (const pending of this.requests.values()) pending.reject(new Error('Completion worker failed'));
        this.requests.clear();
        this.worker?.terminate();
        this.worker = null;
      };
      for (const [client, items] of this.libraries) this.worker.postMessage({ op: 'library', id: 0, key: '', client, items });
      for (const [key, data] of this.data) {
        if (request.op !== 'schema' || request.key !== key) this.worker.postMessage({ op: 'schema', id: 0, key, data });
      }
    }
    const id = ++this.sequence;
    return new Promise<T>((resolve, reject) => {
      this.requests.set(id, { resolve, reject });
      this.worker!.postMessage({ ...request, id });
    });
  }
  getRevision = () => this.revision;
  subscribe = (listener: () => void) => {
    this.listeners.add(listener);
    return () => this.listeners.delete(listener);
  };
  private changed(table: string) {
    this.revision++;
    for (const listener of this.listeners) listener();
    if (typeof document !== 'undefined') document.body.dispatchEvent(new CustomEvent('schema-loaded', { detail: table }));
  }
  setSchemas = (tables: string[]) => {
    this.schemas = tables;
  };
  getSchemas = () => this.schemas;
  setDefaultSchema = (table: string) => {
    if (this.schemas.includes(table)) this.defaultSchema = table;
  };
  getDefaultSchema = () => this.defaultSchema;
  getSchemaData = (table: string, project = '') => this.data.get(this.key(table, project));
  setSchemaData = (table: string, data: SchemaData, project = '') => {
    const key = this.key(table, project);
    if (this.data.get(key) === data) return;
    this.versions.set(key, (this.versions.get(key) || 0) + 1);
    this.data.set(key, data);
    this.local.delete(key);
    if (typeof Worker !== 'undefined') void this.rpc({ op: 'schema', key, data }).catch(() => {});
    this.changed(table);
  };
  load = async (table: string, url: string, project = ''): Promise<void> => {
    const key = this.key(table, project);
    if (this.data.has(key)) return;
    let load = this.loads.get(key);
    if (!load) {
      const version = this.versions.get(key) || 0;
      load = this.rpc<SchemaData>({ op: 'load', key, url: new URL(url, location.href).href })
        .then((data) => {
          if ((this.versions.get(key) || 0) !== version) return;
          this.data.set(key, data);
          this.changed(table);
        })
        .catch((e) => {
          this.loads.delete(key);
          throw e;
        });
      this.loads.set(key, load);
    }
    return load;
  };
  private index(table: string, project = '') {
    let key = this.key(table, project);
    if (!this.data.has(key)) key = this.key(this.defaultSchema, project);
    let index = this.local.get(key);
    if (!index) {
      index = new SchemaIndex(this.data.get(key) || { fields: {} });
      this.local.set(key, index);
    }
    return index;
  }
  resolveNested = async (table: string, prefix: string, project = ''): Promise<CompletionField[]> => {
    if (this.nestedResolver) return this.nestedResolver(table, prefix);
    if (typeof Worker === 'undefined') return this.index(table, project).fields(prefix);
    return this.rpc({ op: 'fields', key: project, table, prefix });
  };
  resolveValues = async (table: string, field: string, project = ''): Promise<string[]> => {
    if (this.valueResolver) return this.valueResolver(table, field);
    if (typeof Worker === 'undefined') return this.index(table, project).values(field);
    return this.rpc({ op: 'values', key: project, table, prefix: field });
  };
  complete = async (text: string, table: string, project: string, client: number): Promise<Suggestion[]> => {
    if (typeof Worker !== 'undefined' && !this.nestedResolver && !this.valueResolver)
      return this.rpc({ op: 'complete', key: project, table, text, client });
    const suggestions = await computeSuggestions(text, {
      tables: this.getSchemas,
      defaultTable: () => table,
      fields: (t, p) => this.resolveNested(t, p, project),
      values: (t, f) => this.resolveValues(t, f, project),
    });
    const top = topSuggestions(suggestions, wordAtCursor(text));
    return !top.length || top.some((s) => s.kind === 'field')
      ? [...top, ...librarySuggestions(this.libraries.get(client) || [], text)]
      : top;
  };
  release = (client: number) => {
    this.clients.delete(client);
    this.libraries.delete(client);
    if (!this.clients.size) {
      this.worker?.terminate();
      this.worker = null;
      for (const request of this.requests.values()) request.reject(new Error('Editor disconnected'));
      this.requests.clear();
      this.loads.clear();
      while (this.data.size > 8) {
        const key = this.data.keys().next().value!;
        this.data.delete(key);
        this.local.delete(key);
        this.versions.delete(key);
      }
    } else if (this.worker) void this.rpc({ op: 'release', key: '', client }).catch(() => {});
  };
  setLibrary = (client: number, items: LibraryItem[]) => {
    this.libraries.set(client, items);
    if (this.worker) void this.rpc({ op: 'library', key: '', client, items }).catch(() => {});
  };
  setNestedResolver = (fn: Resolver) => {
    this.nestedResolver = fn;
    this.revision++;
  };
  setValueResolver = (fn: (table: string, field: string) => Promise<string[]>) => {
    this.valueResolver = fn;
    this.revision++;
  };
  setSchema = (schema: Partial<SchemaData>) => {
    if (schema.fields) this.setSchemaData(this.defaultSchema, { fields: schema.fields });
  };
  setDynamicResolver = (fn: (path: string[]) => Promise<{ name: string; info: FieldInfo }[]>) => {
    this.setNestedResolver(async (_table, prefix) =>
      (await fn(prefix ? prefix.split('.') : [])).map((f) => ({
        name: f.name,
        type: f.info.type || 'string',
        examples: f.info.examples,
        fields: f.info.fields,
      }))
    );
  };
  getFieldSuggestions = async (table = this.defaultSchema) =>
    (await this.resolveNested(table, '')).map((f) => ({ name: f.name, type: f.type, description: f.examples?.join(', ') }));
  getRootFields = async () => (await this.resolveNested(this.defaultSchema, '')).map((f) => ({ name: f.name, info: f }));
  resolveNestedFields = async (path: string[]) =>
    (await this.resolveNested(this.defaultSchema, path.join('.'))).map((f) => ({ name: f.name, info: f }));
}
export const schemaManager = new SchemaManager();
if (typeof window !== 'undefined') (window as any).schemaManager = schemaManager;
