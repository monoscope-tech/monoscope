import type { CompletionField } from './completion';
export type FieldValue = string | number | boolean;
export interface FieldInfo {
  type?: string;
  field_type?: string;
  description?: string;
  examples?: FieldValue[];
  enum?: FieldValue[];
  fields?: Record<string, FieldInfo>;
  properties?: Record<string, FieldInfo>;
  items?: FieldInfo;
}
export interface SchemaData {
  fields: Record<string, FieldInfo>;
  properties?: Record<string, SchemaData>;
  operators?: Record<string, string[]>;
}

interface Node {
  field: CompletionField;
  children: Map<string, Node>;
}
/** One pass over path segments, without constructing or hashing every full prefix. */
export class SchemaIndex {
  private root: Node = { field: { name: '', type: 'object' }, children: new Map() };
  private lists = new WeakMap<Node, CompletionField[]>();
  readonly data: SchemaData;
  constructor(data: SchemaData) {
    this.data = data;
    for (const [path, info] of Object.entries(data.fields)) {
      let parent = this.root;
      for (const name of path.split('.')) {
        let node = parent.children.get(name);
        if (!node) {
          node = { field: { name, type: 'object' }, children: new Map() };
          parent.children.set(name, node);
          parent.field.fields = {};
        }
        parent = node;
      }
      parent.field.type = info.type || info.field_type || 'string';
      parent.field.examples = info.examples?.length ? info.examples : info.enum;
      if (parent.field.type === 'object') parent.field.fields = {};
    }
  }
  fields(prefix = ''): CompletionField[] {
    let node: Node | undefined = this.root;
    for (const part of prefix ? prefix.split('.') : []) node = node?.children.get(part);
    if (!node) return [];
    let fields = this.lists.get(node);
    if (!fields) {
      fields = Array.from(node.children.values(), (child) => child.field);
      this.lists.set(node, fields);
    }
    return fields;
  }
  values(field: string): string[] {
    const info = this.data.fields[field];
    const examples = info?.examples?.length ? info.examples : info?.enum;
    if (examples?.length) return examples.map(String);
    const constants: Record<string, string[]> = {
      status_code: ['OK', 'ERROR', 'UNSET'],
      kind: ['logs', 'span', 'request'],
      level: ['TRACE', 'DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL'],
      'severity.text': ['TRACE', 'DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL'],
    };
    if (constants[field]) return constants[field];
    if (field === 'timestamp' || field === 'observed_timestamp')
      return [
        'ago(1h)|1 hour ago',
        'ago(30m)|30 minutes ago',
        'ago(6h)|6 hours ago',
        'ago(1d)|1 day ago',
        'ago(7d)|7 days ago',
        'now()|current time',
      ];
    return (info?.type || info?.field_type) === 'boolean' ? ['true', 'false'] : [];
  }
}
