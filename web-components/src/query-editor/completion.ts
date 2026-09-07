// KQL completion logic, deliberately free of Monaco and the DOM.
//
// It used to live inside a `registerCompletionItemProvider` callback, and the
// dropdown mirrored Monaco's results by subscribing to private suggest-controller
// internals. That copy silently went stale (a zero-item result was ignored, so an
// old list stayed on screen) and could not be tested without mounting an editor.
// Everything here is a pure function of (text before the cursor, schema), so the
// dropdown reads it directly and the rules are unit-testable in isolation.

export type SuggestionKind = 'field' | 'operator' | 'value' | 'keyword' | 'function' | 'snippet' | 'table';

export interface Suggestion {
  label: string;
  insertText: string;
  kind: SuggestionKind;
  detail?: string;
  documentation?: string;
  /** Ordering hint; the caller sorts on it. */
  sortText?: string;
  fullQuery?: string;
  section?: string;
}

export interface CompletionField {
  name: string;
  type: string;
  examples?: (string | number | boolean)[];
  /** Present when the field has children, so it completes as `name.` */
  fields?: unknown;
}

/** The schema reads completion needs. Injected so tests can supply a fixture. */
export interface SchemaAccess {
  tables: () => string[];
  defaultTable: () => string;
  /** Fields directly under `prefix` ('' for top level). */
  fields: (table: string, prefix: string) => Promise<CompletionField[]>;
  /** Known values for a field, optionally `value|description`. */
  values: (table: string, field: string) => Promise<string[]>;
}

export const DATA_SOURCES = ['spans', 'metrics'];
export const LOGICAL_OPERATORS = ['and', 'or', 'not', 'has', '!has', 'exists', '!exists'];
export const AGGREGATION_COMMANDS = ['summarize', 'timechart', 'stats', 'sort', 'take', 'project', 'extend', 'where'];
export const STATS_FUNCTIONS = ['count', 'sum', 'avg', 'min', 'max', 'median', 'stdev', 'range', 'p50', 'p75', 'p90', 'p95', 'p99', 'p100'];

// Offered left-to-right in this order; `==` first because it dominates real usage.
export const SUGGESTION_OPERATORS = [
  '==',
  '!=',
  '>',
  '<',
  '>=',
  '<=',
  '=~',
  'in',
  '!in',
  'has',
  '!has',
  'has_any',
  'has_all',
  'contains',
  '!contains',
  'startswith',
  '!startswith',
  'endswith',
  '!endswith',
  'matches',
];

export const OPERATOR_DETAILS: Record<string, string> = {
  '==': 'equals',
  '!=': 'not equals',
  '>': 'greater than',
  '<': 'less than',
  '>=': 'greater or equal',
  '<=': 'less or equal',
  '=~': 'regex match',
  in: 'in list',
  '!in': 'not in list',
  has: 'has token',
  '!has': 'not has token',
  has_any: 'has any of',
  has_all: 'has all of',
  contains: 'contains substring',
  '!contains': 'not contains',
  startswith: 'starts with',
  '!startswith': 'not starts with',
  endswith: 'ends with',
  '!endswith': 'not ends with',
  matches: 'regex matches',
};

// Common filter fields sort first; everything else keeps schema order after them.
const PRIORITY_FIELDS = ['status_code', 'level', 'kind', 'name', 'duration', 'timestamp', 'severity', 'body'];

// A trailing word that is one of these is a command or joiner, so the cursor
// after it starts a new field rather than waiting for an operator.
const NON_FIELD_WORDS = new Set([...AGGREGATION_COMMANDS, ...LOGICAL_OPERATORS, 'by', 'asc', 'desc', 'limit']);

const REGEX = { aggregationSegment: /^(summarize|stats|timechart)\b/i, byKeyword: /\bby\s*$/i, timechartKeyword: /timechart/i };

const fieldSortText = (name: string) => {
  const i = PRIORITY_FIELDS.indexOf(name);
  return i >= 0 ? `0_${String(i).padStart(2, '0')}` : `1_${name}`;
};

const operatorSortText = (op: string, i: number) => `${op === '==' ? '0' : '1'}_${String(i).padStart(2, '0')}`;

const fieldSuggestion = (f: CompletionField): Suggestion => ({
  label: f.name,
  kind: 'field',
  detail: f.type,
  // Examples are resolved only when completing a value, not for every field.
  // Object-ish fields complete to a trailing dot so the next keystroke opens their children.
  insertText: f.type === 'object' || f.fields ? `${f.name}.` : `${f.name} `,
  sortText: fieldSortText(f.name),
});

const operatorSuggestions = (): Suggestion[] =>
  SUGGESTION_OPERATORS.map((op, i) => ({
    label: op,
    kind: 'operator' as const,
    detail: OPERATOR_DETAILS[op],
    insertText: `${op} `,
    sortText: operatorSortText(op, i),
  }));

/**
 * Suggestions for a cursor sitting at the end of `text`.
 *
 * The branches are ordered most-specific first and each one returns: a cursor
 * after `attributes.` wants that field's children and nothing else, a cursor
 * after `==` wants values, and so on. Returning `[]` is a real answer — it means
 * "nothing applies here" and the caller must clear whatever it was showing.
 */
export async function computeSuggestions(text: string, schema: SchemaAccess): Promise<Suggestion[]> {
  const context = scanContext(text);
  const { tokens, segment, firstToken, inQuote } = context;
  const last = segment.trim();
  const segments = context.hasPipe ? ['', last] : [last];
  const tables = schema.tables();
  const table = tables.includes(firstToken.toLowerCase()) ? firstToken.toLowerCase() : schema.defaultTable();
  const lastChar = /\s/.test(text.at(-1) || '') ? ' ' : text.at(-1);
  const tail = tokens.at(-1) || '';
  if (inQuote) return [];
  // Only examine the current token; never retry a suffix regex at every input offset.
  const dot = lastChar !== ' ' ? tail.lastIndexOf('.') : -1;
  if (dot > 0 && /^[a-zA-Z_][a-zA-Z0-9_.]*$/.test(tail)) {
    return (await schema.fields(table, tail.slice(0, dot))).map(fieldSuggestion);
  }
  // `status_code == ` — values for the field on the left.
  const operatorMatch = lastChar === ' ' && SUGGESTION_OPERATORS.includes(tail) && tokens.length > 1 ? ['', tokens.at(-2)!, tail] : null;
  if (operatorMatch) {
    const [, fieldName, operator] = operatorMatch;
    if (operator === 'in' || operator === '!in') {
      return [{ label: '("...", "...")', kind: 'snippet', detail: 'comma-separated list', insertText: '("", "") ' }];
    }
    if (operator === 'has_any' || operator === 'has_all') {
      return [{ label: '["...", "..."]', kind: 'snippet', detail: 'comma-separated array', insertText: '["", ""] ' }];
    }
    const values = await schema.values(table, fieldName);
    return values.map((raw) => {
      const str = String(raw);
      const pipeIdx = str.indexOf('|');
      const value = pipeIdx > 0 ? str.substring(0, pipeIdx) : str;
      return {
        label: value,
        kind: 'value' as const,
        // Descriptions travel on the suggestion instead of a side-channel on the
        // schema manager, which only the widget knew to read.
        detail: pipeIdx > 0 ? str.substring(pipeIdx + 1) : undefined,
        insertText: value.includes('(') ? `${value} ` : `"${value}" `,
      };
    });
  }

  if (lastChar === ' ' && (/^["']/.test(tail) || /^\d/.test(tail))) {
    return ['and', 'or', '|'].map((op) => ({ label: op, kind: 'operator' as const, insertText: `${op} ` }));
  }

  // `... and ` — a new field starts here.
  const logicalOperatorMatch = lastChar === ' ' ? /^(and|or|not)$/i.exec(tail) : null;
  if (logicalOperatorMatch) {
    return (await schema.fields(table, '')).map(fieldSuggestion);
  }

  // `spans ` / `... | ` — a segment boundary: a command comes next, though a bare
  // filter is valid too, so fields follow. Checked before the field-then-operator
  // rule below, which would otherwise read the table name as a field and offer `==`.
  // The empty-last-segment half matters after a pipe, where nothing has been named
  // yet and the fallback at the end would otherwise offer comparison operators.
  if ((segments.length === 1 && tables.includes(last.toLowerCase())) || (segments.length > 1 && last === '')) {
    const commands: Suggestion[] = [...AGGREGATION_COMMANDS, 'limit'].map((k) => ({
      label: k,
      kind: 'keyword' as const,
      insertText: `${k} `,
    }));
    return [...commands, ...(await schema.fields(table, '')).map(fieldSuggestion)];
  }

  // `status_code ` — the field is named, an operator comes next. A command word
  // (`where `, `by `) is not a field, so it falls through to the field list.
  const fieldSpaceMatch = lastChar === ' ' ? /^([a-zA-Z_][a-zA-Z0-9_.]*)$/.exec(tail) : null;
  if (fieldSpaceMatch && !NON_FIELD_WORDS.has(fieldSpaceMatch[1].toLowerCase())) {
    return operatorSuggestions();
  }

  // Start of the query: fields first (the common case), table names after —
  // never table names *instead of* fields, which is what made a bare field name
  // look unsupported.
  if (segments.length === 1 && !tables.some((t) => last.toLowerCase().startsWith(t))) {
    const fields = (await schema.fields(table, '')).map(fieldSuggestion);
    const matchingTables: Suggestion[] = tables
      .filter((t) => last === '' || t.toLowerCase().startsWith(last.toLowerCase().trim()))
      .map((t) => ({ label: t, kind: 'table' as const, insertText: `${t} `, sortText: `2_${t}` }));
    return [...fields, ...matchingTables];
  }

  // `| summarize ...` — aggregation functions, `by`, and bin intervals.
  if (REGEX.aggregationSegment.test(last)) {
    // Directly after `by` only a grouping field makes sense — repeating the
    // aggregation functions there is noise the user has to look past.
    if (REGEX.byKeyword.test(last)) return (await schema.fields(table, '')).map(fieldSuggestion);

    const fns: Suggestion[] = STATS_FUNCTIONS.map((fn) => ({ label: fn, kind: 'function' as const, insertText: `${fn}(` }));
    const intervals: Suggestion[] = REGEX.timechartKeyword.test(last)
      ? ['[5m]', '[1h]'].map((iv) => ({ label: iv, kind: 'value' as const, insertText: iv }))
      : [];
    return [...fns, { label: 'by', kind: 'keyword', insertText: 'by ' }, ...intervals];
  }

  // Anywhere else in a filter segment: operators and fields both make sense.
  return [...operatorSuggestions(), ...(await schema.fields(table, '')).map(fieldSuggestion)];
}

/**
 * Narrow a suggestion list to the word being typed, the way an editor widget
 * would. Matching is case-insensitive and prefix-first, with substring matches
 * kept after them so `http` still finds `attributes.http.request.method`.
 *
 * >>> filterSuggestions(['kind','attributes'], 'kin') -> ['kind']
 */
export function filterSuggestions(suggestions: Suggestion[], word: string): Suggestion[] {
  if (!word) return suggestions;
  const w = word.toLowerCase();
  const prefix: Suggestion[] = [];
  const substring: Suggestion[] = [];
  for (const s of suggestions) {
    const label = s.label.toLowerCase();
    if (label.startsWith(w)) prefix.push(s);
    else if (label.includes(w)) substring.push(s);
  }
  return [...prefix, ...substring];
}

/** The partial word left of the cursor that a completion replaces. */
export function wordAtCursor(text: string): string {
  let start = text.length;
  while (start > 0 && /[a-zA-Z0-9_.]/.test(text[start - 1])) start--;
  const word = text.slice(start);
  if (!/^[a-zA-Z_]/.test(word)) return '';
  return word.slice(word.lastIndexOf('.') + 1);
}

/** Linear scanner: quoted pipes are data, escaped quotes do not end a string. */
export function scanContext(text: string) {
  const tokens: string[] = [];
  let firstToken = '',
    segmentStart = 0,
    start = -1,
    quote = '',
    escaped = false,
    hasPipe = false;
  const flush = (end: number) => {
    if (start < 0) return;
    const token = text.slice(start, end);
    if (!firstToken) firstToken = token;
    tokens.push(token);
    start = -1;
  };
  for (let i = 0; i < text.length; i++) {
    const c = text[i];
    if (quote) {
      if (escaped) escaped = false;
      else if (c === '\\') escaped = true;
      else if (c === quote) quote = '';
    } else if (c === '"' || c === "'") {
      if (start < 0) start = i;
      quote = c;
    } else if (c === '|') {
      flush(i);
      tokens.length = 0;
      segmentStart = i + 1;
      hasPipe = true;
    } else if (/\s/.test(c)) flush(i);
    else if (start < 0) start = i;
  }
  flush(text.length);
  return { tokens, firstToken, segment: text.slice(segmentStart), hasPipe, inQuote: !!quote };
}

export interface LibraryItem {
  query: string;
  label: string;
  section: string;
  search: string;
}
export function librarySuggestions(items: LibraryItem[], text: string): Suggestion[] {
  const term = scanContext(text).segment.trim().toLowerCase();
  const counts = new Map<string, number>();
  const result: Suggestion[] = [];
  for (const item of items) {
    if ((counts.get(item.section) || 0) >= 5 || !item.search.includes(term)) continue;
    counts.set(item.section, (counts.get(item.section) || 0) + 1);
    result.push({ label: item.label, insertText: item.query, fullQuery: item.query, section: item.section, kind: 'snippet' });
    if (result.length === 15) break;
  }
  return result;
}

/** Keep only the best candidates: bounded memory, without sorting a whole schema per key. */
export function topSuggestions(suggestions: Suggestion[], word: string, limit = 20): Suggestion[] {
  const top: { item: Suggestion; rank: string }[] = [];
  const term = word.toLowerCase();
  for (const item of suggestions) {
    const label = item.label.toLowerCase();
    if (term && !label.includes(term)) continue;
    const rank = `${term && !label.startsWith(term) ? '1' : '0'}${item.sortText || '\uffff'}`;
    if (top.length === limit && rank >= top[top.length - 1].rank) continue;
    let i = top.length;
    while (i > 0 && rank < top[i - 1].rank) i--;
    top.splice(i, 0, { item, rank });
    if (top.length > limit) top.pop();
  }
  return top.map((entry) => entry.item);
}
