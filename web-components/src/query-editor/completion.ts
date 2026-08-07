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
export const SUGGESTION_OPERATORS = ['==', '!=', '>', '<', '>=', '<=', '=~', 'in', '!in', 'has', '!has', 'has_any', 'has_all', 'contains', '!contains', 'startswith', '!startswith', 'endswith', '!endswith', 'matches'];

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

const REGEX = {
  dotMatchEnd: /([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\.$/,
  dotMatchPartial: /([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\.([a-zA-Z0-9_]*)$/,
  operatorMatch: /([\w.]+)\s*(==|!=|>=|<=|>|<|=~|!in|in|has_any|has_all|!has|has|!contains|contains|!startswith|startswith|!endswith|endswith|matches)\s*$/,
  afterQuotedValue: /".*"\s*$/,
  afterNumericValue: /\d+\s*$/,
  fieldSpace: /([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\s+$/,
  logicalOperator: /\b(and|or|not)\s+$/i,
  aggregationSegment: /^(summarize|stats|timechart)\b/i,
  byKeyword: /\bby\s*$/i,
  timechartKeyword: /timechart/i,
  digit: /\d/,
};

const fieldSortText = (name: string) => {
  const i = PRIORITY_FIELDS.indexOf(name);
  return i >= 0 ? `0_${String(i).padStart(2, '0')}` : `1_${name}`;
};

const operatorSortText = (op: string, i: number) => `${op === '==' ? '0' : '1'}_${String(i).padStart(2, '0')}`;

const fieldSuggestion = (f: CompletionField): Suggestion => ({
  label: f.name,
  kind: 'field',
  detail: f.type,
  documentation: f.examples?.join(', '),
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
  const segments = text.split(/\|/).map((s) => s.trim());
  const last = segments[segments.length - 1];
  const tables = schema.tables();
  const firstToken = text.trim().split(/\s+/)[0]?.toLowerCase() ?? '';
  const table = tables.includes(firstToken) ? firstToken : schema.defaultTable();
  const lastChar = text.charAt(text.length - 1);

  // `attributes.` / `attributes.htt` — children of the prefix.
  const dotMatch = text.includes('.') ? text.match(REGEX.dotMatchEnd) || text.match(REGEX.dotMatchPartial) : null;
  if (dotMatch) {
    const nested = await schema.fields(table, dotMatch[1]);
    return nested.map(fieldSuggestion);
  }

  // `status_code == ` — values for the field on the left.
  const operatorMatch = lastChar === ' ' ? text.match(REGEX.operatorMatch) : null;
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

  // `level == "ERROR" ` — join this clause to the next one.
  if (lastChar === ' ') {
    const trimmed = text.trimEnd();
    const lastCharTrimmed = trimmed.charAt(trimmed.length - 1);
    if (lastCharTrimmed === '"' || REGEX.digit.test(lastCharTrimmed)) {
      if (REGEX.afterQuotedValue.test(text) || REGEX.afterNumericValue.test(text)) {
        return ['and', 'or', '|'].map((op) => ({ label: op, kind: 'operator' as const, insertText: `${op} ` }));
      }
    }
  }

  // `... and ` — a new field starts here.
  const logicalOperatorMatch = lastChar === ' ' ? text.match(REGEX.logicalOperator) : null;
  if (logicalOperatorMatch) {
    return (await schema.fields(table, '')).map(fieldSuggestion);
  }

  // `spans ` — the source is chosen; offer commands and its fields. Checked
  // before the field-then-operator rule below, which would otherwise read the
  // table name as a field and offer `==`.
  if (segments.length === 1 && tables.includes(last.toLowerCase())) {
    const commands: Suggestion[] = [...AGGREGATION_COMMANDS, 'limit'].map((k) => ({ label: k, kind: 'keyword' as const, insertText: `${k} ` }));
    return [...commands, ...(await schema.fields(table, '')).map(fieldSuggestion)];
  }

  // `status_code ` — the field is named, an operator comes next. A command word
  // (`where `, `by `) is not a field, so it falls through to the field list.
  const fieldSpaceMatch = lastChar === ' ' ? text.match(REGEX.fieldSpace) : null;
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
  const m = text.match(/[a-zA-Z_][a-zA-Z0-9_.]*$/);
  if (!m) return '';
  // After a dot only the segment being typed is replaced: `attributes.htt` -> `htt`.
  const word = m[0];
  const dot = word.lastIndexOf('.');
  return dot >= 0 ? word.slice(dot + 1) : word;
}
