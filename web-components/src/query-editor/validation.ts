export interface QueryError {
  message: string;
  startColumn: number;
  endColumn: number;
  line: number;
}

export function unclosedQuote(query: string): QueryError | null {
  let quote = '',
    escaped = false,
    start = 0,
    line = 1,
    column = 1,
    quoteLine = 1,
    quoteColumn = 1;
  for (let i = 0; i < query.length; i++) {
    const c = query[i];
    if (!quote && (c === '"' || c === "'")) {
      quote = c;
      start = i;
      quoteLine = line;
      quoteColumn = column;
    } else if (quote) {
      if (escaped) escaped = false;
      else if (c === '\\') escaped = true;
      else if (c === quote) quote = '';
    }
    if (c === '\n') {
      line++;
      column = 1;
    } else column++;
  }
  const lineEnd = query.indexOf('\n', start);
  return quote
    ? {
        message: `Unclosed ${quote === '"' ? 'double' : 'single'} quote`,
        startColumn: quoteColumn,
        endColumn: quoteColumn + (lineEnd < 0 ? query.length : lineEnd) - start,
        line: quoteLine,
      }
    : null;
}

/** Shape of the /log_explorer/validate response. */
export interface Verdict {
  valid: boolean;
  message?: string;
  column?: number;
  width?: number;
}

export const verdictToError = (v: Verdict): QueryError | null =>
  v.valid || !v.message ? null : { message: v.message, startColumn: v.column ?? 1, endColumn: (v.column ?? 1) + (v.width ?? 1), line: 1 };
