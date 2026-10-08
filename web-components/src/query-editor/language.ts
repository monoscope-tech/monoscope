import { StreamLanguage, HighlightStyle } from '@codemirror/language';
import { tags } from '@lezer/highlight';
import { DATA_SOURCES, AGGREGATION_COMMANDS, STATS_FUNCTIONS } from './completion';

const keywords = new Set([...DATA_SOURCES, ...AGGREGATION_COMMANDS, ...STATS_FUNCTIONS, 'and', 'or', 'not', 'by', 'as']);
export const language = StreamLanguage.define<{ quote: string; comment: boolean }>({
  startState: () => ({ quote: '', comment: false }),
  token(stream, state) {
    if (!state.quote && (state.comment || stream.match('/_'))) {
      state.comment = true;
      while (!stream.eol()) { if (stream.match('_/')) { state.comment = false; break; } stream.next(); }
      return 'comment';
    }
    if (stream.eatSpace()) return null;
    if (!state.quote && stream.match('//')) {
      stream.skipToEnd();
      return 'comment';
    }
    if (!state.quote && (stream.peek() === '"' || stream.peek() === "'")) state.quote = stream.next()!;
    if (state.quote) {
      let escaped = false,
        c;
      while ((c = stream.next()) != null) {
        if (!escaped && c === state.quote) {
          state.quote = '';
          break;
        }
        if (!escaped && c === '\\') escaped = true;
        else escaped = false;
      }
      return 'string';
    }
    if (stream.match(/^\d+(?:\.\d+)?(?:[eE][+-]?\d+)?(?:ns|µs|us|ms|s|m|h|d|w)?/)) return 'number';
    if (stream.match(/^[a-zA-Z_][\w.]*/)) return keywords.has(stream.current().toLowerCase()) ? 'keyword' : 'variableName';
    if (stream.match(/^[=><!~|]+/)) return 'operator';
    stream.next();
    return null;
  },
});
export const darkHighlightStyle = HighlightStyle.define([
  { tag: tags.keyword, color: '#569cd6' },
  { tag: tags.string, color: '#ce9178' },
  { tag: tags.number, color: '#b5cea8' },
  { tag: tags.comment, color: '#6a9955' },
]);
