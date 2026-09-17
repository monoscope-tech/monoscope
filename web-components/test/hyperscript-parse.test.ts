import { describe, it, expect, beforeAll } from 'vitest';
import { readFileSync, readdirSync, statSync } from 'fs';
import { join, relative } from 'path';

// Hyperscript in Lucid attributes is an opaque string to GHC: `[__|on change send
// tab-visible to #x|]` compiles fine and then fails to parse in every browser that
// loads the page. That exact snippet shipped and reached the issue list ("hyperscript:
// 1 parse error(s)"), because nothing between the quasiquoter and the user parses it.
//
// This parses every hyperscript literal in src/ with the same _hyperscript build the
// app serves.

const REPO = join(__dirname, '../..');
const HYPERSCRIPT_LIB = join(REPO, 'static/public/assets/js/thirdparty/_hyperscript_web0_9_93.min.js');

/** Parse errors for a snippet, empty when it is valid. */
let parseErrors: (src: string) => string[];

beforeAll(() => {
  // The vendored bundle is a browser script; evaluate it against jsdom's window.
  const code = readFileSync(HYPERSCRIPT_LIB, 'utf8');
  // eslint-disable-next-line no-new-func
  new Function(code).call(window);
  const hs = (window as any)._hyperscript;
  expect(hs, 'vendored _hyperscript did not publish window._hyperscript').toBeTruthy();
  // Select the browser's whole-program grammar. hs.parse treats a leading `js`
  // as a command and stops before later features; parsing never executes init.
  parseErrors = (src: string) => {
    try {
      const parser = hs.internals.createParser(hs.internals.tokenizer.tokenize(src));
      const program = parser.requireElement('hyperscript');
      const errors = program.collectErrors().map((e: { message: string }) => e.message);
      if (parser.hasMore()) errors.push(`Unexpected Token : ${parser.currentToken().value}`);
      return errors;
    } catch (e) {
      return [String(e)];
    }
  };
});

const haskellFiles = (dir: string): string[] =>
  readdirSync(dir).flatMap(entry => {
    const full = join(dir, entry);
    return statSync(full).isDirectory() ? haskellFiles(full) : full.endsWith('.hs') ? [full] : [];
  });

type Snippet = { file: string; line: number; body: string };

/**
 * Pull `[__|…|]` quasiquotes out of a Haskell source.
 *
 * `${…}` is Haskell string interpolation, not hyperscript, so substitute a benign
 * identifier — the surrounding grammar is what we are checking. Snippets whose
 * interpolation lands somewhere that cannot be stood in for (a bare `${x}` statement)
 * simply parse as an identifier expression, which is still a valid parse.
 */
const extractSnippets = (file: string): Snippet[] => {
  const text = readFileSync(file, 'utf8');
  return extractSource(text, relative(REPO, file));
};

const extractSource = (text: string, file = 'fixture.hs'): Snippet[] => {
  const out: Snippet[] = [];
  const add = (index: number, body: string) => out.push({
    file, line: text.slice(0, index).split('\n').length,
    body: body.replace(/\$\{[^}]*\}|\$[A-Za-z_][A-Za-z0-9_']*/g, 'interpolated'),
  });
  for (const match of text.matchAll(/\[__\|([\s\S]*?)\|\]/g)) add(match.index!, match[1]);
  for (const match of text.matchAll(/term\s+"_"\s*(?:\$\s*)?/g)) {
    const rest = text.slice(match.index! + match[0].length);
    const quasi = /^\[text\|([\s\S]*?)\|\]/.exec(rest);
    const literal = /^"(?:\\.|[^"\\])*"/.exec(rest);
    const variable = /^([a-zA-Z_][\w']*)/.exec(rest);
    if (quasi) add(match.index!, quasi[1]);
    else if (literal && !/^\s*<>/.test(rest.slice(literal[0].length))) add(match.index!, JSON.parse(literal[0]));
    else if (variable) {
      const bindings = [...text.matchAll(new RegExp(`\\b${variable[1]}\\s*=\\s*\\[text\\|([\\s\\S]*?)\\|\\]`, 'g'))];
      if (bindings.length !== 1) throw new Error(`${file}: cannot resolve hyperscript binding ${variable[1]}`);
      add(match.index!, bindings[0][1]);
    } else throw new Error(`${file}: unguarded hyperscript attribute at line ${text.slice(0, match.index).split('\n').length}`);
  }
  return out;
};

describe('hyperscript literals in src/ parse', () => {
  it('parses a valid snippet and rejects the bug this guards against', () => {
    // Without this the suite would pass just as happily against a parser that accepts
    // everything, which is the only way it could silently stop protecting anything.
    expect(parseErrors('on click add .foo to me')).toEqual([]);
    // The real 2026-08-27 defect: an unquoted event name containing a hyphen parses
    // `-` as subtraction.
    expect(parseErrors('on change send tab-visible to #water_fall')).not.toEqual([]);
    expect(parseErrors('on change send "tab-visible" to #water_fall')).toEqual([]);
    expect(parseErrors('on click add .foo end on change send tab-visible to #x')).not.toEqual([]);
    expect(parseErrors('on click add .foo end on change send "tab-visible" to #x')).toEqual([]);
    expect(parseErrors('js return {} end on click send tab-visible to #x')).not.toEqual([]);
    expect(parseErrors('js return {} end on click send "tab-visible" to #x')).toEqual([]);
    expect(parseErrors('init throw "must not execute" end')).toEqual([]);
  });

  it.each(['[__|on click add .foo|]', 'term "_" [text|on click add .foo|]', 'term "_" $ [text|on click add .foo|]'])('extracts %s', source => {
    expect(extractSource(source).map(s => s.body)).toEqual(['on click add .foo']);
  });

  it('covers strings and bound interpolations, and rejects untracked constructions', () => {
    expect(extractSource('term "_" "on click halt"')[0].body).toBe('on click halt');
    expect(extractSource('handler = [text|on click halt|]\nterm "_" handler')[0].body).toBe('on click halt');
    expect(() => extractSource('term "_" unknown')).toThrow('cannot resolve');
    expect(() => extractSource('term "_" "on click " <> command')).toThrow('unguarded');
  });

  it('every [__|…|] quasiquote in src/ parses', () => {
    const snippets = haskellFiles(join(REPO, 'src')).flatMap(extractSnippets);
    // Guard against the regex silently matching nothing after a refactor.
    expect(snippets.length).toBeGreaterThan(50);

    const failures = snippets.flatMap(s => {
      const errors = parseErrors(s.body);
      return errors.length === 0 ? [] : [`${s.file}:${s.line}\n    ${errors.join('\n    ')}`];
    });
    expect(failures, `${failures.length} of ${snippets.length} hyperscript snippets failed to parse`).toEqual([]);
  });
});
