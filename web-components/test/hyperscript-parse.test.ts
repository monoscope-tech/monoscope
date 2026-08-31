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
  // `hs.parse` collects failures into `errors` rather than throwing, and — unlike
  // `processNode`, the path the browser takes — it does not execute `init` features.
  // That matters: running them evaluates real DOM/network code against jsdom.
  //
  // The tradeoff is that `parse` accepts a single feature, so a legitimate
  // multi-feature program (`js … end` followed by `on click …`) reports a spurious
  // "Unexpected Token : <next feature keyword>" at the seam. Drop exactly that shape;
  // everything else — including a bad token *inside* a feature body, which is the
  // whole bug class here — still reports.
  const FEATURE_SEAM = /^Unexpected Token : (on|init|def|js|behavior|worker|eventsource|socket)$/;
  parseErrors = (src: string) => {
    try {
      const result = hs.parse(src) as { errors?: Array<{ message?: string }> };
      return (result?.errors ?? []).map(e => e?.message ?? String(e)).filter(m => !FEATURE_SEAM.test(m));
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
  const out: Snippet[] = [];
  const re = /\[__\|([\s\S]*?)\|\]/g;
  for (let m = re.exec(text); m !== null; m = re.exec(text)) {
    out.push({
      file: relative(REPO, file),
      line: text.slice(0, m.index).split('\n').length,
      body: m[1].replace(/\$\{[^}]*\}/g, 'interpolated'),
    });
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
