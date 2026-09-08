// A multiline log body used to render its newlines in the nowrap list (whitespace-pre),
// blowing the virtual row open across many lines. It must clamp to the first line with
// a "+N lines" hint and carry the full message in the hover tooltip; wrap mode keeps
// the full multiline rendering.
import { describe, test, expect } from 'vitest';
import { render } from 'lit';
import { mountList } from './log-list-harness';

const MSG = 'Arrow evaluator evaluating: StructPatch(\n  ExpressionStructPatch {\n    input_path: None,\n  }\n)';

const renderSummary = async (wrapLines: boolean) => {
  const el = await mountList({ projectId: 'proj-1' } as any);
  const host = document.createElement('div');
  render((el as any).renderSummaryElements([MSG], wrapLines), host);
  return host;
};

describe('multiline summary in the log list', () => {
  test('nowrap mode clamps to the first line with a tooltip and line-count hint', async () => {
    const host = await renderSummary(false);
    const span = host.querySelector('span')!;

    expect(span.textContent).toContain('Arrow evaluator evaluating: StructPatch(');
    expect(span.textContent).not.toContain('input_path');
    expect(span.textContent).toContain('+4 lines');
    expect(span.getAttribute('title')).toBe(MSG);
  });

  test('wrap mode keeps the full multiline message', async () => {
    const host = await renderSummary(true);

    expect(host.textContent).toContain('input_path: None');
    expect(host.textContent).not.toContain('+4 lines');
  });
});
