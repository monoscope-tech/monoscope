import { afterEach, expect, test } from 'vitest';
import '../src/query-preview';

afterEach(() => document.body.replaceChildren());

test.each(['kql', 'sql'])('highlights %s only when opened and keeps the query read-only', language => {
  const query = language === 'sql' ? "SELECT max(value) FROM otel_metrics WHERE metric_name = 'cpu'" : 'metrics | where metric_name == "cpu" | summarize max(value)';
  const panel = document.createElement('div');
  panel.setAttribute('popover', 'auto');
  const preview = document.createElement('query-preview');
  preview.setAttribute('language', language);
  const code = document.createElement('pre');
  code.textContent = query;
  preview.append(code);
  panel.append(preview);
  document.body.append(panel);
  expect(preview.querySelector('.cm-editor')).toBeNull();
  panel.dispatchEvent(Object.assign(new Event('beforetoggle'), { newState: 'open' }));
  expect(preview.querySelector('.cm-content')?.textContent).toBe(query);
  expect(preview.querySelector('.cm-content')?.getAttribute('contenteditable')).toBe('false');
  expect(preview.querySelectorAll('.cm-line span').length).toBeGreaterThan(1);
  panel.dispatchEvent(Object.assign(new Event('beforetoggle'), { newState: 'open' }));
  expect(preview.querySelectorAll('.cm-editor')).toHaveLength(1);
  panel.remove();
  expect(preview.querySelector('.cm-editor')).toBeNull();
});
