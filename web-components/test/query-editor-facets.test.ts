import { test, expect } from 'vitest';
import { QueryEditorComponent } from '../src/query-editor/query-editor';

test('facet selections OR values of the same field, AND different fields, and toggle off', () => {
  const editor = new QueryEditorComponent();
  const api = 'resource.service.name == "api"', worker = 'resource.service.name == "worker"', error = 'level == "ERROR"';
  editor.toggleSubQuery('resource.service.name', 'api');
  editor.toggleSubQuery('resource.service.name', 'worker');
  expect(editor.getValue()).toBe(`(${api} or ${worker})`);
  editor.toggleSubQuery('level', 'ERROR');
  expect(editor.getValue()).toBe(`(${api} or ${worker}) and ${error}`);
  editor.toggleSubQuery('resource.service.name', 'api');
  expect(editor.getValue()).toBe(`${worker} and ${error}`);
  editor.toggleSubQuery('level', 'ERROR');
  expect(editor.getValue()).toBe(worker);
  editor.toggleSubQuery('resource.service.name', 'worker');
  expect(editor.getValue()).toBe('');
});

test.each(['duration > 10ms | summarize count()', 'spans | where duration > 10ms | summarize count()', `name == 'level == "ERROR"' and duration > 10ms | summarize count()`])('facet OR groups preserve other filters and pipelines: %s', initial => {
  const editor = new QueryEditorComponent();
  editor.setValue(initial);
  editor.toggleSubQuery('level', 'ERROR');
  editor.toggleSubQuery('level', 'WARN');
  expect(editor.getValue()).toContain('(level == "ERROR" or level == "WARN")');
  expect(editor.getValue()).toContain('duration > 10ms');
  expect(editor.getValue()).toContain('| summarize count()');
  editor.toggleSubQuery('level', 'ERROR');
  editor.toggleSubQuery('level', 'WARN');
  expect(editor.getValue().replace(/\s+/g, ' ').trim()).toBe(initial);
});

test('facet values with quotes and backslashes can be selected and toggled off', () => {
  const editor = new QueryEditorComponent();
  const value = String.raw`service "api"\worker`;
  const fragment = `resource.service.name == ${JSON.stringify(value)}`;
  editor.toggleSubQuery('resource.service.name', value);
  expect(editor.getValue()).toBe(fragment);
  editor.toggleSubQuery('resource.service.name', value);
  expect(editor.getValue()).toBe('');
});
