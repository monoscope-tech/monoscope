// Dashboard YAML import/export.
//
// This is the escape hatch for the whole dashboard: export, edit by hand or in git,
// import back. A round-trip that mangles the text loses a dashboard someone built, and
// both helpers are reachable from inline hyperscript, so a throw here is an uncaught
// listener error rather than anything the user sees.
import { describe, test, expect, vi, beforeEach, afterEach } from 'vitest';
import { yamlEditorExport, yamlEditorImport } from '../src/yaml-editor';

const YAML = 'title: Overview\nwidgets:\n  - type: timeseries\n    title: Traffic\n';

let downloads: { name: string; body: string }[];
let revoked: string[];
let objectUrls: Map<string, Blob>;

const fakeFile = (text: string) => ({ text: async () => text }) as File;

beforeEach(() => {
  downloads = [];
  revoked = [];
  objectUrls = new Map();
  let n = 0;
  (URL as any).createObjectURL = (blob: Blob) => {
    const url = `blob:fake/${n++}`;
    objectUrls.set(url, blob);
    return url;
  };
  (URL as any).revokeObjectURL = (url: string) => revoked.push(url);
  // jsdom does not implement navigation, so capture the click instead of following it.
  vi.spyOn(HTMLAnchorElement.prototype, 'click').mockImplementation(function (this: HTMLAnchorElement) {
    const blob = objectUrls.get(this.href);
    downloads.push({ name: this.download, body: blob ? (blob as any)[Symbol.for('body')] ?? '' : '' });
  });
});

afterEach(() => {
  vi.restoreAllMocks();
  delete (window as any).yamlEditor;
});

const withEditor = (initial = '') => {
  let value = initial;
  (window as any).yamlEditor = { getValue: () => value, setValue: (v: string) => (value = v) };
  return { get: () => value };
};

describe('exporting', () => {
  test('downloads the editor contents under the requested filename', async () => {
    withEditor(YAML);

    yamlEditorExport('my-dashboard.yaml');

    expect(downloads).toHaveLength(1);
    expect(downloads[0].name).toBe('my-dashboard.yaml');
    expect(await objectUrls.get([...objectUrls.keys()][0])!.text()).toBe(YAML);
  });

  test('defaults to dashboard.yaml', () => {
    withEditor(YAML);
    yamlEditorExport();
    expect(downloads[0].name).toBe('dashboard.yaml');
  });

  test('serves the file as YAML so the browser does not rename it to .txt', () => {
    withEditor(YAML);
    yamlEditorExport();
    expect([...objectUrls.values()][0].type).toBe('application/x-yaml');
  });

  test('releases the object URL rather than leaking it for the life of the tab', () => {
    withEditor(YAML);
    yamlEditorExport();
    expect(revoked).toHaveLength(1);
  });

  test('an empty or missing editor exports nothing instead of an empty file', () => {
    withEditor('');
    yamlEditorExport();
    expect(downloads).toHaveLength(0);

    delete (window as any).yamlEditor;
    expect(() => yamlEditorExport()).not.toThrow();
    expect(downloads).toHaveLength(0);
  });
});

describe('importing', () => {
  test('replaces the editor contents with the file, byte for byte', async () => {
    const editor = withEditor('title: Old\n');

    await yamlEditorImport(fakeFile(YAML));

    expect(editor.get()).toBe(YAML);
  });

  test('a file with no trailing newline or with CRLF is not silently rewritten', async () => {
    const editor = withEditor();
    await yamlEditorImport(fakeFile('title: A'));
    expect(editor.get()).toBe('title: A');

    await yamlEditorImport(fakeFile('title: A\r\nwidgets: []\r\n'));
    expect(editor.get()).toBe('title: A\r\nwidgets: []\r\n');
  });

  // Reachable from inline hyperscript (a cancelled file picker), so it must not throw.
  test('no file selected is a no-op', async () => {
    const editor = withEditor(YAML);
    await expect(yamlEditorImport(undefined as unknown as File)).resolves.toBeUndefined();
    expect(editor.get()).toBe(YAML);
  });

  test('importing before the editor has mounted does not throw', async () => {
    delete (window as any).yamlEditor;
    await expect(yamlEditorImport(fakeFile(YAML))).resolves.toBeUndefined();
  });
});

describe('round-trip', () => {
  test('export then import returns the identical document', async () => {
    const editor = withEditor(YAML);

    yamlEditorExport();
    const exported = await [...objectUrls.values()][0].text();
    editor.get(); // sanity: still the original
    await yamlEditorImport(fakeFile(exported));

    expect(editor.get()).toBe(YAML);
  });
});
