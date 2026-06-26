// This file will be executed before running tests
import { vi } from 'vitest';

// main.ts runs at import time and calls these globals; stub them so any component
// (e.g. log-list → widgets → main) is importable in jsdom without throwing.
(window as any).htmx = { defineExtension: vi.fn(), ajax: vi.fn(), process: vi.fn() };
(window as any).interpolateVarTemplates = vi.fn();

// Mock document.queryCommandSupported and execCommand for Monaco Editor clipboard support
document.queryCommandSupported = vi.fn(() => false);
document.execCommand = vi.fn(() => false);

// Mock CSS.escape for Monaco Editor's CSS class name generation
(globalThis as any).CSS = {
  escape: (str: string) => str.replace(/[!"#$%&'()*+,.\/:;<=>?@[\\\]^`{|}~]/g, '\\$&')
};

// Mock window.matchMedia for Monaco theme detection (must be set early, before Monaco loads)
Object.defineProperty(window, 'matchMedia', {
  writable: true,
  value: vi.fn().mockImplementation((query: string) => ({
    matches: false,
    media: query,
    onchange: null,
    addListener: vi.fn(),
    removeListener: vi.fn(),
    addEventListener: vi.fn(),
    removeEventListener: vi.fn(),
    dispatchEvent: vi.fn(),
  })),
});

// Mock ResizeObserver for Monaco editor size detection
(global as any).ResizeObserver = class ResizeObserver {
  constructor(callback: ResizeObserverCallback) {
    // Mock implementation
  }
  observe() {}
  unobserve() {}
  disconnect() {}
};

// Controllable IntersectionObserver. jsdom/happy-dom compute no layout, so NO
// library (not even the W3C polyfill) can fire these from real visibility — they
// just never trigger. Instead we expose a spec-shaped mock whose callbacks tests
// fire explicitly via triggerIntersection(), exercising the real observer-driven
// code paths (e.g. log-list load-more). For true layout-driven observers, run the
// test under @vitest/browser (real Chromium).
const ioInstances = new Set<any>();
(global as any).IntersectionObserver = class IntersectionObserver {
  callback: IntersectionObserverCallback;
  elements = new Set<Element>();
  root = null; rootMargin = ''; thresholds = [];
  constructor(cb: IntersectionObserverCallback) { this.callback = cb; ioInstances.add(this); }
  observe(el: Element) { this.elements.add(el); }
  unobserve(el: Element) { this.elements.delete(el); }
  disconnect() { this.elements.clear(); ioInstances.delete(this); }
  takeRecords() { return []; }
};
// Fire an intersection for observers watching `el` (or all observed elements).
(globalThis as any).triggerIntersection = (el?: Element, isIntersecting = true) => {
  for (const o of ioInstances) {
    const targets = el ? [...o.elements].filter((e) => e === el) : [...o.elements];
    if (targets.length) o.callback(targets.map((t) => ({ isIntersecting, target: t, intersectionRatio: isIntersecting ? 1 : 0 })) as any, o);
  }
};

// Mock Worker for Monaco Editor since we don't need actual workers in tests
globalThis.Worker = class Worker {
  constructor(url: string | URL) {}
  postMessage() {}
  terminate() {}
  addEventListener() {}
  removeEventListener() {}
  onmessage = null;
  onerror = null;
  onmessageerror = null;
} as any;

// Setup Monaco environment for tests
globalThis.MonacoEnvironment = {
  getWorker: function () {
    return new Worker('');
  }
};

// Canvas 2D context is provided realistically by `vitest-canvas-mock` (first
// entry in setupFiles), so no hand-rolled getContext stub is needed here.
