// This file will be executed before running tests
import { beforeAll } from 'vitest';

// Mock Worker for Monaco Editor since we don't need actual workers in tests
globalThis.Worker = class Worker {
  constructor(url: string | URL) {
    // Mock worker implementation
  }
  postMessage() {}
  terminate() {}
  onmessage = null;
  onerror = null;
} as any;

// Setup Monaco environment for tests
globalThis.MonacoEnvironment = {
  getWorker: function () {
    return new Worker('');
  }
};

// Add any additional global setup for happy-dom environment
beforeAll(() => {
  console.log('Test setup complete with happy-dom');
});