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

// Override createElement to return mocked canvas for Monaco Editor
const originalCreateElement = document.createElement.bind(document);
document.createElement = function(tagName: string, options?: any) {
  const element = originalCreateElement(tagName, options);

  if (tagName.toLowerCase() === 'canvas') {
    const originalGetContext = element.getContext.bind(element);
    element.getContext = function(contextType: any, contextOptions?: any) {
      if (contextType === '2d') {
        const ctx = originalGetContext(contextType, contextOptions) || ({} as any);
        // Mock the properties Monaco Editor needs
        ctx.webkitBackingStorePixelRatio = 1;
        ctx.mozBackingStorePixelRatio = 1;
        ctx.msBackingStorePixelRatio = 1;
        ctx.oBackingStorePixelRatio = 1;
        ctx.backingStorePixelRatio = 1;
        ctx.measureText = ctx.measureText || function() { return { width: 0 }; };
        ctx.fillRect = ctx.fillRect || function() {};
        ctx.clearRect = ctx.clearRect || function() {};
        ctx.fillText = ctx.fillText || function() {};
        ctx.save = ctx.save || function() {};
        ctx.restore = ctx.restore || function() {};
        ctx.beginPath = ctx.beginPath || function() {};
        ctx.closePath = ctx.closePath || function() {};
        ctx.moveTo = ctx.moveTo || function() {};
        ctx.lineTo = ctx.lineTo || function() {};
        ctx.stroke = ctx.stroke || function() {};
        ctx.fill = ctx.fill || function() {};
        return ctx;
      }
      return originalGetContext(contextType, contextOptions);
    };
  }

  return element;
} as any;

// Add any additional global setup for happy-dom environment
beforeAll(() => {
  console.log('Test setup complete with happy-dom');
});