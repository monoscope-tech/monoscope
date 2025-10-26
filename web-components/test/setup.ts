// This file will be executed before running tests
import { beforeAll, vi } from 'vitest';

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

// Canvas mock for Monaco Editor compatibility with jsdom
beforeAll(() => {

  // Mock HTMLCanvasElement methods that Monaco Editor uses
  const mockContext = {
    fillStyle: '',
    strokeStyle: '',
    lineWidth: 1,
    lineCap: 'butt',
    lineJoin: 'miter',
    miterLimit: 10,
    font: '10px sans-serif',
    textAlign: 'start',
    textBaseline: 'alphabetic',
    direction: 'ltr',
    globalAlpha: 1,
    globalCompositeOperation: 'source-over',
    imageSmoothingEnabled: true,
    shadowBlur: 0,
    shadowColor: 'rgba(0, 0, 0, 0)',
    shadowOffsetX: 0,
    shadowOffsetY: 0,
    fillRect: vi.fn(),
    strokeRect: vi.fn(),
    clearRect: vi.fn(),
    beginPath: vi.fn(),
    closePath: vi.fn(),
    moveTo: vi.fn(),
    lineTo: vi.fn(),
    bezierCurveTo: vi.fn(),
    quadraticCurveTo: vi.fn(),
    arc: vi.fn(),
    arcTo: vi.fn(),
    ellipse: vi.fn(),
    rect: vi.fn(),
    fill: vi.fn(),
    stroke: vi.fn(),
    clip: vi.fn(),
    isPointInPath: vi.fn(),
    isPointInStroke: vi.fn(),
    measureText: vi.fn(() => ({ width: 0, actualBoundingBoxAscent: 0, actualBoundingBoxDescent: 0 })),
    fillText: vi.fn(),
    strokeText: vi.fn(),
    drawImage: vi.fn(),
    createImageData: vi.fn(),
    getImageData: vi.fn(() => ({ data: [], width: 0, height: 0 })),
    putImageData: vi.fn(),
    save: vi.fn(),
    restore: vi.fn(),
    scale: vi.fn(),
    rotate: vi.fn(),
    translate: vi.fn(),
    transform: vi.fn(),
    setTransform: vi.fn(),
    resetTransform: vi.fn(),
    createLinearGradient: vi.fn(),
    createRadialGradient: vi.fn(),
    createPattern: vi.fn(),
    getContextAttributes: vi.fn(() => ({})),
    canvas: null as any,
  };

  HTMLCanvasElement.prototype.getContext = vi.fn((contextType: string) => {
    if (contextType === '2d') {
      mockContext.canvas = this;
      return mockContext as any;
    }
    return null;
  });

  HTMLCanvasElement.prototype.toDataURL = vi.fn(() => 'data:image/png;base64,');
  HTMLCanvasElement.prototype.toBlob = vi.fn((callback: any) => {
    callback(new Blob());
  });

  console.log('Test setup complete with jsdom and canvas mocking');
});
