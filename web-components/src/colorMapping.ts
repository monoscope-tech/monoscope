// Deterministic color mapping for charts
// Uses colors from the existing ECharts theme for consistency
//
// SYNC WARNING: This file is duplicated in chartshot (github.com/monoscope-tech/chartshot)
// When updating this logic, also update chartshot/colorMapping.ts to keep server-side
// chart rendering consistent with browser rendering.

// Light-mode marks use darker values so every essential series clears 3:1 on
// the cool chart surface. Dark mode keeps the brighter 400-weight family.
const LIGHT_THEME_COLORS = [
  '#2563eb', '#dc2626', '#15803d', '#b45309', '#9333ea', '#0f766e',
  '#c2410c', '#0369a1', '#e11d48', '#4d7c0f', '#4f46e5', '#a16207',
  '#be185d', '#047857', '#7c3aed', '#0e7490', '#a21caf', '#475569',
  '#1d4ed8', '#b91c1c'
];
const DARK_THEME_COLORS = [
  '#60a5fa', '#f87171', '#4ade80', '#fbbf24', '#c084fc', '#2dd4bf',
  '#fb923c', '#38bdf8', '#fb7185', '#a3e635', '#818cf8', '#facc15',
  '#f472b6', '#34d399', '#a78bfa', '#22d3ee', '#e879f9', '#94a3b8',
  '#73c0de', '#ee6666'
];

const currentTheme = (): 'light' | 'dark' => typeof document !== 'undefined' && document.body?.getAttribute('data-theme') === 'dark' ? 'dark' : 'light';
export const getSeriesPalette = (theme: 'light' | 'dark' = currentTheme()) =>
  theme === 'dark' ? DARK_THEME_COLORS : LIGHT_THEME_COLORS;
const themeColors = () => getSeriesPalette();

// Log Level / Error Pattern Colors (hardcoded fallbacks for server-side rendering).
// Browser callers should use resolveLogLevelColors() for CSS-token-aware colors.
const LOG_LEVEL_COLORS: Record<string, string> = {
  'error': '#ee6666',
  'fail': '#ee6666',
  'failed': '#ee6666',
  'exception': '#ee6666',
  'critical': '#ee6666',
  'warning': '#fac858',
  'warn': '#fac858',
  'success': '#91cc75',
  'ok': '#91cc75',
  'info': '#73c0de',
  'debug': '#9a60b4',
  'trace': '#e7bcf3',
};

// CSS token → log level mapping for runtime resolution from the design system.
// Maps semantic CSS custom properties to the log level keys they should override.
const LOG_LEVEL_TOKEN_MAP: Record<string, string[]> = {
  '--color-fillError-strong': ['error', 'fail', 'failed', 'exception', 'critical'],
  '--color-fillWarning-strong': ['warning', 'warn'],
  '--color-fillSuccess-strong': ['success', 'ok'],
  '--color-fillInformation-strong': ['info'],
};

let _resolvedLogLevelColors: Record<string, string> | null = null;

// Resolve log level colors from CSS custom properties (browser only).
// Caches on first call. Call invalidateLogLevelColors() on theme change.
export function resolveLogLevelColors(): Record<string, string> {
  if (_resolvedLogLevelColors) return _resolvedLogLevelColors;
  if (typeof document === 'undefined') return LOG_LEVEL_COLORS;
  const cs = getComputedStyle(document.body);
  const resolved = { ...LOG_LEVEL_COLORS };
  for (const [token, keys] of Object.entries(LOG_LEVEL_TOKEN_MAP)) {
    const raw = cs.getPropertyValue(token).trim();
    if (!raw) continue;
    const hex = _toHex(raw);
    if (hex) for (const k of keys) resolved[k] = hex;
  }
  _resolvedLogLevelColors = resolved;
  return resolved;
}

export function invalidateLogLevelColors(): void { _resolvedLogLevelColors = null; }

// Minimal oklch/rgb → hex converter using a canvas pixel read
function _toHex(cssColor: string): string {
  if (typeof document === 'undefined') return '';
  if (!_hexCanvas) { _hexCanvas = document.createElement('canvas'); _hexCanvas.width = 1; _hexCanvas.height = 1; _hexCtx = _hexCanvas.getContext('2d', { willReadFrequently: true })!; }
  _hexCtx!.clearRect(0, 0, 1, 1);
  _hexCtx!.fillStyle = cssColor;
  _hexCtx!.fillRect(0, 0, 1, 1);
  const [r, g, b] = _hexCtx!.getImageData(0, 0, 1, 1).data;
  return '#' + [r, g, b].map(c => c.toString(16).padStart(2, '0')).join('');
}
let _hexCanvas: HTMLCanvasElement | null = null;
let _hexCtx: CanvasRenderingContext2D | null = null;

// Simple hash function for deterministic color selection
function hashString(str: string): number {
  let hash = 0;
  for (let i = 0; i < str.length; i++) {
    const char = str.charCodeAt(i);
    hash = ((hash << 5) - hash) + char;
    hash = hash & hash; // Convert to 32-bit integer
  }
  return Math.abs(hash);
}

// Get color for HTTP status codes
export function getStatusCodeColor(code: number | string): string {
  const grouped = typeof code === 'string' && /^[2-5]xx$/i.test(code) ? Number(code[0]) * 100 : Number(code);
  const dark = currentTheme() === 'dark';
  if (grouped >= 200 && grouped < 300) return dark ? '#34d399' : '#047857';
  if (grouped >= 300 && grouped < 400) return dark ? '#38bdf8' : '#0369a1';
  if (grouped >= 400 && grouped < 500) return dark ? '#fbbf24' : '#b45309';
  if (grouped >= 500 && grouped < 600) return dark ? '#f87171' : '#dc2626';
  return themeColors()[4];
}

// Get color for percentiles
export function getPercentileColor(percentile: string): string {
  const normalized = percentile.toLowerCase().trim();
  const dark = currentTheme() === 'dark';
  const colors: Record<string, string> = dark
    ? { p50: '#4ade80', median: '#4ade80', p75: '#34d399', q1: '#34d399', p90: '#fbbf24', p95: '#fb923c', q3: '#fb923c', p99: '#f87171', p100: '#fb7185', max: '#fb7185', min: '#4ade80' }
    : { p50: '#15803d', median: '#15803d', p75: '#047857', q1: '#047857', p90: '#a16207', p95: '#c2410c', q3: '#c2410c', p99: '#dc2626', p100: '#be123c', max: '#be123c', min: '#15803d' };
  const palette = themeColors();
  return colors[normalized] || palette[hashString(percentile) % palette.length];
}

// Get color for log levels and error patterns
export function getLogLevelColor(text: string): string {
  const levels = resolveLogLevelColors();
  const normalized = text.toLowerCase().trim();

  if (levels[normalized]) return levels[normalized];
  for (const [pattern, color] of Object.entries(levels)) {
    if (normalized.includes(pattern)) return color;
  }

  const colors = themeColors();
  return colors[hashString(text) % colors.length];
}

// Main function to get deterministic color for any series
export function getSeriesColor(value: string, context?: 'status' | 'percentile' | 'service' | 'log'): string {
  const colors = themeColors();
  // Handle null and undefined values with visible but muted blue-gray
  if (value && value.toLowerCase() === 'unset') {
    return '#7c8db5'; // Visible desaturated blue — muted but clearly present on dark backgrounds
  }
  if (value && ['null', 'undefined', 'unknown'].includes(value.toLowerCase())) {
    return '#9ca3af'; // Gray-400 — neutral, WCAG-safe contrast on dark backgrounds
  }
  
  // Handle empty values - use default color
  if (!value || value.trim() === '') {
    return colors[0];
  }
  
  // If context is provided, use specific color function
  if (context === 'status') {
    return getStatusCodeColor(value);
  }
  
  if (context === 'percentile') {
    return getPercentileColor(value);
  }
  
  if (context === 'log') {
    return getLogLevelColor(value);
  }
  
  // Try to auto-detect the type
  // Check if it's a status code (3 digits starting with 2-5 or grouped like 2xx, 3xx, etc.)
  if (/^[2-5]\d{2}$/.test(value) || /^[2-5]xx$/i.test(value)) {
    return getStatusCodeColor(value);
  }
  
  // Check if it's a percentile
  if (/^(p|q)\d+|median|max|min/i.test(value)) {
    return getPercentileColor(value);
  }
  
  // Check for log level patterns
  const lowerValue = value.toLowerCase();
  for (const pattern of Object.keys(LOG_LEVEL_COLORS)) {
    if (lowerValue.includes(pattern)) {
      return getLogLevelColor(value);
    }
  }
  
  // Default: Use hash-based color selection for consistent service colors
  return colors[hashString(value) % colors.length];
}

// Tailwind class to hex mapping for service colors. These values are the
// light-mode equivalents of the server's stable class positions.
export const TAILWIND_TO_HEX: Record<string, string> = {
  'bg-blue-400': '#2563eb',
  'bg-red-400': '#dc2626',
  'bg-green-400': '#15803d',
  'bg-amber-400': '#b45309',
  'bg-purple-400': '#9333ea',
  'bg-teal-400': '#0f766e',
  'bg-orange-400': '#c2410c',
  'bg-sky-400': '#0369a1',
  'bg-rose-400': '#e11d48',
  'bg-lime-400': '#4d7c0f',
  'bg-indigo-400': '#4f46e5',
  'bg-yellow-400': '#a16207',
  'bg-pink-400': '#be185d',
  'bg-emerald-400': '#047857',
  'bg-violet-400': '#7c3aed',
  'bg-cyan-400': '#0e7490',
  'bg-fuchsia-400': '#a21caf',
  'bg-slate-400': '#475569',
  'bg-gray-500': '#4b5563',
};

const DARK_TAILWIND_TO_HEX: Record<string, string> = Object.fromEntries(
  Object.keys(TAILWIND_TO_HEX).map((key, index) => [key, DARK_THEME_COLORS[index] ?? DARK_THEME_COLORS[0]])
);

// Convert hex to HSL
function hexToHsl(hex: string): [number, number, number] {
  const r = parseInt(hex.slice(1, 3), 16) / 255;
  const g = parseInt(hex.slice(3, 5), 16) / 255;
  const b = parseInt(hex.slice(5, 7), 16) / 255;
  const max = Math.max(r, g, b), min = Math.min(r, g, b);
  let h = 0, s = 0, l = (max + min) / 2;
  if (max !== min) {
    const d = max - min;
    s = l > 0.5 ? d / (2 - max - min) : d / (max + min);
    h = max === r ? ((g - b) / d + (g < b ? 6 : 0)) / 6
      : max === g ? ((b - r) / d + 2) / 6
      : ((r - g) / d + 4) / 6;
  }
  return [h * 360, s * 100, l * 100];
}

// Convert HSL to hex
function hslToHex(h: number, s: number, l: number): string {
  s /= 100; l /= 100;
  const a = s * Math.min(l, 1 - l);
  const f = (n: number) => {
    const k = (n + h / 30) % 12;
    const color = l - a * Math.max(Math.min(k - 3, 9 - k, 1), -1);
    return Math.round(255 * color).toString(16).padStart(2, '0');
  };
  return `#${f(0)}${f(8)}${f(4)}`;
}

// Calculate contrast text color - returns tinted version of background for softer look
export function getContrastTextColor(hexColor: string): string {
  const [h, s, l] = hexToHsl(hexColor);
  // For dark backgrounds: light tint (85-92% lightness), for light: dark shade (15-22%)
  const newL = l > 50 ? 18 : 88;
  // Reduce saturation slightly for softer appearance
  const newS = Math.min(s, 60);
  return hslToHex(h, newS, newL);
}

// Convert Tailwind class to hex color
export function tailwindToHex(tailwindClass: string): string {
  const palette = currentTheme() === 'dark' ? DARK_TAILWIND_TO_HEX : TAILWIND_TO_HEX;
  return palette[tailwindClass] || themeColors()[0];
}

// One service → one hex, shared by waterfall, timeline and service map. The map
// is the server-rendered getServiceColors output (Tailwind class names).
export const resolveColor = (service: string, colorsMap: Record<string, string>): string => {
  const tw = colorsMap[service] || getSeriesColor(service, 'service');
  return tw.startsWith('#') ? tw : tailwindToHex(tw);
};