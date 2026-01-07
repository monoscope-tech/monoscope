// Deterministic color mapping for charts
// Uses colors from the existing ECharts theme for consistency

// Theme colors with better contrast - removed light/pale colors
const THEME_COLORS = [
  '#1A74A8', // Deep blue
  '#91cc75', // Green
  '#fac858', // Yellow/Gold
  '#ee6666', // Red
  '#73c0de', // Cyan
  '#3ba272', // Dark green
  '#fc8452', // Orange
  '#9a60b4', // Purple
  '#c71585', // Medium violet red (replaced light pink)
  '#37a2da', // Bright blue
  '#32c5e9', // Light blue
  '#20b2aa', // Light sea green (replaced pale cyan)
  '#228b22', // Forest green (replaced pale green)
  '#ff8c00', // Dark orange (replaced pale yellow)
  '#ff6347', // Tomato (replaced pale orange)
  '#dc143c', // Crimson (replaced pale pink)
  '#8b008b', // Dark magenta (replaced light pink)
  '#4b0082', // Indigo (replaced pale purple)
  '#6a5acd', // Slate blue (replaced very pale purple)
  '#4169e1'  // Royal blue (replaced pale purple)
];

// HTTP Status Code Colors
const STATUS_CODE_COLORS: Record<number, string> = {
  // 2xx Success - Blues and Greens
  200: '#1A74A8',  // Primary blue
  201: '#37a2da',  // Lighter blue
  202: '#32c5e9',  // Cyan-blue
  204: '#73c0de',  // Light cyan
  206: '#67e0e3',  // Very light cyan
  
  // 3xx Redirects - Cyans and Teals
  301: '#73c0de',  // Light cyan
  302: '#67e0e3',  // Lighter cyan
  304: '#9fe6b8',  // Cyan-green
  307: '#3ba272',  // Green-cyan
  308: '#91cc75',  // Light green
  
  // 4xx Client Errors - Yellows and Oranges
  400: '#fac858',  // Yellow
  401: '#ffdb5c',  // Bright yellow
  403: '#ff9f7f',  // Light orange
  404: '#fc8452',  // Orange
  405: '#fb7293',  // Orange-pink
  429: '#e062ae',  // Pink-orange
  
  // 5xx Server Errors - Reds and Pinks
  500: '#ee6666',  // Red
  502: '#fb7293',  // Pink-red
  503: '#e062ae',  // Dark pink
  504: '#e690d1',  // Light pink
  507: '#e7bcf3',  // Very light pink
};

// Percentile Colors - Performance gradient
const PERCENTILE_COLORS: Record<string, string> = {
  'p50': '#91cc75',  // Green - good performance
  'median': '#91cc75',
  'p75': '#3ba272',  // Darker green - acceptable
  'q1': '#3ba272',
  'p90': '#fac858',  // Yellow - warning level
  'p95': '#fc8452',  // Orange - concerning
  'q3': '#fc8452',
  'p99': '#dc2626',  // Harsh red - critical (using Tailwind red-600 equivalent)
  'p100': '#991b1b', // Dark red - maximum/worst (using Tailwind red-800 equivalent)
  'max': '#991b1b',
  'min': '#91cc75',  // Green - minimum/best
};

// Log Level / Error Pattern Colors
const LOG_LEVEL_COLORS: Record<string, string> = {
  'error': '#ee6666',
  'fail': '#ee6666',
  'failed': '#ee6666',
  'exception': '#e062ae',
  'critical': '#e062ae',
  'warning': '#fac858',
  'warn': '#fac858',
  'success': '#91cc75',
  'ok': '#91cc75',
  'info': '#73c0de',
  'debug': '#9a60b4',
  'trace': '#e7bcf3',
};

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
  // Handle grouped status codes (2xx, 3xx, etc.)
  if (typeof code === 'string') {
    const lowerCode = code.toLowerCase();
    if (lowerCode === '2xx') return '#1A74A8'; // Primary blue
    if (lowerCode === '3xx') return '#73c0de'; // Light cyan
    if (lowerCode === '4xx') return '#fac858'; // Yellow/Orange
    if (lowerCode === '5xx') return '#ee6666'; // Red
  }
  
  const numCode = typeof code === 'string' ? parseInt(code, 10) : code;
  
  // Check if we have a specific color for this code
  if (STATUS_CODE_COLORS[numCode]) {
    return STATUS_CODE_COLORS[numCode];
  }
  
  // Fallback colors by range
  if (numCode >= 200 && numCode < 300) return '#67e0e3'; // Light cyan
  if (numCode >= 300 && numCode < 400) return '#3ba272'; // Green-cyan
  if (numCode >= 400 && numCode < 500) return '#fb7293'; // Orange-pink
  if (numCode >= 500 && numCode < 600) return '#e7bcf3'; // Light pink
  
  // Default fallback
  return '#9d96f5'; // Purple
}

// Get color for percentiles
export function getPercentileColor(percentile: string): string {
  const normalized = percentile.toLowerCase().trim();
  return PERCENTILE_COLORS[normalized] || THEME_COLORS[hashString(percentile) % THEME_COLORS.length];
}

// Get color for log levels and error patterns
export function getLogLevelColor(text: string): string {
  const normalized = text.toLowerCase().trim();
  
  // Check for exact matches first
  if (LOG_LEVEL_COLORS[normalized]) {
    return LOG_LEVEL_COLORS[normalized];
  }
  
  // Check if text contains any of the patterns
  for (const [pattern, color] of Object.entries(LOG_LEVEL_COLORS)) {
    if (normalized.includes(pattern)) {
      return color;
    }
  }
  
  // Default to hash-based color
  return THEME_COLORS[hashString(text) % THEME_COLORS.length];
}

// Main function to get deterministic color for any series
export function getSeriesColor(value: string, context?: 'status' | 'percentile' | 'service' | 'log'): string {
  // Handle null and undefined values with neutral gray
  if (value && (value.toLowerCase() === 'null' || value.toLowerCase() === 'undefined' || value.toLowerCase() === 'unknown')) {
    return '#9ca3af'; // Gray-500 - neutral color for missing data
  }
  
  // Handle empty values - use default color
  if (!value || value.trim() === '') {
    return THEME_COLORS[0];
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
  return THEME_COLORS[hashString(value) % THEME_COLORS.length];
}

// Tailwind to hex color mapping (ECharts theme colors for consistency across app)
export const TAILWIND_TO_HEX: Record<string, string> = {
  'bg-red-400': '#ee6666',
  'bg-amber-400': '#fac858',
  'bg-orange-400': '#fc8452',
  'bg-yellow-400': '#fac858',
  'bg-lime-400': '#91cc75',
  'bg-green-400': '#3ba272',
  'bg-teal-400': '#20b2aa',
  'bg-cyan-400': '#73c0de',
  'bg-blue-400': '#1A74A8',
  'bg-purple-400': '#9a60b4',
  'bg-violet-400': '#6a5acd',
  'bg-pink-400': '#c71585',
  'bg-rose-400': '#dc143c',
  'bg-emerald-400': '#228b22',
  'bg-fuchsia-400': '#8b008b',
  'bg-indigo-400': '#4b0082',
  'bg-sky-400': '#32c5e9',
  'bg-gray-400': '#9ca3af',
  'bg-gray-500': '#6b7280',
};

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
  return TAILWIND_TO_HEX[tailwindClass] || THEME_COLORS[0];
}