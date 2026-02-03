#!/usr/bin/env bun
// CLI wrapper: reads ECharts options from stdin, outputs PNG to stdout
// Usage: echo '{"echarts": {...}}' | bun run chart-cli.ts

import { createCanvas } from "canvas";
import * as echarts from "echarts";
import { getSeriesColor } from "./colorMapping";

interface RenderInput {
  echarts: any;
  width?: number;
  height?: number;
  theme?: string;
}

// Utility functions that the formatters reference - must be global for ECharts callbacks
(globalThis as any).formatNumber = (value: number): string => {
  if (value == null || isNaN(value)) return "";
  if (Math.abs(value) >= 1e9) return (value / 1e9).toFixed(1) + "B";
  if (Math.abs(value) >= 1e6) return (value / 1e6).toFixed(1) + "M";
  if (Math.abs(value) >= 1e3) return (value / 1e3).toFixed(1) + "K";
  return value.toFixed(value % 1 === 0 ? 0 : 1);
};

(globalThis as any).convertToNanoseconds = (value: number, unit: string): number => {
  const conversions: Record<string, number> = { ns: 1, us: 1e3, "μs": 1e3, ms: 1e6, s: 1e9, m: 6e10, h: 3.6e12 };
  return value * (conversions[unit] || 1);
};

(globalThis as any).formatDuration = (ns: number): string => {
  if (ns == null || isNaN(ns)) return "";
  if (ns >= 3.6e12) return (ns / 3.6e12).toFixed(1) + "h";
  if (ns >= 6e10) return (ns / 6e10).toFixed(1) + "m";
  if (ns >= 1e9) return (ns / 1e9).toFixed(1) + "s";
  if (ns >= 1e6) return (ns / 1e6).toFixed(1) + "ms";
  if (ns >= 1e3) return (ns / 1e3).toFixed(1) + "μs";
  return ns.toFixed(0) + "ns";
};

// Convert function strings to actual functions recursively
function convertFunctionStrings(obj: any): any {
  if (obj === null || obj === undefined) return obj;
  if (typeof obj === "string" && obj.startsWith("function")) {
    try { return new Function("return " + obj)(); } catch { return obj; }
  }
  if (Array.isArray(obj)) return obj.map(convertFunctionStrings);
  if (typeof obj === "object") {
    const result: any = {};
    for (const [key, value] of Object.entries(obj)) result[key] = convertFunctionStrings(value);
    return result;
  }
  return obj;
}

// Apply colors to all series in ECharts options based on series names
function applySeriesColors(options: any): any {
  if (!options?.dataset?.source || !Array.isArray(options.dataset.source)) return options;

  const source = options.dataset.source;
  if (source.length === 0) return options;

  const headers = source[0];
  if (!Array.isArray(headers) || headers.length < 2) return options;

  const seriesNames = headers.slice(1);

  if (!options.series || options.series.length === 0) {
    options.series = seriesNames.map((name: string, i: number) => ({
      type: 'bar',
      name,
      encode: { x: 0, y: i + 1 },
      itemStyle: { color: getSeriesColor(name) },
    }));
  } else {
    options.series = options.series.map((s: any, i: number) => {
      const name = s.name || seriesNames[i] || `Series ${i + 1}`;
      if (!s.itemStyle?.color) {
        return { ...s, name, itemStyle: { ...s.itemStyle, color: getSeriesColor(name) } };
      }
      return s;
    });
  }

  return options;
}

async function main() {
  const chunks: Buffer[] = [];
  for await (const chunk of Bun.stdin.stream()) {
    chunks.push(Buffer.from(chunk));
  }
  const inputRaw = Buffer.concat(chunks).toString("utf-8");
  const input: RenderInput = JSON.parse(inputRaw);
  const { echarts: options, width = 900, height = 300, theme = "default" } = input;

  // Debug: write input to stderr if DEBUG_CHART env var is set
  if (process.env.DEBUG_CHART) {
    console.error("INPUT:", JSON.stringify(input, null, 2));
  }

  const canvas = createCanvas(Math.min(Math.max(width, 100), 2000), Math.min(Math.max(height, 100), 2000));
  const chart = echarts.init(canvas as any, theme);

  const withFunctions = convertFunctionStrings(options);
  const finalOptions = applySeriesColors(withFunctions);
  finalOptions.animation = false;
  finalOptions.backgroundColor = finalOptions.backgroundColor || "#ffffff";

  if (process.env.DEBUG_CHART) {
    console.error("FINAL OPTIONS:", JSON.stringify(finalOptions, null, 2));
  }

  chart.setOption(finalOptions);
  await Bun.write(Bun.stdout, canvas.toBuffer("image/png"));
  chart.dispose();
}

main().catch((e) => { console.error(e); process.exit(1); });
