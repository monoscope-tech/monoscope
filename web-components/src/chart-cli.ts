#!/usr/bin/env bun
// CLI wrapper: reads ECharts options from stdin, outputs PNG to stdout
// Colors are now set server-side in Haskell (Utils.getSeriesColorHex)
// Usage: echo '{"echarts": {...}}' | bun run chart-cli.ts

import { createCanvas } from "canvas";
import * as echarts from "echarts";

interface RenderInput {
  echarts: any;
  width?: number;
  height?: number;
  theme?: string;
}

// Global formatters for ECharts callbacks (referenced in Haskell-generated formatter strings)
(globalThis as any).formatNumber = (value: number): string => {
  if (value == null || isNaN(value)) return "";
  if (Math.abs(value) >= 1e9) return (value / 1e9).toFixed(1) + "B";
  if (Math.abs(value) >= 1e6) return (value / 1e6).toFixed(1) + "M";
  if (Math.abs(value) >= 1e3) return (value / 1e3).toFixed(1) + "K";
  return value.toFixed(value % 1 === 0 ? 0 : 1);
};

(globalThis as any).convertToNanoseconds = (value: number, unit: string): number => {
  const conv: Record<string, number> = { ns: 1, us: 1e3, "μs": 1e3, ms: 1e6, s: 1e9, m: 6e10, h: 3.6e12 };
  return value * (conv[unit] || 1);
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

// Convert function strings to actual functions (Haskell sends formatters as strings)
function convertFunctionStrings(obj: any): any {
  if (obj === null || obj === undefined) return obj;
  if (typeof obj === "string" && obj.startsWith("function")) {
    try { return new Function("return " + obj)(); } catch { return obj; }
  }
  if (Array.isArray(obj)) return obj.map(convertFunctionStrings);
  if (typeof obj === "object") {
    const result: any = {};
    for (const [k, v] of Object.entries(obj)) result[k] = convertFunctionStrings(v);
    return result;
  }
  return obj;
}

async function main() {
  const chunks: Buffer[] = [];
  for await (const chunk of Bun.stdin.stream()) chunks.push(Buffer.from(chunk));
  const input: RenderInput = JSON.parse(Buffer.concat(chunks).toString("utf-8"));
  const { echarts: options, width = 900, height = 300, theme = "default" } = input;

  if (process.env.DEBUG_CHART) console.error("INPUT:", JSON.stringify(input, null, 2));

  const canvas = createCanvas(Math.min(Math.max(width, 100), 2000), Math.min(Math.max(height, 100), 2000));
  const chart = echarts.init(canvas as any, theme);

  const finalOptions = convertFunctionStrings(options);
  finalOptions.animation = false;
  finalOptions.backgroundColor = finalOptions.backgroundColor || "#ffffff";

  // Add 1% padding for PNG rendering (doesn't affect dashboard widgets)
  finalOptions.grid = finalOptions.grid || {};
  delete finalOptions.grid.width; // Remove width so left/right padding works
  finalOptions.grid.left = (parseFloat(finalOptions.grid.left) || 0) + 1 + "%";
  finalOptions.grid.right = (parseFloat(finalOptions.grid.right) || 0) + 1 + "%";
  finalOptions.grid.top = (parseFloat(finalOptions.grid.top) || 5) + 1 + "%";
  finalOptions.grid.bottom = (parseFloat(finalOptions.grid.bottom) || 2) + 1 + "%";

  if (process.env.DEBUG_CHART) console.error("FINAL OPTIONS:", JSON.stringify(finalOptions, null, 2));

  chart.setOption(finalOptions);
  await Bun.write(Bun.stdout, canvas.toBuffer("image/png"));
  chart.dispose();
}

main().catch((e) => { console.error(e); process.exit(1); });
