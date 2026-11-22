#!/usr/bin/env bun
// CLI wrapper: reads ECharts options from stdin, outputs PNG to stdout
// Colors are now set server-side in Haskell (Utils.getSeriesColorHex)
// Usage: echo '{"echarts": {...}}' | bun run chart-cli.ts

import { createCanvas } from "@napi-rs/canvas";
import * as echarts from "echarts";
import { preparePngOptions, type PngProfile, type PngMode } from "./chart-png-options";
import { reviveFunctions } from "./chart-functions";
import { geomapOptions, registerWorld } from "./geomap";
import { formatNumber, formatBytes, convertToNanoseconds, formatDuration } from "./stat-value";
import world from "../../static/public/assets/deps/world-110m.json";

interface RenderInput {
  echarts: any;
  width?: number;
  height?: number;
  theme?: string;
  darkMode?: boolean;
  profile?: PngProfile;
  mode?: PngMode;
}

// Global formatters for ECharts callbacks (referenced in Haskell-generated formatter strings)
Object.assign(globalThis, { formatNumber, formatBytes, convertToNanoseconds, formatDuration });


async function main() {
  const chunks: Buffer[] = [];
  for await (const chunk of Bun.stdin.stream()) chunks.push(Buffer.from(chunk));
  const input: RenderInput = JSON.parse(Buffer.concat(chunks).toString("utf-8"));
  const { echarts: options, width = 900, height = 300, theme = "default", darkMode = false, profile = "standard", mode = "chart" } = input;

  if (process.env.DEBUG_CHART) console.error("INPUT:", JSON.stringify(input, null, 2));

  const canvas = createCanvas(Math.min(Math.max(width, 100), 2000), Math.min(Math.max(height, 100), 2000));
  const chart = echarts.init(canvas as any, darkMode ? "dark" : theme);

  let finalOptions = mode === "table" ? options : reviveFunctions(options);
  finalOptions.animation = false;
  preparePngOptions(finalOptions, canvas.width, canvas.height, darkMode, profile, mode);
  // The page's theme tokens, as hex: there is no stylesheet to read them from here.
  if (finalOptions.series?.[0]?.type === "map") {
    registerWorld(echarts, world);
    const styles = darkMode
      ? { brandColor: "#4f86f7", textColor: "#cbd5e1", tooltipBg: "#111827", tooltipBorderColor: "#2a2f3a" }
      : { brandColor: "#0d64ff", textColor: "#334155", tooltipBg: "#ffffff", tooltipBorderColor: "#d3d6e4" };
    finalOptions = { ...geomapOptions(echarts, finalOptions, finalOptions.dataset?.source ?? [], "", styles), tooltip: { show: false } };
  }

  if (process.env.DEBUG_CHART) console.error("FINAL OPTIONS:", JSON.stringify(finalOptions, (_, value) => typeof value === "function" ? value.toString() : value, 2));

  chart.setOption(finalOptions);
  await Bun.write(Bun.stdout, canvas.toBuffer("image/png"));
  chart.dispose();
}

main().catch((e) => { console.error(e); process.exit(1); });
