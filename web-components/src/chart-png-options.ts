// PNG layout uses ECharts pixel/percentage units and the email color scheme.
export function preparePngOptions(finalOptions: any, width: number, height: number, darkMode: boolean): void {
  finalOptions.backgroundColor = darkMode ? "#111827" : (finalOptions.backgroundColor || "#ffffff");

  // ECharts numbers are pixels; strings may be percentages. Preserve their units.
  const paddedInset = (value: number | string | undefined, fallback: number, extent: number): number => {
    const pixels = typeof value === "string" && value.endsWith("%")
      ? parseFloat(value) * extent / 100
      : Number(value ?? fallback);
    return pixels + extent * 0.01;
  };
  finalOptions.grid = finalOptions.grid || {};
  delete finalOptions.grid.width;
  for (const side of ["left", "right", "top", "bottom"] as const) {
    finalOptions.grid[side] = paddedInset(finalOptions.grid[side], 8, side === "left" || side === "right" ? width : height);
  }

  // The dashboard supplies light axis/grid colors explicitly, overriding ECharts' theme.
  if (darkMode) {
    for (const key of ["xAxis", "yAxis"] as const) {
      const axes = Array.isArray(finalOptions[key]) ? finalOptions[key] : [finalOptions[key]];
      for (const axis of axes) {
        if (!axis) continue;
        axis.axisLabel = { ...axis.axisLabel, color: "#cbd5e1" };
        axis.axisLine = { ...axis.axisLine, lineStyle: { ...axis.axisLine?.lineStyle, color: "#64748b", opacity: 1 } };
        axis.splitLine = { ...axis.splitLine, lineStyle: { ...axis.splitLine?.lineStyle, color: "#374151" } };
      }
    }
    if (finalOptions.legend) finalOptions.legend.textStyle = { ...finalOptions.legend.textStyle, color: "#cbd5e1" };
  }
}
