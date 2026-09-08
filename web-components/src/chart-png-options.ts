// PNG layout uses ECharts pixel/percentage units and the email color scheme.
export function preparePngOptions(finalOptions: any, width: number, height: number, darkMode: boolean): void {
  finalOptions.backgroundColor = darkMode ? "#111827" : (finalOptions.backgroundColor || "#ffffff");
  finalOptions.useUTC = true;
  finalOptions.textStyle = { ...finalOptions.textStyle, fontFamily: 'sans-serif' };
  finalOptions.tooltip = { show: false };
  finalOptions.toolbox = { show: false };

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

  const xAxes = Array.isArray(finalOptions.xAxis) ? finalOptions.xAxis : [finalOptions.xAxis];
  for (const axis of xAxes) {
    if (axis?.type !== 'time') continue;
    // The supplied window is authoritative, including periods without samples.
    // Boundary ticks must be visible even when the automatic ticks land inside it.
    axis.boundaryGap = [0, 0];
    axis.axisLabel = { ...axis.axisLabel, showMinLabel: true, showMaxLabel: true, hideOverlap: true };
  }
  const series = Array.isArray(finalOptions.series) ? finalOptions.series : [];
  for (const item of series) {
    if (item.type !== 'line') continue;
    item.connectNulls = false;
    item.smooth = false;
    item.showSymbol = true;
    item.symbolSize = 5;
    item.lineStyle = { ...item.lineStyle, width: 2 };
  }

  // A monitor's threshold can sit outside its observed values, especially for
  // "below" conditions. Keep it visible, and allow negative gauge readings.
  // Limit this scaling to a single unstacked measurement's threshold chart.
  if (series.length === 1 && series[0].type === 'line' && !series[0].stack && series[0].markLine?.data?.length && !Array.isArray(finalOptions.yAxis)) {
    const source = finalOptions.dataset?.source;
    const yColumn = series[0].encode?.y ?? 1;
    const values = Array.isArray(source) ? source.slice(1).map((row: any) => row[yColumn]).filter((v: unknown) => typeof v === 'number' && Number.isFinite(v)) : [];
    const thresholds = series[0].markLine.data.map((line: any) => line.yAxis).filter((v: unknown) => typeof v === 'number' && Number.isFinite(v));
    if (values.length && thresholds.length) {
      const low = Math.min(0, ...values, ...thresholds);
      const high = Math.max(0, ...values, ...thresholds);
      const padding = (high - low || 1) * 0.08;
      const step = 10 ** Math.floor(Math.log10(high - low || 1)) / 2;
      finalOptions.yAxis = { ...finalOptions.yAxis, min: low < 0 ? Math.floor((low - padding) / step) * step : 0, max: Math.ceil((high + padding) / step) * step };
    }
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
