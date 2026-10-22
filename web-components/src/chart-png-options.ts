export type PngProfile = "standard" | "slack";
export type PngMode = "chart" | "table";

// PNG layout uses ECharts pixel/percentage units and the email color scheme.
export function preparePngOptions(finalOptions: any, width: number, height: number, darkMode: boolean, profile: PngProfile = "standard", mode: PngMode = "chart"): void {
  finalOptions.backgroundColor = darkMode ? "#111827" : (finalOptions.backgroundColor || "#ffffff");
  finalOptions.useUTC = true;
  finalOptions.textStyle = { ...finalOptions.textStyle, fontFamily: 'sans-serif' };
  finalOptions.tooltip = { show: false };
  finalOptions.toolbox = { show: false };

  if (mode === 'table') {
    const padding = 24, rowHeight = 32, cellHeight = 24, minColumnWidth = 120;
    const textColor = darkMode ? '#cbd5e1' : '#334155';
    const source = finalOptions.dataset?.source;
    const headers: unknown[] = Array.isArray(source?.[0]) ? source[0] : [];
    const rows: unknown[][] = Array.isArray(source) ? source.slice(1).filter(Array.isArray) : [];
    const columns = Math.min(headers.length, Math.max(1, Math.floor((width - 2 * padding) / minColumnWidth)));
    const rowLimit = Math.max(0, Math.floor((height - 3 * padding - rowHeight) / rowHeight));
    const columnWidth = (width - 2 * padding) / Math.max(1, columns);
    const visibleHeaders = headers.slice(0, columns);
    const cell = (value: unknown, column: number, row: number, header = false) => ({
      type: 'text', left: padding + column * columnWidth, top: padding + row * rowHeight,
      style: {
        text: value == null ? '—' : typeof value === 'object' ? JSON.stringify(value) : String(value),
        fontSize: 14, fontFamily: 'sans-serif', fontWeight: header ? 600 : 400,
        fill: textColor, width: columnWidth - 16, height: cellHeight,
        overflow: 'truncate', ellipsis: '…',
      },
    });
    finalOptions.graphic = [
      ...visibleHeaders.map((value, column) => cell(value, column, 0, true)),
      ...rows.slice(0, rowLimit).flatMap((row, index) => visibleHeaders.map((_, column) => cell(row[column], column, index + 1))),
      ...(!rows.length ? [{ type: 'text', left: padding, top: padding + rowHeight, style: { text: 'No rows returned', fontSize: 14, fill: textColor } }] : []),
      ...(rows.length > rowLimit || headers.length > columns ? [{
        type: 'text', left: padding, top: height - padding,
        style: { text: `Showing ${Math.min(rowLimit, rows.length)} of ${rows.length} rows · ${columns} of ${headers.length} columns`, fontSize: 12, fill: darkMode ? '#94a3b8' : '#64748b' },
      }] : []),
    ];
    finalOptions.series = [];
    finalOptions.xAxis = { show: false };
    finalOptions.yAxis = { show: false };
    finalOptions.legend = { show: false };
    delete finalOptions.dataset;
    return;
  }

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

  if (profile === "slack") {
    finalOptions.backgroundColor = darkMode ? "#111827" : "#ffffff";
    finalOptions.textStyle.fontSize = 14;
    finalOptions.grid.left = Math.max(finalOptions.grid.left, 24);
    finalOptions.grid.right = Math.max(finalOptions.grid.right, 24);
    finalOptions.grid.top = Math.max(finalOptions.grid.top, 16);
    finalOptions.grid.bottom = Math.max(finalOptions.grid.bottom, 24);
    finalOptions.grid.containLabel = true;
    if (series.length === 1) finalOptions.legend = { ...finalOptions.legend, show: false };
    for (const key of ["xAxis", "yAxis"] as const) {
      const axes = Array.isArray(finalOptions[key]) ? finalOptions[key] : [finalOptions[key]];
      for (const axis of axes) {
        if (!axis) continue;
        // ECharts otherwise expands bar axes by a data band even with min/max.
        if (axis.type === "time") axis.containShape = false;
        axis.axisLabel = { ...axis.axisLabel, fontSize: 14 };
        axis.splitLine = { ...axis.splitLine, lineStyle: { ...axis.splitLine?.lineStyle, opacity: 0.35 } };
      }
    }
    // Widget datasets have a header row and explicit numeric column encoding.
    // Sparse count buckets alone do not establish missing telemetry or zero events.
    const source = finalOptions.dataset?.source;
    const axis = xAxes.length === 1 ? xAxes[0] : undefined;
    if (axis?.type === 'time' && Array.isArray(source)) {
      const rows = source.slice(1);
      const column = series.length === 1 ? series[0].encode?.y : undefined;
      const values = typeof column === 'number' ? rows.map((row: any) => row[column]) : undefined;
      const noObservations = rows.length === 0 || values?.every((value: unknown) => value === null);
      const isGauge = series.length === 1 && series[0].type === 'line';
      const text = noObservations ? 'No observations returned for this window'
        : isGauge && values?.some((value: unknown) => value === null) ? 'Missing measurements · gaps are not interpolated'
        : isGauge && series[0].encode?.x === 0 && typeof axis.min === 'number' && rows.every((row: any) => typeof row[0] === 'number' && row[0] > axis.min) ? 'No earlier measurements in this window'
        : undefined;
      if (text) {
        const existing = finalOptions.graphic;
        finalOptions.graphic = [...(Array.isArray(existing) ? existing : existing ? [existing] : []), {
          type: 'text', left: 'center', top: noObservations ? 'middle' : 8,
          style: { text, fontSize: 14, fill: darkMode ? '#cbd5e1' : '#334155', textAlign: 'center' },
        }];
        if (noObservations) {
          const yAxes = Array.isArray(finalOptions.yAxis) ? finalOptions.yAxis : [finalOptions.yAxis];
          for (const yAxis of yAxes) if (yAxis) yAxis.show = false;
        } else finalOptions.grid.top = Math.max(finalOptions.grid.top, 40);
      }
    }
    for (const item of series) {
      if (item.markLine) item.markLine.label = { ...item.markLine.label, fontSize: 14 };
      if (series.length === 1 && item.type === "bar") {
        // Keep boundary buckets visible without changing their timestamps or
        // stretching the requested time window. The grid reserves half a bar.
        item.barMaxWidth = 24;
        item.clip = false;
      }
    }
  }

  // A monitor's threshold can sit outside its observed values, especially for
  // "below" conditions. Keep it visible, and allow negative gauge readings.
  // A single Slack gauge cannot stack with another series; the widget generator
  // still supplies a stack name. Preserve standard exports with explicit stacking.
  if (series.length === 1 && series[0].type === 'line' && (profile === 'slack' || !series[0].stack) && series[0].markLine?.data?.length && !Array.isArray(finalOptions.yAxis)) {
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
