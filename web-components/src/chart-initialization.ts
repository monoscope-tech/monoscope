const CHART_PREFETCH_PX = 150;

type VerticalRect = Pick<DOMRect, 'top' | 'bottom'>;

export const isNearChartViewport = ({ top, bottom }: VerticalRect, viewportHeight: number) =>
  top < viewportHeight + CHART_PREFETCH_PX && bottom > -CHART_PREFETCH_PX;

// Fixed ECharts insets consume too much of a short, wide widget.
export const chartLayoutForSize = (options: any, width: number, height: number): any => {
  if (!height || height > 240 || width < height * 4) return options;
  const bottomLegend = options.legend?.show && options.legend.bottom !== undefined;
  return {
    ...options,
    grid: {
      ...options.grid,
      top: options.legend?.show && options.legend.top !== undefined ? options.grid.top : Math.min(options.grid.top, 4),
      bottom: Math.min(options.grid.bottom, bottomLegend ? 8 : 4),
    },
    legend: bottomLegend ? { ...options.legend, bottom: 0, padding: [0, 4, 0, 4] } : options.legend,
    xAxis: { ...options.xAxis, axisLabel: { ...options.xAxis.axisLabel, margin: 4 } },
  };
};
