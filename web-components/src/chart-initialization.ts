const CHART_PREFETCH_PX = 150;

type VerticalRect = Pick<DOMRect, 'top' | 'bottom'>;

export const isNearChartViewport = ({ top, bottom }: VerticalRect, viewportHeight: number) =>
  top < viewportHeight + CHART_PREFETCH_PX && bottom > -CHART_PREFETCH_PX;
