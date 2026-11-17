// A day_bars widget reads `summarize count(*) by bin(timestamp, 1d), <status>`: one series per status
// per day. Every series counts toward the day's total; error-named ones (ERROR, error, failed) are its
// failures. One bar per day across the queried window, oldest first. A day with failures stands full
// height in warning or error, so health never rests on colour alone.
export type DayBarStyles = { successColor: string; warningColor: string; errorColor: string; tooltipBorderColor: string; textColor: string; tooltipBg: string };

const DAY = 86_400_000;
const ms = (t: unknown) => (Number(t) < 1e12 ? Number(t) * 1000 : Number(t));

export const dayBarsOptions = (opt: any, source: unknown[][], styles: DayBarStyles) => {
  const failing = (source[0] ?? []).map((h, i) => i > 0 && /^(error|fail(ed|ure)?)$/i.test(String(h)));
  const counts = new Map<number, [number, number]>();
  source.slice(1).forEach(([t, ...vs]) => {
    const d = Math.floor(ms(t) / DAY) * DAY;
    const [n, f] = counts.get(d) ?? [0, 0];
    const sum = (keep: (i: number) => boolean) => vs.reduce<number>((a, v, i) => a + (keep(i + 1) ? Number(v) || 0 : 0), 0);
    counts.set(d, [n + sum(() => true), f + sum((i) => failing[i])]);
  });
  const end = Math.floor((Number.isFinite(ms(opt.xAxis?.max)) ? ms(opt.xAxis.max) : Date.now()) / DAY) * DAY;
  const start = Number.isFinite(ms(opt.xAxis?.min)) ? Math.floor(ms(opt.xAxis.min) / DAY) * DAY : Math.min(end, ...counts.keys());
  const slots = Array.from({ length: Math.min(366, Math.max(1, Math.round((end - start) / DAY) + 1)) }, (_, i) => end - i * DAY).reverse();
  const pct = (n: number, f: number) => (n ? `${(((n - f) * 100) / n).toFixed(2).replace(/\.?0+$/, '')}% up` : 'No data');
  const day = (d: number) => window.echarts.time.format(d, '{MMM} {d}', false);
  const data = slots.map((d) => {
    const [n, f] = counts.get(d) ?? [0, 0];
    const color = n === 0 ? styles.tooltipBorderColor : f === 0 ? styles.successColor : f * 20 < n ? styles.warningColor : styles.errorColor;
    return { value: f > 0 ? 1 : 0.75, n, f, itemStyle: { color, borderRadius: 2 } };
  });
  const [n, f] = [...counts.values()].reduce(([a, b], [c, e]) => [a + c, b + e], [0, 0]);
  return {
    ...opt,
    dataset: undefined,
    legend: { show: false },
    grid: { left: 0, right: 0, top: 4, bottom: 22, containLabel: false },
    xAxis: {
      type: 'category',
      data: slots,
      axisTick: { show: false },
      axisLine: { show: false },
      axisLabel: { interval: (i: number) => i === 0 || i === slots.length - 1, formatter: (d: string, i: number) => (i === 0 ? day(Number(d)) : 'Today'), color: styles.textColor, fontSize: 10, hideOverlap: false },
    },
    yAxis: { type: 'value', show: false, min: 0, max: 1 },
    tooltip: { ...opt.tooltip, trigger: 'item', formatter: (p: any) => `${day(slots[p.dataIndex])} · ${pct(p.data.n, p.data.f)}${p.data.f ? ` · ${p.data.f} failed` : ''}` },
    graphic: [{ type: 'text', left: 'center', bottom: 2, style: { text: `${pct(n, f).replace(' up', ' uptime')}`, fill: styles.textColor, fontSize: 10 } }],
    series: [{ type: 'bar', data, barCategoryGap: '25%', silent: false, animation: false }],
  };
};
