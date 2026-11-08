import { formatStatValue } from './stat-value';

// Shared by the dashboard widget and chart-cli's PNG export, which hand in their own echarts.
export type GeoStyles = { brandColor: string; textColor: string; tooltipBg: string; tooltipBorderColor: string };

// Country shapes keyed by ISO 3166-1 alpha-2 (static/public/assets/deps/world-110m.json: Natural Earth
// 110m, Antarctica dropped).
const countryNames = new Map<string, string>();
export const registerWorld = (ec: any, geo: { features: { properties: { iso: string; name: string } }[] }) => {
  geo.features.forEach(({ properties: p }) => countryNames.set(p.iso, p.name));
  ec.registerMap('world', geo);
};

// A geomap reads `summarize <agg> by <ISO alpha-2 country>`, usually `attributes.geo.country.iso_code`, which
// ingest stamps from the client's address. Shades are log-scaled, as the heatmap's are: the busiest country
// would otherwise wash every other one out to the no-data colour. Rows without a country cannot be placed.
export const geomapOptions = (ec: any, opt: any, source: unknown[][], unit: string, styles: GeoStyles) => {
  const values = new Map<string, number>();
  source.slice(1).forEach(([iso, ...vs]) => {
    const code = String(iso ?? '').toUpperCase();
    if (/^[A-Z]{2}$/.test(code)) values.set(code, (values.get(code) ?? 0) + vs.reduce<number>((a, v) => a + (Number(v) || 0), 0));
  });
  const max = Math.max(...values.values(), 0);
  const fmt = (v: number) => formatStatValue(v, unit);
  const shade = (t: number) => ec.color.lerp(0.35 + 0.65 * t, [styles.tooltipBg, styles.brandColor]);
  const data = [...values].map(([name, value]) => {
    const areaColor = shade(max > 0 ? Math.log1p(Math.max(value, 0)) / Math.log1p(max) : 1);
    return { name, value, itemStyle: { areaColor }, emphasis: { itemStyle: { areaColor } } };
  });
  const land = { areaColor: styles.tooltipBorderColor, borderColor: styles.tooltipBg, borderWidth: 0.5 };
  const label = (text: string, x: number, align: string) => ({ type: 'text', x, y: 10, style: { text, fill: styles.textColor, fontSize: 10, align } });
  return {
    ...opt,
    dataset: undefined,
    legend: { show: false },
    xAxis: { show: false },
    yAxis: { show: false },
    tooltip: {
      ...opt.tooltip,
      trigger: 'item',
      formatter: ({ name, value }: { name: string; value: number }) => `${countryNames.get(name) ?? name}: <b>${Number.isFinite(value) ? fmt(value) : 'no data'}</b>`,
    },
    // Low to high under the map, in the ramp's own colours.
    graphic: max > 0 ? [{ type: 'group', left: 4, bottom: 2, children: [
      { type: 'rect', shape: { width: 120, height: 6, r: 3 }, style: { fill: new ec.graphic.LinearGradient(0, 0, 1, 0, [{ offset: 0, color: shade(0) }, { offset: 1, color: shade(1) }]) } },
      label(fmt(Math.min(...values.values())), 0, 'left'),
      label(fmt(max), 120, 'right'),
    ] }] : [],
    // No roam: a wheel over a dashboard card scrolls the page, it does not zoom the map.
    series: [{ type: 'map', map: 'world', nameProperty: 'iso', roam: false, selectedMode: false, showLegendSymbol: false, top: 4, bottom: max > 0 ? 28 : 4, left: 0, right: 0, data,
      itemStyle: land, emphasis: { label: { show: false }, itemStyle: { ...land, borderColor: styles.textColor, borderWidth: 1 } } }],
  };
};
