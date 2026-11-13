import escape from 'lodash/escape';
const compactNumber = new Intl.NumberFormat('en', { notation: 'compact', maximumFractionDigits: 1 });
// Real glyph widths where a canvas exists; the PNG renderer in Node falls back to an average.
const ctx = typeof OffscreenCanvas === 'undefined' ? null : new OffscreenCanvas(1, 1).getContext('2d');
const sides = (p: number | number[] = 5) => (Array.isArray(p) ? [p[0], p[1] ?? p[0], p[2] ?? p[0], p[3] ?? p[1] ?? p[0]] : [p, p, p, p]);

// The long tail is already folded into "Other (k)", so every entry fits: the legend wraps instead
// of scrolling, and the plot gives up exactly the rows it takes. Names shrink before rows grow
// past ~30% of the height. `orient: 'vertical'` (legend_position: right) becomes a name/value
// table beside the plot when it fits, and falls back to the wrapped bottom legend when it doesn't.
export const legendLayout = (options: any, width: number, height: number, format: (v: number) => string = (v) => compactNumber.format(v)): any => {
  const legend = options.legend;
  const names: string[] = (legend?.data ?? (options.series ?? []).map((s: any) => s?.name) ?? []).filter(Boolean);
  if (!legend || legend.show === false || !names.length || !width) return options;
  // One entry only repeats the query's column alias (count_); the widget title already names it.
  // Its row goes too, back to the inset the server gives a chart without a legend.
  if (names.length === 1 && legend.orient !== 'vertical') {
    const at = legend.top !== undefined ? 'top' : 'bottom';
    return { ...options, grid: { ...options.grid, [at]: (legend[at] ?? 0) + 6 }, legend: { ...legend, show: false } };
  }
  const fontSize = legend.textStyle?.fontSize ?? 12, charPx = fontSize * 0.62;
  const font = `${fontSize}px ${legend.textStyle?.fontFamily ?? options.textStyle?.fontFamily ?? 'sans-serif'}`;
  // The canvas is shared and the formatter measures at draw time, after other charts set their own font.
  const textPx = (t: string) => (ctx ? ((ctx.font = font), ctx.measureText(t).width) : t.length * charPx);
  const icon = (legend.itemWidth ?? 25) + 5, gap = legend.itemGap ?? 10;
  const [pt, pr, pb, pl] = sides(legend.padding);
  const pitch = Math.max(fontSize + 2, legend.itemHeight ?? 14) + gap;
  const base = { ...legend, type: 'plain', tooltip: { show: true } };

  if (legend.orient === 'vertical') {
    const tableW = Math.min(300, Math.floor(width * 0.32)), valueW = 52;
    const nameW = tableW - pl - pr - icon - valueW;
    if (nameW >= 72 && names.length * pitch - gap + pt + pb <= height) {
      const [header, ...rows] = options.dataset?.source ?? [[]];
      const line = (options.series ?? []).every((s: any) => s?.type === 'line');
      const value = (name: string) => {
        const nums = rows.map((r: any[]) => r?.[header.indexOf(name)]).filter(Number.isFinite) as number[];
        return nums.length ? format(nums.reduce((a, b) => a + b, 0) / (line ? nums.length : 1)) : '–';
      };
      const fit = (n: string) => (textPx(n) > nameW ? n.slice(0, Math.max(1, Math.floor(nameW / charPx) - 1)) + '…' : n).replaceAll('}', '｝');
      return {
        ...options,
        grid: { ...options.grid, right: tableW + 8 },
        legend: {
          ...base, top: 'middle', right: 0, bottom: undefined, left: undefined,
          formatter: (n: string) => `{n|${fit(n)}}{v|${value(n)}}`,
          textStyle: { ...legend.textStyle, rich: { n: { width: nameW, fontSize }, v: { width: valueW, align: 'right', fontSize } } },
          tooltip: { show: true, formatter: (p: any) => `${escape(p.name)}<br/>${line ? 'Mean' : 'Total'}: ${value(p.name)}` },
        },
      };
    }
  }

  // A grid, not a ragged wrap: equal columns, left-aligned, names cut to the column, and the prefix
  // every name shares (`srv-captain--`) dropped so the part that differs is what shows.
  const side = legend.orient !== 'vertical' && legend.top !== undefined ? 'top' : 'bottom';
  const usable = width - pl - pr - 8;
  const named = names.filter((n) => !/^Other \(\d+\)$/.test(n));
  const common = named.length > 1 ? named.reduce((p, n) => { let i = 0; while (i < p.length && p[i] === n[i]) i++; return p.slice(0, i); }) : '';
  // Cut at a separator, backing off until every name keeps enough to tell it apart.
  let shared = common.slice(0, common.search(/[^A-Za-z0-9][A-Za-z0-9]*$/) + 1);
  while (shared && named.some((n) => n.length - shared.length < 8)) shared = shared.slice(0, shared.slice(0, -1).search(/[^A-Za-z0-9][A-Za-z0-9]*$/) + 1);
  const short = (n: string) => (shared.length >= 4 && n.startsWith(shared) && n.length > shared.length ? n.slice(shared.length) : n);
  const widest = Math.max(...names.map((n) => textPx(short(n))));
  const colsAt = (textW: number) => Math.max(1, Math.floor((usable + gap) / (icon + textW + gap)));
  const rowsAt = (textW: number) => Math.ceil(names.length / colsAt(textW));
  const maxRows = height ? Math.max(1, Math.floor((height * 0.3) / pitch)) : Infinity;
  let textW = Math.min(widest, 32 * charPx);
  while (textW > 8 * charPx && rowsAt(textW) > maxRows) textW -= charPx;
  // Columns widen to share the row, but never past the longest name.
  const cols = colsAt(textW);
  // Unfloored: a floored share fell a fraction short of the widest name and cut "ERROR" to "ERR…".
  textW = Math.min(widest, (usable + gap) / cols - gap - icon);
  const fit = (n: string) => {
    let t = short(n);
    if (textPx(t) > textW) { while (t.length > 1 && textPx(t + '…') > textW) t = t.slice(0, -1); t += '…'; }
    return t.replaceAll('}', '｝').replaceAll('{', '｛');
  };
  const offset = legend.orient === 'vertical' ? 0 : (legend[side] ?? 0);
  return {
    ...options,
    grid: { ...options.grid, [side]: offset + pt + pb + rowsAt(textW) * pitch - gap + 6 },
    legend: {
      ...base, orient: 'horizontal', [side]: offset, left: 4, ...(legend.orient === 'vertical' && { top: undefined, right: undefined }),
      formatter: (n: string) => `{n|${fit(n)}}`,
      textStyle: { ...legend.textStyle, rich: { n: { width: Math.max(1, Math.ceil(textW)), fontSize } } },
      tooltip: { show: true, formatter: (p: any) => escape(p.name) },
    },
  };
};
