const { test, expect } = require('@playwright/test');
const path = require('node:path');

const root = path.resolve(__dirname, '../..');
const gridCss = path.join(root, 'static/public/assets/deps/gridstack/gridstack.min.css');
const gridJs = path.join(root, 'static/public/assets/deps/gridstack/gridstack-all.js');

const item = (id, x, y, w, h, content = '') => `
  <div id="${id}" class="grid-stack-item" gs-x="${x}" gs-y="${y}" gs-w="${w}" gs-h="${h}"
       style="--grid-preload-left:${x * 100 / 12}%;--grid-preload-top:${y * 80}px;--grid-preload-width:${w * 100 / 12}%;--grid-preload-height:${h * 80}px">
    <div class="grid-stack-item-content">${content}</div>
  </div>`;

const fixtures = {
  root: item('root-a', 0, 0, 8, 2) + item('root-b', 8, 0, 4, 2),
  tabbed: item('tab-a', 0, 0, 4, 1) + item('tab-b', 4, 0, 8, 1),
  normalizedMalformed: item('wide-clamped', 0, 0, 12, 2) + item('overlap-resolved', 0, 2, 6, 1) + item('missing-position', 6, 2, 6, 1),
  savedVisualOrder: item('golden-signals', 0, 0, 12, 2) + item('lower-in-yaml-first', 0, 2, 6, 2),
  nested: item('group', 0, 0, 12, 3, `<div class="grid-stack grid-stack-preloaded nested-grid" style="height:160px">${item('nested-a', 0, 0, 8, 1)}${item('nested-b', 0, 1, 8, 1)}</div>`),
  empty: '',
};

function documentFor(content) {
  return `<!doctype html><meta name="viewport" content="width=device-width,initial-scale=1">
    <style>
      *{box-sizing:border-box}.grid-stack{position:relative;width:100%;--gs-column-width:8.3333333333%;--gs-cell-height:80px;--gs-item-margin-top:8px;--gs-item-margin-right:4px;--gs-item-margin-bottom:8px;--gs-item-margin-left:4px}
      .grid-stack-preloaded:not(.grid-stack-initialized)>.grid-stack-item{left:var(--grid-preload-left)!important;top:var(--grid-preload-top)!important;width:var(--grid-preload-width)!important;height:var(--grid-preload-height)!important}
      .grid-stack-item-content{border:1px solid transparent}
      @media(max-width:767px){.grid-stack{display:flex!important;flex-direction:column;gap:8px;height:auto!important}.grid-stack>.grid-stack-item,.grid-stack.grid-stack-preloaded:not(.grid-stack-initialized)>.grid-stack-item{position:relative!important;top:auto!important;left:auto!important;width:100%!important;height:180px!important}.nested-grid>.grid-stack-item,.nested-grid.grid-stack-preloaded:not(.grid-stack-initialized)>.grid-stack-item{height:120px!important}}
    </style><div id="grid" class="grid-stack grid-stack-preloaded" style="height:240px">${content}</div>`;
}

const rects = page => page.evaluate(() => Object.fromEntries(
  [...document.querySelectorAll('.grid-stack-item')].map(el => {
    const r = el.getBoundingClientRect();
    return [el.id, [r.x, r.y, r.width, r.height].map(value => Math.round(value * 10) / 10)];
  })
));

for (const viewport of [{ name: 'desktop', width: 1280, height: 900 }, { name: 'mobile', width: 390, height: 844 }]) {
  for (const [fixture, content] of Object.entries(fixtures)) {
    test(`${fixture} has zero rectangle delta during ${viewport.name} hydration`, async ({ page }) => {
      await page.setViewportSize(viewport);
      await page.setContent(documentFor(content));
      await page.addStyleTag({ path: gridCss });
      const before = await rects(page);
      await page.addScriptTag({ path: gridJs });
      await page.evaluate(() => {
        const main = document.getElementById('grid');
        GridStack.init({ column: 12, cellHeight: '80px', margin: '8px 4px', animate: false, columnOpts: { breakpointForWindow: true, breakpoints: [{ w: 768, c: 1 }], layout: 'list' } }, main);
        main.classList.add('grid-stack-initialized');
        document.querySelectorAll('.nested-grid').forEach(el => {
          GridStack.init({ column: 12, cellHeight: '80px', margin: '8px 4px', animate: false, columnOpts: { breakpointForWindow: true, breakpoints: [{ w: 768, c: 1 }], layout: 'list' } }, el);
          el.classList.add('grid-stack-initialized');
        });
      });
      const after = await rects(page);
      expect(after).toEqual(before);
    });
  }
}
