import { html, TemplateResult } from 'lit';

// Content-hashed URLs for the static assets this bundle references, injected by
// BodyWrapper (see `hashAssetFile`). Without them the browser would keep a
// year-long cached copy of a sprite sheet that has since gained new symbols.
type AssetUrls = { echarts: string; echartsTheme: string; spriteSolid: string; spriteRegular: string };

const urls = (): Partial<AssetUrls> => (window as any).assetUrls ?? {};

export const echartsUrls = () => ({
  echarts: urls().echarts ?? '/public/assets/deps/echarts/echarts.min.js',
  theme: urls().echartsTheme ?? '/public/assets/roma-echarts.js',
});

export const spriteUrl = (kind: string) =>
  (kind === 'solid' ? urls().spriteSolid : urls().spriteRegular) ?? `/public/assets/svgs/fa-sprites/${kind}.svg`;

export const faSprite_ = (iconName: string, kind: string, classes: string): TemplateResult =>
  html`<svg class="${classes}" width="1em" height="1em" fill="currentColor"><use href="${spriteUrl(kind)}#${iconName}"></use></svg>`;
