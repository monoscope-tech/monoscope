import { describe, expect, it } from 'vitest';
import * as echarts from 'echarts';
import { geomapOptions } from '../src/geomap';

const styles = { brandColor: '#0000ff', textColor: '#333333', tooltipBg: '#ffffff', tooltipBorderColor: '#cccccc' };

describe('geomapOptions', () => {
  it('sums each country once, drops rows it cannot place, and shades the busiest darkest', () => {
    const opt = geomapOptions(echarts, { tooltip: {} }, [['country', 'n'], ['us', 900], ['US', 100], ['GB', 9], ['', 50], ['Unknown', 7]], '', styles);
    const data = opt.series[0].data;
    expect(data.map((d: any) => [d.name, d.value])).toEqual([['US', 1000], ['GB', 9]]);
    expect(data[0].itemStyle.areaColor).toBe(echarts.color.lerp(1, [styles.tooltipBg, styles.brandColor]));
    expect(opt.graphic[0].children.slice(1).map((c: any) => c.style.text)).toEqual(['9', '1.0K']);
  });

  it('draws a bare map without a scale when nothing is placed', () => {
    const opt = geomapOptions(echarts, { tooltip: {} }, [['country', 'n']], '', styles);
    expect([opt.series[0].data, opt.graphic]).toEqual([[], []]);
  });
});
