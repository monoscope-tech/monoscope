import { beforeAll, describe, expect, it } from 'vitest';
import * as echarts from 'echarts';
import { dayBarsOptions } from '../src/day-bars';

const styles = { successColor: 'green', warningColor: 'amber', errorColor: 'red', tooltipBorderColor: 'grey', textColor: '#333', tooltipBg: '#fff' };
const day = (d: string) => Date.parse(`${d}T00:00:00Z`);

beforeAll(() => { (window as any).echarts = echarts; });

describe('dayBarsOptions', () => {
  it('draws one bar per day of the window: failures by status name, full height and coloured by their share', () => {
    const opt = dayBarsOptions({ tooltip: {}, xAxis: { min: day('2025-01-01'), max: day('2025-01-04') + 3600_000 } }, [
      ['timestamp', 'OK', 'ERROR'],
      [day('2025-01-01'), 100, null],
      [day('2025-01-02'), 99, 1],
      [day('2025-01-03'), 1, 1],
    ], styles);
    const bars = opt.series[0].data.map((b: any) => [b.itemStyle.color, b.value, b.n, b.f]);
    expect(bars).toEqual([['green', 0.75, 100, 0], ['amber', 1, 100, 1], ['red', 1, 2, 1], ['grey', 0.75, 0, 0]]);
    expect(opt.graphic[0].style.text).toBe('99.01% uptime');
  });

  it('reads a cron job’s lowercase statuses the same way', () => {
    const opt = dayBarsOptions({ tooltip: {}, xAxis: { min: day('2025-01-01'), max: day('2025-01-01') } }, [['timestamp', 'ok', 'error'], [day('2025-01-01'), 3, 1]], styles);
    expect(opt.series[0].data.map((b: any) => [b.n, b.f])).toEqual([[4, 1]]);
  });
});
