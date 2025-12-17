module.exports = {
  content: [
    './src/**/*.{html,js,hs}',
    './static/public/assets/js/charts.js',
    './static/public/assets/js/widgets.js',
    './static/public/assets/testeditor.js',
    './static/public/assets/explorer-list.js',
    './static/public/**/*.{html,js}',
    './web-components/**/*.{js,jsx,ts,tsx,vue}',
  ],
  safelist: [
    'group-has-[.errors:checked]/pg:block',
    'group-has-[.api-changes:checked]/pg:block',
    'group-has-[.healthcheckpings:checked]/pg:block',
    'group-has-[.multistep-api-test:checked]/pg:block',
    'group-has-[.throughput:checked]/pg:block',
    'group-has-[.error-logs:checked]/pg:block',
    'group-has-[.tm-toggle:checked]/tm:rotate-90',
    'group-has-[.tmplRadio0:checked]/md:bg-slate-100',
    'group-has-[.tmplRadio0:checked]/md:border',
    'group-has-[.tmplRadio0:checked]/md:block',
    'group-has-[.tmplRadio1:checked]/md:bg-slate-100',
    'group-has-[.tmplRadio1:checked]/md:border',
    'group-has-[.tmplRadio1:checked]/md:block',
    'group-has-[.tmplRadio2:checked]/md:bg-slate-100',
    'group-has-[.tmplRadio2:checked]/md:border',
    'group-has-[.tmplRadio2:checked]/md:block',
    'group-has-[.tmplRadio3:checked]/md:bg-slate-100',
    'group-has-[.tmplRadio3:checked]/md:border',
    'group-has-[.tmplRadio3:checked]/md:block',
    'group-has-[.tmplRadio4:checked]/md:bg-slate-100',
    'group-has-[.tmplRadio4:checked]/md:border',
    'group-has-[.tmplRadio4:checked]/md:block',

    'basis-full',
    'w-[16ch] ',
  ],
  theme: {
    extend: {
      gridTemplateColumns: {
        16: 'repeat(16, minmax(0, 1fr))',
      },
      gridColumn: {
        'span-13': 'span 13 / span 13',
        'span-14': 'span 14 / span 14',
      },
    },
  },
}
