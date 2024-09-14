module.exports = {
  content: [
    './src/**/*.{html,js,hs}',
    './static/public/assets/js/charts.js',
    './static/public/assets/filtercomponent.js',
    './static/public/assets/testeditor.js',
    './static/public/assets/steps-editor.js',
  ],
  safelist: [
    'group-has-[.errors:checked]/pg:block',
    'group-has-[.api-changes:checked]/pg:block',
    'group-has-[.healthcheckpings:checked]/pg:block',
    'group-has-[.multistep-api-test:checked]/pg:block',
    'group-has-[.throughput:checked]/pg:block',
    'group-has-[.error-logs:checked]/pg:block',
  ],
  daisyui: {
    themes: ['winter'],
  },
  theme: {
    extend: {
      fontFamily: {
        Poppins: ['Poppins', 'sans-serif'],
        Inconsolata: ['Inconsolata', 'monospace'],
      },
    },
  },
  plugins: [require('daisyui'), require('@tailwindcss/typography')],
}
