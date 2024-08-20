module.exports = {
  content: ['./src/**/*.{html,js,hs}', './static/public/assets/filtercomponent.js', './static/public/assets/testeditor.js', './static/public/assets/steps-editor.js'],
  daisyui: {
    themes: ['light', 'dark', 'winter', 'dim', 'forest'],
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
