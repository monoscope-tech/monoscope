module.exports = {
  content: ['./src/**/*.{html,js,hs}', './static/public/assets/*.js'],
  theme: {
    extend: {
      fontFamily: {
        Poppins: ['Poppins', 'sans-serif'],
        Inconsolata: ['Inconsolata', 'monospace'],
      },
    },
  },
  plugins: [require('rippleui')],
  rippleui: {
    removeThemes: ['dark'],
  },
};
