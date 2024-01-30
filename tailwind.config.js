module.exports = {
  content: ["./src/**/*.{html,js,hs}"],
  daisyui: {
    themes: ["light", "dark", "winter", "dim"],
  },
  theme: {
    extend: {
      fontFamily: {
        'Poppins': ['Poppins', 'sans-serif']
        , 'Inconsolata': ['Inconsolata', 'monospace']
      },
    },
  },
  plugins: [require("daisyui"), require("@tailwindcss/typography")],
}
