module.exports = {
  content: ["./src/**/*.{html,js,hs}", "./static/public/assets/testeditor.js"],
  daisyui: {
    themes: ["light", "dark", "winter", "dim"],
  },
  theme: {
    extend: {
      fontFamily: {
        Poppins: ["Poppins", "sans-serif"],
        Inconsolata: ["Inconsolata", "monospace"],
      },
    },
  },
  plugins: [require("daisyui"), require("@tailwindcss/typography")],
};
