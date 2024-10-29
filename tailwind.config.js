module.exports = {
  content: [
    './src/**/*.{html,js,hs}',
    './static/public/assets/js/charts.js',
    './static/public/assets/filtercomponent.js',
    './static/public/assets/testeditor.js',
    './static/public/assets/steps-editor.js',
    './static/public/assets/steps-assertions.js',
  ],
  safelist: [
    'group-has-[.errors:checked]/pg:block',
    'group-has-[.api-changes:checked]/pg:block',
    'group-has-[.healthcheckpings:checked]/pg:block',
    'group-has-[.multistep-api-test:checked]/pg:block',
    'group-has-[.throughput:checked]/pg:block',
    'group-has-[.error-logs:checked]/pg:block',
    'group-has-[.tm-toggle:checked]/tm:rotate-90',
    'basis-full',
  ],
  daisyui: {
    themes: ['winter', 'fantasy', 'cmyk', 'pastel', 'emerald', {
      antdtheme: {
        "color-scheme": "light",
        "primary": "#1677ff", // Ant Design primary color
        "primary-content": "#ffffff", // White text on primary color
        "secondary": "#463AA2", // Ant Design purple
        "accent": "#C148AC", 
        "neutral": "#021431", 
        "base-100": "#ffffff", // Base background
        "base-200": "#f2f7ff",
        "base-300": "#e3e9f4",
        "base-content": "#394e6a", 
        "info": "#1677ff",
        "success": "#81CFD1",
        "warning": "#EFD7BB",
        "error": "#E58B8B",
        // "--rounded-box": "0.4rem", // 2px border radius
        // "--rounded-btn": "0.4rem",
        // "--rounded-badge": "0.4rem",
        // "--tab-radius": "0.4rem",
        // "--btn-text-case": "none", // Sentence case for buttons
        // "--font-family": "system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Helvetica, 'PingFang SC', 'Hiragino Sans GB', 'Microsoft YaHei', SimSun, sans-serif;",
      },
    }],
  },
  theme: {
    extend: {
      gridTemplateColumns: {
        '16': 'repeat(16, minmax(0, 1fr))',
      },
      gridColumn: {
        'span-13': 'span 13 / span 13',
        'span-14': 'span 14 / span 14',
      },
      fontFamily: {
        sans: [
          "-apple-system",
          "BlinkMacSystemFont",
          "'Segoe UI'",
          "Roboto",
          "'Helvetica Neue'",
          "Helvetica",
          "'PingFang SC'",
          "'Hiragino Sans GB'",
          "'Microsoft YaHei'",
          "SimSun",
          "sans-serif",
        ],
      },
    },
  },
  plugins: [require('daisyui'), require('@tailwindcss/typography')],
}
