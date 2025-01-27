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
  daisyui: {
    themes: [
      'winter',
      'fantasy',
      'cmyk',
      'pastel',
      'emerald',
      {
        antdtheme: {
          'color-scheme': 'light',
          primary: '#1677ff', // Ant Design primary color
          'primary-content': '#ffffff', // White text on primary color
          secondary: '#463AA2', // Ant Design purple
          accent: '#C148AC',
          neutral: '#021431',
          'base-100': '#f8fafc', // Base background
          'base-200': '#f2f7ff',
          'base-300': '#e3e9f4',
          'base-content': '#394e6a',
          info: '#1677ff',
          success: '#81CFD1',
          warning: '#EFD7BB',
          error: '#E58B8B',
          // "--rounded-box": "0.4rem", // 2px border radius
          // "--rounded-btn": "0.4rem",
          // "--rounded-badge": "0.4rem",
          // "--tab-radius": "0.4rem",
          // "--btn-text-case": "none", // Sentence case for buttons
          // "--font-family": "system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Helvetica, 'PingFang SC', 'Hiragino Sans GB', 'Microsoft YaHei', SimSun, sans-serif;",
        },
      },
    ],
  },
  theme: {
    extend: {
      colors: {
        'gray-25': '#fcfcfd',
        'gray-150': '#ecedf0',
        'slate-25': '#fbfcfd',
        'slate-150': '#e9eef4',
        'slate-250': '#d6dee8',
        bgAlternate: {
          DEFAULT: '#F5F6FA',
          dark: '#1C1E26',
        },
        bgBase: {
          DEFAULT: '#FBFCFD',
          dark: '#12131A',
        },
        bgInverse: {
          DEFAULT: '#12131A',
          dark: '#FFFFFF',
        },
        bgOverlay: {
          DEFAULT: '#FBFCFD',
          dark: '#292A33',
        },
        bgRaised: {
          DEFAULT: '#FBFCFD',
          dark: '#1C1E26',
        },
        bgSunken: {
          DEFAULT: '#F5F6FA',
          dark: '#000000',
        },

        fillBrand: {
          strong: {
            DEFAULT: '#0068FF',
            dark: '#0068FF',
          },
          weak: {
            DEFAULT: '#0068FF0D',
            dark: '#0068FF0F',
          },
        },
        fillDisabled: {
          DEFAULT: '#0011661A',
          dark: '#FFFFFF1F', // see “Fill/Disabled” + inverse variants
        },
        fillStrong: {
          DEFAULT: '#000626E5',
          dark: '#FFF',
        },
        fillError: {
          strong: {
            DEFAULT: '#C73A3A',
            dark: '#FF9C9C', // or from JSON => #FF9C9C is a guess if fully opaque
            // Actually, from “fill/Error strong” => Dark: #FF9C9C?
            // The JSON says it’s #FF4A4A alpha?
            // If you want the 1000 variant in dark: #FF9C9C or #FF9898.
            // You could also set EXACT #FF4A4A if your design calls for that.
          },
          weak: {
            DEFAULT: '#FF4A4A0D',
            dark: '#FF4A4A14', // ~8% in dark
          },
        },
        fillHover: {
          DEFAULT: '#0015800A',
          dark: '#FFFFFF0F', // from “fill/Hover” in dark => #FFF…0A–0F
        },
        fillInformation: {
          strong: {
            DEFAULT: '#1A74A8',
            dark: '#7EC0E5', // from “fill/Information strong” => #7EC0E5 or #7EC0E5? (approx)
          },
          weak: {
            DEFAULT: '#26B0FF0D',
            dark: '#26B0FF14',
          },
        },
        fillInverse: {
          disabled: {
            DEFAULT: '#FFFFFF1F',
            dark: '#0011661A',
          },
          weak: {
            DEFAULT: '#FFFFFF0F',
            dark: '#0015800A',
          },
        },
        fillOverlay: {
          DEFAULT: '#000D4D73',
          dark: '#000626E6', // from “fill/Overlay” => dark is #000626E6
        },
        fillPress: {
          DEFAULT: '#0011661A',
          dark: '#FFFFFF1F',
        },
        fillSelected: {
          DEFAULT: '#0068FF',
          dark: '#0068FF',
        },
        fillSuccess: {
          strong: {
            DEFAULT: '#067A57',
            dark: '#46C5AF', // from “Green/Dark/1000” approx
          },
          weak: {
            DEFAULT: '#0ACC920D',
            dark: '#46C5AF14',
          },
        },
        fillWarning: {
          strong: {
            DEFAULT: '#8F6C1A',
            dark: '#E0BD70', // from “Amber/Dark/1000” approx
          },
          weak: {
            DEFAULT: '#FFC02E0D',
            dark: '#FFC02E14',
          },
        },
        fillWeak: {
          DEFAULT: '#0015800A',
          dark: '#FFFFFF0F',
        },
        fillWeaker: {
          DEFAULT: '#00158005',
          dark: '#FFFFFF08', // ~3% in dark
        },
        fillWhite: {
          DEFAULT: '#FFFFFF',
          dark: '#FFFFFF', // same in dark
        },
        fillYellow: {
          DEFAULT: '#FEC62E',
          dark: '#FEC62E', // same in dark
        },

        iconBrand: {
          DEFAULT: '#0068FFCC',
          dark: '#0068FFCC',
        },
        iconDisabled: {
          DEFAULT: '#0011661A',
          dark: '#FFFFFF1F',
        },
        iconError: {
          DEFAULT: '#C73A3ACC',
          dark: '#FF9C9CCC',
        },
        iconInformation: {
          DEFAULT: '#1A74A8CC',
          dark: '#7EC0E5CC',
        },
        iconInverse: {
          DEFAULT: '#FFFFFF99',
          dark: '#000D4D7F', // from “Icon/Inverse => Light 50% or 60%”
          disabled: '#FFFFFF1F',
          strong: {
            DEFAULT: '#FFFFFF',
            dark: '#000626E6',
          },
        },
        iconNeutral: {
          DEFAULT: '#000D4D73',
          dark: '#FFFFFF99',
        },
        iconSuccess: {
          DEFAULT: '#067A57CC',
          dark: '#46C5AFCC',
        },
        iconWarning: {
          DEFAULT: '#8F6C1ACC',
          dark: '#E0BD70CC',
        },

        strokeBrand: {
          strong: {
            DEFAULT: '#0068FFCC',
            dark: '#0068FFCC',
          },
          weak: {
            DEFAULT: '#0068FF33',
            dark: '#0068FF33',
          },
        },
        strokeDisabled: {
          DEFAULT: '#0011661A',
          dark: '#FFFFFF1F',
        },
        strokeError: {
          strong: {
            DEFAULT: '#C73A3ACC',
            dark: '#FF9C9CCC',
          },
          weak: {
            DEFAULT: '#C73A3A24',
            dark: '#FF9C9C33',
          },
        },
        strokeFocus: {
          DEFAULT: '#0068FF',
          dark: '#0068FF',
        },
        strokeInformation: {
          strong: {
            DEFAULT: '#1A74A8CC',
            dark: '#7EC0E5CC',
          },
          weak: {
            DEFAULT: '#1A74A833',
            dark: '#7EC0E54D',
          },
        },
        strokeInverse: {
          disabled: {
            DEFAULT: '#FFFFFF1F',
            dark: '#0011661A',
          },
          strong: {
            DEFAULT: '#FFFFFF99',
            dark: '#000D4D73',
          },
          weak: {
            DEFAULT: '#FFFFFF1F',
            dark: '#000D4D1A',
          },
        },
        strokeSelected: {
          DEFAULT: '#0068FF',
          dark: '#0068FF',
        },
        strokeStrong: {
          DEFAULT: '#000D4D73',
          dark: '#FFFFFF99',
        },
        strokeSuccess: {
          strong: {
            DEFAULT: '#067A57CC',
            dark: '#46C5AFCC',
          },
          weak: {
            DEFAULT: '#067A5733',
            dark: '#46C5AF33',
          },
        },
        strokeWarning: {
          strong: {
            DEFAULT: '#8F6C1ACC',
            dark: '#E0BD70CC',
          },
          weak: {
            DEFAULT: '#8F6C1A33',
            dark: '#E0BD7033',
          },
        },
        strokeWeak: {
          DEFAULT: '#0011661A',
          dark: '#FFFFFF1F',
        },

        textBrand: {
          DEFAULT: '#0068FF',
          dark: '#0068FF',
        },
        textDisabled: {
          DEFAULT: '#000D4D73',
          dark: '#FFFFFF1F',
        },
        textError: {
          DEFAULT: '#C73A3A',
          dark: '#FF9C9C',
        },
        textInverse: {
          disabled: {
            DEFAULT: '#FFFFFF1F',
            dark: '#0011661A',
          },
          strong: {
            DEFAULT: '#FFFFFF',
            dark: '#000626E6',
          },
          weak: {
            DEFAULT: '#FFFFFFC7',
            dark: '#000833A6',
          },
        },
        textStrong: {
          DEFAULT: '#000626E6',
          dark: '#FFFFFF',
        },
        textWeak: {
          DEFAULT: '#000833A6',
          dark: '#FFFFFFC7',
        },
      },
      gridTemplateColumns: {
        16: 'repeat(16, minmax(0, 1fr))',
      },
      gridColumn: {
        'span-13': 'span 13 / span 13',
        'span-14': 'span 14 / span 14',
      },
      fontFamily: {
        sans: ['-apple-system', 'BlinkMacSystemFont', "'Segoe UI'", 'Roboto', "'Helvetica Neue'", 'Helvetica', "'PingFang SC'", "'Hiragino Sans GB'", "'Microsoft YaHei'", 'SimSun', 'sans-serif'],
      },
    },
  },
  plugins: [require('daisyui'), require('@tailwindcss/typography')],
}
