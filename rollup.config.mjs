// File: rollup.config.js
import typescript from '@rollup/plugin-typescript'
import { compileLitTemplates } from '@lit-labs/compiler'

export default {
  input: 'static/public/assets/explorer-list.mts', // Change this to your entry file
  output: {
    file: 'static/public/assets/explorer-list.js', // Change as needed
  },
  plugins: [
    typescript({
      transformers: {
        before: [compileLitTemplates({ strict: false })], // Ensure strict transformation
      },
    }),
  ],
}
