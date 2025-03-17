// File: rollup.config.js
import typescript from '@rollup/plugin-typescript'
import { compileLitTemplates } from '@lit-labs/compiler'
import { nodeResolve } from '@rollup/plugin-node-resolve'

export default {
  input: 'static/public/assets/explorer-list.ts',
  output: {
    file: 'static/public/assets/explorer-list.js',
    format: 'es',
  },
  plugins: [
    nodeResolve(),
    typescript({
      transformers: {
        tsconfig: './tsconfig.json',
        before: [compileLitTemplates({ strict: false })],
      },
    }),
  ],
}
