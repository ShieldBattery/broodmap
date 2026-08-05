import { defineConfig } from 'vite'
import { resolve } from 'path'
import wasm from 'vite-plugin-wasm'
import topLevelAwait from 'vite-plugin-top-level-await'

export default defineConfig({
  root: 'examples',
  build: {
    rollupOptions: {
      input: {
        main: resolve(__dirname, 'examples/index.html'),
      },
    },
  },
  plugins: [wasm(), topLevelAwait()],
  server: {
    port: 3000,
    open: true,
    fs: {
      allow: ['..'],
    },
  },
})
