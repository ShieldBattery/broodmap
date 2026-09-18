import { defineConfig } from 'vite'
import { resolve } from 'path'

export default defineConfig({
  root: 'examples',
  build: {
    rollupOptions: {
      input: {
        main: resolve(__dirname, 'examples/index.html'),
      },
    },
  },
  // wasm-pack's explicit async init needs no top-level-await transform.
  worker: { format: 'es' },
  server: {
    port: 3000,
    open: true,
    fs: {
      allow: ['..'],
    },
  },
})
