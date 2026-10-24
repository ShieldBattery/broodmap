import { defineConfig } from 'vite'

export default defineConfig({
  root: 'examples',
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
