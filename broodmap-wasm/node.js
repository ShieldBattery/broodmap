// The package's Node entry: the web build, initialized synchronously from disk so it can be used
// as soon as it's imported (or required). Calling the default init export again is a no-op, so code
// shared with the browser can still `await init()`.
import { readFileSync } from 'node:fs'
import { initSync } from './pkg/broodmap_wasm.js'

initSync({ module: readFileSync(new URL('./pkg/broodmap_wasm_bg.wasm', import.meta.url)) })

export * from './pkg/broodmap_wasm.js'
export { default } from './pkg/broodmap_wasm.js'
