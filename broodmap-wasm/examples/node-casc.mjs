// Node usage example: render maps with assets read straight from a SC:R install's CASC storage,
// no prefetch or extracted asset directory. Build the package first: `pnpm run build`.
//
//   node examples/node-casc.mjs <install-dir> <map.scm>...
import { readFileSync, writeFileSync } from 'node:fs'
import path from 'node:path'
import { CascStorage, MapRenderer } from '@shieldbattery/broodmap'
import { nodeCascFiles } from './casc-files.mjs'

const [installDir, ...mapPaths] = process.argv.slice(2)
if (!installDir || !mapPaths.length) {
  console.error('usage: node examples/node-casc.mjs <install-dir> <map.scm>...')
  process.exit(1)
}

let start = performance.now()
const storage = new CascStorage(nodeCascFiles(installDir))
console.log(`opened SC:R ${storage.version} in ${(performance.now() - start).toFixed(0)} ms`)

const opts = JSON.stringify({ style: 'original', size: 1024 })
for (const mapPath of mapPaths) {
  start = performance.now()
  const map = new MapRenderer(readFileSync(mapPath))
  const missing = map.loadAssets(storage, opts)
  const png = map.renderPng(opts)
  map.free()
  const outPath = `${path.basename(mapPath, path.extname(mapPath))}.png`
  writeFileSync(outPath, png)
  console.log(
    `wrote ${outPath} in ${(performance.now() - start).toFixed(0)} ms` +
      (missing.length ? ` (${missing.length} art assets missing)` : ''),
  )
}
console.log(`cache: ${(storage.cachedBytes / 1e6).toFixed(1)} MB`)
storage.free()
