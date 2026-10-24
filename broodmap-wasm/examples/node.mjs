// Node usage example: the same prefetch -> render flow as the browser, with assets read from a
// `fetch-assets` directory on disk. Build the package first: `pnpm run build`.
//
//   node examples/node.mjs <map.scm> <assets-dir> [out.png]
import { readFileSync, writeFileSync } from 'node:fs'
import { MapRenderer } from '@shieldbattery/broodmap'

const [mapPath, assetsDir, outPath = 'preview.png'] = process.argv.slice(2)
if (!mapPath || !assetsDir) {
  console.error('usage: node examples/node.mjs <map.scm> <assets-dir> [out.png]')
  process.exit(1)
}

const map = new MapRenderer(readFileSync(mapPath))
console.log(`map: ${map.widthTiles}x${map.heightTiles} tiles`)

const opts = JSON.stringify({ style: 'original', size: 1024 })
let fetched = 0
let skipped = 0
const addAll = (paths) => {
  for (const path of paths) {
    try {
      map.addAsset(path, readFileSync(`${assetsDir}/${path}`))
      fetched++
    } catch {
      skipped++ // a missing art asset degrades one drawable, like the native renderer
    }
  }
}
addAll(map.requiredAssets(opts))
addAll(map.requiredGraphics(opts))
console.log(`assets: ${fetched} fetched, ${skipped} skipped`)

writeFileSync(outPath, Buffer.from(map.renderPng(opts)))
console.log(`wrote ${outPath}`)
