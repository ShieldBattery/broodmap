// The browser flow, end to end: parse the map in WASM, ask it which assets it needs (the
// two-round prefetch — asset keys are CASC catalog paths, which double as URL suffixes under
// the asset base), fetch them, render synchronously from memory.
import init, { MapRenderer } from '../pkg/broodmap_wasm.js'

const el = (id) => document.getElementById(id)
const status = (text) => {
  el('status').textContent = text
}

await init()

let map = null
let mapName = ''

el('map').addEventListener('change', async (event) => {
  const file = event.target.files[0]
  if (!file) return
  try {
    map = new MapRenderer(new Uint8Array(await file.arrayBuffer()))
    mapName = file.name
    el('render').disabled = false
    status(`${file.name}: ${map.widthTiles}x${map.heightTiles} tiles. Ready to render.`)
    // The zero-asset minimap needs nothing fetched, so it can draw immediately.
    drawMinimap()
  } catch (e) {
    map = null
    el('render').disabled = true
    status(`Failed to parse ${file.name}: ${e}`)
  }
})

el('render').addEventListener('click', () => render().catch((e) => status(`Render failed: ${e}`)))

function optionsJson() {
  return JSON.stringify({
    style: el('style').value,
    size: Number(el('size').value) || 1024,
  })
}

async function fetchInto(paths, base, counters) {
  for (const path of paths) {
    const response = await fetch(`${base}/${path}`)
    if (!response.ok) {
      // A missing art asset degrades exactly one drawable, matching the native renderer;
      // missing round-1 tables will surface as an error from the render itself.
      counters.skipped.push(path)
      continue
    }
    map.addAsset(path, new Uint8Array(await response.arrayBuffer()))
    counters.fetched++
  }
}

async function render() {
  const base = el('assetBase').value.replace(/\/+$/, '')
  const opts = optionsJson()
  const counters = { fetched: 0, skipped: [] }

  status(`Fetching assets for ${mapName}...`)
  // Round 2 (requiredGraphics) resolves through the .dat tables, so round 1 must be fetched
  // and added first.
  await fetchInto(map.requiredAssets(opts), base, counters)
  await fetchInto(map.requiredGraphics(opts), base, counters)

  status(`Rendering (${counters.fetched} assets)...`)
  const started = performance.now()
  const image = map.renderRgba(opts)
  const elapsed = Math.round(performance.now() - started)

  drawTo(el('preview'), image)

  const notes = [
    `${mapName}: rendered ${image.width}x${image.height} in ${elapsed} ms (${counters.fetched} assets).`,
    ...(counters.skipped.length ? [`Skipped ${counters.skipped.length} missing assets.`] : []),
    ...image.warnings,
  ]
  status(notes.join('\n'))
}

function drawMinimap() {
  drawTo(el('minimap'), map.minimapRgba(JSON.stringify({ scale: 2 })))
}

function drawTo(canvas, image) {
  canvas.width = image.width
  canvas.height = image.height
  canvas
    .getContext('2d')
    .putImageData(new ImageData(new Uint8ClampedArray(image.data), image.width, image.height), 0, 0)
}
