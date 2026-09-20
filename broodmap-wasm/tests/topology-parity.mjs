// Optional real-asset regression: node tests/topology-parity.mjs <asset-directory> <map>...
// Build pkg-node first. Assets/maps are local inputs and are not bundled with the repository.
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { resolve, join, basename } from 'node:path'
import { createRequire } from 'node:module'
import { analyzeBaseAreas } from '../examples/base-areas.js'
import { analyzeBaseAreas as reference } from './topology-reference/base-areas.js'

const [assets, ...maps] = process.argv.slice(2)
if (!assets || !maps.length) throw new Error('Usage: node tests/topology-parity.mjs <asset-directory> <map>...')
const { MapRenderer } = createRequire(import.meta.url)('../pkg-node/broodmap_wasm.js')
for (const path of maps) {
  const map = new MapRenderer(readFileSync(resolve(path)))
  let analysis
  try {
    analysis = map.analyzeMap(...map.requiredMapAnalysisAssets().map(p => readFileSync(join(assets, p))))
    const bases = JSON.parse(analysis.findBasesJson('{}')).bases
    const started = performance.now()
    const actual = await analyzeBaseAreas(analysis, bases, 25)
    const rustMs = performance.now() - started
    const referenceStart = performance.now()
    const expected = await reference(analysis, bases, 25)
    const referenceMs = performance.now() - referenceStart
    const { observations, ...compatible } = actual
    assert.deepEqual(compatible, expected, basename(path))
    assert.equal(observations.length, actual.observationCount)
    const reordered = await analyzeBaseAreas(analysis, [...bases].reverse(), 25)
    assert.deepEqual(reordered, actual, `${basename(path)} reordered base input`)
    console.log(`${basename(path)}: exact match; ${actual.boundaries.length} boundaries; Rust ${rustMs.toFixed(0)}ms, reference ${referenceMs.toFixed(0)}ms`)
  } finally {
    analysis?.free()
    map.free()
  }
}
