// Checks the built package as it's published: run `pnpm run build` first.
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { createRequire } from 'node:module'
import path from 'node:path'
import { test } from 'node:test'

const require = createRequire(import.meta.url)
const mapPath = path.resolve(import.meta.dirname, '../../broodmap/assets/lt.scm')

test('summarizes a map through the CommonJS entry point', () => {
  const { summarizeMapJson } = require('@shieldbattery/broodmap')
  const summary = JSON.parse(summarizeMapJson(readFileSync(mapPath)))
  assert.equal(summary.width, 128)
  assert.equal(summary.height, 128)
  assert.equal(summary.players.length, 8)
  assert.equal(summary.forces.length, 4)
  assert.equal(summary.startLocations, 4)
  assert.equal(summary.usesEud, false)
})

test('summarizes a map through the ES module entry point', async () => {
  const { summarizeMapJson } = await import('@shieldbattery/broodmap')
  const summary = JSON.parse(summarizeMapJson(readFileSync(mapPath)))
  assert.equal(summary.width, 128)
})

test('takes resource limits as options', () => {
  const { summarizeMapJson } = require('@shieldbattery/broodmap')
  const bytes = readFileSync(mapPath)
  assert.equal(JSON.parse(summarizeMapJson(bytes, '{"maxChkBytes":268435456}')).width, 128)
  assert.throws(() => summarizeMapJson(bytes, '{"maxChkBytes":16}'))
})

test('rejects files that are not maps', () => {
  const { summarizeMapJson } = require('@shieldbattery/broodmap')
  assert.throws(() => summarizeMapJson(new Uint8Array([1, 2, 3])))
})

test('CascStorage rejects incomplete file callbacks and directories that are not installs', async () => {
  const { CascStorage } = require('@shieldbattery/broodmap')
  const { nodeCascFiles } = await import('../examples/casc-files.mjs')
  const { open, ...noOpen } = nodeCascFiles(import.meta.dirname)
  assert.throws(() => new CascStorage(noOpen), /no `open` function/)
  assert.throws(() => new CascStorage(nodeCascFiles(import.meta.dirname)), /failed to open CASC/)
})

test('renders from a SC:R install through CascStorage', async t => {
  const installDir = process.env.BROODMAP_TEST_SCR_DIR
  if (!installDir) {
    t.skip('BROODMAP_TEST_SCR_DIR not set')
    return
  }
  const { CascStorage, MapRenderer } = require('@shieldbattery/broodmap')
  const { nodeCascFiles } = await import('../examples/casc-files.mjs')
  const storage = new CascStorage(nodeCascFiles(installDir))
  assert.match(storage.version, /^\d+\.\d+/)

  const opts = JSON.stringify({ style: 'original', size: 256 })
  const render = () => {
    const map = new MapRenderer(readFileSync(mapPath))
    assert.deepEqual(map.loadAssets(storage, opts), [])
    const png = map.renderPng(opts)
    map.free()
    return png
  }
  const first = render()
  assert.deepEqual([...first.subarray(0, 4)], [0x89, 0x50, 0x4e, 0x47])
  const cached = storage.cachedBytes
  assert.ok(cached > 0)
  assert.deepEqual(render(), first)
  assert.equal(storage.cachedBytes, cached, 'a second render of the same map reads only from the cache')
  storage.free()
})
