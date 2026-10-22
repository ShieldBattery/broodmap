// Checks the package as it's published: run `pnpm run build` first.
import assert from 'node:assert/strict'
import { execSync } from 'node:child_process'
import { readFileSync } from 'node:fs'
import { createRequire } from 'node:module'
import path from 'node:path'
import { test } from 'node:test'
import * as esm from '@shieldbattery/broodmap'
import { nodeCascFiles } from '../examples/casc-files.mjs'

const require = createRequire(import.meta.url)
const packageDir = path.resolve(import.meta.dirname, '..')
const lt = readFileSync(path.join(packageDir, '../broodmap/assets/lt.scm'))

test('npm publishes one build, the Node entry and the licenses', () => {
  const [packed] = JSON.parse(
    execSync('npm pack --dry-run --ignore-scripts --json', { cwd: packageDir, encoding: 'utf8' }),
  )
  assert.deepEqual(packed.files.map(file => file.path).sort(), [
    'LICENSE-APACHE',
    'LICENSE-MIT',
    'README.md',
    'node.js',
    'package.json',
    'pkg/broodmap_wasm.d.ts',
    'pkg/broodmap_wasm.js',
    'pkg/broodmap_wasm_bg.wasm',
    'pkg/broodmap_wasm_bg.wasm.d.ts',
  ])
})

test('the Node entry is ready on import, and init is a no-op', async () => {
  assert.equal(JSON.parse(esm.summarizeMapJson(lt)).width, 128)
  await esm.default()
  esm.initSync()
  assert.equal(JSON.parse(esm.summarizeMapJson(lt)).height, 128)
})

test('require() loads the same module as import', () => {
  const commonjs = require('@shieldbattery/broodmap')
  assert.equal(commonjs.summarizeMapJson, esm.summarizeMapJson)
})

test('summarizes a map', () => {
  const summary = JSON.parse(esm.summarizeMapJson(lt))
  assert.equal(summary.players.length, 8)
  assert.equal(summary.forces.length, 4)
  assert.equal(summary.startLocations, 4)
  assert.equal(summary.usesEud, false)
  assert.equal(summary.triggerCount, 3)
  assert.ok(!summary.triggerActionKinds.includes('NoAction'))
})

test('MapRenderer summarizes the map it parsed', () => {
  const map = new esm.MapRenderer(lt)
  try {
    assert.deepEqual(JSON.parse(map.summaryJson()), JSON.parse(esm.summarizeMapJson(lt)))
  } finally {
    map.free()
  }
})

test('takes resource limits as options', () => {
  assert.equal(JSON.parse(esm.summarizeMapJson(lt, '{"maxChkBytes":268435456}')).width, 128)
  assert.throws(() => esm.summarizeMapJson(lt, '{"maxChkBytes":16}'))
  assert.throws(() => new esm.MapRenderer(lt, '{"maxChkBytes":16}'))
  assert.throws(() => esm.summarizeMapJson(lt, '{"maxChkByte":16}'))
})

test('rejects files that are not maps', () => {
  assert.throws(() => esm.summarizeMapJson(new Uint8Array([1, 2, 3])))
})

test('CascStorage rejects incomplete file callbacks and directories that are not installs', () => {
  const { open, ...noOpen } = nodeCascFiles(import.meta.dirname)
  assert.throws(() => new esm.CascStorage(noOpen), /no `open` function/)
  assert.throws(() => new esm.CascStorage(nodeCascFiles(import.meta.dirname)), /failed to open CASC/)
})

test('renders from a SC:R install through CascStorage', async t => {
  const installDir = process.env.BROODMAP_TEST_SCR_DIR
  if (!installDir) {
    t.skip('BROODMAP_TEST_SCR_DIR not set')
    return
  }
  const storage = new esm.CascStorage(nodeCascFiles(installDir))
  assert.match(storage.version, /^\d+\.\d+/)

  const opts = JSON.stringify({ style: 'original', size: 256 })
  const render = () => {
    const map = new esm.MapRenderer(lt)
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
