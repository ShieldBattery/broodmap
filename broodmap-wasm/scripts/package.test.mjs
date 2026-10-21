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

test('rejects files that are not maps', () => {
  const { summarizeMapJson } = require('@shieldbattery/broodmap')
  assert.throws(() => summarizeMapJson(new Uint8Array([1, 2, 3])))
})
