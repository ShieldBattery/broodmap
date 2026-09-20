import assert from 'node:assert/strict'
import test from 'node:test'
import { hasBuildableTerrainNearMidpoint } from './boundary-buildability.js'

test('buildability hint uses two-tile distance to cell rectangles, including exact contact', () => {
  const width = 40
  const flags = new Uint8Array(width * width)
  const section = { endpoints: [[160, 80], [160, 240]] }
  assert.equal(hasBuildableTerrainNearMidpoint(section, width, flags), false)
  // Left cell edge is exactly 64px from the midpoint. Other flags are irrelevant.
  flags[20 * width + 28] = 2 | 32 | 16
  assert.equal(hasBuildableTerrainNearMidpoint(section, width, flags), true)
  flags.fill(0)
  // Right cell edge touches the other side of the same circle.
  flags[20 * width + 11] = 2
  assert.equal(hasBuildableTerrainNearMidpoint(section, width, flags), true)
  flags.fill(0)
  flags[20 * width + 29] = 2
  assert.equal(hasBuildableTerrainNearMidpoint(section, width, flags), false)
  flags.fill(0)
  // Inside the search square but outside the circle.
  flags[28 * width + 28] = 2
  assert.equal(hasBuildableTerrainNearMidpoint(section, width, flags), false)
})

test('map edges are clipped and nearby buildability does not depend on endpoint ordering', () => {
  const flags = new Uint8Array(8 * 8)
  flags[0] = 2
  const section = { endpoints: [[0, 0], [0, 32]] }
  assert.equal(hasBuildableTerrainNearMidpoint(section, 8, flags), true)
  assert.equal(hasBuildableTerrainNearMidpoint({ endpoints: [...section.endpoints].reverse() }, 8, flags), true)
  flags[0] = 1 | 4 | 32
  assert.equal(hasBuildableTerrainNearMidpoint(section, 8, flags), false)
})
