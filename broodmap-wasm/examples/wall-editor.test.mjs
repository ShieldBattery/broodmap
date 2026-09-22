import assert from 'node:assert/strict'
import test from 'node:test'
import {
  MAX_WALL_BUILDINGS, addWallBuilding, clampWallWindow, defaultWallEndpoints, pointInHalfOpenRect,
  removeWallBuildingAt, wallQuery,
} from './wall-editor.js'

test('wall window clamps its requested center and never exceeds the supported bound', () => {
  assert.deepEqual(clampWallWindow([20, 30], [2000, 1500]), [0, 0, 768, 768])
  assert.deepEqual(clampWallWindow([1980, 1490], [2000, 1500], 2000), [976, 476, 2000, 1500])
})

test('entrance defaults lie on opposite sides of the entrance and inside its window', () => {
  const window = [0, 0, 768, 768]
  const [a, b] = defaultWallEndpoints([[300, 200], [300, 400]], window)
  assert.ok(a[0] > 300 && b[0] < 300)
  assert.ok(pointInHalfOpenRect(a, window))
  assert.ok(pointInHalfOpenRect(b, window))
})

test('placements use build tiles, remove their topmost footprint, and serialize only the worker query', () => {
  const depot = { id: 106, name: 'Command Center', footprint: [4, 3], collision: [0, 0, 128, 96] }
  const first = addWallBuilding([], depot, [10, 11])
  const second = addWallBuilding(first, depot, [11, 12])
  assert.equal(removeWallBuildingAt(second, [12, 13]).length, 1)
  assert.deepEqual(wallQuery([0, 0, 768, 768], [[1, 2], [3, 4]], { collision: [1, 2, 3, 4] }, first), {
    window: [0, 0, 768, 768], start: [1, 2], end: [3, 4], mover: [1, 2, 3, 4],
    buildings: [{ tile: [10, 11], footprint: [4, 3], collision: [0, 0, 128, 96] }],
  })
})

test('placement state caps the worker request at 32 buildings', () => {
  const pylon = { id: 156, name: 'Pylon', footprint: [2, 2], collision: [16, 16, 16, 16] }
  let buildings = []
  for (let index = 0; index < MAX_WALL_BUILDINGS + 1; index++) buildings = addWallBuilding(buildings, pylon, [index, 0])
  assert.equal(buildings.length, MAX_WALL_BUILDINGS)
})

test('center snapping handles even and odd footprints and map edges', async () => {
  const { centeredBuildingTile } = await import('./wall-editor.js')
  assert.deepEqual(centeredBuildingTile([320, 320], [4, 2]), [8, 9])
  assert.deepEqual(centeredBuildingTile([336, 320], [3, 2]), [9, 9])
  assert.deepEqual(centeredBuildingTile([0, 0], [3, 2]), [0, 0])
  assert.deepEqual(centeredBuildingTile([351, 320], [3, 2]), [9, 9])
})

test('preview distinguishes terrain, obstacle toggle, proposed occupancy and map edges', async () => {
  const { placementPreviewCells } = await import('./wall-editor.js')
  const terrain = { widthWalkTiles: 8, heightWalkTiles: 8, flags: new Uint8Array(64).fill(3), obstaclesEnabled: true }
  terrain.flags[0] = 35
  assert.equal(placementPreviewCells([0, 0], [1, 1], terrain, [])[0].valid, false)
  terrain.obstaclesEnabled = false
  assert.equal(placementPreviewCells([0, 0], [1, 1], terrain, [])[0].valid, true)
  terrain.flags[0] = 1
  assert.equal(placementPreviewCells([0, 0], [1, 1], terrain, [])[0].valid, false)
  assert.equal(placementPreviewCells([1, 1], [1, 1], terrain, [{ tile: [1, 1], footprint: [1, 1] }])[0].valid, false)
  assert.equal(placementPreviewCells([2, 0], [1, 1], terrain, [])[0].valid, false)
})


test('map selection picks the nearest finite choke segment and has stable ties', async () => {
  const { nearestWallSpan } = await import('./wall-editor.js')
  const spans = [{ endpoints: [[0, 0], [100, 0]] }, { endpoints: [[10, 25], [20, 25]] }]
  assert.equal(nearestWallSpan([0, 1], spans), 0)
  assert.equal(nearestWallSpan([101, 2], spans), 0)
  assert.equal(nearestWallSpan([12, 24], spans), 1)
  assert.equal(nearestWallSpan([1000, 1000], spans), 0)
  assert.equal(nearestWallSpan([0, 0], [...spans, spans[0]]), 0)
  assert.equal(nearestWallSpan([0, 0], []), -1)
})

test('ramp-side selection uses the matching ramp end and leaves ordinary mouths unrestricted', async () => {
  const { wallSearchSide } = await import('./wall-editor.js')
  const lower = { kind: 'ramp', rampId: 2, rampEnd: 'lower', lowerElevation: 0, upperElevation: 1 }
  const upper = { ...lower, rampEnd: 'upper' }
  const other = { ...upper, rampId: 1 }
  assert.deepEqual(wallSearchSide(lower, [other, lower, upper], 'high'), { span: upper, elevation: 1 })
  assert.deepEqual(wallSearchSide(upper, [other, lower, upper], 'low'), { span: lower, elevation: 0 })
  assert.deepEqual(wallSearchSide(lower, [lower, upper], 'any'), { span: lower, elevation: null })
  const ordinary = { kind: 'entrance' }
  assert.deepEqual(wallSearchSide(ordinary, [lower, upper], 'high'), { span: ordinary, elevation: null })
})

test('placement area remains within a shifted window, even close to map edges', async () => {
  const { wallPlacementArea } = await import('./wall-editor.js')
  assert.deepEqual(wallPlacementArea([32, 64, 800, 832], [[32, 64], [64, 96]], 512), [32, 64, 544, 576])
  assert.deepEqual(wallPlacementArea([0, 0, 128, 128], [[10, 10], [100, 100]], 512), [0, 0, 128, 128])
})


test('entrance-relative reach permits footprint overhang and stops at finite endpoints', async () => {
  const { wallPlacementSite, pointInWallPlacementSite } = await import('./wall-editor.js')
  const span = { endpoints: [[1896, 112], [2000, 216]] }
  const site = wallPlacementSite(span, 384)
  assert.equal(site.maxDistancePixels, 192)
  assert.equal(wallPlacementSite(null, 384), null)
  // Python's known Barracks center is within reach, even though its right edge (2176)
  // is outside the old centered twelve-tile box (right edge 2140).
  assert(pointInWallPlacementSite([2112, 176], site))
  assert(!pointInWallPlacementSite([2500, 716], site))
  const horizontal = wallPlacementSite({ endpoints: [[100, 100], [200, 100]] }, 64)
  assert(pointInWallPlacementSite([232, 100], horizontal))
  assert(!pointInWallPlacementSite([233, 100], horizontal))
  assert(pointInWallPlacementSite([150, 132], horizontal))
  assert(!pointInWallPlacementSite([150, 133], horizontal))
  const reverse = { ...horizontal, entrance: [...horizontal.entrance].reverse() }
  assert.equal(pointInWallPlacementSite([150, 132], reverse), pointInWallPlacementSite([150, 132], horizontal))
})


test('placement reach uses the local mask origin and stride and hides missing previews', async () => {
  const { pointInWallPlacementReach } = await import('./wall-editor.js')
  const reach = { origin: [7, 11], width: 3, height: 2, cells: new Uint8Array([1, 0, 0, 0, 1, 0]) }
  assert.equal(pointInWallPlacementReach([56, 88], reach), true)
  assert.equal(pointInWallPlacementReach([63, 95], reach), true)
  assert.equal(pointInWallPlacementReach([64, 88], reach), false)
  assert.equal(pointInWallPlacementReach([64, 96], reach), true)
  assert.equal(pointInWallPlacementReach([55, 88], reach), false)
  assert.equal(pointInWallPlacementReach([80, 88], reach), false)
  assert.equal(pointInWallPlacementReach([56, 104], reach), false)
  assert.equal(pointInWallPlacementReach([56, 88], null), false)
})
